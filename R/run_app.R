#' Run the Shiny Application
#'
#' @description
#'
#' This function serves as an entry point for the applications created by the user.
#'
#' @param data the list of datasets to be used in the Shiny application. This data parameter can be defined as:
#'
#'    - a named list of lists in which each of the lists will be a list of data.frames. Each of the lists of data.frames
#'     contains a domain, a structure similar to that returned in a `dv.loader::load_data` call.
#'
#'    - a named list of functions in which each of the functions will return a list of data.frames.
#' 
#'    All `character` variables will be automatically mapped into `factors`.
#'
#' @param module_list a list of the modules to be included in the Shiny application
#' @param title title to be displayed in the browser tab
#' @param filter_data a string indicating which of the loaded datasets is used for filtering.
#' @param filter_key a string specifying a common field across all datasets that will be used to expand the filtering.
#'  Default = "USUBJID" or NULL if no data = NULL
#' @param startup_msg a message to be displayed at the start of the application. It can be either NULL or a modal
#' message defined with shiny::modalDialog.
#' @param azure_options A named vector/list containing the options for the Azure Authentication:
#' Required fields: redirect, resource, tenant, app, version, password.
#' See AzureAuth::build_authorization_uri for these. If NULL then application will not be secure.
#' Note that azure_options is only required when the applications handles the authentication by itself. It is not
#' required in other environments such as POSIT Connect or other environments where the authentication is handled
#' externally.
#' @param reload_period Either a lubridate object to specify a duration
#' or a positive numeric value which is then interpreted as a lubridate duration object in days. By default NULL
#' @param enable_dataset_filter A boolean flag indicating if dataset filters are enabled. The default value is FALSE.
#' @param .launch by default it should always be TRUE. It should only be false for debugging and testing.
#' When TRUE it will return the app. When FALSE it will return the options with which the app will be launched.
#' @inheritParams shiny::shinyApp
#'
#'
#' @export
#'

run_app <- function(data = NULL,
                    module_list = list(),
                    title = "Untitled",
                    filter_data = NULL,
                    filter_key = if (!is.null(data)) {
                      "USUBJID"
                    } else {
                      NULL
                    },
                    startup_msg = NULL,
                    azure_options = NULL,
                    reload_period = NULL,
                    enableBookmarking = "server", # nolint
                    enable_dataset_filter = FALSE,
                    .launch = TRUE) {
  check_deprecated_calls(filter_data)

  dataset_lists <- data

  if (is.null(azure_options)) {
    app_args <- list(
      ui_func = app_ui,
      srv_func = app_server,
      options = list()
    )
  } else {
    app_args <- build_secure_arguments(azure_options, app_ui, app_server)
  }

  config <- list()
  config[["module_info"]] <- check_resolved_modules(process_module_list(module_list))
  # The automatic mapping will influence reporting when it is implemented in the future
  config[["data"]] <- char_vars_to_factor_vars_dataset_lists(check_data(dataset_lists))
  config[["filter_data"]] <- check_filter_data(filter_data, dataset_lists)
  config[["filter_key"]] <- check_filter_key(filter_key, dataset_lists)
  config[["startup_msg"]] <- check_startup_msg(startup_msg)
  config[["title"]] <- title
  config[["reload_period"]] <- get_reload_period(check_reload_period(reload_period))
  config[["enable_dataset_filter"]] <- enable_dataset_filter
  
  # NEW DATAFILTER OPTIONS
  config[["dv.manager.use.blockly.filter"]] <- isTRUE(getOption("dv.manager.use.blockly.filter"))
  config[["dv.manager.blockly.predefined.filter"]] <- getOption("dv.manager.blockly.predefined.filter")
  # NEW DATAFILTER OPTIONS (F)

  check_meta_mtime_attribute(dataset_lists)

  # Add logging
  call_args <- list(
    ui = function(req) {
      (log_with_logging_handlers(app_args[["ui_func"]]))(req)
    },
    # We explicitely call server with input, output, session because otherwise shiny does not pass the session argument
    server = function(input, output, session) {
      (log_with_logging_handlers(app_args[["srv_func"]]))(input, output, session)
    },
    options = app_args[["options"]],
    enableBookmarking = enableBookmarking
  )

  if (.launch) {
    app <- do.call(shiny::shinyApp, call_args)
    app <- set_config(app, config)
    app
  } else {
    c(app_args, list(config = config))
  }
}

#' Securize an application using an Azure
#'
#' @param app_ui,app_server the shiny ui and server functions
#'
#' @inheritParams run_app
#'
#' @return
#'
#' A list with the entries `ui_func` and `srv_func` and `options` that contains the securized version of the app.
#'
#' @export
#'

build_secure_arguments <- function(azure_options, app_ui, app_server) {
  check_azure_options(azure_options)

  port <- as.numeric(httr::parse_url(azure_options[["redirect"]])[["port"]])

  sec_ui <- function(request) {
    opts <- shiny::parseQueryString(request[["QUERY_STRING"]])

    # ====================================================
    # SECURE AUTHENTICATION STRATEGY
    # ====================================================
    # The secure authentication is done in three steps:

    # 1. No code parameter is passed in to the URL parameters, therefore we create the authorization URL and redirect
    # to that URL. We encode the URL parameters of the original petition into the state URL parameter when building the
    # authentication URL. Server part does not run.

    # 2. The user has already authenticated using the SSO, we are redirected to the app again with a code and a state
    # URL parameter. We DO NOT try to get a token with the provided code right now in the server, because the code
    # is only usable once and the URL we are petitioning would not have the original URL parameters (including
    # bookmarking) in place yet. Hijacking Shiny bookmarking is not viable. Therefore we construct a new URL with the
    # code obtained during Azure authentication plus the original URL parameters, and we redirect to this new URL.
    # If state parameter is an empty string we skip this step.

    # 3. Now, the URL parameters (including bookmarking) are available when the app is launched. Now, we validate the
    # code in the URL by getting a token in the server part and if it is valid we run the server and the application
    # starts.


    ##### Step 1
    if (is.null(opts[["code"]])) {
      shinyjs::useShinyjs()
      auth_uri <- AzureAuth::build_authorization_uri(
        resource = azure_options[["resource"]],
        tenant = azure_options[["tenant"]],
        app = azure_options[["app"]],
        redirect_uri = azure_options[["redirect"]],
        version = azure_options[["version"]],
        state = request[["QUERY_STRING"]]
      )
      redir_js <- sprintf("location.replace(\"%s\");", auth_uri)
      shiny::tags[["script"]](shiny::HTML(redir_js))
    } else {
      if (!is.null(opts[["state"]])) {
        ##### Step 2
        if (opts[["state"]] != "") {
          query <- paste0(
            substr(opts[["state"]], 1, 1),
            "code=", opts[["code"]], "&",
            substr(opts[["state"]], 2, nchar(opts[["state"]]))
          )
        } else {
          query <- paste0("/?code=", opts[["code"]])
        }

        url_query <- paste0(azure_options[["redirect"]], query)

        redir_js <- sprintf("location.replace(\"%s\");", url_query)
        shiny::tags[["script"]](shiny::HTML(redir_js))
      } else {
        ##### Step 3
        shiny::tagList(
          shinyjs::useShinyjs(),
          app_ui(request)
        )
      }
    }
  }

  sec_server <- function(input, output, session) {
    opts <- shiny::parseQueryString(shiny::isolate(session$clientData$url_search))
    if (is.null(opts[["code"]]) || !is.null(opts[["state"]])) {
      #### Step 1 and 2
      return()
    } else {
      #### Step 3
      shinyjs::runjs("{
                      let url = new URL(location.href);
                      url.searchParams.delete('code');
                      history.replaceState({}, '', url.href);
                      }
                     ")

      token <- AzureAuth::get_azure_token(
        resource = azure_options[["resource"]],
        tenant = azure_options[["tenant"]],
        app = azure_options[["app"]],
        password = azure_options[["password"]],
        auth_type = "authorization_code",
        authorize_args = list(redirect_uri = azure_options[["redirect"]]),
        version = azure_options[["version"]],
        use_cache = FALSE,
        auth_code = opts[["code"]]
      )

      if (AzureAuth::is_azure_token(token) && token$validate()) {
        app_server(input, output, session)
      } else {
        rlang::abort("Error authenticating: returned token is not valid")
      }
    }
  }

  list(
    ui_func = sec_ui,
    srv_func = sec_server,
    options = list(port = port)
  )
}

run_app_dev_filter <- function(..., state = NULL) {
  msg <- paste(
    "##############################################################",
    "# You are using the application using an experimental filter #",
    "# If this is not intended, please use the regular `run_app`  #",
    "#                                                            #",
    "# This function is NOT SUPPORTED for production              #",
    "# This function WILL BREAK and WILL DISAPPEAR without notice #",    
    "##############################################################",
    sep = "\n"
  )
  warning(msg)

  old_use <- getOption("dv.manager.use.blockly.filter")
  old_state <- getOption("dv.manager.blockly.predefined.filter")
  on.exit({
    message(paste("Reset to", old_use))
    options(dv.manager.use.blockly.filter = old_use)
  }, add = TRUE)
  on.exit(options(dv.manager.blockly.predefined.filter = old_state), add = TRUE)
  options(dv.manager.use.blockly.filter = TRUE)
  options(dv.manager.blockly.predefined.filter = state)
  run_app(...)
}
