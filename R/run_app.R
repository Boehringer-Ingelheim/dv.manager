#' Run the Shiny Application
#'
#' @description
#'
#' This function serves as an entry point for the applications created by the user.
#'
#' @param data the list of datasets to be used in the Shiny application. This data parameter can be defined as:
#'
#'    - a named list of lists in which each of the lists will be a list of data.frames. Each of the lists of data.frames
#'     contains a domain, a structure similar to that returned in a \code{dv.loader::load_data} call.
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
#' @param reload_period Either a lubridate object to specify a duration
#' or a positive numeric value which is then interpreted as a lubridate duration object in days. By default NULL
#' @param filter_type **DEPRECATED** Indicates which filter type, `simple`, `datasets`, `development`, will be used in the application.
#' @param filter_default_state A JSON string or file (usually exported from the app) that describes the default state of the filter (Only available for `development` filters).
#' @param enable_dataset_filter **DEPRECATED** A boolean flag indicating if dataset filters are enabled. The default value is FALSE.
#' @param enable_subgroup  A boolean flag indicating if subgroup controls are enabled. The default value is FALSE.
#' @param .launch by default it should always be TRUE. It should only be false for debugging and testing.
#' When TRUE it will return the app. When FALSE it will return the options with which the app will be launched.
#' @inheritParams shiny::shinyApp
#'
#'
#' @export
#'

run_app <- function(
  data = NULL,
  module_list = list(),
  title = "Untitled",
  filter_data = NULL,
  filter_key = if (!is.null(data)) {
    "USUBJID"
  } else {
    NULL
  },
  startup_msg = NULL,
  reload_period = NULL,
  enableBookmarking = "server", # nolint
  filter_type = "simple",
  enable_dataset_filter = NULL,
  enable_subgroup = FALSE,
  filter_default_state = NULL,
  .launch = TRUE
) {
  dataset_lists <- data

  if (!missing(enable_dataset_filter)) {
    stop("`enable_dataset_filter` argument has been removed. This error will disappear in future releases")
  }

  if (!missing(filter_type)) {
    stop(
      "`filter_type` argument has been removed. Filter types cannot be selected any more. This error will disappear in future releases"
    )
  }

  app_args <- list(
    ui_func = app_ui,
    srv_func = app_server,
    options = list()
  )

  config <- list()
  config[["module_info"]] <- check_resolved_modules(process_module_list(module_list))

  # The automatic mapping will influence reporting when it is implemented in the future
  config[["data"]] <- local({
    check_data(dataset_lists)
    d <- char_vars_to_factor_vars_dataset_lists(dataset_lists)
    d <- ungroup2df_datasets_dataset_lists(d)
    d
  })

  config[["filter_data"]] <- check_filter_data(filter_data, dataset_lists)
  config[["filter_key"]] <- check_filter_key(filter_key, dataset_lists)
  config[["startup_msg"]] <- check_startup_msg(startup_msg)
  config[["title"]] <- title
  config[["reload_period"]] <- get_reload_period(check_reload_period(reload_period))
  config[["filter_info"]] <- check_set_filter_info(filter_default_state)
  config[["subgroup"]] <- check_set_subgroup_info(enable_subgroup)

  assert_not_shiny_1_11_0()

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
