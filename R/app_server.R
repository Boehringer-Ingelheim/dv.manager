#' Server side of the dv.manager
#'
#' @section data:
#' *data* is the fundamental data source for the application launched by the dv.manager
#'   - This data source is a named list of:
#'      - lists of data.frames as loaded by dv.loader
#'      - of functions that when called return a list of data.frames
#'   - Entries of the list will selected and then each of the data.frames will be dispatched to each module according to
#'    the code that defines the application
#'   - The data dispatched to the modules will be filtered according to a global *dv.filter* (see filter_data section)
#'   - The name of the selected dataset will appear in the sidebar, according to the name provided the list
#'   - The date of the dataset will be automatically displayed by selecting the earliest and the latest date of the
#'    composing data.frames.
#'     - *dv.manager* assumes that all data.frames within a dataset have an attribute *meta*, and within it,
#'      it will look for the entry *mtime*
#'     - This assumption is met when the dataset is loaded directly with dv.loader, nonetheless, this attributes may
#'      need to be created for new derived data.frames during preprocessing
#'     It is the programmer obligation to check that all datasets have this attribute and that is correct.
#'
#' @section filter_data:
#' The datasets in *data* will be filtered in the following way:
#'   1. A dataset for global filtering is defined by *filter_data*
#'   1. This dataset is filtered by *dv.filter* and the user input
#'   1. The remaining USUBJIDs (by default, see *filter_key* field in run_app) are used to filter the rest of the
#'    datasets
#'
#' @section module_list:
#' *module_list* will contain a list of the modules to be used in the application. Each entry in the list will contain
#'  the following fields:
#' - The name of each entry of the list (unique), will be used in the selection menus, therefore must be meaningful and
#'  intended for human use.
#' - **ui**: A UI function that will need a single parameter, id. Almost always it will be the other entry module_id
#' - **server**: A function that will receive a single parameter. See `vignette("arguments_from_module_manager")`
#' - **module_id**: A unique alphanumeric string that will identify the module, it will be used internally therefore it
#'  is intended for machine use.
#' Additionally, it will expect a value **input$mod_select** to identify which module is currently selected. Here it is
#'  only used to produce a log with the selection
#' but it will be used for selecting in the UI conditional panels.
#'
#' @param id id when app_server is used as a module
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#'
#' @keywords internal

app_server <- function(input = NULL, output = NULL, session = NULL) {
  opts <- list(
    "module_list" = get_config("module_list"),
    "data" = get_config("data"),
    "filter_data" = get_config("filter_data"),
    "filter_key" = get_config("filter_key"),
    "startup_msg" = get_config("startup_msg"),
    "reload_period" = get_config("reload_period")
  )

  app_server_(input, output, session, opts)
}

app_server_module <- function(id) {
  opts <- list(
    "module_list" = get_config("module_list"),
    "data" = get_config("data"),
    "filter_data" = get_config("filter_data"),
    "filter_key" = get_config("filter_key"),
    "startup_msg" = get_config("startup_msg"),
    "reload_period" = get_config("reload_period")
  )
  shiny::moduleServer(id = id, module = function(input, output, session) app_server_(input, output, session, opts))
}

app_server_ <- function(input, output, session, opts) {
  ns <- session[["ns"]]

  # Inject tools available for the rest of modules
  session$userData$manager_utils <- list(
    switch_tab = function(selected) {
      .Defunct(
        "affm[[\"utils\"]][[\"switch_function\"]]",
        msg = "Switch function has been moved to the list of arguments passed to the module"
      )
      shiny::updateTabsetPanel(session, "main_tab_panel", selected)
    }
  )

  module_list <- opts[["module_list"]]
  data <- opts[["data"]]
  filter_data <- opts[["filter_data"]]
  filter_key <- opts[["filter_key"]]
  startup_msg <- opts[["startup_msg"]]
  reload_period <- opts[["reload_period"]]

  datasets_filters_info <- get_dataset_filters_info(data, filter_data)

  # Check if dataset must be reloaded in the next session
  check_data_reload(reload_period)

  if (!is.null(startup_msg)) {
    shiny::showModal(startup_msg)
  }

  log_inform(glue::glue("Filtering key: {filter_key}"))

  url_parameters <- shiny::reactiveVal()

  if (length(data) > 1) {
    shinyjs::toggle(id = "dataset_selector")
  }

  shiny::observe({
    url_parameters(shiny::parseQueryString(session$clientData$url_search))
    if (!is.null(url_parameters()[["data_name"]])) {
      shiny::updateSelectizeInput(inputId = "selector", selected = url_parameters()[["data_name"]])
    }
  })

  unfiltered_dataset <- shinymeta::metaReactive({
    shiny::req(input$selector)
    if (is.function(data[[input$selector]])) {
      add_date_range(data[[input$selector]]())
    } else {
      add_date_range(data[[input$selector]])
    }
  })

  global_filtered_values <- dv.filter::data_filter_server(
    "global_filter",
    shiny::reactive(unfiltered_dataset()[[filter_data]])
  )

  dataset_filters <- local({
    l <- vector(mode = "list", length = length(datasets_filters_info))
    names(l) <- names(datasets_filters_info)
    for (idx in seq_along(datasets_filters_info)) {
      l[[idx]] <- local({
        curr_dataset_filter_info <- datasets_filters_info[[idx]]        
        dv.filter::data_filter_server(
          curr_dataset_filter_info[["id"]],        
          shiny::reactive({unfiltered_dataset()[[curr_dataset_filter_info[["name"]]]] %||% data.frame()})
        )        
      })
    }

    l
  })

  filtered_dataset <- shinymeta::metaReactive({  
    # dv.filter returns a logical vector. This contemplates the case of empty lists    
    shiny::req(is.logical(global_filtered_values()))

    # Depend on all datasets
    purrr::walk(
      dataset_filters, ~shiny::req(is.logical(.x()))
    )

    # We do not react to changed in unfiltered dataset, otherwise when a dataset changes
    # We filter the previous dataset which in the best case produces and extra reactive beat
    # and in the worst case produces an error in (mvbc)
    # We don't want to control the error in (mvbc) because filtered dataset only changes when filter changes
    ufds <- shiny::isolate(unfiltered_dataset())

    curr_dataset_filters <- dataset_filters[intersect(names(dataset_filters), names(ufds))]    
    shiny::req(all(purrr::map_lgl(curr_dataset_filters, ~is.logical(.x()))))
    
    # Current dataset must be logical with length above 0
    # Check dataset filters check all datafilters are initialized
    purrr::walk(curr_dataset_filters, ~shiny::req(checkmate::test_logical(.x(), min.len = 1)))
    
    log_inform("New filter applied")
    filtered_key_values <- ufds[[filter_data]][[filter_key]][global_filtered_values()] # nolint

    fds <- ufds
    
    # First we apply filtered datasets

    fds[names(curr_dataset_filters)] <- purrr::imap(
      fds[names(curr_dataset_filters)],
       function(val, nm) {
        # (mvbc)
        fds[[nm]][dataset_filters[[nm]](), , drop = FALSE]  
      }
    )
    
    # Then we apply global
    global_filtered <- purrr::map(
      fds,
      ~ dplyr::filter(.x, .data[[filter_key]] %in% filtered_key_values) # nolint
    )


  })


  # Prepare module_output argument
  module_output_env <- rlang::current_env()
  module_output_func <- function() {
    return(base::get("module_output", envir = module_output_env))
  }

  if (length(module_list) > 0) {
    module_names <- names(module_list)
    names(module_names) <- purrr::map_chr(module_list, "module_id")
  } else {
    module_names <- NULL
  }

  module_args <- list(
    unfiltered_dataset = unfiltered_dataset,
    filtered_dataset = filtered_dataset,
    url_parameters = url_parameters,
    dataset_name = shiny::reactive({
      rlang::warn("afmm[[\"dataset_name\"]] will be deprecated in future versions. Please replace by afmm[[\"dataset_metadata\"]][[\"name\"]].") # nolintr
      input$selector
    }),
    dataset_metadata = list(
      name = shiny::reactive(input$selector),
      date_range = shiny::reactive(attr(unfiltered_dataset(), "date_range"))
    ),
    module_output = module_output_func,
    module_names = module_names,
    utils = list(
      switch2 = function(selected) {
        shiny::updateTabsetPanel(session, "main_tab_panel", selected)
      }
    )
  )

  module_ids <- purrr::map(module_list, "module_id")
  module_output <- module_list %>% # nolint module output is used above and lintr assumes it is not used
    purrr::set_names(nm = module_ids) %>% # Set names to access module outputs with `module_output[module_id]`
    purrr::map(function(module) {
      if (rlang::is_expression(module[["server"]])) {
        .Defunct(
          "server = function(args) {...}",
          msg = "Passing the server as an expression is going to be deprecated soon."
        )
        return(rlang::eval_tidy(module[["server"]]))
      }
      if (is.function(module[["server"]])) {
        return(module[["server"]](module_args))
      }
    })


  #### Report modal

  # REPORT IS DEACTIVATED

  # nolint start

  # shiny::observeEvent(input$open_report_modal, {
  #   shiny::showModal(create_report_modal(ns = ns))
  # })
  #
  # output$download_script <- shiny::downloadHandler(
  #   filename = glue::glue("report_{format.Date(Sys.time(), \"%d%m%Y-%H%M%S\")}.zip"),
  #   contentType = "application/zip",
  #   content = function(zip_file) {
  #     # We begin by closing the modal that created this event.
  #     shiny::removeModal()
  #
  #     report_create_bundle(
  #       title = input[["report_title"]],
  #       summary = input[["report_summary"]],
  #       module_code = purrr::imap(module_output, ~ attr(.x, "code")),
  #       module_names = purrr::map(module_list, "module_id"),
  #       unfiltered_dataset = unfiltered_dataset,
  #       filtered_dataset = filtered_dataset,
  #       load_function = attr(data[[input[["selector"]]]], "code"),
  #       do_render = input[["report_do_render"]],
  #       include_code = input[["report_include_code"]],
  #       render_format = input[["report_render_format"]],
  #       zip_file = zip_file
  #     )
  #   }
  # )

  # nolint end

  # Dataset name and date

  output$dataset_name <- shiny::renderText({
    paste0("Dataset name: ", input$selector)
  })

  output$dataset_date <- shiny::renderText({
    date_range <- attr(unfiltered_dataset(), "date_range")

    if (!any(is.na(date_range))) {
      date_range <- format(date_range, "%Y-%b-%d (%Z)")
      if (date_range[1] != date_range[2]) {
        date_string <- glue::glue("{date_range[1]} - {date_range[2]}")
      } else {
        date_string <- glue::glue("{date_range[1]}")
      }
    } else {
      date_string <- glue::glue("Date unavailable")
    }
    paste0("Dataset date: ", date_string)
  })

  #### Options modal

  shiny::observeEvent(input$open_options_modal, {
    shiny::showModal(create_options_modal(session = session, input = input, ns = ns))
  })

  shiny::observeEvent(
    input$change_theme,
    session$setCurrentTheme(get_app_theme(input$change_theme))
  )
}


# Convoluted way of having a testable server function
# TestServer reads the caller environment
# Therefore, when running a wrapped function like
# nolint start
# function(opts){
#    function(input,output,session){
#     # This environment is returned not the one inside
#      app_server_(input, output, session, opts)
#   }
# }
# No instropection of the internals of app_server_ is possible
# nolint end

app_server_test <- function(opts) {
  # Remove opts argument. It will be taken from this closure
  f <- rlang::new_function(rlang::exprs(input = , output = , session = ), rlang::fn_body(app_server_))
  f
}
