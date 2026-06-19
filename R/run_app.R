#' Run the Shiny Application
#'
#' @description
#'
#' This function serves as an entry point for the applications created by the user.
#'
#' @param data the list of datasets to be used in the Shiny application. This data parameter can be defined as:
#'
#'    - a named list of lists in which each of the lists will be a list of data.frames. Each of the lists of data.frames
#'     contains a domain, a structure similar to that returned in a \verb{dv.loader::load_data} call.
#'
#'    - a named list of functions in which each of the functions will return a list of data.frames.
#'
#'    All `character` variables will be automatically mapped into `factors`.
#'
#' @param module_list a list of the modules to be included in the Shiny application
#' @param title title to be displayed in the browser tab
#' @param filter_data ( **DEPRECATED** , see `filter_dataset_name`)
#' @param filter_dataset_name a string indicating the name of the dataset used for population filtering.
#' @param filter_key a string specifying a common field across all datasets that will be used for population filtering.
#'  Default = "USUBJID" or NULL if no data = NULL
#' @param startup_msg a message to be displayed at the start of the application. It can be either NULL or a modal
#' message defined with shiny::modalDialog.
#' @param reload_period Either a lubridate object to specify a duration
#' or a positive numeric value which is then interpreted as a lubridate duration object in days. By default NULL
#' @param filter_type ( **DEPRECATED** )
#' @param filter_default_state A JSON string or file (usually exported from the app) that describes the default state of the filter (Only available for `development` filters).
#' @param enable_dataset_filter ( **DEPRECATED** )
#' @param enable_subgroup  A boolean flag indicating if subgroup controls are enabled. The default value is FALSE.
#' @param .launch by default it should always be TRUE. It should only be false for debugging and testing.
#' When TRUE it will return the app. When FALSE it will return the options with which the app will be launched.
#' @param .bypass_checks by default it should always be FALSE. Only for advanced use. If set to TRUE, the app creator must make sure that
#' application parameters and modules are correctly configured for all trials loaded in the application, otherwise application may fail. Configuration errors can be checked with a
#' dry run. To do a dry run use the parameter `.launch = FALSE`.
#' @param .bypass_filter_precomputation by default it should always be FALSE. Only for advanced use. If set to TRUE, filters are not precomputed per datasetlist.
#' this increases the app start time and the time required to do a dataset switch.
#'
#' @inheritParams shiny::shinyApp
#'
#'
#' @export
#'

run_app <- function(
  data = NULL,
  module_list = list(),
  title = "Untitled",
  filter_data,
  filter_dataset_name = NULL,
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
  .launch = TRUE,
  .bypass_checks = FALSE,
  .bypass_filter_precomputation = FALSE
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

  if (!missing(filter_data) && !missing(filter_dataset_name)) {
    stop(
      "`filter_data` argument has been replaced by `filter_dataset_name`. Both arguments cannot be specified at the same time"
    )
  }

  if (!missing(filter_data) && missing(filter_dataset_name)) {
    warning(
      "`filter_data` argument has been replaced by `filter_dataset_name`. This warning will disappear in future releases"
    )
    filter_dataset_name <- filter_data
  }

  app_args <- list(
    ui_func = app_ui,
    srv_func = app_server,
    options = list()
  )

  config <- list()
  config[["module_info"]] <- check_resolved_modules(resolve_module_list(module_list))
  # The automatic mapping will influence reporting when it is implemented in the future
  config[["afmm_static"]] <- local({
    check_data(dataset_lists)
    d <- char_vars_to_factor_vars_dataset_lists(dataset_lists)
    d <- ungroup2df_datasets_dataset_lists(d)
    if (!.bypass_filter_precomputation) {
      d <- attach_computed_filter_data_as_attribute(d)
    }
    list(
      data = d,
      module_names = config[["module_info"]][["module_name"]]
    )
  })

  if (!isTRUE(.bypass_checks)) {
    log_inform("Running EEF checkers")
    config[["module_info"]] <- check_EEF(config[["module_info"]], config[["afmm_static"]])
    config[["filter_dataset_name"]] <- check_filter_dataset_name(filter_dataset_name, dataset_lists)
    config[["filter_key"]] <- check_filter_key(filter_key, dataset_lists)
    check_meta_mtime_attribute(dataset_lists)
  } else {
    config[["filter_dataset_name"]] <- filter_dataset_name
    config[["filter_key"]] <- filter_key
    log_inform("EEF checkers disabled!")
  }

  config[["startup_msg"]] <- check_startup_msg(startup_msg)
  config[["title"]] <- title
  config[["reload_period"]] <- get_reload_period(check_reload_period(reload_period))
  config[["filter_info"]] <- check_set_filter_info(filter_default_state)
  config[["subgroup"]] <- check_set_subgroup_info(enable_subgroup)

  assert_not_shiny_1_11_0()

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
