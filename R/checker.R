check_resolved_modules <- function(resolved_module_list) {
  if (length(resolved_module_list[["module_id"]]) == 0) {
    msg <- "module_list has length 0. No modules are included in the app."
    log_warn(msg)
  }

  if (!all(is.character(resolved_module_list[["module_id"]]))) {
    msg <- "module_list has at least one module_id that is not of type character"
    stop(msg)
  }

  if (any(duplicated(resolved_module_list[["module_id"]]))) {
    msg <- "module_list has repeated module_ids"
    stop(msg)
  }

  if (any(nchar(resolved_module_list[["module_id"]]) == 0)) {
    msg <- "module ids must have at least one character"
    stop(msg)
  }

  if (any(duplicated(resolved_module_list[["module_name"]]))) {
    msg <- "module_list has repeated module_names"
    stop(msg)
  }

  return(resolved_module_list)
}

check_dataset_list <- function(dataset_list) {
  ok <- FALSE
  if (is.list(dataset_list)) {
    ok <- TRUE
    for (df in dataset_list) {
      ok <- ok && is.data.frame(df)
    }
  }

  if (!ok) {
    msg <- "data must be list of lists of dataframes, or a list of functions that returns a list of dataframes"
    stop(msg)
  }

  dataset_list
}

check_data_while_ignoring_dataset_list_fns <- function(data) {
  # NULL data is disallowed
  if (is.null(data)) {
    msg <- "data argument is NULL. If you are trying to run an application without data, use an empty list 'dv.manager::run_app(data = list(), ...)'" # nolint
    stop(msg)
  }

  # Check we are passing a named list
  if (!has_all_items_named(data)) {
    msg <- "data argument must be a list an all its entries must be named"
    stop(msg)
  }

  for (dataset_list in data) {
    if (!is.function(dataset_list)) {
      check_dataset_list(dataset_list)
    }
  }

  data
}

check_filter_dataset_name <- function(filter_dataset_name, dataset_list, dataset_list_name) {
  # TODO it is possible to improve the feedback from this function. It stops in the first error found. We could check
  # all of them at once. This way the app creator can correct all the errors in one go.

  if (is.null(filter_dataset_name)) {
    msg <- "No filter_dataset_name specified!"
    stop(msg)
  }

  if (!filter_dataset_name %in% names(dataset_list)) {
    stop(sprintf("%s has no `%s` table", dataset_list_name, filter_dataset_name))
  }

  filter_dataset_name
}

check_filter_key <- function(filter_key, dataset_list) {
  if (is.null(filter_key)) {
    msg <- "filter_key is not specified"
    stop(msg)
  }

  filter_key_present <- TRUE

  for (idx in seq_along(dataset_list)) {
    if (!filter_key %in% names(dataset_list[[idx]])) {
      filter_key_present <- FALSE
      break
    }
  }

  if (!filter_key_present) {
    msg <- "Selected filtering key is not present in all datasets"
    stop(msg)
  } else {
    log_inform("Filter Key is present in all datasets")
  }

  filter_key
}

check_meta_mtime_attribute <- function(dataset_list, dataset_list_name) {
  log_inform(sprintf("Checking date for dataset_list %s", dataset_list_name))

  captured_warnings <- character()
  withCallingHandlers(
    get_date_range(
      lapply(dataset_list, function(el) attr(el, "meta")[["mtime"]])
    ),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, w$message)
      invokeRestart("muffleWarning")
    }
  )

  check_passed <- length(captured_warnings) == 0
  if (check_passed) {
    log_inform("Check date: Passed")
  } else {
    log_warn("Check date: Not passed. One or more datasets are not dated.")
    for (wm in captured_warnings) {
      log_warn(sprintf("%s -> %s", dataset_list_name, wm))
    }
  }

  check_passed
}

check_deprecated_calls <- function(filter_data) {
  # No deprecated calls for the moment
}

check_startup_msg <- function(startup_msg) {
  is_modal <- purrr::pluck(startup_msg, "attribs", "class", .default = "not modal") != "modal"
  if (!is.null(startup_msg) && (!inherits(startup_msg, "shiny.tag") || is_modal)) {
    msg <- "Startup msg is not a shiny.tag or a shiny modal element"
    stop(msg)
  }
  startup_msg
}

# checker for the reload functionality
check_reload_period <- function(reload_period) {
  if (!is.null(reload_period) && (!is.numeric(reload_period) || reload_period < 0)) {
    msg <- "reload_period has to be a positive numeric value larger than zero or a lubridate duration object"
    stop(msg)
  }
  reload_period
}

check_set_filter_info <- function(filter_default_state) {
  if (!is.null(filter_default_state)) {
    if (file.exists(filter_default_state)) {
      msg <- paste("Loading filter state from file", filter_default_state)
      log_inform(msg)
      filter_default_state <- paste0(readLines(filter_default_state), collapse = "\n")
    }
    utils::capture.output(x <- try(deserialize_filter_state_from_client(filter_default_state), silent = TRUE))
    if (inherits(x, "try-error")) {
      # We only parse to check JSON is correctly set, it will be used further down the code
      stop("`filter_default_state` cannot be parsed as JSON")
    }
  }

  list(filter_default_state = filter_default_state)
}

check_set_subgroup_info <- function(enable_subgroup) {
  checkmate::assert_logical(enable_subgroup, len = 1, any.missing = FALSE)
  res <- list(enable = enable_subgroup)
  res
}

check_parsable_json_input <- function(x) {
  utils::capture.output(p <- try(deserialize_filter_state_from_client(x), silent = TRUE))
  if (inherits(p, "try-error")) {
    msg <- paste("Error parsing JSON:", substitute(x))
    stop(msg)
  }
  NULL
}


assert_not_shiny_1_11_0 <- function() {
  shiny_forbidden_version <- "1.11.0"
  shiny_current_version <- as.character(utils::packageVersion("shiny"))

  if (shiny_forbidden_version == shiny_current_version) {
    stop("shiny version 1.11.0 has critical bugs please update")
  }
}
