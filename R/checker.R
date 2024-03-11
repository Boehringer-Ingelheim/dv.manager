check_modules <- function(module_list) {
  # check all modules are named
  if (!has_all_items_named(module_list)) {
    msg <- "All entries in module_list must be named"
    rlang::abort(msg)
  }

  if (length(module_list) == 0) {
    msg <- "module_list has length 0. No modules are included in the app."
    log_warn(msg)
  }

  # Warn when module[["server"]] is an expression
  purrr::keep(module_list, ~ rlang::is_expression(.x[["server"]])) %>%
    purrr::iwalk(~ {
      msg <- glue::glue_safe("\"{.y}\" module server is an expression this will be deprecated soon.")
      log_warn(msg)
    })

  # Check module_ids and names are not repeated

  if (anyDuplicated(names(module_list)) > 0) {
    msg <- "module_list has repeated names"
    rlang::abort(msg)
  }

  if (anyDuplicated(purrr::map(module_list, "module_id")) > 0) {
    msg <- "module_list has repeated module_ids"
    rlang::abort(msg)
  }

  return(module_list)
}

check_data <- function(data) {
  # NULL data is disallowed
  if (is.null(data)) {
    msg <- "data argument is NULL. If you are trying to run an application without data, use an empty list 'dv.manager::run_app(data = list(), ...)'" # nolint
    rlang::abort(msg)
  }

  if (length(data) > 0) {
    is_expected <- all(
      purrr::map_lgl(
        data,
        function(el) {
          if (is.list(el)) {
            return(all(purrr::map_lgl(el, ~ is.data.frame(.x))))
          }
          if (is.function(el)) {
            return(TRUE)
          }
          return(FALSE)
        }
      )
    )

    if (!is_expected) {
      msg <- "data must be list of lists of dataframes, or a list of functions that returns a list of dataframes"
      rlang::abort(msg)
    }
  }

  # Check we are passing a named list
  if (!has_all_items_named(data)) {
    msg <- "All entries in data must be named"
    rlang::abort(msg)
  }

  data
}

check_filter_data <- function(filter_data, datasets) {
  if (length(datasets) == 0) {
    return(filter_data)
  }

  if (is.null(filter_data)) {
    msg <- "No filter_data specified!"
    rlang::abort(msg)
  }

  filter_data_check <- purrr::map(
    datasets,
    function(.x) {
      filter_data %in% names(.x)
    }
  ) %>% purrr::keep(~ !.x)


  if (length(filter_data_check) > 0) {
    purrr::iwalk(
      filter_data_check,
      ~ rlang::abort(glue::glue("{.y} has no '{filter_data}' table"))
    )
    msg <- glue::glue("Not all datasets have a '{filter_data}' table")
    rlang::abort(msg)
  }

  filter_data
}

check_filter_key <- function(filter_key, datasets) {
  if (length(datasets) == 0) {
    return(filter_key)
  }

  if (is.null(filter_key)) {
    msg <- "filter_key is not specified"
    rlang::abort(msg)
  }

  filter_key_present <- all(
    purrr::map_lgl(
      datasets,
      function(x) {
        if (is.function(x)) {
          d <- x()
        } else {
          d <- x
        }
        all(purrr::map_lgl(d, ~ filter_key %in% names(.x)))
      }
    )
  )
  if (!filter_key_present) {
    msg <- "Selected filtering key is not present in all datasets"
    rlang::abort(msg)
    stop(msg)
  } else {
    log_inform("Filter Key is present in all datasets")
  }

  filter_key
}

check_meta_mtime_attribute <- function(datasets) {
  check_warning <- purrr::imap(
    datasets,
    function(x, y) {
      log_inform(glue::glue("Checking date for dataset {y}"))
      if (is.function(x)) {
        d <- x()
      } else {
        d <- x
      }
      purrr::quietly(get_date_range)(
        purrr::map(d, ~ attr(.x, "meta")[["mtime"]])
      )
    }
  )

  warned_dataset <- purrr::keep(check_warning, ~ length(.x[["warnings"]]) != 0)

  check_passed <- length(warned_dataset) == 0

  if (check_passed) {
    log_inform("Check date: Passed")
  } else {
    log_warn("Check date: Not passed. One or more datasets are not dated.")
    purrr::iwalk(
      warned_dataset,
      ~ {
        purrr::walk(.x[["warnings"]], function(wm) {
          log_warn(glue::glue("{.y} -> {wm}"))
        })
      }
    )
  }

  check_passed
}

check_azure_options <- function(azure_options) {
  nm_az_opt <- names(azure_options)

  azure_options_required_entries <- c("redirect", "resource", "tenant", "app", "password", "version")
  if (!setequal(azure_options_required_entries, nm_az_opt)) {
    msg <- "azure_options does not contain all required entries or contains unneeded entries"
    rlang::abort(msg)
  }

  azure_options
}

check_deprecated_calls <- function(filter_data) {
  # No deprecated calls for the moment
}

check_startup_msg <- function(startup_msg) {
  is_modal <- purrr::pluck(startup_msg, "attribs", "class", .default = "not modal") != "modal"
  if (!is.null(startup_msg) && (!inherits(startup_msg, "shiny.tag") || is_modal)) {
    msg <- "Startup msg is not a shiny.tag or a shiny modal element"
    rlang::abort(msg)
  }
  startup_msg
}

# checker for the reload functionality
check_reload_period <- function(reload_period) {
  if (!is.null(reload_period) && (!is.numeric(reload_period) || reload_period < 0)) {
    msg <- "reload_period has to be a positive numeric value larger than zero or a lubridate duration object"
    rlang::abort(msg)
  }
  reload_period
}
