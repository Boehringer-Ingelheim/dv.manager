# nolint start

# Testing checkers ----

# Testing check_filter_key ----

test_that(
  vdoc[["add_spec"]](
    "check_filter_key should pass the check when all data tables contain the filter key and return the checked element",
    c(specs$FILTERING$FILTER_GLOBAL_TABLE)
  ),
  {
    data <- list(
      "D1" = list(DD1 = tibble::tibble(A = 1, C = 2), DD2 = tibble::tibble(A = 1, B = 2)),
      "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
    )

    check_filter_key("A", data) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_filter_key should error the check when no data table contain the filter key",
    c(specs$FILTERING$FILTER_GLOBAL_KEY)
  ),
  {
    data <- list(
      "D1" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, B = 2)),
      "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, D = 2))
    )

    check_filter_key("C", data) %>%
      expect_error(regexp = "Selected filtering key is not present in all datasets")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_filter_key should error the check when not all data tables contain the filter key",
    c(specs$FILTERING$FILTER_GLOBAL_KEY)
  ),
  {
    data <- list(
      "D1" = list(DD1 = tibble::tibble(A = 1, C = 2), DD2 = tibble::tibble(A = 1, B = 2)),
      "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
    )

    check_filter_key("C", data) %>%
      expect_error(regexp = "Selected filtering key is not present in all datasets")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_filter_key should pass the check when the filter key is present in all datasets",
    c(specs$FILTERING$FILTER_GLOBAL_KEY)
  ),
  {
    data <- list(
      "D1" = list(DD1 = tibble::tibble(A = 1, C = 2), DD2 = tibble::tibble(A = 1, B = 2)),
      "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
    )

    check_filter_key("A", data) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_filter_key should pass the check when data is empty. Should return the checked element",
    c(specs$FILTERING$FILTER_GLOBAL_KEY)
  ),
  {
    check_filter_key("A", list()) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_filter_key should error the check when the filter key is NULL",
    c(specs$FILTERING$FILTER_GLOBAL_KEY)
  ),
  {
    data <- list(
      "D1" = list(DD1 = tibble::tibble(A = 1, C = 2), DD2 = tibble::tibble(A = 1, B = 2)),
      "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
    )

    check_filter_key(NULL, data) %>%
      expect_error(regexp = "filter_key is not specified")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_filter_key should error the check when the filter key is a numeric value and not a name",
    c(specs$FILTERING$FILTER_GLOBAL_KEY)
  ),
  {
    data <- list(
      "D1" = list(DD1 = tibble::tibble(A = 1, C = 2), DD2 = tibble::tibble(A = 1, B = 2)),
      "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
    )

    check_filter_key(1, data) %>%
      expect_error(regexp = "Selected filtering key is not present in all datasets")
  }
)

## Testing check_meta_mtime ----

component <- "check_meta_mtime"

test_that(
  vdoc[["add_spec"]](
    "check_meta_mtime should pass the check when all data tables in all datasets have a date. Should return the checked element",
    c(specs$DATASETS$DATASET_LIST_MOD_DATE_SINGLE, specs$DATASETS$DATASET_LIST_MOD_DATE_RANGE)
  ),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-14 00:00:00"),
      c = lubridate::ymd_hms("2021-01-16 00:00:00")
    )

    domain_list <- list(
      a = mtcars,
      b = mtcars,
      c = mtcars
    ) %>%
      purrr::map2(
        date_list,
        ~ {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )

    data <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    check_meta_mtime_attribute(data) %>%
      expect_true()
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_meta_mtime should not pass the check when at least one of the tables do not have a date",
    c(specs$DATASETS$DATASET_LIST_MOD_DATE_UNAVAILABLE_UI)
  ),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-14 00:00:00"),
      c = lubridate::ymd_hms("2021-01-16 00:00:00")
    )

    domain_list <- list(
      a = mtcars,
      b = mtcars,
      c = mtcars
    ) %>%
      purrr::map2(
        date_list,
        ~ {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )

    data <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    attr(data[["DS1"]][["a"]], "meta") <- list()

    check_meta_mtime_attribute(data) %>%
      expect_false() %>%
      expect_warning("Check date: Not passed. One or more datasets are not dated.", fixed = TRUE) %>%
      expect_warning("DS1 -> a has no date. no meta attribute or no mtime entry", fixed = TRUE)
  }
)

## Testing check_filter_data ----

test_that(
  vdoc[["add_spec"]](
    "check_filter_data should pass when all datasets contain the filter_data field. Should return the checked element",
    c(specs$FILTERING$FILTER_GLOBAL_TABLE)
  ),
  {
    data <- list(
      D1 = list(a = 1, b = 2),
      D2 = list(a = 1, b = 2),
      D3 = list(a = 1, b = 2)
    )

    check_filter_data("a", data) %>%
      expect_error(regexp = NA) %>%
      expect_equal("a")
  }
)

test_that(
  vdoc[["add_spec"]]("check_filter_data should pass when data is empty", c(specs$FILTERING$FILTER_GLOBAL_TABLE)),
  {
    check_filter_data("A", list()) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_filter_data should fail when at least 1 dataset does not contain the filter_data field",
    c(specs$FILTERING$FILTER_GLOBAL_TABLE)
  ),
  {
    data <- list(
      D1 = list(a = 1, b = 2),
      D2 = list(a = 1, b = 2),
      D3 = list(c = 1, b = 2)
    )
    check_filter_data("a", data) %>%
      expect_error(regexp = "D3 has no 'a' table", fixed = TRUE)
  }
)

test_that(
  vdoc[["add_spec"]]("check_filter_data should fail when filter_data is NULL", c(specs$FILTERING$FILTER_GLOBAL_TABLE)),
  {
    data <- list(
      D1 = list(a = 1, b = 2),
      D2 = list(a = 1, b = 2),
      D3 = list(c = 1, b = 2)
    )
    check_filter_data(NULL, data) %>%
      expect_error(regexp = "No filter_data specified!")
  }
)

## Testing check_data ----

test_that(
  vdoc[["add_spec"]]("check_data should error when the data is NULL", c(specs$DATASETS$DATASET_ENTRY_STRUCTURE)),
  {
    check_data(NULL) %>%
      expect_error(
        regexp = "data argument is NULL\\. If you are trying to run an application without data, use an empty list 'dv\\.manager::run_app\\(data = list\\(\\), \\.\\.\\.\\)'" # nolint
      )
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_data should error when we do not pass a list of lists of dataframes or a list of functions.",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE)
  ),
  {
    check_data(list(A = 1)) %>% # A list that is not a list of dataframes or a list of functions
      expect_error("data must be list of lists of dataframes, or a list of functions that returns a list of dataframes")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_data should error when any of the entries are not named",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE)
  ),
  {
    check_data(list(list(data.frame(a = 1)))) %>%
      expect_error("All entries in data must be named")
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_data should pass the check when a named list of lists of dataframes or a list of functions is passed. Should return the checked element",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE)
  ),
  {
    # List of lists of dataframes
    data <- list(a = list(a = data.frame(a = 1)))
    check_data(data) %>%
      expect_error(NA) %>%
      expect_equal(data)

    # List of functions
    data <- list(a = function(x) {
      x
    })
    check_data(data) %>%
      expect_error(NA) %>%
      expect_equal(data)
  }
)

## Testing "check_module" ----

test_that(
  vdoc[["add_spec"]](
    "check_module should warn when the module_list is empty",
    c(specs$MODULES$MODULE_LIST_EMPTY_ALLOWED)
  ),
  {
    check_resolved_modules(resolve_module_list(list())) %>%
      expect_warning(regexp = "module_list has length 0\\. No modules are included in the app\\.") # nolint
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_module should error when the module_id in the list are repeated",
    c(specs$MODULES$MODULE_DEFINITION_STRUCTURE)
  ),
  {
    check_resolved_modules(resolve_module_list(list(
      a = list(ui = 1, server = function(x) x, module_id = "a"),
      b = list(ui = 1, server = function(x) x, module_id = "a")
    ))) %>%
      expect_error(regexp = "module_list has repeated module_ids") # nolint
  }
)

test_that(
  vdoc[["add_spec"]](
    "check_module should error when at least one module_id is an empty string",
    c(specs$MODULES$MODULE_DEFINITION_STRUCTURE)
  ),
  {
    check_resolved_modules(resolve_module_list(list(
      a = list(ui = 1, server = function(x) x, module_id = "")
    ))) %>%
      expect_error(regexp = "module ids must have at least one character") # nolint
  }
)

test_that("check_set_filter_info should error when filter_default_state is not a JSON parsable string", {
  check_set_filter_info("UNPARSABLE") |>
    expect_error(regexp = "^`filter_default_state` cannot be parsed as JSON")
})

test_that("check_set_filter_info should error when filter_default_state is not a JSON parsable string", {
  check_set_filter_info("UNPARSABLE") |>
    expect_error(regexp = "^`filter_default_state` cannot be parsed as JSON")
})

test_that("check_parsable_json_input should error when passed a JSON parsable string", {
  check_parsable_json_input("UNPARSABLE") |>
    expect_error(regexp = "^Error parsing JSON:")
})


test_that("check_set_filter_info should return the JSON string", {
  expect_identical(check_set_filter_info("{}")[["filter_default_state"]], "{}")
})

test_that("check_set_filter_info should error when filter_default_state is not a JSON parsable file", {
  tmp_file <- tempfile()
  writeLines("UNPARSABLE", tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  check_set_filter_info(tmp_file) |>
    expect_error(regexp = "^`filter_default_state` cannot be parsed as JSON")
})

test_that("check_set_filter_info should return the JSON string inside a file", {
  tmp_file <- tempfile()
  writeLines("{}", tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  expect_identical(check_set_filter_info(tmp_file)[["filter_default_state"]], "{}")
})

test_that("check_set_subgroup_info should return a list with enable entry", {
  expect_identical(check_set_subgroup_info(TRUE), list(enable = TRUE))
  expect_identical(check_set_subgroup_info(FALSE), list(enable = FALSE))
})

# nolint end
