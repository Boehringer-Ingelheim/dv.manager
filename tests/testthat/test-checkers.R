# nolint start

# Testing checkers ----

# Testing check_filter_key ----

test_that(
  vdoc[["add_spec"]]("check_filter_key should pass the check when all data tables contain the filter key and return the checked element", c(specs$filter_key_check)),
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
  vdoc[["add_spec"]]("check_filter_key should error the check when no data table contain the filter key", c(specs$filter_key_check)),
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
  vdoc[["add_spec"]]("check_filter_key should error the check when not all data tables contain the filter key", c(specs$filter_key_check)),
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
  vdoc[["add_spec"]]("check_filter_key should pass the check when the filter key is present in all datasets", c(specs$filter_key_check)),
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
  vdoc[["add_spec"]]("check_filter_key should pass the check when data is empty. Should return the checked element", c(specs$filter_key_check)),
  {
    check_filter_key("A", list()) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  vdoc[["add_spec"]]("check_filter_key should error the check when the filter key is NULL", c(specs$filter_key_check)),
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
  vdoc[["add_spec"]]("check_filter_key should error the check when the filter key is a numeric value and not a name", c(specs$filter_key_check)),
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
  vdoc[["add_spec"]]("check_meta_mtime should pass the check when all data tables in all datasets have a date. Should return the checked element", c(specs$data_table_meta_check)),
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
  vdoc[["add_spec"]]("check_meta_mtime should not pass the check when at least one of the tables do not have a date", c(specs$data_table_meta_check)),
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
  vdoc[["add_spec"]]("check_filter_data should pass when all datasets contain the filter_data field. Should return the checked element", c(specs$filter_data_check)),
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
  vdoc[["add_spec"]]("check_filter_data should pass when data is empty", c(specs$filter_data_check)),
  {
    check_filter_data("A", list()) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  vdoc[["add_spec"]]("check_filter_data should fail when at least 1 dataset does not contain the filter_data field", c(specs$filter_data_check)),
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
  vdoc[["add_spec"]]("check_filter_data should fail when filter_data is NULL", c(specs$filter_data)),
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
  vdoc[["add_spec"]]("check_data should error when the data is NULL", c(specs$data_structure_check)),
  {
    check_data(NULL) %>%
      expect_error(
        regexp = "data argument is NULL\\. If you are trying to run an application without data, use an empty list 'dv\\.manager::run_app\\(data = list\\(\\), \\.\\.\\.\\)'" # nolint
      )
  }
)

test_that(
  vdoc[["add_spec"]]("check_data should error when we do not pass a list of lists of dataframes or a list of functions.", c(specs$data_structure_check)),
  {
    check_data(list(A = 1)) %>% # A list that is not a list of dataframes or a list of functions
      expect_error("data must be list of lists of dataframes, or a list of functions that returns a list of dataframes")
  }
)

test_that(
  vdoc[["add_spec"]]("check_data should error when any of the entries are not named", c(specs$data_structure_check)),
  {
    check_data(list(list(data.frame(a = 1)))) %>%
      expect_error("All entries in data must be named")
  }
)

test_that(
  vdoc[["add_spec"]]("check_data should pass the check when a named list of lists of dataframes or a list of functions is passed. Should return the checked element", c(specs$data_structure_check)),
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

# Testing check_azure_options

test_that(
  vdoc[["add_spec"]]("check_azure_options: should pass when a list with all entries is provided. Should return the checked element", c(specs$azure_options_check)),
  {
    azure_options <- setNames(
      list("redirect", "resource", "tenant", "app", "version", "password"),
      c("redirect", "resource", "tenant", "app", "version", "password")
    )

    expect_error(
      check_azure_options(
        azure_options
      ),
      regexp = NA
    ) %>%
      expect_equal(azure_options)
  }
)

test_that(
  vdoc[["add_spec"]]("check_azure_options: should error when the list does not have all entries", c(specs$azure_options_check)),
  {
    azure_options <- setNames(
      list("resource", "tenant", "app", "version"),
      c("resource", "tenant", "app", "version")
    )

    expect_error(
      check_azure_options(
        azure_options
      ),
      regexp = "azure_options does not contain all required entries or contains unneeded entries"
    )
  }
)


## Testing "check_module" ----

test_that(
  vdoc[["add_spec"]]("check_module should warn when the module_list is empty", c(specs$module_list_check)),
  {
    check_resolved_modules(resolve_module_list(list())) %>%
      expect_warning(regexp = "module_list has length 0\\. No modules are included in the app\\.") # nolint
  }
)

test_that(
  vdoc[["add_spec"]]("check_module should error when the module_id in the list are repeated", c(specs$module_list_check)),
  {
    check_resolved_modules(resolve_module_list(list(
      a = list(ui = 1, server = function(x) x, module_id = "a"),
      b = list(ui = 1, server = function(x) x, module_id = "a")
    ))) %>%
      expect_error(regexp = "module_list has repeated module_ids") # nolint
  }
)

test_that(
  vdoc[["add_spec"]]("check_module should error when at least one module_id is an empty string", c(specs$module_list_check)),
  {
    check_resolved_modules(resolve_module_list(list(
      a = list(ui = 1, server = function(x) x, module_id = "")
    ))) %>%
      expect_error(regexp = "module ids must have at least one character") # nolint
  }
)

# nolint end
