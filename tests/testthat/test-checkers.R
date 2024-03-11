# Testing checkers ----

# Testing check_filter_key ----

component <- "check_filter_key"

test_that(
  paste(
    component,
    "should pass the check when all data tables contain the filter key
    
    should return the checked element
    
    "
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
  paste(
    component,
    "should error the check when no data table contain the filter key
    
    "
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
  paste(
    component,
    "should error the check when not all data tables contain the filter key
    
    "
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
  paste(
    component,
    "should pass the check when the filter key is present in all datasets
    
    "
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
  paste(
    component,
    "should pass the check when data is empty
    
    should return the checked element
    
    "
  ),
  {
    check_filter_key("A", list()) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  paste(
    component,
    "should error the check when the filter key is NULL
    
    "
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
  paste(
    component,
    "should error the check when the filter key is a numeric value and not a name
    
    "
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
  paste(
    component,
    "should pass the check when all data tables in all datasets have a date
    
    should return the checked element
    
    "
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
  paste(
    component,
    "should not pass the check when at least one of the tables do not have a date
       "
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

component <- "check_filter_data"

test_that(
  paste(
    component,
    "should pass when all datasets contain the filter_data field
    
    should return the checked element
    
    "
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
  paste(
    component,
    "should pass when data is empty
    
    "
  ),
  {
    check_filter_data("A", list()) %>%
      expect_error(regexp = NA) %>%
      expect_equal("A")
  }
)

test_that(
  paste(
    component,
    "should fail when at least 1 dataset does not contain the filter_data field
    
    "
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
  paste(
    component,
    "should fail when filter_data is NULL
    
    "
  ),
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

component <- "check_data"

test_that(
  paste(
    component,
    "should error when the data is NULL
    
    "
  ),
  {
    check_data(NULL) %>%
      expect_error(
        regexp = "data argument is NULL\\. If you are trying to run an application without data, use an empty list 'dv\\.manager::run_app\\(data = list\\(\\), \\.\\.\\.\\)'" # nolint
      )
  }
)

test_that(
  paste(
    component,
    "should error when we do not pass a list of lists of dataframes or a list of functions.
    
    "
  ),
  {
    check_data(list(A = 1)) %>% # A list that is not a list of dataframes or a list of functions
      expect_error("data must be list of lists of dataframes, or a list of functions that returns a list of dataframes")
  }
)

test_that(
  paste(
    component,
    "should error when any of the entries are not named
    
    "
  ),
  {
    check_data(list(list(data.frame(a = 1)))) %>%
      expect_error("All entries in data must be named")
  }
)

test_that(
  paste(
    component,
    "should pass the check when a named list of lists of dataframes or a list of functions is passed
    
    should return the checked element
    
    "
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

# Testing check_azure_options

component <- "check_azure_options:"

test_that(
  paste(
    component,
    "should pass when a list with all entries is provided
    
    should return the checked element
    
    "
  ),
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
  paste(
    component,
    "should error when the list does not have all entries
    
    "
  ),
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

component <- "check_module"

test_that(
  paste(
    component,
    "should error when the any of the module_list entries are not named
    
    "
  ),
  {
    check_modules(list(1)) %>%
      expect_error("All entries in module_list must be named")
  }
)

test_that(
  paste(
    component,
    "should pass the check when a named list with non-repeated names
    
    should return the checked element
    
    "
  ),
  {
    check_modules(
      list(a = list(ui = 1, server = function(x) x, module_id = 3))
    ) %>%
      expect_error(NA)
  }
)

test_that(
  paste(
    component,
    "should warn when the module_list is empty
    
    "
  ),
  {
    check_modules(list()) %>%
      expect_warning(regexp = "module_list has length 0\\. No modules are included in the app\\.") # nolint
  }
)

test_that(
  paste(
    component,
    "should error when the names in the list are repeated
    
    "
  ),
  {
    check_modules(list(
      a = list(ui = 1, server = function(x) x, module_id = 3),
      a = list(ui = 1, server = function(x) x, module_id = 4)
    )) %>%
      expect_error(regexp = "module_list has repeated names") # nolint
  }
)

test_that(
  paste(
    component,
    "should error when the module_id in the list are repeated
    
    "
  ),
  {
    check_modules(list(
      a = list(ui = 1, server = function(x) x, module_id = 3),
      b = list(ui = 1, server = function(x) x, module_id = 3)
    )) %>%
      expect_error(regexp = "module_list has repeated module_ids") # nolint
  }
)
