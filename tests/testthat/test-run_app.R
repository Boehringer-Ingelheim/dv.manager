component <- "run_app"

test_that(
  paste(
    component,
    "should throw an error when not all datasets contain the filter_data field

    "
  ),
  {
    run_app(
      data = list(
        "D1" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, B = 2)),
        "D2" = list(DC1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
      ),
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "DD1",
      filter_key = "C",
      .launch = FALSE
    ) %>%
      expect_error(regexp = "D2 has no 'DD1' table")
  }
)


test_that(
  paste(
    component,
    "should pass when all datasets contain the filter_data field
    "
  ),
  {
    run_app(
      data = list(
        "D1" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, B = 2)),
        "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, B = 2))
      ),
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "DD1",
      filter_key = "A",
      .launch = FALSE
    ) %>%
      expect_warning(regexp = "Check date: Not passed", fixed = TRUE) %>%
      expect_warning(regexp = "D1 -> DD1 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D1 -> DD2 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D2 -> DD1 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D2 -> DD2 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_error(regexp = NA)
  }
)

test_that(
  paste(
    component,
    "should throw an error when no filter_data is specified
    "
  ),
  {
    run_app(
      data = list(
        "D1" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, B = 2)),
        "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
      ),
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_key = "C",
      .launch = FALSE
    ) %>%
      expect_error(regexp = "No filter_data specified!")
  }
)

test_that(
  paste(
    component,
    "should throw an error when there filter key is not present in any dataset
     "
  ),
  {
    run_app(
      data = list(
        "D1" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, B = 2)),
        "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
      ),
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "DD1",
      filter_key = "C",
      .launch = FALSE
    ) %>%
      expect_error(regexp = "Selected filtering key is not present in all datasets")
  }
)

test_that(
  paste(
    component,
    "should not throw an error when filter key is present in all data tables
     "
  ),
  {
    run_app(
      data = list(
        "D1" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, B = 2)),
        "D2" = list(DD1 = tibble::tibble(A = 1, B = 2), DD2 = tibble::tibble(A = 1, C = 2))
      ),
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "DD1",
      filter_key = "A",
      .launch = FALSE
    ) %>%
      expect_error(regexp = NA) %>%
      expect_warning(regexp = "Check date: Not passed") %>%
      expect_warning(regexp = "D1 -> DD1 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D1 -> DD2 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D2 -> DD1 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D2 -> DD2 has no date. no meta attribute or no mtime entry", fixed = TRUE)
  }
)

test_that(
  paste(
    component,
    "should use USUBJID as the default filter key
    "
  ),
  {
    run_app(
      data = list(
        "D1" = list(DD1 = tibble::tibble(USUBJID = 1, B = 2), DD2 = tibble::tibble(A = 1, USUBJID = 2)),
        "D2" = list(DD1 = tibble::tibble(USUBJID = 1, B = 2), DD2 = tibble::tibble(A = 1, USUBJID = 2))
      ),
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "DD1",
      .launch = FALSE
    )[["config"]][["filter_key"]] %>%
      expect_equal("USUBJID") %>%
      expect_warning(regexp = "Check date: Not passed") %>%
      expect_warning(regexp = "D1 -> DD1 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D1 -> DD2 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D2 -> DD1 has no date. no meta attribute or no mtime entry", fixed = TRUE) %>%
      expect_warning(regexp = "D2 -> DD2 has no date. no meta attribute or no mtime entry", fixed = TRUE)
  }
)

test_that(
  paste(
    component,
    "should return an S3 class object representing an app
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

    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    expect_s3_class(run_app(
      data = datasets,
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "a",
      filter_key = "mpg"
    ), "shiny.appobj")
  }
)

test_that(
  paste(
    component,
    "should error when the filter key is not present in any data table
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
      c = iris
    ) %>%
      purrr::map2(
        date_list,
        ~ {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )

    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )
    run_app(
      data = datasets,
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "a",
      filter_key = "mpg",
      .launch = FALSE
    ) %>%
      expect_error(regexp = "Selected filtering key is not present in all datasets")
  }
)

test_that(
  paste(
    component,
    "should warn when a date is missing in any data table
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

    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    attr(datasets[["DS1"]][["a"]], "meta") <- list()
    run_app(
      data = datasets,
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "mod1")),
      filter_data = "a",
      filter_key = "mpg",
      .launch = FALSE
    ) %>%
      expect_warning("Check date: Not passed", fixed = TRUE) %>%
      expect_warning("DS1 -> a has no date. no meta attribute or no mtime entry", fixed = TRUE)
  }
)

test_that(
  paste(
    component,
    "should error when we pass a NULL dataset

    "
  ),
  {
    run_app(
      data = NULL,
      module_list = list(),
      filter_data = "",
      filter_key = ""
    ) %>%
      expect_error(regexp = "data argument is NULL\\. If you are trying to run an application without data, use an empty list 'dv\\.manager::run_app\\(data = list\\(\\), \\.\\.\\.\\)'") %>% # nolint
      expect_warning(regexp = "module_list has length 0\\. No modules are included in the app\\.") # nolint
  }
)

test_that(
  paste(
    component,
    "should error when we pass something that is not a shiny.tag or modal as startup_msg

     "
  ),
  {
    run_app(
      data = list(),
      module_list = list("Empty" = mod_empty("a")),
      startup_msg = shiny::h1("Sample startup message"),
      filter_data = "",
      filter_key = "USUBJID"
    ) %>%
      expect_error(regexp = "Startup msg is not a shiny.tag or a shiny modal element")

    run_app(
      data = list(),
      module_list = list("Empty" = mod_empty("a")),
      startup_msg = "Sample startup message",
      filter_data = "",
      filter_key = "USUBJID"
    ) %>%
      expect_error(regexp = "Startup msg is not a shiny.tag or a shiny modal element")
  }
)

test_that(
  paste(
    component,
    "should pass when we pass something a shiny.tag modal as startup_msg

     "
  ),
  {
    run_app(
      data = list(),
      module_list = list("Empty" = mod_empty("a")),
      startup_msg = shiny::modalDialog("Sample startup message"),
      filter_data = "",
      filter_key = "USUBJID"
    ) %>%
      expect_error(regexp = NA)
  }
)
