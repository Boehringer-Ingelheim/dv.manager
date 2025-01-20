# nolint start
# Testing app_server ----
domain_list <- list(
  a = mtcars,
  b = mtcars,
  c = mtcars
)
test_that(
  vdoc[["add_spec"]]("app_server_ should set as output the selected dataset name", c(specs$selected_dataset_name)),
  {
    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      module_info = resolve_module_list(list()),
      filter_key = "mpg",
      enable_dataset_filter = FALSE
    )


    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_name, "Dataset name: DS1")
    }) %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]]("app_server_ should work with a functions that return datasets", c(specs$data_list_structure)),
  {
    datasets <- list(
      DS1 = function() {
        domain_list
      },
      DS2 = function() {
        domain_list
      }
    )
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      module_info = resolve_module_list(list()),
      filter_key = "mpg",
      enable_dataset_filter = FALSE
    )

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_name, "Dataset name: DS1")
    }) %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]]("app_server_ should accept a list of modules and display them in the application", c(specs$display_modules, specs$module_list_structure)),
  {
    testing_options <- list(
      data = list(),
      module_info = resolve_module_list(list("mod_1" = mod_identity(1, "mod_1"), "mod_2" = mod_identity(2, "mod_2"))),      
      enable_dataset_filter = FALSE
    )

    shiny::testServer(app_server_test(testing_options), {
      expect_equal(module_output[["mod_1"]], 1)
      expect_equal(module_output[["mod_2"]], 2)
    })
  }
)

test_that(
  vdoc[["add_spec"]]("app_server_ should accept an empty list of modules", c(specs$module_list_check)),
  {
    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      module_info = resolve_module_list(list()),
      filter_key = "mpg",
      enable_dataset_filter = FALSE
    )

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_name, "Dataset name: DS1")
    }) %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]]("app_server_ should accept an empty list of datasets", c(specs$data_structure_check)),
  {
    testing_options <- list(
      data = list(),
      filter_data = NULL,
      module_info = resolve_module_list(list("mod_1" = mod_identity(1, mod_id = "mod_1"))),
      filter_key = NULL,
      enable_dataset_filter = FALSE
    )

    testServer(app_server_test(testing_options), {
      expect_equal(module_output[["mod_1"]], 1)
    })
  }
)

test_that(
  vdoc[["add_spec"]]("app_server_ should accept an empty list of datasets and an empty list of modules", c(specs$data_structure_check, specs$module_list_check)),
  {
    testing_options <- list(
      data = list(),
      module_info = resolve_module_list(list()),
      filter_key = NULL,
      enable_dataset_filter = FALSE
    )
    shiny::testServer(app_server_test(testing_options), {
    }) %>%
      expect_error(regexp = NA)
  }
)

# Testing date_output ----

component <- "date output"

test_that(
  vdoc[["add_spec"]]("date output should output the earliest and latest date of the selected dataset if not all are equal", c(specs$modification_date_display)),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-14 00:00:00"),
      c = lubridate::ymd_hms("2021-01-16 00:00:00")
    )

    domain_list <- domain_list %>%
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
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      module_info = resolve_module_list(list()),
      filter_key = "mpg",
      enable_dataset_filter = FALSE
    )

    withr::local_locale(.new = list("LC_TIME" = "en_US.UTF-8"))

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_date, "Dataset date: 2021-Jan-13 (UTC) - 2021-Jan-16 (UTC)")
    })
  }
)

test_that(
  vdoc[["add_spec"]]("date output should output only one date of the selected dataset if all are equal", c(specs$modification_date_display)),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-13 00:00:00"),
      c = lubridate::ymd_hms("2021-01-13 00:00:00")
    )

    domain_list <- domain_list %>%
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
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      module_info = resolve_module_list(list()),
      filter_key = "mpg",
      enable_dataset_filter = FALSE
    )

    withr::local_locale(.new = list("LC_TIME" = "en_US.UTF-8"))

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_date, "Dataset date: 2021-Jan-13 (UTC)")
    })
  }
)

test_that(
  vdoc[["add_spec"]]("date output should output 'date unavailable' when at least one date is not available", c(specs$modification_date_display)),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-13 00:00:00"),
      c = lubridate::ymd_hms("2021-01-13 00:00:00")
    )

    domain_list <- domain_list %>%
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

    attr(datasets[["DS2"]][["a"]], "meta") <- list(mtime = NULL)

    testing_options <- list(
      data = datasets,
      filter_data = "a",
      module_info = resolve_module_list(list()),
      filter_key = "mpg",
      enable_dataset_filter = FALSE
    )

    withr::local_locale(.new = list("LC_TIME" = "en_US.UTF-8"))

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_date, "Dataset date: 2021-Jan-13 (UTC)")
      session$setInputs(selector = "DS2")
      expect_equal(output$dataset_date, "Dataset date: Date unavailable")
    }) %>%
      expect_warning(regexp = "a has no date. no meta attribute or no mtime entry")
  }
)

# Testing reload_period ----

test_that(
  vdoc[["add_spec"]]("date output restart.txt time is altered when being touched", c(specs$data_reloading, specs$data_reload)),
  {
    datasets <- list(
      DS1 = function() {
        domain_list
      },
      DS2 = function() {
        domain_list
      }
    )
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      module_info = resolve_module_list(list()),
      filter_key = "mpg",
      reload_period = lubridate::duration(1, "seconds"),
      enable_dataset_filter = FALSE
    )
    withr::with_dir(tempdir(), {
      system2(command = "touch", args = c("restart.txt"), stdout = TRUE)
      time_pre_touch <- lubridate::as_datetime(file.info("restart.txt")[["mtime"]])
      Sys.sleep(1)

      shiny::testServer(app_server_test(testing_options), {
        time_post_touch <- lubridate::as_datetime(file.info("restart.txt")[["mtime"]])
        expect_true(time_pre_touch != time_post_touch)
      })
    })
  }
)

# nolint end
