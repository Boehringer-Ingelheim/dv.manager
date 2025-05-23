# nolint start

local({
  # Common code for all tests ----
  datasets <- list(
    mpg_carb = list(
      mpg = dplyr::select(
        tibble::as_tibble(mtcars, rownames = "car"),
        car, mpg
      ),
      carb = dplyr::select(
        tibble::as_tibble(mtcars, rownames = "car"),
        car, carb
      )
    ),
    mpg_wt = list(
      mpg = dplyr::select(
        tibble::as_tibble(mtcars, rownames = "car"),
        car, mpg
      ),
      wt = dplyr::select(
        tibble::as_tibble(mtcars, rownames = "car"),
        car, wt
      )
    )
  )

  date_list <- list(
    a = lubridate::ymd_hms("2021-01-13 00:00:00"),
    b = lubridate::ymd_hms("2021-01-14 00:00:00")
  )

  datasets <- purrr::map(
    datasets,
    function(dataset) {
      purrr::map2(
        dataset,
        date_list,
        function(.x, .y) {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )
    }
  )

  testing_options <- list(
    data = datasets,
    filter_data = "mpg",
    module_info = resolve_module_list(list(
      "identity" = mod_identity(
        mm_dispatch("unfiltered_dataset"),
        "id_1"
      )
    )),
    filter_key = "car",
    enable_dataset_filter = TRUE
  )


  # Tests ----

  test_that(
    vdoc[["add_spec"]]("dv.manager should receive a list of datasets that is available for the modules of the application", c(specs$dataset_list_availability)),
    {
      testServer(app_server_test(testing_options), {
        # Test we can select one
        session$setInputs(selector = "mpg_wt")
        # Test it is loaded and available in the app
        expect_equal(unfiltered_dataset(), datasets[["mpg_wt"]],
          ignore_attr = TRUE
        )
        # Test when passed through identity module is the same as the input one
        expect_equal(module_output[["id_1"]](), datasets[["mpg_wt"]],
          ignore_attr = TRUE
        )
      })
    }
  )

  test_that(
    vdoc[["add_spec"]]("dv.manager should allow datasets to be labelled", c(specs$dataset_label_display)),
    {
      testServer(app_server_test(testing_options), {
        session$setInputs(selector = "mpg_carb") # Test we can select one using the label provided
        expect_equal(unfiltered_dataset(), datasets[["mpg_carb"]],
          ignore_attr = TRUE
        )
      })
    }
  )

  test_that(
    vdoc[["add_spec"]]("dv.manager should display datasets labels in the application", c(specs$dataset_label_display)),
    {
      testServer(app_server_test(testing_options), {
        session$setInputs(selector = "mpg_carb") # Test we can select one
        expect_equal(output$dataset_name, "Dataset name: mpg_carb") # Test the output is the expected
      })
    }
  )

  test_that(
    vdoc[["add_spec"]]("dv.manager should allow datasets switching in the application", c(specs$dataset_switching)),
    {
      testServer(app_server_test(testing_options), {
        session$setInputs(selector = "mpg_carb") # Test we can select one
        expect_equal(unfiltered_dataset(), datasets[["mpg_carb"]],
          ignore_attr = TRUE
        ) # Test expected output
        session$setInputs(selector = "mpg_wt") # Test we can select another one
        expect_equal(unfiltered_dataset(), datasets[["mpg_wt"]],
          ignore_attr = TRUE
        )
      })
    }
  )

  test_that(
    vdoc[["add_spec"]]("dv.manager should only one dataset is displayed in the application at any given time", c(specs$single_dataset_display)),
    {
      testServer(app_server_test(testing_options), {
        session$setInputs(selector = "mpg_carb") # Test we can select one
        expect_equal(unfiltered_dataset(), datasets[["mpg_carb"]],
          ignore_attr = TRUE
        ) # Test expected output
        session$setInputs(selector = "mpg_wt") # Test we can select another one
        expect_equal(unfiltered_dataset(), datasets[["mpg_wt"]],
          ignore_attr = TRUE
        )
      })
    }
  )

  test_that(
    vdoc[["add_spec"]]("dv.manager should show the earliest and latest modification date of all data tables from the selected dataset", c(specs$modification_dates_display)),
    {
      withr::local_locale(.new = list("LC_TIME" = "en_US.UTF-8"))

      testServer(app_server_test(testing_options), {
        session$setInputs(selector = "mpg_carb")
        expect_equal(output$dataset_date, "Dataset date: 2021-Jan-13 (UTC) - 2021-Jan-14 (UTC)")
      })
    }
  )

  test_that(
    vdoc[["add_spec"]]("dv.manager should show the 'Date unavailable' if no date is available in any of the data table", c(specs$date_unavailability_message)),
    {
      datasets <- list(
        mpg_carb = list(
          mpg = dplyr::select(
            tibble::as_tibble(mtcars, rownames = "car"),
            car, mpg
          ),
          carb = dplyr::select(
            tibble::as_tibble(mtcars, rownames = "car"),
            car, carb
          )
        ),
        mpg_wt = list(
          mpg = dplyr::select(
            tibble::as_tibble(mtcars, rownames = "car"),
            car, mpg
          ),
          wt = dplyr::select(
            tibble::as_tibble(mtcars, rownames = "car"),
            car, wt
          )
        )
      )

      testing_options <- list(
        data = datasets,
        filter_data = "mpg",
        module_list = list(
          "identity" = mod_identity(
            dv.manager::mm_dispatch("unfiltered_dataset"),
            "id_1"
          )
        ),
        filter_key = "car",
        enable_dataset_filter = TRUE
      )


      testServer(app_server_test(testing_options), {
        session$setInputs(selector = "mpg_carb")
        expect_equal(output$dataset_date, "Dataset date: Date unavailable")
      }) %>%
        expect_warning(regexp = "(?:mpg|carb|wt) has no date. no meta attribute or no mtime entry") %>%
        expect_warning(regexp = "(?:mpg|carb|wt) has no date. no meta attribute or no mtime entry")
    }
  )

  # nolint start
  # test_that(
  #   paste(
  #     component,
  #     "should expose a variable url_parameters variable that will give access to the parameters passed
  #      to the app through the url when launched
  #      "
  #   ),
  #   {
  #     skip("Untestable from development without selenium/URL disabled")
  #   }
  # )
  # nolint end
})

# nolint end
