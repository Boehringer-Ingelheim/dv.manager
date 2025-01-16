test_that(
  "integration with dv.loader;
  checks that we can load a dataset with dv.loader,
  the date is correctly interpreted and the dataset
  is available inside module manager.",
  {
    mpg <- dplyr::select(
      tibble::as_tibble(mtcars, rownames = "car"),
      car, mpg
    )

    carb <- dplyr::select(
      tibble::as_tibble(mtcars, rownames = "car"),
      car, carb
    )

    local_dataset_dir <- withr::local_tempdir(
      pattern = "dataset",
      tmpdir = tempdir(),
      clean = TRUE
    )

    saveRDS(mpg, file = file.path(local_dataset_dir, "mpg.rds"))
    Sys.sleep(2) # Wait two seconds so timestamps differ between both
    saveRDS(carb, file = file.path(local_dataset_dir, "carb.rds"))

    withr::local_dir(local_dataset_dir)
    datasets <- list(mpg_carb = dv.loader::load_data(sub_dir = ".", use_wd = TRUE, file_names = c("mpg", "carb")))

    testing_options <- list(
      data = datasets,
      filter_data = "mpg",
      module_list = list(
        "identity" = mod_identity(
          mm_dispatch(
            "unfiltered_dataset"
          ),
          "id_1"
        )
      ),
      filter_key = "car",
      use_dataset_filter = FALSE
    )


    dated_dataset <- dv.manager:::add_date_range(datasets[["mpg_carb"]])
    date_range <- attr(dated_dataset, "date_range")
    date_range <- format(date_range, "%Y-%b-%d (%Z)")
    expected_date_string <- as.character(glue::glue("Dataset date: {date_range[1]}"))

    testServer(app_server_test(testing_options), {
      session$setInputs(selector = "mpg_carb")
      expect_equal(unfiltered_dataset(), dated_dataset)
      expect_equal(output$dataset_date, expected_date_string)
    })
  }
)
