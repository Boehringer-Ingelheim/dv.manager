test_that(
  vdoc[["add_spec"]](
    "dv.manager support datasets with 0 rows",
    c(specs$DATASETS$DATASET_LIST_EMPTY_SUPPORTED)
  ),
  {
    skip_if_not_running_shiny_tests()

    app <- start_app_driver({
      dv.manager::run_app(
        data = list(one_dataset = list(a = data.frame(a = 1), empty = tibble::tibble(a = numeric(0)))),
        module_list = list(module = dv.manager::mod_simple2(dataset_name = "empty", module_id = "module")),
        filter_data = "a",
        filter_key = "a"
      )
    }) |>
      suppressWarnings()

    expect_identical(app$get_value(output = "module-text"), as.character(0))
  }
)

test_that("labels are preserved when the datasets are data.frames", {
  skip_if_not_running_shiny_tests()  

  app <- start_app_driver({
    data <- list(
      D1 = list(
        t1 = data.frame(fk = 0, a = 1, b = 2),
        t2 = data.frame(fk = 0, c = 1, d = 2)
      )
    )

    # Leave two with no label to cover the no label case
    attr(data[["D1"]][["t1"]][["a"]], "label") <- "label a"
    attr(data[["D1"]][["t2"]][["c"]], "label") <- "label c"
    dv.manager:::run_mock_app_labels(data = data)
  }) |>
    suppressWarnings()

  data <- app$get_value(export = "mod1-data")
  expect_identical(attr(data[["t1"]][["a"]], "label"), "label a")
  expect_null(attr(data[["t1"]][["b"]], "label"))
  expect_identical(attr(data[["t2"]][["c"]], "label"), "label c")
  expect_null(attr(data[["t2"]][["d"]], "label"))
})
