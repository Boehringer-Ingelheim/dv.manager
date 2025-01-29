test_that("values are returned when datafilter returns are false", {
  datasets <- list(
    DS1 = list(
      a = mtcars
    )
  )

  testing_options <- list(
    data = datasets,
    filter_data = "a",
    module_list = list(),
    enable_dataset_filter = TRUE,
    filter_key = "mpg" # This filter key is not really good as it is not unique!
  )


  shiny::testServer(app_server_test(testing_options), {
    session$setInputs(selector = "DS1")
    var <- "global_filter-cyl"
    val <- c(100, 100)

    # I don't particularly like this block because I am manipulating the filter using internal knowledge of shiny filter
    # This means that future changes of the internals of data filter may break this test
    session$setInputs("global_filter-vars" = "cyl")
    do.call(session$setInputs, setNames(list(val), var))
    # End of disliked block

    session$elapse(1001) # Datafilter now has a debounce


    expect_s3_class(filtered_dataset()[["a"]], "data.frame")
    expect_equal(nrow(filtered_dataset()[["a"]]), 0)
  }) %>%
    expect_warning(regexp = "a has no date.*")
})

test_that(
  vdoc[["add_spec"]](
    "dv.manager support datasets with 0 rows", c(specs$empty_datasets)
  ), {
    skip_if_not_running_shiny_tests()
    skip_if_suspect_check()

    app <- start_app_driver({
      dv.manager::run_app(
        data = list(one_dataset = list(a = data.frame(a = 1), empty = tibble::tibble(a = numeric(0)))),
        module_list = list(module = dv.manager::mod_simple2(dataset_name = "empty", module_id = "module")),
        filter_data = "a",
        filter_key = "a",
        enable_dataset_filter = TRUE
      )
    }) |> suppressWarnings()

    expect_identical(app$get_value(output = "module-text"), as.character(0))
})
