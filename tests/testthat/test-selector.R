test_that(
  "dataset selector is removed when only one dataset is passed
  
  ",
  {
    skip_if_not_running_shiny_tests()
    skip_if_suspect_check()

    app_one <- start_app_driver({
      dv.manager::run_app(
        data = list(one_dataset = list(a = data.frame(a = 1))),
        module_list = list(),
        filter_data = "a",
        filter_key = "a"
      )
    }) |> suppressWarnings()

    expect_true(
      app_one$get_js("$('#dataset_selector').length") == 1 &&
        !app_one$get_js("$('#dataset_selector').is(':visible')")
    )
  }
)
