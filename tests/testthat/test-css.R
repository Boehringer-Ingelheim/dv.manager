# nolint start

test_that(
  vdoc[["add_spec"]]("css from a module should not affect another module when css namespacing is activated", c(specs$css_namespacing)),
  {
    skip_if_not_running_shiny_tests()
    skip_if_suspect_check()

    app <- start_app_driver({
      dv.manager::run_app(
        data = list(),
        module_list = list(
          "no css" = dv.manager:::mod_button_no_css("mod1"),
          "css" = dv.manager:::mod_button_css("mod2")
        ),
        filter_data = "",
        filter_key = ""
      )
    })

    expect_identical(app$get_js("$('#mod1-button').css('color')"), "rgb(51, 51, 51)")
    expect_identical(app$get_js("$('#mod2-button').css('color')"), "rgb(255, 0, 0)")
  }
)

test_that(
  vdoc[["add_spec"]]("css from a module should not affect another module when css namespacing is activated", c(specs$css_namespacing)),
  {
    skip_if_not_running_shiny_tests()
    skip_if_suspect_check()

    app <- start_app_driver({
      options("dv.manager.disable_css_namespacing" = TRUE)

      dv.manager::run_app(
        data = list(),
        module_list = list(
          "no css" = dv.manager:::mod_button_no_css("mod1"),
          "css" = dv.manager:::mod_button_css("mod2")
        ),
        filter_data = "",
        filter_key = ""
      )
    })


    expect_identical(app$get_js("$('#mod1-button').css('color')"), "rgb(255, 0, 0)")
    expect_identical(app$get_js("$('#mod2-button').css('color')"), "rgb(255, 0, 0)")
  }
)

# nolint end
