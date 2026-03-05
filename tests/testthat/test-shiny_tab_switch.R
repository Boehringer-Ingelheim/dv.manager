test_that("modules can switch between tabs using switch2mod" |>
  vdoc[["add_spec"]](c(specs$MODULES$MODULE_SWITCHING_PROGRAMMATIC)), {
  skip_if_not_running_shiny_tests()

  app <- start_app_driver({
    dv.manager::run_app(
      data = list(),
      module_list = list(
        "Mod 1" = dv.manager:::mod_switch(
          "Mod 1",
          "mod2",
          "mod1"
        ),
        "Mod 2" = dv.manager:::mod_switch(
          "Mod 2",
          "mod1",
          "mod2"
        )
      ),
      filter_data = "",
      filter_key = ""
    )
  })

  app$click("mod1-switch")
  app$wait_for_idle()
  expect_equal(app$get_value(input = ID$NAV_HEADER), "mod2")
  app$click("mod2-switch")
  app$wait_for_idle()
  expect_equal(app$get_value(input = ID$NAV_HEADER), "mod1")
})
