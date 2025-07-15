test_that("modules can switch between tabs using switch2 and switch2mod" |>
  vdoc[["add_spec"]](c(specs$module_tab_switching)), {
  skip_if_not_running_shiny_tests()
  skip_if_suspect_check()

  app <- start_app_driver({
    dv.manager::run_app(
      data = list(),
      module_list = list(
        "Mod 1" = dv.manager:::mod_switch(
          "Mod 1",
          "mod2",
          dv.manager::mm_dispatch("utils", "switch2mod"),
          "mod1"
        ),
        "Mod 2" = dv.manager:::mod_switch(
          "Mod 2",
          "mod1",
          dv.manager::mm_dispatch("utils", "switch2mod"),
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
