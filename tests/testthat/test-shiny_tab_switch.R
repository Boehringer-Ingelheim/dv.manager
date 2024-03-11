test_that("tabs can be switched from modules
          
          ", {
  skip_if_not_running_shiny_tests()
  skip_if_suspect_check()

  app <- start_app_driver({
    dv.manager::run_app(
      data = list(),
      module_list = list(
        "Mod 1" = dv.manager:::mod_switch(
          "Mod 1",
          "Mod 2",
          dv.manager::mm_dispatch("utils", "switch2"),
          "mod1"
        ),
        "Mod 2" = dv.manager:::mod_switch(
          "Mod 2",
          "Mod 1",
          dv.manager::mm_dispatch("utils", "switch2"),
          "mod2"
        )
      ),
      filter_data = "",
      filter_key = ""
    )
  })

  app$click("mod1-switch")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "main_tab_panel"), "Mod 2")
  app$click("mod2-switch")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "main_tab_panel"), "Mod 1")
})
