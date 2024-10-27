# app ----

local({
  skip_if_not_running_shiny_tests()
  skip_if_suspect_check()

  app_expr <- rlang::quo({
    dv.manager:::run_mock_app_tab_group()
  })
  root_app <- start_app_driver(app_expr)
  test_that("tab_group allows grouping of modules" |>
    vdoc[["add_spec"]](c(specs$tab_group$group_modules)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$set_inputs("__tabset_0__" = "__tabset_1__")
    app$wait_for_idle()
    html_code <- app$get_html("#__tabset_1__")
    expect_true(
      startsWith(html_code, "<ul class=\"nav nav-pills shiny-tab-input shiny-bound-input")
    )
    expect_true(
      grepl("Simple2", html_code)
    )
    expect_true(
      grepl("Simple3", html_code)
    )
    expect_true(
      grepl("Nested modules", html_code)
    )

    v2 <- app$get_values(output = "mod2-text")[["output"]][["mod2-text"]]
    expect_equal(
      v2, "306"
    )
  })

  test_that("tab_group allows nesting of modules" |>
    vdoc[["add_spec"]](c(specs$tab_group$allows_nesting)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$set_inputs("__tabset_0__" = "__tabset_1__")
    app$set_inputs("__tabset_1__" = "__tabset_2__")
    app$wait_for_idle()
    html_code <- app$get_html("#__tabset_2__")
    expect_true(
      startsWith(html_code, "<ul class=\"nav nav-pills shiny-tab-input shiny-bound-input")
    )
    expect_true(
      grepl("Simple4", html_code)
    )
    expect_true(
      grepl("Simple5", html_code)
    )

    v4 <- app$get_values(output = "mod4-text")[["output"]][["mod4-text"]]
    expect_equal(
      v4, "306"
    )
  })

  test_that("tab_group output of grouped modules can be accesed by other modules" |>
    vdoc[["add_spec"]](c(specs$tab_group$output_accesible)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$set_inputs("__tabset_0__" = "__tabset_1__")
    app$set_inputs("__tabset_1__" = "mod_rec_2")
    app$wait_for_idle()
    app$set_inputs("__tabset_1__" = "__tabset_2__")
    app$set_inputs("__tabset_2__" = "mod_rec_1")
    app$wait_for_idle()

    v1 <- app$get_values(output = TRUE)[["output"]][["mod_rec_1-output"]]
    v2 <- app$get_values(output = TRUE)[["output"]][["mod_rec_2-output"]]

    expect_equal(v1, "a")
    expect_equal(v2, "1")
  })


  # 
  test_that("tab_group other modules can switch into nested tabs" |>
    vdoc[["add_spec"]](c(specs$tab_group$allows_switching)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$set_inputs("__tabset_0__" = "mod_switch1")
    app$wait_for_idle()
    app$click("mod_switch1-switch")
    app$wait_for_idle()

    t0 <- app$get_values(input = TRUE)[["input"]][["__tabset_0__"]]
    t1 <- app$get_values(input = TRUE)[["input"]][["__tabset_1__"]]
    t2 <- app$get_values(input = TRUE)[["input"]][["__tabset_2__"]]

    expect_equal(t0, "__tabset_1__")
    expect_equal(t1, "__tabset_2__")
    expect_equal(t2, "mod5")
  })
})
