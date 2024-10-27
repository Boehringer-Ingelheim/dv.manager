test_that("flatten_srv_module_list handles a simple list", {
  input_list <- list(
    list(server = "server1", module_id = "module1"),
    list(server = "server2", module_id = "module2")
  )
  expected_output <- list(
    list(server = "server1", module_id = "module1"),
    list(server = "server2", module_id = "module2")
  )
  expect_equal(flatten_srv_module_list(input_list), expected_output)
})

test_that("flatten_srv_module_list handles nested server collections", {
  srv_collection <- list(
    list(server = "server3", module_id = "module3"),
    list(server = "server4", module_id = "module4")
  )
  class(srv_collection) <- "server_collection"

  nested_srv_collection <- list(
    list(server = "server2", module_id = "module2"),
    list(module_id = "sub_level", server = srv_collection)
  )
  class(nested_srv_collection) <- "server_collection"

  input_list <- list(
    list(server = "server1", module_id = "module1"),
    list(module_id = "top_level", server = nested_srv_collection)
  )

  expected_output <- list(
    list(server = "server1", module_id = "module1"),
    list(server = "server2", module_id = "module2"),
    list(server = "server3", module_id = "module3"),
    list(server = "server4", module_id = "module4")
  )
  expect_equal(flatten_srv_module_list(input_list), expected_output)
})

test_that("flatten_srv_module_list handles empty list", {
  input_list <- list()
  expected_output <- list()
  expect_equal(flatten_srv_module_list(input_list), expected_output)
})


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
    app$set_inputs("__tabset_0__" = "Module Tab")
    app$wait_for_idle()
    html_code <- app$get_html("#mod_tab")
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
    app$set_inputs("__tabset_0__" = "Module Tab")
    app$set_inputs("mod_tab" = "Nested modules")
    app$wait_for_idle()
    app$view()
    html_code <- app$get_html("#nested_mod_tab")
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
      v2, "306"
    )
  })

  test_that("tab_group output of grouped modules can be accesed by other modules" |>
    vdoc[["add_spec"]](c(specs$tab_group$output_accesible)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$set_inputs("__tabset_0__" = "Module Tab")
    app$set_inputs("mod_tab" = "Send and Receive 2")
    app$wait_for_idle()
    app$set_inputs("mod_tab" = "Nested modules")
    app$set_inputs("nested_mod_tab" = "Send and Receive 1")
    app$wait_for_idle()
    app$view()

    v1 <- app$get_values(output = TRUE)[["output"]][["mod_rec_1-output"]]
    v2 <- app$get_values(output = TRUE)[["output"]][["mod_rec_2-output"]]

    expect_equal(v1, "a")
    expect_equal(v2, "1")
  })

  test_that("tab_group other modules can switch into nested tabs" |>
    vdoc[["add_spec"]](c(specs$tab_group$allows_switching)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Switch to Module tab
    app$set_inputs("__tabset_0__" = "Switch to nest")
    app$wait_for_idle()
    app$click("mod_switch-switch")
    app$wait_for_idle()

    top <- app$get_values(input = TRUE)[["input"]][["__tabset_0__"]]
    first <- app$get_values(input = TRUE)[["input"]][["mod_tab"]]
    second <- app$get_values(input = TRUE)[["input"]][["nested_mod_tab"]]

    expect_equal(top, "Module Tab")
    expect_equal(first, "Nested modules")
    expect_equal(second, "Simple5")
  })
})
