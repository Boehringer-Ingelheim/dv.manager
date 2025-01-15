# app ----

local({
  skip_if_not_running_shiny_tests()
  skip_if_suspect_check()

  app_expr <- rlang::quo({
    dv.manager::run_app(
      data = list(dummy = list(d1 = mtcars, d2 = mtcars, d3 = mtcars)),
      module_list = list(
        "Uses d1 which is filter data therefore no filters" = dv.manager:::mod_simple2(
          module_id = "use_d_1",
          dataset_name = "d1"
        ),
        dummy = dv.manager::tab_group(
          "Uses d2" = dv.manager:::mod_simple2(
            module_id = "use_d_2",
            dataset_name = "d2"
          ),
          dummy2 = dv.manager::tab_group(
            "Uses d3" = dv.manager:::mod_simple2(
              module_id = "use_d_3",
              dataset_name = "d3"
            )
          ),
          "No meta" = dv.manager:::mod_simple(
              module_id = "use_d_3_with_no_meta",
              dataset = mm_dispatch("unfiltered_dataset", "d3")
          )
        )
      ),
      filter_data = "d1",
      filter_key = "cyl",
      enableBookmarking = "url"
    )
  })

  root_app <- start_app_driver(app_expr)
  app <- shinytest2::AppDriver$new(root_app$get_url())

  d2_filter <- "dataset_filter_a2b14ad9_cont"
  d3_filter <- "dataset_filter_85fa8a21_cont"

  is_hidden <- function(x) identical(app$get_js(paste0("document.getElementById(\"", x, "\").style.display")), "none")

  test_that("filters are hidden because first module uses the filter data" |>
    vdoc[["add_spec"]](c(specs$filtering$filter_hidding)), {
    expect_true(is_hidden(d2_filter))
    expect_true(is_hidden(d3_filter))
  })

  test_that("tab grouped modules. d2 filter is visible and d3 is hidden because second module uses d2" |>
    vdoc[["add_spec"]](c(specs$filtering$filter_hidding)), {
    app$set_inputs("__tabset_0__" = "__tabset_1__")
    app$set_inputs("__tabset_1__" = "use_d_2")
    app$wait_for_idle()
    expect_false(is_hidden(d2_filter))
    expect_true(is_hidden(d3_filter))
  })

  test_that("tab grouped modules. d3 filter is visible and d2 is hidden because third module uses d3" |>
    vdoc[["add_spec"]](c(specs$filtering$filter_hidding)), {
    app$set_inputs("__tabset_0__" = "__tabset_1__")
    app$set_inputs("__tabset_1__" = "__tabset_2__")
    app$set_inputs("__tabset_2__" = "use_d_3")
    app$wait_for_idle()
    expect_true(is_hidden(d2_filter))
    expect_false(is_hidden(d3_filter))
  })

  test_that("tab grouped modules. All filters are visible when no meta is provided" |>
    vdoc[["add_spec"]](c(specs$filtering$filter_hidding)), {
    app$set_inputs("__tabset_0__" = "__tabset_1__")
    app$set_inputs("__tabset_1__" = "use_d_3_with_no_meta")    
    app$wait_for_idle()
    expect_false(is_hidden(d2_filter))
    expect_false(is_hidden(d3_filter))
  })
})
