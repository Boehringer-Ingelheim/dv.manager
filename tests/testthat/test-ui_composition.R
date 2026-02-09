# app ----

local({
  skip_if_not_running_shiny_tests()

  add_date <- function(dataset, date) {
    purrr::map2(
      dataset,
      date,
      function(.x, .y) {
        attr(.x, "meta") <- list(mtime = .y)
        .x
      }
    )
  }

  dsl1 <- list(
    ds1 = data.frame(a = c(1, 2, 3)),
    ds2 = data.frame(a = c(1, 2, 3), b = c(7, 8, 9))
  )

  date13 <- lubridate::ymd_hms("2021-01-13 00:00:00")
  date14 <- lubridate::ymd_hms("2021-01-14 00:00:00")
  dataset_lists <- list(
    both_dates = add_date(dsl1, list(date13, date14))
  )

  module_list <- list(
    "UNGROUPED_MODULE" = dv.manager:::mod_afmm_export("ungrouped_afmm"),
    "TAB_GROUP" = dv.manager::tab_group(
      "GROUPED_MODULE" = dv.manager:::mod_afmm_export("grouped_afmm"),
      "NESTED_GROUP" = dv.manager::tab_group(
        "NESTED_MODULE" = dv.manager:::mod_afmm_export("nested_afmm")
      )
    )
  )

  args <- list(
    data = dataset_lists,
    module_list = module_list,
    filter_data = "ds1",
    filter_key = "a"
  )

  app_expr <- rlang::quo({
    Sys.setenv("LC_TIME" = "en_US.UTF-8")
    do.call(dv.manager::run_app, !!args)
  })

  rlang::eval_tidy(app_expr)

  app <- start_app_driver(app_expr)
  if (is.null(app)) {
    stop("App could not be initialized")
  }

  test_that(
    "tab_group allows grouping of modules" |>
      vdoc[["add_spec"]](c(specs$MODULES$MODULE_GROUPING)),
    {
      ..switch_to_module("grouped_afmm", app)

      expect_equal(
        app$get_js(sprintf("$('#%s .dv_child_button_level.active').attr('value')", ID$NAV_HEADER)),
        "__tabset_1__"
      )

      expect_equal(app$get_js("$('.dv_tab_container .dv_tab_content.active').attr('value')"), "grouped_afmm")

      expect_equal(app$get_value(input = ID$NAV_HEADER), "grouped_afmm")

      grouped_afmm_output <- app$get_values(output = "grouped_afmm-test_text")[["output"]][["grouped_afmm-test_text"]]
      expect_equal(grouped_afmm_output, "test")
    }
  )

  test_that(
    "tab_group allows nesting of modules" |>
      vdoc[["add_spec"]](c(specs$MODULES$MODULE_NESTING)),
    {
      ..switch_to_module("nested_afmm", app)

      app$wait_for_idle()

      active_tabs <- app$get_js(sprintf(
        "
        (function(){
            let res = {};
          try{
            let al = $('#%s .dv_child_button_level.active');
            res.length = al.length;
            res.value = [];
            for(let idx = 0; idx < al.length; ++idx) {
              console.lo
              res.value.push($(al[idx]).attr('value'))
            }
          } catch(error) {
            res.error = error;
          }

          return(res);
        })()
      ",
        ID$NAV_HEADER
      ))

      expect_equal(active_tabs$length, 2)

      expect_equal(active_tabs$value[[1]], "__tabset_1__")

      expect_equal(active_tabs$value[[2]], "__tabset_2__")

      expect_equal(
        app$get_js(
          "$('.dv_tab_container .dv_tab_content.active').attr('value')"
        ),
        "nested_afmm"
      )

      expect_equal(
        app$get_value(input = ID$NAV_HEADER),
        "nested_afmm"
      )

      nested_afmm_output <- app$get_values(output = "nested_afmm-test_text")[["output"]][["nested_afmm-test_text"]]
      expect_equal(nested_afmm_output, "test")
    }
  )

  test_that(
    "tab_group output of grouped modules can be accesed by other modules" |>
      vdoc[["add_spec"]](c(specs$MODULES$MODULE_ACCESS_OTHER_OUTPUTS)),
    {
      exported_values <- app$get_values(export = "ungrouped_afmm-afmm")[["export"]][["ungrouped_afmm-afmm"]]
      ungrouped_afmm_output <- shiny::isolate(exported_values[["module_output"]]()[["ungrouped_afmm"]]())
      expect_identical("ungrouped_afmm", ungrouped_afmm_output)

      grouped_afmm_output <- shiny::isolate(exported_values[["module_output"]]()[["grouped_afmm"]]())
      expect_identical("grouped_afmm", grouped_afmm_output)

      nested_afmm_output <- shiny::isolate(exported_values[["module_output"]]()[["nested_afmm"]]())
      expect_identical("nested_afmm", nested_afmm_output)
    }
  )

  #
  test_that(
    "tab_group other modules can switch into nested tabs" |>
      vdoc[["add_spec"]](c(specs$MODULES$MODULE_SWITCHING_PROGRAMMATIC)),
    {
      ..switch_to_module("ungrouped_afmm", app)

      app$set_inputs("ungrouped_afmm-target_id" = "grouped_afmm")
      app$click("ungrouped_afmm-switch_to_target")
      app$wait_for_idle()
      expect_equal(app$get_value(input = ID$NAV_HEADER), "grouped_afmm")

      app$set_inputs("grouped_afmm-target_id" = "nested_afmm")
      app$click("grouped_afmm-switch_to_target")
      app$wait_for_idle()

      expect_equal(app$get_value(input = ID$NAV_HEADER), "nested_afmm")
    }
  )
})
