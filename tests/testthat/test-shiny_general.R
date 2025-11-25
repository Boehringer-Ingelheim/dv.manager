# nolint start

# General E2E test

local({
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

  dsl2 <- list(
    ds1 = data.frame(a = c(2, 3, 4)),
    ds2 = data.frame(a = c(2, 3, 4), b = c(8, 9, 10))
  )

  dsl3 <- list(
    ds1 = data.frame(a = c(3, 4, 5)),
    ds2 = data.frame(a = c(3, 4, 5), b = c(9, 10, 11))
  )

  date13 <- lubridate::ymd_hms("2021-01-13 00:00:00")
  date14 <- lubridate::ymd_hms("2021-01-14 00:00:00")

  set_filter <- function(app, range_a = NA) {
    empty_json <- r"--(dv_filter.request_dataset_filter_state({id:"filter", state:'{"filters": {"datasets_filter": {"children": []},"subject_filter": {"children": []}},"dataset_list_name": "dummy"}'}))--"
    json_fmt <- r"--(
{
  "filters": {
    "datasets_filter": {
      "children": []
    },
    "subject_filter": {
      "children": [
        {
          "kind": "row_operation",
          "operation": "and",
          "children": [
            {
              "kind": "filter",
              "dataset": "ds1",
              "operation": "select_range",
              "variable": "a",
              "min": %d,
              "max": %d,
              "include_NA": true
            }
          ]
        }
      ]
    }
  },
  "dataset_list_name": "dummy"
}
  )--"
    if (isTRUE(is.na(range_a))) {
      json <- empty_json
    } else {
      json <- sprintf(json_fmt, range_a[[1]], range_a[[2]])
    }

    js_fmt <- r"--(dv_filter.request_dataset_filter_state({id:"filter", state:`%s`}))--"

    js <- sprintf(js_fmt, json)

    app$run_js(js)
  }

  dataset_lists <- list(
    both_dates = add_date(dsl1, list(date13, date14)),
    one_date = add_date(dsl2, list(date13, date13)),
    no_date = add_date(dsl1, list(NULL))
  )

  create_dummy <- function(dataset) {
    list(
      ds1 = data.frame(a = c(1, 2, 3)),
      ds2 = data.frame(a = c(4, 5, 6))
    )
  }

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

  create_dataset <- function(dataset, date) {
    add_date(
      create_dummy(dataset),
      date
    )
  }

  mpg_two_date_expr <- create_dataset(
    mtcars[1:4, ],
    list(
      lubridate::ymd_hms("2021-01-13 00:00:00"),
      lubridate::ymd_hms("2021-01-14 00:00:00")
    )
  )
  mpg_one_date_expr <- create_dataset(
    mtcars[5:10, ],
    list(
      lubridate::ymd_hms("2021-01-13 00:00:00"),
      lubridate::ymd_hms("2021-01-13 00:00:00")
    )
  )
  mpg_no_date_expr <- create_dataset(
    mtcars[5:10, ],
    list(NULL)
  )

  module_list <- list(
    "AFMM" = dv.manager:::mod_afmm_export("afmm"),
    "AFMM2" = dv.manager:::mod_afmm_export("afmm2")
  )

  args <- list(
    data = dataset_lists,
    module_list = module_list,
    filter_data = "ds1",
    filter_key = "a",
    title = "Custom title",
    startup_msg = shiny::modalDialog("Sample startup message")
  )

  app_expr <- rlang::quo({
    Sys.setenv("LC_TIME" = "en_US.UTF-8")
    do.call(dv.manager::run_app, !!args)
  })

  root_app <- start_app_driver(app_expr)
  if (is.null(root_app)) {
    stop("App could not be initialized")
  }

  # We store these values for using while testing
  app_env <- new.env()
  .x <- eval(app_expr, envir = app_env)

  # For some reason the withr::local_locales does not work for this particular test therefore we used the old approach
  # with on.exit
  old_locale <- Sys.getenv("LC_TIME")
  on.exit(expr = Sys.setenv("LC_TIME" = old_locale), add = TRUE, after = TRUE)
  Sys.setenv("LC_TIME" = "en_US.UTF-8")

  local({
    # all tests can be run in a single app as there is no interaction with it
    app <- shinytest2::AppDriver$new(root_app$get_url())

    test_that(
      vdoc[["add_spec"]]("app should show an startup message", c(specs$INTERFACE$INTERFACE_STARTUP_MESSAGE)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()
        expect_equal(app$get_html(".modal-body"), "<div class=\"modal-body\">Sample startup message</div>")
      }
    )

    test_that(
      "custom title is displayed" |>
        vdoc[["add_spec"]](c(specs$INTERFACE$INTERFACE_TITLE)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()
        expect_identical(app$get_js("document.title"), args[["title"]])
      }
    )

    test_that(
      "sidebar is present in the application" |>
        vdoc[["add_spec"]](c(specs$INTERFACE$INTERFACE_SIDEBAR)),
      {
        skip_if_not_running_shiny_tests()

        expect_gt(nchar(app$get_html("div.sidebar")), 1)
      }
    )

    test_that(
      "modules appear in navbar in correct order" |>
        vdoc[["add_spec"]](c(specs$MODULES$MODULE_TABS_TOPNAV, specs$MODULES$UI_TAB_SELECTOR)),
      {
        skip_if_not_running_shiny_tests()

        # Get all navbar elements elements
        tabs <- app$get_js(
          "
          const root_buttons = document.querySelectorAll('.dv_root_button_level button');
          const button_texts = [];
          for (let i = 0; i < root_buttons.length; i++) {
            button_texts.push(root_buttons[i].textContent);
          }
          button_texts
        "
        )

        tabs <- as.character(tabs)

        expect_identical(tabs, names(module_list))
      }
    )

    test_that(
      "content of modules is displayed" |>
        vdoc[["add_spec"]](c(specs$MODULES$MODULE_CONTENT_DISPLAY)),
      {
        skip_if_not_running_shiny_tests()
        output_values <- app$get_values(output = TRUE)[["output"]]
        expect_identical(output_values[["afmm-test_text"]], "test")
      }
    )
  })

  test_that(
    "can select between different datasets" |>
      vdoc[["add_spec"]](c(specs$DATASETS$DATASET_LIST_SWITCHING_ALLOWED)),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(root_app$get_url())
      unfiltered_dataset_list <- shiny::isolate(app$get_values(export = "afmm-afmm")[["export"]][["afmm-afmm"]][[
        "unfiltered_dataset_list"
      ]]())
      current_name <- attr(unfiltered_dataset_list, "dataset_list_name")
      expect_identical(current_name, names(dataset_lists)[[1]])

      app$set_inputs(selector = names(dataset_lists)[[2]])
      app$wait_for_idle()
      unfiltered_dataset_list <- shiny::isolate(app$get_values(export = "afmm-afmm")[["export"]][["afmm-afmm"]][[
        "unfiltered_dataset_list"
      ]]())
      current_name <- attr(unfiltered_dataset_list, "dataset_list_name")
      expect_identical(current_name, names(dataset_lists)[[2]])
    }
  )

  test_that(
    "dv.manager can bookmark identity of loaded dataset" |>
      vdoc[["add_spec"]](c(
        specs$BOOKMARKING$BOOKMARK_DATASET_LIST,
        specs$DATASETS$DATASET_LIST_SELECTION_BOOKMARKABLE,
        specs$BOOKMARKING$BOOKMARK_BUTTON,
        specs$BOOKMARKING$BOOKMARK_ACTIVE_MODULE,
        specs$BOOKMARKING$BOOKMARK_MODULE_STATES,
        specs$MODULES$MODULE_BOOKMARKABLE
      )),
    {
      skip_if_not_running_shiny_tests()

      # TODO: brittle too coupled with dv.filters

      app <- shinytest2::AppDriver$new(root_app$get_url())

      selected_dataset <- names(args[["data"]])[[2]]
      app$set_inputs(selector = selected_dataset)
      ..switch_to_module("afmm2", app)
      app_input_values <- app$get_values()[["input"]]

      app$run_js("document.getElementById('._bookmark_').click()")

      tries <- 10
      while (is.null(app$get_js("document.querySelector('.modal-dialog textarea').value")) && tries > 0) {
        tries <- tries - 1
        Sys.sleep(1)
      }

      bmk_url <- app$get_js("document.querySelector('.modal-dialog textarea').value")

      bookmark_app <- shinytest2::AppDriver$new(bmk_url)
      bookmark_app$wait_for_idle()

      app_input_values <- app$get_values()[["input"]]
      # Explicitely exclude click
      bmk_input_values <- bookmark_app$get_values()[["input"]]
      excluded_inputs <- "._bookmark_"
      app_input_values[excluded_inputs] <- NULL
      bmk_input_values[excluded_inputs] <- NULL
      expect_identical(app_input_values, bmk_input_values)
    }
  )

  local({
    app <- shinytest2::AppDriver$new(root_app$get_url())
    exported_values <- app$get_values(export = "afmm-afmm")[["export"]][["afmm-afmm"]]

    test_that(
      "modules can access the unfiltered dataset" |>
        vdoc[["add_spec"]](c(specs$MODULES$MODULE_ACCESS_UNFILTERED_DATASET_LIST)),
      {
        unfiltered_dataset_list <- shiny::isolate(exported_values[["unfiltered_dataset_list"]]())
        current_name <- attr(unfiltered_dataset_list, "dataset_list_name")
        expected <- dataset_lists[[current_name]]
        attr(expected, "dataset_list_name") <- current_name
        attr(expected, "date_range") <- c(date13, date14)
        expect_identical(unfiltered_dataset_list, expected)
      }
    )

    test_that(
      "modules can access the filtered dataset" |>
        vdoc[["add_spec"]](c(specs$MODULES$MODULE_ACCESS_FILTERED_DATASET_LIST)),
      {
        skip_if_not_running_shiny_tests()

        set_filter(app, c(2, 3))
        app$wait_for_idle()

        exported_values <- app$get_values(export = "afmm-afmm")[["export"]][["afmm-afmm"]]
        filtered_dataset_list_dsl1 <- shiny::isolate(exported_values[["filtered_dataset_list"]]())

        expect_identical(filtered_dataset_list_dsl1[["ds1"]][["a"]], c(2, 3))

        set_filter(app, NA)
      }
    )

    test_that(
      "other_module_output_access" |>
        vdoc[["add_spec"]](c(specs$MODULES$MODULE_ACCESS_OTHER_OUTPUTS)),
      {
        skip_if_not_running_shiny_tests()
        afmm_output <- shiny::isolate(exported_values[["module_output"]]()[["afmm"]]())
        expect_identical("afmm", afmm_output)
      }
    )

    test_that(
      "modules can access the selected dataset name
       modules can access its name and other module names
       modules can access its dataset modification dates
      " |>
        vdoc[["add_spec"]](c(
          specs$MODULES$MODULE_ACCESS_DATASET_LIST_NAME,
          specs$MODULES$MODULE_ACCESS_MOD_DATES,
          specs$MODULES$MODULE_ACCESS_SELF_NAME,
          specs$MODULES$MODULE_ACCESS_OTHER_NAMES
        )),
      {
        skip_if_not_running_shiny_tests()

        unfiltered_dataset_list <- shiny::isolate(exported_values[["unfiltered_dataset_list"]]())
        current_name <- attr(unfiltered_dataset_list, "dataset_list_name")
        expect_identical(current_name, names(dataset_lists)[[1]])

        dataset_metadata_name <- shiny::isolate(exported_values[["dataset_metadata"]][["name"]]())
        expect_identical(dataset_metadata_name, names(dataset_lists)[[1]])

        module_names <- exported_values[["module_names"]]
        expect_identical(module_names, c("afmm" = "AFMM", "afmm2" = "AFMM2"))

        date_range <- shiny::isolate(exported_values[["dataset_metadata"]][["date_range"]]())
        expect_identical(c(date13, date14), date_range)
      }
    )
  })

  test_that(
    vdoc[["add_spec"]](
      "dataset name and date are present in the UI",
      c(specs$INTERFACE$INTERFACE_DATASET_LIST_NAME, specs$INTERFACE$INTERFACE_MOD_DATE_LIST)
    ),
    {
      skip_if_not_running_shiny_tests()
      skip_if_suspect_check()

      app <- shinytest2::AppDriver$new(root_app$get_url())

      # Collapse sidebar so dataset name and date are visible
      app$run_js("document.getElementById('click').checked = false;")

      exported_values <- app$get_values(export = "afmm-afmm")[["export"]][["afmm-afmm"]]

      unfiltered_dataset_list <- shiny::isolate(exported_values[["unfiltered_dataset_list"]]())
      current_name <- attr(unfiltered_dataset_list, "dataset_list_name")

      output_values <- app$get_values(output = TRUE)
      expect_match(output_values[["output"]][["dataset_name"]], names(dataset_lists)[[1]])

      date_range <- shiny::isolate(exported_values[["dataset_metadata"]][["date_range"]]())
      expect_match(output_values[["output"]][["dataset_date"]], format(date_range[[1]], "%Y-%b-%d (%Z)"), fixed = TRUE)
      expect_match(output_values[["output"]][["dataset_date"]], format(date_range[[2]], "%Y-%b-%d (%Z)"), fixed = TRUE)
    }
  )

  test_that(
    vdoc[["add_spec"]](
      "filter remains after data switching",
      c(specs$FILTERING$FILTER_ACTIVE_DATASET_LIST, specs$DATASETS$DATASET_LIST_SWITCHING_ALLOWED)
    ),
    {
      skip_if_not_running_shiny_tests()
      skip_if_suspect_check()

      app <- shinytest2::AppDriver$new((root_app$get_url()))

      set_filter(app, c(2, 3))
      app$wait_for_idle()

      exported_values <- app$get_values(export = "afmm-afmm")[["export"]][["afmm-afmm"]]
      filtered_dataset_list_dsl1 <- shiny::isolate(exported_values[["filtered_dataset_list"]]())

      app$set_inputs(selector = names(dataset_lists)[[2]])
      app$wait_for_idle()

      exported_values <- app$get_values(export = "afmm-afmm")[["export"]][["afmm-afmm"]]
      filtered_dataset_list_dsl2 <- shiny::isolate(exported_values[["filtered_dataset_list"]]())

      expect_identical(filtered_dataset_list_dsl1[["ds1"]][["a"]], c(2, 3))
      expect_identical(filtered_dataset_list_dsl2[["ds1"]][["a"]], c(2, 3))
    }
  )
})

# nolint end
