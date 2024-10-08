# nolint start

test_that(
  vdoc[["add_spec"]]("app should show an startup message", c(specs$startup_message, specs$custom_startup_message)),
  {
    skip_if_not_running_shiny_tests()
    skip_if_suspect_check()

    app <- start_app_driver({
      dv.manager::run_app(
        data = list(one_dataset = list(a = data.frame(a = 1))),
        module_list = list(),
        startup_msg = shiny::modalDialog("Sample startup message"),
        filter_data = "a",
        filter_key = "a"
      )
    }) |> suppressWarnings()

    expect_equal(app$get_html(".modal-body"), "<div class=\"modal-body\">Sample startup message</div>")
  }
)


# General E2E test

local({
  create_dummy <- function(dataset) {
    list(
      mpg = dplyr::select(
        tibble::as_tibble(dataset, rownames = "car"),
        car, mpg
      ),
      carb = dplyr::select(
        tibble::as_tibble(dataset, rownames = "car"),
        car, carb
      )
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
    "Filtered Tab" = dv.manager:::mod_simple(
      dv.manager::mm_dispatch("filtered_dataset", "mpg"),
      "mod1"
    ),
    "Returned Filtered" = dv.manager:::mod_identity(
      dv.manager::mm_dispatch("filtered_dataset", "mpg"),
      "mod2"
    ),
    "Read Output" = dv.manager:::mod_simple(
      dv.manager::mm_dispatch("module_output", "mod2"),
      "mod3"
    ),
    "Unfiltered Tab" = dv.manager:::mod_simple(
      dv.manager::mm_dispatch("unfiltered_dataset", "mpg"),
      "mod4"
    ),
    "Name And Dataset" = dv.manager:::mod_dataset_name_date("mod_dataset_name_date"),
    "Filtered Carb" = dv.manager:::mod_simple(
      dv.manager::mm_dispatch("filtered_dataset", "carb"),
      "mod5"
    )
  )

  datasets <- list(
    mpg_two_date = mpg_two_date_expr,
    mpg_one_date = mpg_one_date_expr,
    mpg_no_date = mpg_no_date_expr
  )

  args <- list(data = datasets, module_list = module_list, filter_data = "mpg", filter_key = "car", title = "Custom title")

  app_expr <- rlang::quo({
    Sys.setenv("LC_TIME" = "en_US.UTF-8")
    do.call(dv.manager::run_app, !!args)
  })
  root_app <- start_app_driver(app_expr)

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
      "sidebar is present in the application" |>
        vdoc[["add_spec"]](c(specs$sidebar_menu_display)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()


        expect_gt(nchar(app$get_html("div.sidebar")), 1)
      }
    )

    test_that(
      "modules appear in navbar in correct order" |>
        vdoc[["add_spec"]](c(specs$top_navigation_bar_module_list, specs$tab_selector)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()

        # Get all navbar elements elements
        tabs <- app$get_js("
          const ulElement = document.getElementById('main_tab_panel');
          const liElements = ulElement.getElementsByTagName('li');
          const liTexts = [];
          for (let i = 0; i < liElements.length; i++) {
            const aElement = liElements[i].getElementsByTagName('a')[0];
            if (aElement) {
              liTexts.push(aElement.textContent);
            }
          }
          liTexts
        ")

        tabs <- as.character(tabs)

        expect_identical(tabs, names(module_list))
      }
    )

    test_that(
      "content of modules is displayed" |>
        vdoc[["add_spec"]](c(specs$module_content_display)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()

        # Get all navbar elements elements

        expect_identical(app$get_values(output = "mod1-text")[["output"]][["mod1-text"]], "4")
      }
    )

    test_that(
      "custom title is displayed" |>
        vdoc[["add_spec"]](c(specs$custom_title_display, specs$app_title)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()

        expect_identical(app$get_js("document.title"), args[["title"]])
      }
    )

    test_that(
      "filtering menu is displayed" |>
        vdoc[["add_spec"]](c(specs$filtering_menu_display)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()

        # TODO: brittle, too coupled with dv.filter
        expect_gt(nchar(app$get_html("#global_filter-text")), 1)
      }
    )
  })

  test_that(
    "can select between different datasets" |>
      vdoc[["add_spec"]](c(specs$dataset_selection_sidebar_menu)),
    {
      skip_if_not_running_shiny_tests()
      skip_if_suspect_check()

      app <- shinytest2::AppDriver$new(root_app$get_url())

      datasets <- names(args[["data"]])

      app$set_inputs(selector = datasets[[2]])
      app$wait_for_idle()
      expect_true(endsWith(app$get_values(output = "dataset_name")[["output"]][["dataset_name"]], datasets[[2]]))

      app$set_inputs(selector = datasets[[1]])
      app$wait_for_idle()
      expect_true(endsWith(app$get_values(output = "dataset_name")[["output"]][["dataset_name"]], datasets[[1]]))
    }
  )

  test_that(
    "active dataset can be filtered using the global filter" |>
      vdoc[["add_spec"]](c(specs$active_dataset_filtering, specs$filter_key, specs$global_filtering)),
    {
      skip_if_not_running_shiny_tests()
      skip_if_suspect_check()

      app <- shinytest2::AppDriver$new(root_app$get_url())
      # TODO: brittle too coupled with dv.filters

      app$set_inputs("global_filter-vars" = "car")
      app$wait_for_idle()
      app$set_inputs("global_filter-car" = "Datsun 710")
      app$wait_for_idle()
      val <- app$wait_for_value(output = "mod1-text", ignore = list("4"), timeout = 10000)
      expect_identical(val, "1")
    }
  )

  test_that(
    "single data table from active dataset can be filtered using the single data table filter menu" |>
      vdoc[["add_spec"]](c(specs$active_dataset_filtering, specs$single_filtering)),
    {
      skip_if_not_running_shiny_tests()
      skip_if_suspect_check()

      app <- shinytest2::AppDriver$new(root_app$get_url())

      # TODO: brittle too coupled with dv.filters

      app$set_inputs("dataset_filter_46ab8635-vars" = "carb")
      app$wait_for_idle()
      app$set_inputs("dataset_filter_46ab8635-carb" = c(1, 1))
      app$wait_for_idle()
      app$set_inputs("main_tab_panel" = "Filtered Carb")
      app$wait_for_idle()
      val <- app$wait_for_value(output = "mod5-text", ignore = list("4"), timeout = 10000)
      expect_identical(val, "2")
    }
  )

  test_that(
    "dv.manager can bookmark identity of loaded dataset" |>
      vdoc[["add_spec"]](c(specs$bookmarking_features, specs$bookmark_button, specs$bookmarking_button_display)),
    {
      skip_if_not_running_shiny_tests()
      skip_if_suspect_check()

      # TODO: brittle too coupled with dv.filters

      app <- shinytest2::AppDriver$new(root_app$get_url())

      selected_dataset <- names(args[["data"]])[[2]]
      app$set_inputs(selector = selected_dataset)
      app$set_inputs("global_filter-vars" = "car")
      app$wait_for_idle()
      app$set_inputs("global_filter-car" = "Duster 360")
      app$wait_for_idle()
      app$set_inputs("main_tab_panel" = "Unfiltered Tab")
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

    # TODO: brittle too coupled with dv.filters

    selected_dataset <- names(args[["data"]])[[2]]
    app$set_inputs(selector = selected_dataset)
    app$set_inputs("global_filter-vars" = "car")
    app$wait_for_idle()
    app$set_inputs("global_filter-car" = "Duster 360")
    app$wait_for_idle()

    test_that(
      "modules can access the unfiltered dataset" |>
        vdoc[["add_spec"]](c(specs$unfiltered_dataset_access)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()
        app$set_inputs("main_tab_panel" = "Unfiltered Tab")

        tries <- 10
        while (!identical(app$get_values(output = "mod4-text")[["output"]][["mod4-text"]], "6") && tries > 0) {
          tries <- tries - 1
          Sys.sleep(1)
        }

        expect_identical(app$get_values(output = "mod4-text")[["output"]][["mod4-text"]], "6")
      }
    )

    test_that(
      "modules can access the filtered dataset" |>
        vdoc[["add_spec"]](c(specs$filtered_dataset_access)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()
        app$set_inputs("main_tab_panel" = "Filtered Tab")

        tries <- 10
        while (!identical(app$get_values(output = "mod1-text")[["output"]][["mod1-text"]], "1") && tries > 0) {
          tries <- tries - 1
          Sys.sleep(1)
        }

        expect_identical(app$get_values(output = "mod1-text")[["output"]][["mod1-text"]], "1")
      }
    )

    test_that(
      "other_module_output_access" |>
        vdoc[["add_spec"]](c(specs$other_module_output_access)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()
        app$set_inputs("main_tab_panel" = "Read Output")
        app$wait_for_idle()

        tries <- 10
        while (!identical(app$get_values(output = "mod3-text")[["output"]][["mod3-text"]], "1") && tries > 0) {
          tries <- tries - 1
          Sys.sleep(1)
        }

        val <- app$get_values(output = "mod3-text")[["output"]][["mod3-text"]]

        expect_identical(val, "1")
      }
    )

    test_that(
      "modules can access the selected dataset name
       modules can access its name and other module names
       modules can access its dataset modification dates
      " |>
        vdoc[["add_spec"]](c(specs$selected_dataset_name_access, specs$modification_dates_access, specs$module_name_access)),
      {
        skip_if_not_running_shiny_tests()
        skip_if_suspect_check()

        app$set_inputs("main_tab_panel" = "Name And Dataset")
        app$wait_for_idle()

        tries <- 10
        while (
          !(
            length(app$get_values(output = "mod_dataset_name_date-text")[["output"]][["mod_dataset_name_date-text"]]) > 0 &&
              nchar(app$get_values(output = "mod_dataset_name_date-text")[["output"]][["mod_dataset_name_date-text"]]) > 0
          ) &&
            tries > 0
        ) {
          tries <- tries - 1
          Sys.sleep(1)
        }

        val <- app$get_values(output = "mod_dataset_name_date-text")[["output"]][["mod_dataset_name_date-text"]]

        # TODO: Improved hardcoded test value
        # TODO: Split test
        expect_identical(
          val <- app$get_values(output = "mod_dataset_name_date-text")[["output"]][["mod_dataset_name_date-text"]],
          "dataset_name: mpg_one_date ; dataset_date_range: 2021-01-13 2021-01-13 ; module_name: Filtered Tab,Returned Filtered,Read Output,Unfiltered Tab,Name And Dataset,Filtered Carb"
        )
      }
    )
  })


  test_that(vdoc[["add_spec"]]("dataset name and date are present in the UI", c(specs$modification_date_display, specs$selected_dataset_name)), {
    skip_if_not_running_shiny_tests()
    skip_if_suspect_check()

    app <- shinytest2::AppDriver$new(root_app$get_url())

    # Collapse sidebar so dataset name and date are visible
    app$run_js("document.getElementById('click').checked = false;")

    stopifnot(app$get_value(input = "selector") == "mpg_two_date")

    mpg_two_date <- list(
      current = list(
        name = app$get_value(output = "dataset_name"),
        date = app$get_value(output = "dataset_date")
      ),
      expected = list(
        name = "Dataset name: mpg_two_date",
        date = "Dataset date: 2021-Jan-13 (UTC) - 2021-Jan-14 (UTC)"
      )
    )

    expect_equal(mpg_two_date[["current"]], mpg_two_date[["expected"]])

    app$set_inputs(selector = "mpg_one_date")
    mpg_one_date <- list(
      current = list(
        name = app$get_value(output = "dataset_name"),
        date = app$get_value(output = "dataset_date")
      ),
      expected = list(
        name = "Dataset name: mpg_one_date",
        date = "Dataset date: 2021-Jan-13 (UTC)"
      )
    )
    expect_equal(mpg_one_date[["current"]], mpg_one_date[["expected"]])

    app$set_inputs(selector = "mpg_no_date")
    mpg_no_date <- list(
      current = list(
        name = app$get_value(output = "dataset_name"),
        date = app$get_value(output = "dataset_date")
      ),
      expected = list(
        name = "Dataset name: mpg_no_date",
        date = "Dataset date: Date unavailable"
      )
    )

    expect_equal(mpg_no_date[["current"]], mpg_no_date[["expected"]])
  })

  test_that(vdoc[["add_spec"]]("filtering and dataset switching", c(specs$filtering_menus, specs$dataset_selector)), {
    skip_if_not_running_shiny_tests()
    skip_if_suspect_check()

    app <- shinytest2::AppDriver$new((root_app$get_url()))

    # Just to avoid a warning message when we change the tab to the already active one
    app$set_inputs(main_tab_panel = "Read Output")

    get_all_ <- function() {
      set_if_not <- function(tab) {

      }

      list(
        filt_tab = {
          # Tab may be set from the outside
          if (app$get_value(input = "main_tab_panel") != "Filtered Tab") app$set_inputs(main_tab_panel = "Filtered Tab")
          app$get_value(output = "mod1-text")
        },
        read_tab = {
          app$set_inputs(main_tab_panel = "Read Output")
          app$get_value(output = "mod3-text")
        },
        unfilt_tab = {
          app$set_inputs(main_tab_panel = "Unfiltered Tab")
          app$get_value(output = "mod4-text")
        }
      )
    }

    stopifnot(app$get_value(input = "selector") == "mpg_two_date")
    mpg_two_date_no_filter <- list(
      current = get_all_(),
      expected = list(
        filt_tab = "4",
        read_tab = "4",
        unfilt_tab = "4"
      )
    )

    expect_equal(mpg_two_date_no_filter[["current"]], mpg_two_date_no_filter[["expected"]])
    app$set_inputs(`global_filter-vars` = "car")
    app$wait_for_idle()
    app$set_inputs(`global_filter-car` = c("Mazda RX4", "Mazda RX4 Wag"))
    app$wait_for_idle()

    # Poor mans wait for accepted value
    app$set_inputs(main_tab_panel = "Filtered Tab")
    app$wait_for_value(output = "mod1-text", ignore = as.character(setdiff(1:20, 2)), timeout = 10000)
    mpg_two_date_filter <- list(
      current = get_all_(),
      expected = list(
        filt_tab = "2",
        read_tab = "2",
        unfilt_tab = "4"
      )
    )
    expect_equal(mpg_two_date_filter[["current"]], mpg_two_date_filter[["expected"]])

    app$set_inputs(selector = "mpg_one_date")
    app$wait_for_idle()

    # Poor mans wait for accepted value
    app$set_inputs(main_tab_panel = "Filtered Tab")
    app$wait_for_idle()
    value <- local({
      value <- app$get_values(output = "mod1-text")[["output"]][["mod1-text"]]
      tries <- 10
      while (!identical(value, "6") && tries > 0) {
        tries <- tries - 1
        Sys.sleep(1)
      }
    })    

    # app$wait_for_value(output = "mod1-text", ignore = as.character(setdiff(1:20, 6)), timeout = 10000)
    mpg_one_date_no_filter <- list(
      current = get_all_(),
      expected = list(
        filt_tab = "6",
        read_tab = "6",
        unfilt_tab = "6"
      )
    )
    expect_equal(mpg_one_date_no_filter[["current"]], mpg_one_date_no_filter[["expected"]])

    app$set_inputs(`global_filter-vars` = "car")
    app$wait_for_idle()


    app$set_inputs(`global_filter-car` = c("Merc 230", "Merc 240D", "Merc 280", "Valiant"))
    app$wait_for_idle()

    # Poor mans wait for accepted value
    app$set_inputs(main_tab_panel = "Filtered Tab")
    app$wait_for_idle()
    value <- local({
      value <- app$get_values(output = "mod1-text")[["output"]][["mod1-text"]]
      tries <- 10
      while (!identical(value, "4") && tries > 0) {
        tries <- tries - 1
        Sys.sleep(1)
      }
    })   
    mpg_one_date_filter <- list(
      current = get_all_(),
      expected = list(
        filt_tab = "4",
        read_tab = "4",
        unfilt_tab = "6"
      )
    )
    expect_equal(mpg_one_date_filter[["current"]], mpg_one_date_filter[["expected"]])

    app$set_inputs(selector = "mpg_no_date")
    app$wait_for_idle()

    # Poor mans wait for accepted value
    app$set_inputs(main_tab_panel = "Filtered Tab")
    app$wait_for_idle()
    value <- local({
      value <- app$get_values(output = "mod1-text")[["output"]][["mod1-text"]]
      tries <- 10
      while (!identical(value, "6") && tries > 0) {
        tries <- tries - 1
        Sys.sleep(1)
      }
    })  
    mpg_no_date_no_filter <- list(
      current = get_all_(),
      expected = list(
        filt_tab = "6",
        read_tab = "6",
        unfilt_tab = "6"
      )
    )
    expect_equal(mpg_no_date_no_filter[["current"]], mpg_no_date_no_filter[["expected"]])

    app$set_inputs(`global_filter-vars` = "car")
    app$set_inputs(`global_filter-car` = c("Merc 230", "Merc 240D", "Merc 280", "Valiant"))
    app$wait_for_idle()

    # Poor mans wait for accepted value
    app$set_inputs(main_tab_panel = "Filtered Tab")
    app$wait_for_idle()
    value <- local({
      value <- app$get_values(output = "mod1-text")[["output"]][["mod1-text"]]
      tries <- 10
      while (!identical(value, "4") && tries > 0) {
        tries <- tries - 1
        Sys.sleep(1)
      }
    })  
    
    mpg_no_date_filter <- list(
      current = get_all_(),
      expected = list(
        filt_tab = "4",
        read_tab = "4",
        unfilt_tab = "6"
      )
    )
    expect_equal(mpg_no_date_filter[["current"]], mpg_no_date_filter[["expected"]])
  })
})



test_that("Bookmarking", {
  skip("Untestable from development without selenium/Manual testing")
  # should display a bookmarking button
  #   Bookmarking will include:
  #   the identity of the loaded dataset,
  #   the set of filters applied to the loaded dataset,
  #   the inner state of all modules included in the app,
  #   which module is active
})

# nolint end
