# nolint start

test_that(
  vdoc[["add_spec"]]("app should show an startup message", c(specs$startup_message)),
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
  expr_generator <- function(dataset, date) {
    rlang::expr({
      function() {
        add_date <- (!!add_date)
        create_dummy <- (!!create_dummy)
        add_date(
          create_dummy(!!rlang::enexpr(dataset)),
          !!rlang::enexpr(date)
        )
      }
    })
  }

  app_expr <- rlang::quo({
    Sys.setenv("LC_TIME" = "en_US.UTF-8")

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


    # Otherwise !! operators missbehave
    expr_generator <- !!expr_generator

    mpg_two_date_expr <- expr_generator(
      mtcars[1:4, ],
      list(
        lubridate::ymd_hms("2021-01-13 00:00:00"),
        lubridate::ymd_hms("2021-01-14 00:00:00")
      )
    )
    mpg_one_date_expr <- expr_generator(
      mtcars[5:10, ],
      list(
        lubridate::ymd_hms("2021-01-13 00:00:00"),
        lubridate::ymd_hms("2021-01-13 00:00:00")
      )
    )
    mpg_no_date_expr <- expr_generator(
      mtcars[5:10, ],
      list(NULL)
    )


    datasets <- list(
      mpg_two_date = structure(rlang::eval_tidy(mpg_two_date_expr)(),
        code = mpg_two_date_expr
      ),
      mpg_one_date = structure(rlang::eval_tidy(mpg_one_date_expr)(),
        code = mpg_one_date_expr
      ),
      mpg_no_date = structure(rlang::eval_tidy(mpg_no_date_expr)(),
        code = mpg_no_date_expr
      )
    )

    dv.manager::run_app(
      data = datasets,
      module_list = list(
        "Filtered Tab" = dv.manager:::mod_simple(
          dv.manager::mm_dispatch("filtered_dataset", "mpg"),
          "mod1"
        ),
        "Returned Filtered" = dv.manager:::mod_identity(
          dv.manager::mm_dispatch("filtered_dataset", "carb"),
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
        "Name and dataset" = dv.manager:::mod_dataset_name_date("mod_dataset_name_date")
      ),
      filter_data = "mpg",
      filter_key = "car"
    )
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

  test_that(vdoc[["add_spec"]]("filtering and dataset switching", c(specs$filtering_menu, specs$dataset_selector)), {
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
    app$wait_for_value(output = "mod1-text", ignore = as.character(setdiff(1:20, 6)), timeout = 10000)
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
    app$wait_for_value(output = "mod1-text", ignore = as.character(setdiff(1:20, 4)), timeout = 10000)
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
    app$wait_for_value(output = "mod1-text", ignore = as.character(setdiff(1:20, 6)), timeout = 10000)
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
    app$wait_for_value(output = "mod1-text", ignore = as.character(setdiff(1:20, 4)), timeout = 10000)
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
