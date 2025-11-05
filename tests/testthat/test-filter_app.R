# Many tests in this file are strongly coupled with the internal js, selectors and ionRange, selectpicker, etc
# We could consider decoupling them by adding JS helpers, at the moment the double programming price is already paid
# but when they break this should be considered.

skip_if_not_running_shiny_tests()

times <- list()
timed <- identity

# nolint start
# timed <- function(fn) {
#   function(...) {
#     fn_name <- as.character(sys.call()[[1]])
#     start <- Sys.time()
#     ret <- fn(...)
#     times[[fn_name]] <<- c(times[[fn_name]], Sys.time() - start)
#     return(ret)
#   }
# }
# nolint end

local({
  dataset_lists <- local({
    date_var <- as.Date("2024-01-01") + c(0L:4L, NA)

    dataset_list_1 <- list(
      dataset_1 = data.frame(
        row.names = 1:6,
        range_var = c(1.0:5.0, NA),
        date_var = date_var,
        posix_var = as.POSIXct(date_var),
        subset_var = factor(c(letters[1:5], NA)),
        logical_var = c(FALSE, TRUE, TRUE, FALSE, FALSE, NA),
        sbj_var = paste0("SBJ-", 1:6)
      ),
      dataset_2 = data.frame(
        row.names = 1:6,
        sbj_var = paste0("SBJ-", 1:6)
      )
    )

    dataset_list_2 <- dataset_list_1
    dataset_list_2$dataset_1$sbj_var <- paste0("SBJ-", (1:6) * 2)
    dataset_list_2$dataset_2$sbj_var <- paste0("SBJ-", (1:6) * 2)

    dataset_lists <- list(dataset_list_1 = dataset_list_1, dataset_list_2 = dataset_list_2)
  })

  get_app_expr <- function(fd = NULL) {
    rlang::quo({
      filter_test_UI <- function(id) { # nolint
        ns <- shiny::NS(id)
        shiny::tagList(
          shiny::uiOutput(ns("out"))
        )
      }

      filter_test_server <- function(id, filtered_dataset_list, unfiltered_dataset_list, filter_metadata) {
        shiny::moduleServer(
          id,
          function(input, output, session) {
            root_session <- shiny:::find_ancestor_session(session)
            shiny::observe({
              shiny::reactiveValuesToList(root_session[["input"]]) # Trigger this observer every time an input changes
              session$doBookmark()
            })
            shiny::onBookmarked(shiny::updateQueryString, session = root_session)

            output[["out"]] <- shiny::renderUI({
              dsl_count <- function(dsl, name) {
                shiny::div(
                  id = substitute(dsl),
                  shiny::h3(substitute(dsl)),
                  local({
                    r_dataset <- dsl()
                    shiny::tags[["pre"]](paste("{", paste("{", names(r_dataset), ":", purrr::map_dbl(r_dataset, nrow), "}"), collapse = ",", "}"))
                  })
                )
              }

              filter_metadata_ui <- shiny::div(
                id = "filter_metadata",
                shiny::h3("filter_metadata"),
                shiny::h4("json"),
                shiny::tags[["pre"]](id = "raw", filter_metadata$output()$raw),
                shiny::h4("parsed"),
                shiny::verbatimTextOutput(outputId = session$ns("parsed"))
              )

              output[["parsed"]] <- shiny::renderPrint(filter_metadata$output()$parsed)

              list(
                dsl_count(unfiltered_dataset_list),
                dsl_count(filtered_dataset_list),
                filter_metadata_ui
              )
            })

            shiny::exportTestValues(
              unfiltered_dataset_list = unfiltered_dataset_list,
              filtered_dataset_list = filtered_dataset_list,
              filter_metadata = filter_metadata
            )
          }
        )
      }

      mod_filter_test <- function(module_id) {
        mod <- list(
          ui = filter_test_UI,
          server = function(afmm) {
            filter_test_server(
              id = module_id,
              filtered_dataset_list = afmm[["filtered_dataset_list"]],
              unfiltered_dataset_list = afmm[["unfiltered_dataset_list"]],
              filter_metadata = afmm[["filter_metadata"]]
            )
          },
          module_id = module_id
        )
        mod
      }

      app <- dv.manager::run_app(
        data = !!dataset_lists,
        module_list = list(
          "filter_test" = mod_filter_test(
            module_id = "filter_test"
          )
        ),
        filter_data = "dataset_1",
        filter_key = "sbj_var",
        enableBookmarking = "url",
        filter_type = "development",
        filter_default = !!fd
      )

      app
    })
  }

  root_app <- start_app_driver(get_app_expr()) |> suppressWarnings()
  if (is.null(root_app)) stop("App could not be initialized")

  # Tide to internal representations but it does not matter, when it breaks we
  toggle_filter <- timed(function(app, dataset_name, var_name) {
    selector <- sprintf("dv-filter-dataset-filter[data-dataset-name='%s'] div.card-header button", dataset_name)
    app$click(selector = selector)

    click_js <- sprintf(
      r"--{
      let options_spans = document.querySelectorAll(".inner.show span.text");
      let option_element = [...options_spans].filter(el => el.textContent.includes("%s"));
      option_element[0].click();
      document.querySelector("body").click() // Close menu by clicking out
      }--",
      var_name
    )
    app$run_js(click_js)
  })

  click_remove_filter_x_button <- timed(function(app, dataset_name, var_name) {
    selector <- sprintf(
      "dv-filter-dataset-filter[data-dataset-name='%s'] dv-filter-variable-filter[data-variable='%s'] button.close-btn",
      dataset_name,
      var_name
    )
    app$click(selector = selector)
  })

  get_filtered_dataset_list <- timed(function(app) {
    shiny::isolate(app$get_value(export = "filter_test-filtered_dataset_list")())
  })

  get_unfiltered_dataset_list <- timed(function(app) {
    shiny::isolate(app$get_value(export = "filter_test-unfiltered_dataset_list")())
  })

  get_filter_state <- timed(function(app) {
    shiny::isolate(app$get_value(export = "filter_test-filter_metadata")$output()$parsed)
  })

  get_saved_filter_states <- timed(function(app) {
    shiny::isolate(app$get_value(export = "filter_test-filter_metadata")$output()$parsed)
  })

  set_NA_include_js_code <- timed(function(dataset_name, var_name, checked) {
    input_selector <- sprintf(
      "dv-filter-variable-filter[data-variable='%s'] [na_control] input[type='checkbox']",
      var_name
    )

    set_js <- sprintf(
      r"--{
            let input_el = document.querySelector("%s");
            input_el.checked = %s;
            $(input_el).trigger("change");
            }--",
      input_selector,
      if (checked) "true" else "false"
    )
  })

  add_saved_filter <- timed(function(app, filter_name) {
    app$run_js(
      sprintf(
        r"--(
        let input_el = document.querySelector("dv-filter-save-button").previousSibling;
        input_el.value = "%s";
      )--",
        filter_name
      )
    )
    app$click(selector = "dv-filter-save-button")
  })

  restore_saved_filter <- timed(function(app, filter_name) {
    app$click(selector = sprintf(
      r"--(dv-filter-saved-state-button[data-saved-filter-name="%s"])--",
      filter_name
    ))
  })

  remove_saved_filter <- timed(function(app, filter_name) {
    app$click(selector = sprintf(
      r"--(dv-filter-removed-saved-state-button[data-saved-filter-name="%s"])--",
      filter_name
    ))
  })

  empty_filter <- list(
    filters = list(
      datasets_filter = list(
        children = list()
      ),
      subject_filter = list(
        children = list()
      )
    ),
    dataset_list_name = "dataset_list_1"
  )

  test_that("clear filter button removes all filters", {
    app <- shinytest2::AppDriver$new(root_app$get_url())
    expect_identical(get_filter_state(app), empty_filter)
    toggle_filter(app, dataset_name = "dataset_1", "range_var")
    toggle_filter(app, dataset_name = "dataset_1", "logical_var")
    toggle_filter(app, dataset_name = "dataset_2", "sbj_var")
    app$wait_for_idle()
    expect_false(identical(get_filter_state(app), empty_filter))
    app$click(selector = "dv-filter-clear-all-button")
    app$wait_for_idle()
    expect_identical(get_filter_state(app), empty_filter)
    app$stop()
  })

  local({
    dataset_name <- "dataset_1"
    var_names <- names(dataset_lists[["dataset_list_1"]][["dataset_1"]])

    build_modified_state <- function(variable_filter_list, dataset_name) {
      list(
        filters = list(
          datasets_filter = list(children = list()),
          subject_filter = list(
            children = list(
              list(
                kind = "row_operation",
                operation = "and",
                children = list(
                  variable_filter_list
                )
              )
            )
          )
        ),
        dataset_list_name = "dataset_list_1"
      )
    }

    test_cases <- list(
      list(
        var_name = "range_var",
        get_js_code = function(dataset_name, var_name, filter_values) {
          input_selector <- sprintf(
            "dv-filter-dataset-filter[data-dataset-name='%s'] dv-filter-variable-filter[data-variable='%s'] input[filter_value]",
            dataset_name,
            var_name
          )
          set_js <- sprintf(
            r"--{
            let input_el = document.querySelector("%s");
            let input = $(input_el).data("ionRangeSlider");
            input.update({from: %f, to: %f});
            $(input_el).trigger("finished.ion.range.slider");
            }--",
            input_selector,
            filter_values[[1]],
            filter_values[[2]]
          )
        },
        get_modified_variable_filter_state = function(dataset_name, filter_values, include_NA) {
          build_modified_state(
            list(
              kind = "filter",
              dataset = dataset_name,
              operation = "select_range",
              variable = "range_var",
              min = filter_values[[1]],
              max = filter_values[[2]],
              include_NA = include_NA
            )
          )
        }
      ),
      list(
        var_name = "date_var",
        get_js_code = function(dataset_name, var_name, filter_values) {
          input_selector <- sprintf(
            "dv-filter-dataset-filter[data-dataset-name='%s'] dv-filter-variable-filter[data-variable='%s'] div[filter_value] input",
            dataset_name,
            var_name
          )
          set_js <- sprintf(
            r"--{
            let input_els = document.querySelectorAll("%s");
            $(input_els[0]).bsDatepicker('setDate', '%s');
            $(input_els[1]).bsDatepicker('setDate', '%s');
            $(input_el[0]).trigger("changeDate");
            $(input_el[1]).trigger("changeDate");
            }--",
            input_selector,
            filter_values[[1]],
            filter_values[[2]]
          )
        },
        get_modified_variable_filter_state = function(dataset_name, filter_values, include_NA) {
          build_modified_state(
            list(
              kind = "filter",
              dataset = dataset_name,
              operation = "select_date",
              variable = "date_var",
              min = as.character(filter_values[[1]]),
              max = as.character(filter_values[[2]]),
              include_NA = include_NA
            )
          )
        }
      ),
      list(
        var_name = "posix_var",
        get_js_code = function(dataset_name, var_name, filter_values) {
          input_selector <- sprintf(
            "dv-filter-dataset-filter[data-dataset-name='%s'] dv-filter-variable-filter[data-variable='%s'] div[filter_value] input",
            dataset_name,
            var_name
          )
          set_js <- sprintf(
            r"--{
            let input_els = document.querySelectorAll("%s");
            $(input_els[0]).bsDatepicker('setDate', '%s');
            $(input_els[1]).bsDatepicker('setDate', '%s');
            $(input_el[0]).trigger("changeDate");
            $(input_el[1]).trigger("changeDate");
            }--",
            input_selector,
            filter_values[[1]],
            filter_values[[2]]
          )
          set_js
        },
        get_modified_variable_filter_state = function(dataset_name, filter_values, include_NA) {
          build_modified_state(
            list(
              kind = "filter",
              dataset = dataset_name,
              operation = "select_date",
              variable = "posix_var",
              min = as.character(filter_values[[1]]),
              max = as.character(filter_values[[2]]),
              include_NA = include_NA
            )
          )
        }
      ),
      list(
        var_name = "subset_var",
        get_js_code = function(dataset_name, var_name, filter_values) {
          input_selector <- sprintf(
            "dv-filter-dataset-filter[data-dataset-name='%s'] dv-filter-variable-filter[data-variable='%s'] select[filter_value]",
            dataset_name,
            var_name
          )
          set_js <- sprintf(
            r"--{
            let select_el = document.querySelector("%s");
            $(select_el).selectpicker('val', ['%s', '%s']);
            $(select_el).trigger("changed.bs.select");
            }--",
            input_selector,
            filter_values[[1]],
            filter_values[[2]]
          )
        },
        get_modified_variable_filter_state = function(dataset_name,  filter_values, include_NA) {
          build_modified_state(
            list(
              kind = "filter",
              dataset = dataset_name,
              operation = "select_subset",
              variable = "subset_var",
              values = unique(as.character(filter_values)),              
              include_NA = include_NA
            )
          )
        }
      ),
      list(
        var_name = "logical_var",
        get_js_code = function(dataset_name, var_name, filter_values) {
          input_selector <- sprintf(
            "dv-filter-dataset-filter[data-dataset-name='%s'] dv-filter-variable-filter[data-variable='%s'] select[filter_value]",
            dataset_name,
            var_name
          )
          set_js <- sprintf(
            r"--{
            let select_el = document.querySelector("%s");
            $(select_el).selectpicker('val', ['%s', '%s']);
            $(select_el).trigger('changed.bs.select');
            }--",
            input_selector,
            filter_values[[1]],
            filter_values[[2]]
          )
        },
        get_modified_variable_filter_state = function(dataset_name,  filter_values, include_NA) {
          build_modified_state(
            list(
              kind = "filter",
              dataset = dataset_name,
              operation = "select_subset",
              variable = "logical_var",
              values = unique(as.character(filter_values)),
              include_NA = include_NA
            )
          )
        }
      )
    )

    app <- shinytest2::AppDriver$new(root_app$get_url())
    for (idx in seq_along(test_cases)) {
      vn <- test_cases[[idx]][["var_name"]]
      gjsc <- test_cases[[idx]][["get_js_code"]]
      gmdfv <- test_cases[[idx]][["get_modified_variable_filter_state"]]
      fv <- dataset_lists[["dataset_list_1"]][["dataset_1"]][[vn]][c(2, 3)] # Middle values

      stopifnot(identical(get_filter_state(app), empty_filter))

      test_that(sprintf("a `%s` filter can be added and modified", vn)|>
    vdoc[["add_spec"]](c(specs$FILTERING$FILTER_ADD_REMOVE, specs$FILTERING$FILTER_SUPPORTED_TYPES, specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA)), {
        toggle_filter(app, dataset_name, vn)
        app$run_js(gjsc(dataset_name, vn, fv))
        expect_identical(gmdfv(dataset_name, fv, TRUE), get_filter_state(app))
        expect_identical(nrow(get_filtered_dataset_list(app)[["dataset_1"]]), 3L)
      })

      test_that(sprintf("a `%s` NAs can be removed", vn)|>
    vdoc[["add_spec"]](c(specs$FILTERING$FILTER_ADD_REMOVE, specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA)), {
        app$run_js(set_NA_include_js_code(dataset_name, vn, FALSE))
        app$wait_for_idle()        
        expect_identical(gmdfv(dataset_name, fv, FALSE), get_filter_state(app))
        expect_identical(nrow(get_filtered_dataset_list(app)[["dataset_1"]]), 2L)
      })

      test_that(sprintf("a `%s` filter can be removed via menu", vn)|>
    vdoc[["add_spec"]](c(specs$FILTERING$FILTER_ADD_REMOVE)), {
        toggle_filter(app, dataset_name, vn)
        app$wait_for_idle()        
        expect_identical(get_filter_state(app), empty_filter)
      })

      test_that(sprintf("a `%s` filter can be added via menu and removed via x button", vn)|>
    vdoc[["add_spec"]](c(specs$FILTERING$FILTER_ADD_REMOVE)), {
        toggle_filter(app, dataset_name, vn)
        click_remove_filter_x_button(app, dataset_name, vn)
        app$wait_for_idle()
        expect_identical(get_filter_state(app), empty_filter)
      })
      app$click(selector = "dv-filter-clear-all-button")
      app$wait_for_idle()
    }
    app$stop()
  })

  test_that("filters can be exported", {
    app <- shinytest2::AppDriver$new(root_app$get_url())    
    file_dwnld <- paste(readLines(app$get_download("filter-export_code_button_input")), collapse = "\n")
    empty_filter_json <- r"--({"filters":{"datasets_filter":{"children":[]},"subject_filter":{"children":[]}},"dataset_list_name":"dataset_list_1"})--"
    expect_identical(gsub("[[:space:]]", "", file_dwnld), empty_filter_json)

    app$click(selector = "dv-filter-clear-all-button")
    app$wait_for_idle()
  })

  test_that("filters can be saved/restored/removed"|>
    vdoc[["add_spec"]](c(specs$FILTERING$FILTER_SAVE_RESTORE)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())
    filter_name <- "A"

    toggle_filter(app, dataset_name = "dataset_1", "range_var")
    add_saved_filter(app, filter_name)
    app$wait_for_idle()
    expected <- get_filter_state(app)

    app$click(selector = "dv-filter-clear-all-button")
    app$wait_for_idle()

    restore_saved_filter(app, filter_name)
    app$wait_for_idle()

    expect_identical(expected, get_filter_state(app))

    remove_saved_filter(app, filter_name)
    app$wait_for_idle()

    after_removal <- app$get_html(selector = sprintf(
      r"--(dv-filter-saved-state-button[data-saved-filter-name="%s"])--",
      filter_name
    ))
    expect_length(after_removal, 0)

    app$click(selector = "dv-filter-clear-all-button")
    app$wait_for_idle()
  })

  test_that("filters can be bookmarked and restored"|>
    vdoc[["add_spec"]](c(specs$FILTERING$FILTER_BOOKMARKABLE)), {
    app <- shinytest2::AppDriver$new(root_app$get_url())
    toggle_filter(app, "dataset_1", "range_var")
    app$wait_for_idle()
    add_saved_filter(app, "A")
    toggle_filter(app, "dataset_1", "subset_var")
    app$wait_for_idle()

    bmk_url <- app$get_js("window.location.href")
    bookmark_app <- suppressWarnings(shinytest2::AppDriver$new(bmk_url))
    bookmark_app$wait_for_idle()
    app_input_values <- app$get_values()[["input"]]
    bmk_input_values <- bookmark_app$get_values()[["input"]]
    expect_identical(app_input_values, bmk_input_values)
  })


  test_that("incompatible filters appear as such in the UI", {
    app <- start_app_driver(get_app_expr(fd = r"--(
      {
      "filters": {
        "datasets_filter": {
          "children": []
        },
        "subject_filter": {
          "children": [
            {
              "kind": "row_operation",
              "operation": "or",
              "children": [
                {
                  "kind": "filter",
                  "dataset": "dataset_1",
                  "operation": "select_range",
                  "variable": "range_var",
                  "min": 1,
                  "max": 5,
                  "include_NA": true
                }
              ]
            }
          ]
        }
      },
      "dataset_list_name": "dataset_list_1"
    }
      )--")) |> suppressWarnings()
    if (is.null(app)) stop("App could not be initialized")

    expect_true(app$get_js(r"--(document.querySelector('dv-filter-filter[data-filter-mode="simple"].dv-disabled-controls').classList.contains('dv-disabled-controls'))--"))
  })

  test_that("an can start with an specified filter"|>
    vdoc[["add_spec"]](c(specs$FILTERING$FILTER_INITIAL_STATE)), {
    fd <- r"--(
      {
      "filters": {
        "datasets_filter": {
          "children": []
        },
        "subject_filter": {
          "children": [
            {
              "kind": "row_operation",
              "operation": "or",
              "children": [
                {
                  "kind": "filter",
                  "dataset": "dataset_1",
                  "operation": "select_range",
                  "variable": "range_var",
                  "min": 1,
                  "max": 5,
                  "include_NA": true
                }
              ]
            }
          ]
        }
      },
      "dataset_list_name": "dataset_list_1"
    }
      )--"

    parsed_fd <- deserialize_filter_data_from_client(fd)

    app <- start_app_driver(get_app_expr(fd = fd)) |> suppressWarnings()
    if (is.null(app)) stop("App could not be initialized")
    expect_identical(get_filter_state(app), parsed_fd)
  })

  # dataset_list switching. What to test? More undefined cases than anything else...
  # Missing variables,


  ## OUT OF SCOPE THIS IS FILTERING AND IS COVERED IN OTHER TESTS CONSIDER IF WE WANT TO MOVE THEM HERE
  # Check the effect of subject filter in datasets
  # Check the effect of datasets on itself
  # Check the effect of datasets on itself
  # OOS
})
