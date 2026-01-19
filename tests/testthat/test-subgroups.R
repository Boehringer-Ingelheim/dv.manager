local({
  set_subgroup_filter <- function(app, filter_id, dataset_name, var_name, filter_values) {
    clear_all_selector <- sprintf("#%s dv-filter-clear-all-button", filter_id)
    app$click(selector = clear_all_selector)

    selector <- sprintf(
      "#%s dv-filter-dataset-filter[data-dataset-name='%s'] div.card-header button",
      filter_id,
      dataset_name
    )
    app$click(selector = selector)
    app$wait_for_idle()

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
    app$wait_for_idle()

    input_selector <- sprintf(
      "#%s dv-filter-dataset-filter[data-dataset-name='%s'] dv-filter-variable-filter[data-variable='%s'] select[filter_value]",
      filter_id,
      dataset_name,
      var_name
    )

    values <- paste(sprintf("'%s'", filter_values), collapse = ",")

    set_js <- sprintf(
      r"--{
            let select_el = document.querySelector("%s");
            $(select_el).selectpicker('val', [%s]);
            $(select_el).trigger("changed.bs.select");
            }--",
      input_selector,
      values
    )

    app$run_js(set_js)
    app$wait_for_idle()
  }

  get_unfiltered_dataset_from_afmm <- function(app) {
    afmm <- app$get_value(export = "afmm-afmm")
    shiny::isolate(afmm[["unfiltered_dataset_list"]]())
  }

  prepare_subgroup <- function(app, subgroup) {
    app$click(selector = "[data-value='Subgroup']")
    app$wait_for_idle()

    app$set_inputs(`subgroup-subgroup_name` = subgroup[["name"]])
    app$set_inputs(`subgroup-subgroup_label` = subgroup[["label"]])

    cat_num <- app$get_value(input = "subgroup-subgroup_cat_num")
    if (cat_num != as.character(length(subgroup[["categories"]]))) {
      app$set_inputs(`subgroup-subgroup_cat_num` = as.character(length(subgroup[["categories"]])))
      app$wait_for_idle()
    }

    cat_num <- length(subgroup[["categories"]])

    get_assign_selector <- function(x) {
      sprintf(
        '#subgroup-subgroup_cat_container > div:nth-child(%d) > div > button[title="Assign filtered subjects"]',
        x
      )
    }

    for (idx in seq_len(cat_num - 1)) {
      curr_cat <- subgroup[["categories"]][[idx]]
      args <- stats::setNames(
        list(curr_cat[["label"]]),
        c(sprintf("subgroup-label_%d", idx))
      )
      do.call(app$set_inputs, args)
      app$wait_for_idle()
      set_subgroup_filter(
        app,
        "subgroup-filter",
        subgroup[["dataset"]],
        curr_cat[["filter"]][["var"]],
        curr_cat[["filter"]][["val"]]
      )
      app$wait_for_idle()

      if (cat_num > 2) {
        app$click(selector = get_assign_selector(idx))
        app$wait_for_idle()
      }
    }

    app$set_inputs(`subgroup-label_others` = subgroup[["categories"]][[length(subgroup[["categories"]])]][["label"]])
    app$wait_for_idle()
  }

  get_exported_values <- function(app, dataset_name) {
    subgroups <- shiny::isolate(app$get_value(export = "subgroup-subgroups")())

    afmm <- app$get_value(export = "afmm-afmm")
    unfiltered_dataset_list <- shiny::isolate(afmm[["unfiltered_dataset_list"]]())
    relevant_dataset <- unfiltered_dataset_list[[dataset_name]]

    return(
      list(subgroups = subgroups, dataset = relevant_dataset)
    )
  }

  root_app <- start_app_driver(
    {
      datasets <- list(
        list1 = list(
          dataset1 = data.frame(
            var1 = c("a1", "b1", "c1"),
            var2 = c("d2", "e2", "f2")
          ),
          dataset2 = data.frame(
            var1 = c("a2", "b2", "c2"),
            var2 = c("d2", "e2", "f2")
          )
        ),
        list2 = list(
          dataset1 = data.frame(
            var1 = c("a1", "b1", "c1")
          ),
          dataset2 = data.frame(
            var1 = c("a2", "b2", "c2"),
            var2 = c("d2", "e2", "f2")
          )
        )
      )

      app <- dv.manager::run_app(
        data = datasets,
        module_list = list(
          afmm = mod_afmm_export("afmm")
        ),
        filter_type = "development",
        enable_subgroup = TRUE,
        filter_data = "dataset1",
        filter_key = "var1"
      )

      app
    }
  )

  if (is.null(app)) {
    skip("App could not be initialized")
  }

  test_that(
    "User can create a 2-category subgroup" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(
        root_app$get_url()
      )
      subgroup <- list(
        name = "group",
        label = "label",
        dataset = "dataset1",
        categories = list(
          list(
            label = "label1",
            filter = list(
              var = "var1",
              val = c("a1", "b1")
            )
          ),
          list(
            label = "excluded",
            filter = list(
              var = "var1",
              val = c("c1")
            )
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      exported <- get_exported_values(app, subgroup[["dataset"]])
      dataset <- exported[["dataset"]]

      expect_true(subgroup[["name"]] %in% names(dataset))
      expect_identical(attr(dataset[[subgroup[["name"]]]], "label"), subgroup[["label"]])

      subgroup_var <- dataset[[subgroup[["name"]]]]
      expect_identical(
        unique(as.character(subgroup_var[subgroup_var == subgroup[["categories"]][[1]][["label"]]])),
        subgroup[["categories"]][[1]][["label"]]
      )

      expect_identical(
        unique(as.character(subgroup_var[subgroup_var == subgroup[["categories"]][[2]][["label"]]])),
        subgroup[["categories"]][[2]][["label"]]
      )
    }
  )

  test_that(
    "User can create a 3-category subgroup with non-overlapping filters" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(
        root_app$get_url()
      )

      subgroup <- list(
        name = "group_3cat",
        label = "Three Category Group",
        dataset = "dataset1",
        categories = list(
          list(
            label = "Category A",
            filter = list(var = "var1", val = c("a1"))
          ),
          list(
            label = "Category B",
            filter = list(var = "var1", val = c("b1"))
          ),
          list(
            label = "Others",
            filter = list(var = "var1", val = c("c1"))
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      exported <- get_exported_values(app, subgroup[["dataset"]])
      dataset <- exported[["dataset"]]

      expect_true(subgroup[["name"]] %in% names(dataset))
      expect_identical(attr(dataset[[subgroup[["name"]]]], "label"), subgroup[["label"]])

      subgroup_var <- dataset[[subgroup[["name"]]]]
      expect_identical(
        unique(as.character(subgroup_var[subgroup_var == subgroup[["categories"]][[1]][["label"]]])),
        subgroup[["categories"]][[1]][["label"]]
      )
      expect_identical(
        unique(as.character(subgroup_var[subgroup_var == subgroup[["categories"]][[2]][["label"]]])),
        subgroup[["categories"]][[2]][["label"]]
      )
      expect_identical(
        unique(as.character(subgroup_var[subgroup_var == subgroup[["categories"]][[3]][["label"]]])),
        subgroup[["categories"]][[3]][["label"]]
      )
    }
  )

  test_that(
    "Overlapping categories show conflict indicators" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(
        root_app$get_url()
      )

      subgroup <- list(
        name = "group_conflict",
        label = "Conflicting Group",
        dataset = "dataset1",
        categories = list(
          list(
            label = "Category A",
            filter = list(var = "var1", val = c("a1", "b1"))
          ),
          list(
            label = "Category B",
            filter = list(var = "var1", val = c("b1", "c1"))
          ),
          list(
            label = "Others"
          )
        )
      )

      prepare_subgroup(app, subgroup)

      conflict_icon <- app$get_html(selector = "#subgroup-subgroup_cat_container .fa-circle-xmark")
      expect_true(length(conflict_icon) > 0 && !anyNA(conflict_icon))
    }
  )

  test_that(
    "Subgroup with invalid name is rejected" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(
        root_app$get_url()
      )

      subgroup <- list(
        name = "invalid name!",
        label = "Test Label",
        dataset = "dataset1",
        categories = list(
          list(
            label = "Included",
            filter = list(var = "var1", val = c("a1", "b1"))
          ),
          list(
            label = "Excluded"
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      exported <- get_exported_values(app, subgroup[["dataset"]])

      expect_false(subgroup[["name"]] %in% names(exported[["subgroups"]]))
      expect_false(subgroup[["name"]] %in% names(exported[["dataset"]]))

      app$set_inputs(`subgroup-subgroup_name` = "")

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      exported <- get_exported_values(app, subgroup[["dataset"]])
      expect_equal(length(exported[["subgroups"]]), 0)
      expect_false("" %in% names(exported[["dataset"]]))
    }
  )

  test_that(
    "Subgroup with duplicate labels is not applied" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(
        root_app$get_url()
      )

      subgroup <- list(
        name = "group_dup_labels",
        label = "Duplicate Labels Test",
        dataset = "dataset1",
        categories = list(
          list(
            label = "Same Label",
            filter = list(var = "var1", val = c("a1", "b1"))
          ),
          list(
            label = "Same Label",
            filter = list(var = "var1", val = c("c1"))
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      exported <- get_exported_values(app, subgroup[["dataset"]])

      expect_false(subgroup[["name"]] %in% names(exported[["dataset"]]))
    }
  )

  test_that(
    "Subgroup name colliding with existing column is rejected" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      skip_if_not_running_shiny_tests()

      app <- shinytest2::AppDriver$new(
        root_app$get_url()
      )

      subgroup <- list(
        name = "var1",
        label = "Collision Test",
        dataset = "dataset1",
        categories = list(
          list(
            label = "Included",
            filter = list(var = "var1", val = c("a1", "b1"))
          ),
          list(
            label = "Excluded"
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      exported <- get_exported_values(app, subgroup[["dataset"]])

      expect_false(subgroup[["name"]] %in% names(exported[["subgroups"]]))

      expect_true(all(exported[["dataset"]][[subgroup[["name"]]]] %in% c("a1", "b1", "c1")))
    }
  )

  test_that(
    "subgroups can be bookmarked and restored" |>
      vdoc[["add_spec"]](c(specs$SUBGROUPS$SUBGROUP_BOOKMARKABLE)),
    {
      app <- shinytest2::AppDriver$new(root_app$get_url())
      on.exit(app$stop(), add = TRUE, after = FALSE)
      subgroup <- list(
        name = "group",
        label = "label",
        dataset = "dataset1",
        categories = list(
          list(
            label = "label1",
            filter = list(
              var = "var1",
              val = c("a1", "b1")
            )
          ),
          list(
            label = "excluded",
            filter = list(
              var = "var1",
              val = c("c1")
            )
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      app_subgroups <- get_exported_values(app, subgroup[["dataset"]])[["subgroups"]]

      bmk_url <- app$get_js("window.location.href")
      bookmark_app <- suppressWarnings(shinytest2::AppDriver$new(bmk_url))
      on.exit(app$stop(), add = TRUE, after = FALSE)
      bookmark_app$wait_for_idle()

      bmk_exported <- get_exported_values(app, subgroup[["dataset"]])
      bmk_dataset <- bmk_exported[["dataset"]]

      expect_true(subgroup[["name"]] %in% names(bmk_dataset))
    }
  )

  test_that(
    "subgroups are applied on dataset switch" |>
      vdoc[["add_spec"]](c(specs$SUBGROUPS$SUBGROUP_CREATION)),
    {
      app <- shinytest2::AppDriver$new(root_app$get_url())
      on.exit(app$stop(), add = TRUE, after = FALSE)
      subgroup <- list(
        name = "group",
        label = "label",
        dataset = "dataset1",
        categories = list(
          list(
            label = "label1",
            filter = list(
              var = "var1",
              val = c("a1")
            )
          ),
          list(
            label = "excluded",
            filter = list(
              var = "var1",
              val = c("b1", "c1")
            )
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      app$set_inputs("selector" = "list2")

      exported <- get_exported_values(app, subgroup[["dataset"]])
      dataset <- exported[["dataset"]]
      expect_true(subgroup[["name"]] %in% names(dataset))
    }
  )

  test_that(
    "subgroups are excluded when the filter cannot be applied" |>
      vdoc[["add_spec"]](c(specs$SUBGROUPS$SUBGROUP_CREATION)),
    {
      app <- shinytest2::AppDriver$new(root_app$get_url())
      on.exit(app$stop(), add = TRUE, after = FALSE)
      subgroup <- list(
        name = "group",
        label = "label",
        dataset = "dataset1",
        categories = list(
          list(
            label = "label1",
            filter = list(
              var = "var2",
              val = c("d2")
            )
          ),
          list(
            label = "excluded",
            filter = list(
              var = "var2",
              val = c("e2", "f2")
            )
          )
        )
      )

      prepare_subgroup(app, subgroup)

      app$click(selector = "#subgroup-add_subgroup")
      app$wait_for_idle()

      app$set_inputs("selector" = "list2")

      exported <- get_exported_values(app, subgroup[["dataset"]])
      dataset <- exported[["dataset"]]
      expect_false(subgroup[["name"]] %in% names(dataset))

      expect_match(app$get_html("#subgroup-subgroups > div > span"), "bg-danger", fixed = TRUE)
    }
  )

  # test dataset switching works fine
  # test dataset switching and restarting the app with one less variable
  # check that data-value = "subgroup" returns several html items
})
