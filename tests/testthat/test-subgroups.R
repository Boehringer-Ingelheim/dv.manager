local({
  set_subgroup_filter <- function(app, filter_id, dataset_name, var_name, filter_values) {
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

    app$run_js(set_js)
    app$wait_for_idle()
  }

  test_that("User can create a binary subgroup", {
    skip_if_not_running_shiny_tests()

    # Test configuration variables
    subgroup_name <- "group1"
    subgroup_label <- "My First Group"
    category_included_label <- "Included"
    category_excluded_label <- "Excluded"
    filter_dataset <- "dataset1"
    filter_variable <- "var1"
    included_values <- c("a1", "b1")
    excluded_values <- c("c1")

    app <- start_app_driver(
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
              var1 = c("a1", "b1", "c1"),
              var2 = c("d1", "e1", "f1")
            ),
            dataset2 = data.frame(
              var1 = c("a2", "b2", "c2"),
              var2 = c("d2", "e2", "f2")
            )
          )
        )

        dv.manager::run_app(
          data = datasets,
          module_list = list(
            afmm = mod_afmm_export("afmm")
          ),
          filter_type = "development",
          enable_subgroup = TRUE,
          filter_data = "dataset1",
          filter_key = "var1"
        )
      }
    )

    if (is.null(app)) {
      skip("App could not be initialized")
    }

    # Navigate to the Subgroup tab
    app$click(selector = "[data-value='Subgroup']")
    app$wait_for_idle()

    # Fill in the subgroup name
    app$set_inputs(`subgroup-subgroup_name` = subgroup_name)

    # Fill in the subgroup label (optional)
    app$set_inputs(`subgroup-subgroup_label` = subgroup_label)

    # Set category labels
    app$set_inputs(`subgroup-label_1` = category_included_label)
    app$set_inputs(`subgroup-label_others` = category_excluded_label)

    # Apply a filter using the filter UI within the subgroup module
    set_subgroup_filter(app, "subgroup-filter", filter_dataset, filter_variable, included_values)

    # Click Add subgroup button
    app$click(selector = "#subgroup-add_subgroup")
    app$wait_for_idle()

    # Verify the subgroup was created by checking exported test values
    subgroups <- app$get_value(export = "subgroup-subgroups")
    subgroups_r <- shiny::isolate(subgroups())
    expect_true(subgroup_name %in% names(subgroups_r))
    expect_equal(subgroups_r[[subgroup_name]][["label"]], subgroup_label)
    expect_equal(subgroups_r[[subgroup_name]][["cat_labels"]], c(category_included_label, category_excluded_label))

    afmm <- app$get_value(export = "afmm-afmm")
    unfiltered_dataset_list <- shiny::isolate(afmm[["unfiltered_dataset_list"]]())

    # Check that subgroup column exists in the dataset
    expect_true(subgroup_name %in% names(unfiltered_dataset_list[[filter_dataset]]))

    # Check that included values have the correct category label
    dataset <- unfiltered_dataset_list[[filter_dataset]]
    expect_true(
      as.character(dataset[dataset[[filter_variable]] == included_values[[1]], subgroup_name]) ==
        category_included_label ||
        as.character(dataset[dataset[[filter_variable]] == included_values[[2]], subgroup_name]) ==
          category_included_label
    )

    # Check that excluded value has the correct category label
    expect_equal(
      as.character(dataset[dataset[[filter_variable]] == excluded_values[[1]], subgroup_name]),
      category_excluded_label
    )
  })
})
