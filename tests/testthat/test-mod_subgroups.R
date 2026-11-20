local({
  dataset_list <- structure(
    list(adsl = data.frame(USUBJID = paste0("S", 1:4), AGE = c(20, 30, 40, 50), SEX = c("F", "M", "F", "M"))),
    dataset_list_name = "DL"
  )

  range_json <- function(variable, min, max) {
    sprintf(
      paste0(
        '{"filters": {"datasets_filter": {"children": []}, "subject_filter": {"children": [',
        '{"kind": "filter", "dataset": "adsl", "operation": "select_range", "variable": "%s", ',
        '"min": %s, "max": %s, "include_NA": false}]}}, "dataset_list_name": "DL"}'
      ),
      variable,
      min,
      max
    )
  }

  two_category_subgroup <- function(variable) {
    list(label = NULL, cat_labels = c("TRUE", "FALSE"), cat_filters = range_json(variable, 0, 30))
  }

  # Clicks "add subgroup" for a two-category subgroup `new`, with `subgroup_filter_json` set in the subgroup filter
  add_subgroup <- function(existing_subgroups, subgroup_filter_json) {
    out <- new.env()
    out$notifications <- character(0)
    testthat::local_mocked_bindings(
      showNotification = function(ui, ...) out$notifications <- c(out$notifications, format(ui)),
      .package = "shiny"
    )
    args <- list(
      id = "sg",
      selected_dataset_list = shiny::reactive(dataset_list),
      subject_filter_dataset_name = "adsl",
      filter_key_var = "USUBJID"
    )
    shiny::testServer(mod_subgroup_server, args = args, {
      subgroups(existing_subgroups)
      session$setInputs(
        `filter-filter_state_json_input` = subgroup_filter_json,
        subgroup_cat_num = "2",
        subgroup_name = "new",
        subgroup_label = "",
        label_1 = "young",
        label_others = "other"
      )
      session$setInputs(add_subgroup = 1)
      out$subgroups <- names(subgroups())
    })
    as.list(out)
  }

  test_that(
    "a subgroup can be added while an existing one cannot be applied to the current data" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      # e.g. created on another dataset list, which had the variable
      res <- add_subgroup(list(old = two_category_subgroup("MISSING")), range_json("AGE", 0, 30))
      expect_identical(res[["subgroups"]], c("old", "new"))
      expect_identical(res[["notifications"]], character(0))
    }
  )

  test_that(
    "a subgroup can be added while an existing one is named like a column of the current data" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      res <- add_subgroup(list(SEX = two_category_subgroup("AGE")), range_json("AGE", 0, 30))
      expect_identical(res[["subgroups"]], c("SEX", "new"))
    }
  )

  test_that(
    "a subgroup that cannot be applied is refused with its own error" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      res <- add_subgroup(list(old = two_category_subgroup("AGE")), range_json("MISSING", 0, 30))
      expect_identical(res[["subgroups"]], "old")
      expect_length(res[["notifications"]], 1)
      expect_match(res[["notifications"]], "Skipping subgroup: `new`", fixed = TRUE)
    }
  )

  test_that(
    "adding a subgroup before the filter sends its state notifies that the filter is not ready" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      res <- add_subgroup(list(), subgroup_filter_json = "")
      expect_length(res[["subgroups"]], 0)
      expect_identical(res[["notifications"]], "Filter is not ready")
    }
  )
})
