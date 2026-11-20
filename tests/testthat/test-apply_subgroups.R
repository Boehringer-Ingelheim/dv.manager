local({
  dataset_list <- structure(
    list(adsl = data.frame(USUBJID = paste0("S", 1:4), AGE = c(20, 30, 40, 50))),
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

  # The last label is the category of the subjects no filter selects
  subgroup <- function(cat_filters, cat_labels = c(paste0("cat", seq_along(cat_filters)), "other")) {
    list(label = "Subgroup", cat_labels = cat_labels, cat_filters = cat_filters)
  }

  run <- function(subgroups) apply_subgroups(dataset_list, "adsl", "USUBJID", subgroups)

  test_that(
    "apply_subgroups adds the valid subgroups that follow one that fails" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      res <- run(list(
        g1 = subgroup(range_json("AGE", 0, 30)),
        g2 = subgroup(range_json("MISSING", 0, 30)),
        g3 = subgroup(range_json("AGE", 35, 60))
      ))

      adsl <- res[["result"]][["dataset_list"]][["adsl"]]
      expect_identical(names(adsl), c("USUBJID", "AGE", "g1", "g3"))
      expect_identical(as.character(adsl[["g1"]]), c("cat1", "cat1", "other", "other"))
      expect_identical(as.character(adsl[["g3"]]), c("other", "other", "cat1", "cat1"))
      expect_identical(res[["result"]][["correct_subgroups"]], c("g1", "g3"))
      expect_identical(res[["result"]][["incorrect_subgroups"]], "g2")
      expect_length(res[["error_list"]][["get_messages"]](), 1)
      expect_match(res[["error_list"]][["get_messages"]](), "Skipping subgroup: `g2`", fixed = TRUE)
      expect_identical(attr(res[["result"]][["dataset_list"]], "dataset_list_name"), "DL")
    }
  )

  test_that(
    "apply_subgroups reports every skipped subgroup as incorrect and keeps the others" |>
      vdoc[["add_spec"]](specs$SUBGROUPS$SUBGROUP_CREATION),
    {
      res <- run(list(
        AGE = subgroup(range_json("AGE", 0, 30)), # already a column
        duplicated_labels = subgroup(range_json("AGE", 0, 30), cat_labels = c("a", "a")),
        overlapping = subgroup(c(range_json("AGE", 0, 30), range_json("AGE", 25, 60))),
        valid = subgroup(range_json("AGE", 0, 30))
      ))

      adsl <- res[["result"]][["dataset_list"]][["adsl"]]
      expect_identical(names(adsl), c("USUBJID", "AGE", "valid"))
      expect_identical(res[["result"]][["correct_subgroups"]], "valid")
      expect_identical(res[["result"]][["incorrect_subgroups"]], c("AGE", "duplicated_labels", "overlapping"))
      expect_length(res[["error_list"]][["get_messages"]](), 3)
    }
  )

  # The code of the output documentation, with the data `load_fn` would reload replaced by `doc_dataset_list`
  doc_filtered_dataset_list <- function(subgroups, doc_dataset_list) {
    module_info <- resolve_module_list(list())
    testing_options <- list(
      afmm_static = list(data = list(DL = dataset_list), module_names = module_info[["module_name"]]),
      filter_dataset_name = "adsl",
      module_info = module_info,
      filter_key = "USUBJID",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = TRUE
    )
    empty_filter <- paste0(
      '{"filters": {"datasets_filter": {"children": []}, "subject_filter": {"children": []}},',
      ' "dataset_list_name": "DL"}'
    )

    res <- NULL
    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DL")
      subgroups[["subgroups"]](!!subgroups)
      # The filter info only updates when the filter sends its state, which the filter does after subgroups change
      session$setInputs(`filter-filter_state_json_input` = empty_filter)

      ec <- shinymeta::newExpansionContext()
      ec$substituteMetaReactive(selected_dataset_list, function() shinymeta::metaExpr(doc_dataset_list))
      code <- shinymeta::expandChain(invisible(filtered_dataset_list()), .expansionContext = ec) |>
        shinymeta::formatCode(formatter = format_odg_code, width = 400L) |>
        as.character() |>
        paste(collapse = "\n")

      doc_env <- new.env(parent = globalenv())
      doc_env[["doc_dataset_list"]] <- !!doc_dataset_list
      res <<- list(
        app = filtered_dataset_list(),
        doc = tryCatch(eval(parse(text = code), envir = doc_env), error = function(e) e)
      )
    }) |> rlang::inject()
    res
  }

  test_that("the generated code keeps the subgroups the app left out when the data is the same", {
    res <- doc_filtered_dataset_list(
      list(valid = subgroup(range_json("AGE", 0, 30)), failing = subgroup(range_json("MISSING", 0, 30))),
      doc_dataset_list = dataset_list
    )

    expect_identical(names(res[["app"]][["adsl"]]), c("USUBJID", "AGE", "valid"))
    expect_identical(res[["doc"]], res[["app"]])
  })

  test_that("the generated code stops when the subgroups do not apply to the reloaded data as in the app", {
    # `load_fn` reloads the data in the document, here without the variable a subgroup is defined on
    reloaded_dataset_list <- dataset_list
    reloaded_dataset_list[["adsl"]][["AGE"]] <- NULL

    res <- doc_filtered_dataset_list(list(valid = subgroup(range_json("AGE", 0, 30))), reloaded_dataset_list)

    expect_s3_class(res[["doc"]], "error")
    expect_match(
      conditionMessage(res[["doc"]]),
      "may differ from the data in the app.*not applied in the app: \\[\\], in this document: \\[valid\\]"
    )
    expect_match(conditionMessage(res[["doc"]]), "Skipping subgroup: `valid`", fixed = TRUE)
  })
})
