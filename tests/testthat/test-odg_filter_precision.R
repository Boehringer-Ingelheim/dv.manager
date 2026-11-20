# The document parses the filter from the JSON text the app received, so range bounds that need 17 significant
# digits stay exact. Before, the parsed filter was deparsed into the code with 15 digits.

# `deparse()` writes -1/3 as -0.333333333333333 (above the true value) and 400/3 as 133.333333333333 (below it), so
# a filter on [-1/3, 400/3] rounded to 15 digits drops the subjects at both bounds
precision_dataset_list <- function() {
  adsl <- data.frame(USUBJID = c("S1", "S2", "S3"), VAL = c(-1 / 3, 0.5, 400 / 3))
  attr(adsl, "meta") <- list(mtime = as.POSIXct("2026-01-01", tz = "UTC"))
  list(DL = list(adsl = adsl))
}

# Bounds written with 17 significant digits, which read back as the same doubles, like the JSON the filter sends
precision_filter_json <- function(min, max) {
  sprintf(
    paste0(
      '{"filters": {"datasets_filter": {"children": []}, "subject_filter": {"children": [',
      '{"kind": "filter", "dataset": "adsl", "operation": "select_range", "variable": "VAL", ',
      '"min": %.17g, "max": %.17g, "include_NA": false}]}}, "dataset_list_name": "DL"}'
    ),
    min,
    max
  )
}

test_that("r_filter_from_json_filter() returns NA until the filter sends a state", {
  expect_identical(r_filter_from_json_filter(NULL), NA_character_)
  expect_identical(r_filter_from_json_filter(""), NA_character_)
  expect_identical(r_filter_from_json_filter(NA_character_), NA_character_)
})

test_that("r_filter_from_json_filter() parses range bounds exactly", {
  json <- precision_filter_json(-1 / 3, 400 / 3)
  res <- r_filter_from_json_filter(json)

  range_filter <- res[["parsed"]][["filters"]][["subject_filter"]][["children"]][[1]]
  expect_identical(range_filter[["min"]], -1 / 3)
  expect_identical(range_filter[["max"]], 400 / 3)
  expect_identical(res[["raw"]], json)
})

test_that("the generated code keeps the same rows as the app when filter bounds need 17 digits", {
  module_info <- resolve_module_list(list())
  testing_options <- list(
    afmm_static = list(data = precision_dataset_list(), module_names = module_info[["module_name"]]),
    filter_dataset_name = "adsl",
    module_info = module_info,
    filter_key = "USUBJID",
    filter_info = list(filter_default_state = NULL),
    enable_subgroup = FALSE
  )

  shiny::testServer(app_server_test(testing_options), {
    session$setInputs(selector = "DL")
    session$setInputs(`filter-filter_state_json_input` = precision_filter_json(-1 / 3, 400 / 3))

    # The download handler's path: the selected data stands in for `load_fn`, then expand, deparse and format
    ec <- shinymeta::newExpansionContext()
    ec$substituteMetaReactive(selected_dataset_list, function() shinymeta::metaExpr(app_selected_dataset_list))
    code <- shinymeta::expandChain(invisible(filtered_dataset_list()), .expansionContext = ec) |>
      shinymeta::formatCode(formatter = format_odg_code, width = 400L) |>
      as.character() |>
      paste(collapse = "\n")

    # Outside the package namespace, as when the Rmd from the zip is knitted again
    doc_env <- new.env(parent = globalenv())
    doc_env[["app_selected_dataset_list"]] <- selected_dataset_list()
    doc_filtered_dataset_list <- eval(parse(text = code), envir = doc_env)

    expect_identical(nrow(filtered_dataset_list()[["adsl"]]), 3L)
    expect_identical(doc_filtered_dataset_list, filtered_dataset_list())
  })
})

test_that("the generated code stops with the filter errors when the filter does not apply to the reloaded data", {
  module_info <- resolve_module_list(list())
  testing_options <- list(
    afmm_static = list(data = precision_dataset_list(), module_names = module_info[["module_name"]]),
    filter_dataset_name = "adsl",
    module_info = module_info,
    filter_key = "USUBJID",
    filter_info = list(filter_default_state = NULL),
    enable_subgroup = FALSE
  )

  shiny::testServer(app_server_test(testing_options), {
    session$setInputs(selector = "DL")
    session$setInputs(`filter-filter_state_json_input` = precision_filter_json(-1 / 3, 400 / 3))

    ec <- shinymeta::newExpansionContext()
    ec$substituteMetaReactive(selected_dataset_list, function() shinymeta::metaExpr(app_selected_dataset_list))
    code <- shinymeta::expandChain(invisible(filtered_dataset_list()), .expansionContext = ec) |>
      shinymeta::formatCode(formatter = format_odg_code, width = 400L) |>
      as.character() |>
      paste(collapse = "\n")

    # `load_fn` reloads the data in the document, here it no longer has the filtered variable
    reloaded_dataset_list <- selected_dataset_list()
    reloaded_dataset_list[["adsl"]][["VAL"]] <- NULL
    doc_env <- new.env(parent = globalenv())
    doc_env[["app_selected_dataset_list"]] <- reloaded_dataset_list

    expect_error(
      eval(parse(text = code), envir = doc_env),
      "may differ from the data in the app.*does not contain col `VAL`"
    )
  })
})
