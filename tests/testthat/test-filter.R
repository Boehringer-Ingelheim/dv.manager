local({
  date_col <- as.Date("2024-01-01") + c(0L:4L, NA)

  data_list <- list(
    d = data.frame(
      row.names = 1:6,
      range_col = c(1.0:5.0, NA),
      date_col = date_col,
      posix_col = as.POSIXct(date_col),
      subset_col = factor(c(letters[1:5], NA)),
      sbj_col = paste0("SBJ-", 1:6)
    )
  )

  # Column filters

  test_that("process_dataset_filter_element - select_range filter returns mask excluding NAs", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = FALSE,
      field = "range_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_range filter returns mask including NAs", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = TRUE,
      field = "range_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - select_subset filter returns mask excluding NAs", {
    e <- list(
      kind = "filter",
      operation = "select_subset",
      values = c("b", "c", "d"),
      include_NA = FALSE,
      field = "subset_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_subset filter returns mask including NAs", {
    e <- list(
      kind = "filter",
      operation = "select_subset",
      values = c("b", "c", "d"),
      include_NA = TRUE,
      field = "subset_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask excluding NAs for Date type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = FALSE,
      field = "date_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask including NAs for Date type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = TRUE,
      field = "date_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask excluding NAs for POSIX type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = FALSE,
      field = "posix_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask including NAs for POSIX type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = TRUE,
      field = "posix_col",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - dataset filters fail when the current_table_is not the same as the table in the filter", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = FALSE,
      field = "range_col",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "INCORRECT_D"),
      regexp = "Filtering on the wrong dataset",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - dataset filters fail when the table does not contain the filtered field", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = FALSE,
      field = "NON_EXISTING_FIELD",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "data[['d']] does not contain col `NON_EXISTING_FIELD`",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - select_range filter fail when the field is not numeric", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = FALSE,
      field = "subset_col",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "Field values must be numerical",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - select_date filter fail when the field is not numeric", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      max = 4,
      min = 2,
      include_NA = FALSE,
      field = "subset_col",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "Field values must be POSIX.ct or Date",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - select_range min must be lower or equal than max", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 2,
      min = 4,
      include_NA = FALSE,
      field = "range_col",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "min <= max",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - select_date min must be lower or equal than max", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-04",
      max = "2024-01-02",
      include_NA = TRUE,
      field = "posix_col",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "min <= max",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - fails for unknown operations", {
    e <- list(
      kind = "filter",
      operation = "UNKNOWN OPERATION",
      min = "2024-01-04",
      max = "2024-01-02",
      include_NA = TRUE,
      field = "posix_col",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "Operation unknown: `UNKNOWN OPERATION`",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - and filter operation returns mask for 1 element", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )

    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - and filter operation returns mask for n elements", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - and filter operation fails when it does not have at least 1 children", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list()
    )
    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "`and` operation requires at least one child",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - or filter operation returns mask for 1 element", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )

    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - or filter operation returns mask for n elements", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 1,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - or filter operation fails when it does not have at least 1 children", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list()
    )
    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "`or` operation requires at least one child",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - not filter operation returns mask", {
    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )

    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, !c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - not filter operation fails when it does not have exactly 1 children", {
    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list()
    )
    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "`not` operation requires exactly one child",
      fixed = TRUE
    )

    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )
    expect_error(
      process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"),
      regexp = "`not` operation requires exactly one child",
      fixed = TRUE
    )
  })

  local({
    data_list <- list(
      d1 = data.frame(var1 = c("a", "NOT IN SUBSET")),
      d2 = data.frame(var2 = "b"),
      d3 = data.frame(var3 = "c"),
      d4 = data.frame(var4 = "d")
    )

    e <- list(
      kind = "datasets",
      children = list(
        list(
          kind = "dataset",
          name = "d1",
          children = list(
            list(
              kind = "filter",
              operation = "select_subset",
              values = c("a"),
              include_NA = FALSE,
              field = "var1",
              dataset = "d1"
            )
          )
        ),
        list(
          kind = "dataset",
          name = "d2",
          children = list()
        )
      )
    )

    r <- create_datasets_filter_masks(data_list, e)

    test_that("create_dataset_filter_masks creates a mask per dataset in the filter and none for those not in the filter", {
      expect_identical(names(r), c("d1", "d2"))
      expect_length(r, 2)
    })

    test_that("create_dataset_filter_masks creates a mask for dataset filters with children", {
      expect_identical(r[["d1"]], c(TRUE, FALSE))
    })

    test_that("create_dataset_filter_masks creates a TRUE mask for dataset filters with no children", {
      expect_true(r[["d2"]])
    })
  })

  test_that("create_datasets_filter_masks fails when a dataset appears more than once in the dataset filters", {
    data_list <- list(
      d1 = data.frame(var1 = "a"),
      d2 = data.frame(var2 = "b")
    )

    e <- list(
      kind = "datasets",
      children = list(
        list(),
        list(
          kind = "dataset",
          name = "d1",
          children = list()
        )
      )
    )

    expect_error(
      create_datasets_filter_masks(data_list, e),
      regexp = "a dataset can only appear once inside dataset_filters",
      fixed = TRUE
    )
  })

  test_that("create_datasets_filter_masks fails when a children is not of kind dataset", {
    data_list <- list(
      d1 = data.frame(var1 = "a"),
      d2 = data.frame(var2 = "b")
    )

    e <- list(
      kind = "datasets",
      children = list(
        list(
          kind = "NOT DATASET",
          name = "d1",
          children = list()
        )
      )
    )

    expect_error(
      create_datasets_filter_masks(data_list, e),
      regexp = "dataset_filters children can only be of kind `dataset`",
      fixed = TRUE
    )
  })

  test_that("process_subject_filter_element returns subjects set", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = FALSE,
      field = "range_col",
      dataset = "d"
    )
    subject_set <- process_subject_filter_element(data_list = data_list, element = e, sbj_var = "sbj_col", complete_subject_list = data_list[["d"]][["sbj_col"]])
    expect_identical(subject_set, c("SBJ-2", "SBJ-3", "SBJ-4"))
  })

  test_that("process_subject_filter_element - and filter operation returns subject set for 1 element", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )

    subject_set <- process_subject_filter_element(data_list = data_list, element = e, sbj_var = "sbj_col", complete_subject_list = data_list[["d"]][["sbj_col"]])
    expect_identical(subject_set, c("SBJ-2", "SBJ-3", "SBJ-4"))
  })

  test_that("process_subject_filter_element - and filter operation returns subject set for n elements", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )
    subject_set <- process_subject_filter_element(data_list = data_list, element = e, sbj_var = "sbj_col", complete_subject_list = data_list[["d"]][["sbj_col"]])
    expect_identical(subject_set, c("SBJ-2", "SBJ-3"))
  })

  test_that("process_subject_filter_element - and filter operation fails when it does not have at least 1 children (subject filter)", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list()
    )
    expect_error(
      process_subject_filter_element(data_list = data_list, element = e),
      regexp = "`and` operation requires at least one child",
      fixed = TRUE
    )
  })

  test_that("process_subject_filter_element - or filter operation returns subject set for 1 element", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )

    subject_set <- process_subject_filter_element(data_list = data_list, element = e, sbj_var = "sbj_col", complete_subject_list = data_list[["d"]][["sbj_col"]])
    expect_identical(subject_set, c("SBJ-2", "SBJ-3", "SBJ-4"))
  })

  test_that("process_subject_filter_element - or filter operation returns subject set for n elements", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 1,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )
    subject_set <- process_subject_filter_element(data_list = data_list, element = e, sbj_var = "sbj_col", complete_subject_list = data_list[["d"]][["sbj_col"]])
    expect_identical(sort(subject_set), c("SBJ-1", "SBJ-2", "SBJ-3", "SBJ-4"))
  })

  test_that("process_subject_filter_element - or filter operation fails when it does not have at least 1 children (subject filter)", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list()
    )
    expect_error(
      process_subject_filter_element(data_list = data_list, element = e),
      regexp = "`or` operation requires at least one child",
      fixed = TRUE
    )
  })

  test_that("process_subject_filter_element - not filter operation returns subject set", {
    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )

    subject_set <- process_subject_filter_element(data_list = data_list, element = e, sbj_var = "sbj_col", complete_subject_list = data_list[["d"]][["sbj_col"]])
    expect_identical(subject_set, c("SBJ-1", "SBJ-5", "SBJ-6"))
  })

  test_that("process_subject_filter_element - not filter operation fails when it does not have exactly 1 children (subject filter)", {
    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list()
    )
    expect_error(
      process_subject_filter_element(data_list = data_list, element = e),
      regexp = "`not` operation requires exactly one child",
      fixed = TRUE
    )

    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list(
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          field = "range_col",
          dataset = "d"
        )
      )
    )
    expect_error(
      process_subject_filter_element(data_list = data_list, element = e),
      regexp = "`not` operation requires exactly one child",
      fixed = TRUE
    )
  })

  test_that("dataset filter and subject filter fails when a field of element is not present", {
    e <- list()
    expect_error(process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d"))
    expect_error(process_subject_filter_element(data_list = data_list, element = e, sbj_var = "sbj_col", complete_subject_list = data_list[["d"]][["sbj_col"]]))
  })

  test_that("create_subject_set returns a subject set", {
    data_list <- list(
      d1 = data.frame(var1 = c("a", "b"), sbj = c("SBJ1", "SBJ2")),
      d2 = data.frame(var1 = "a", sbj = "SBJ3")
    )

    e <- list(
      children = list(
        list(
          kind = "filter",
          operation = "select_subset",
          values = c("a"),
          include_NA = FALSE,
          field = "var1",
          dataset = "d1"
        )
      )
    )

    expect_identical(
      create_subject_set(data_list = data_list, e, "sbj"),
      "SBJ1"
    )
  })

  test_that("create_subject_set returns NA for an empty filter", {
    data_list <- list(
      d1 = data.frame(var1 = c("a", "b"), sbj = c("SBJ1", "SBJ2")),
      d2 = data.frame(var1 = "a", sbj = "SBJ3")
    )

    e <- list(
      children = list()
    )

    expect_true(is.na(create_subject_set(data_list = data_list, e, "sbj")))
  })
})

local({
  test_that("get_single_filter_data show correct counts and entries for character columns", {
    d <- data.frame(
      var = c("A", "A", "B", NA_character_)
    )
    attr(d[["var"]], "label") <- "var_label"
    r <- get_single_filter_data(d)
    expect_length(r, 1)
    expect_identical(
      r[[1]],
      list(
        name = jsonlite::unbox("var"),
        label = jsonlite::unbox("var_label"),
        kind = jsonlite::unbox("categorical"),
        NA_count = jsonlite::unbox(1L),
        values_count = list(
          list(value = jsonlite::unbox("A"), count = jsonlite::unbox(2L)),
          list(value = jsonlite::unbox("B"), count = jsonlite::unbox(1L))
        )
      )
    )
  })

  test_that("get_single_filter_data show correct counts and entries for factor columns", {
    d <- data.frame(
      var = factor(c("A", "A", "B", NA_character_))
    )
    attr(d[["var"]], "label") <- "var_label"
    r <- get_single_filter_data(d)
    expect_length(r, 1)
    expect_identical(
      r[[1]],
      list(
        name = jsonlite::unbox("var"),
        label = jsonlite::unbox("var_label"),
        kind = jsonlite::unbox("categorical"),
        NA_count = jsonlite::unbox(1L),
        values_count = list(
          list(value = jsonlite::unbox("A"), count = jsonlite::unbox(2L)),
          list(value = jsonlite::unbox("B"), count = jsonlite::unbox(1L))
        )
      )
    )
  })

  test_that("get_single_filter_data show correct max and min for numeric columns", {
    d <- data.frame(
      var = c(1, 1, 2, NA_real_)
    )
    attr(d[["var"]], "label") <- "var_label"
    r <- get_single_filter_data(d)
    expect_length(r, 1)
    expect_identical(
      r[[1]],
      list(
        name = jsonlite::unbox("var"),
        label = jsonlite::unbox("var_label"),
        kind = jsonlite::unbox("numerical"),
        NA_count = jsonlite::unbox(1L),
        min = jsonlite::unbox(1),
        max = jsonlite::unbox(2)
      )
    )
  })

  test_that("get_single_filter_data show correct max and min for Date columns", {
    d <- data.frame(
      var = as.Date(c("2024-01-01", "2024-01-02", NA))
    )
    attr(d[["var"]], "label") <- "var_label"
    r <- get_single_filter_data(d)
    expect_length(r, 1)
    expect_identical(
      r[[1]],
      list(
        name = jsonlite::unbox("var"),
        label = jsonlite::unbox("var_label"),
        kind = jsonlite::unbox("date"),
        NA_count = jsonlite::unbox(1L),
        min = jsonlite::unbox(as.Date(c("2024-01-01"))),
        max = jsonlite::unbox(as.Date(c("2024-01-02")))
      )
    )
  })

  test_that("get_single_filter_data show correct max and min for POSIXct columns", {
    d <- data.frame(
      var = as.POSIXct(as.Date(c("2024-01-01", "2024-01-02", NA)))
    )
    attr(d[["var"]], "label") <- "var_label"
    r <- get_single_filter_data(d)
    expect_length(r, 1)
    expect_identical(
      r[[1]],
      list(
        name = jsonlite::unbox("var"),
        label = jsonlite::unbox("var_label"),
        kind = jsonlite::unbox("date"),
        NA_count = jsonlite::unbox(1L),
        min = jsonlite::unbox(as.Date(c("2024-01-01"))),
        max = jsonlite::unbox(as.Date(c("2024-01-02")))
      )
    )
  })

  test_that("get_single_filter_data fails for unsupported types", {
    d <- data.frame(
      var = TRUE
    )
    attr(d[["var"]], "label") <- "var_label"
    expect_error(
      get_single_filter_data(d),
      regexp = "column type unsupported: logical",
      fixed = TRUE
    )
  })

  test_that("get_single_filter_data returns one entry per column", {
    d <- data.frame(
      var1 = "a",
      var2 = "a"
    )
    r <- get_single_filter_data(d)
    expect_length(r, 2)
  })

  test_that("get_filter_data returns one entry per dataset_list and dataset", {
    dataset_lists <- list(
      dl1 = list(
        ds1 = data.frame(var1 = "a"),
        ds2 = data.frame(var2 = "a")
      ),
      dl2 = list(
        ds1 = data.frame(var3 = "a"),
        ds2 = data.frame(var4 = "a")
      )
    )

    r <- get_filter_data(dataset_lists)

    expect_length(r[["datasets"]], 2)
    expect_length(r[["datasets"]][[1]][["tables"]], 2)

    expect_identical(r[["datasets"]][[1]][["name"]], jsonlite::unbox("dl1"))
    expect_identical(r[["datasets"]][[1]][["tables"]][[1]][["name"]], jsonlite::unbox("ds1"))
    expect_identical(r[["datasets"]][[1]][["tables"]][[1]][["fields"]][[1]][["name"]], jsonlite::unbox("var1"))
  })
})
