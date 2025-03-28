local({
  date_var <- as.Date("2024-01-01") + c(0L:4L, NA)

  dataset_list <- list(
    d = data.frame(
      row.names = 1:6,
      range_var = c(1.0:5.0, NA),
      date_var = date_var,
      posix_var = as.POSIXct(date_var),
      subset_var = factor(c(letters[1:5], NA)),
      logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
      sbj_var = paste0("SBJ-", 1:6)
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
      variable = "range_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_range filter returns mask including NAs", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = TRUE,
      variable = "range_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - select_subset filter returns mask excluding NAs", {
    e <- list(
      kind = "filter",
      operation = "select_subset",
      values = c("b", "c", "d"),
      include_NA = FALSE,
      variable = "subset_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_subset filter returns mask including NAs", {
    e <- list(
      kind = "filter",
      operation = "select_subset",
      values = c("b", "c", "d"),
      include_NA = TRUE,
      variable = "subset_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - select_subset filter returns mask for logical excluding NAs", {
    e <- list(
      kind = "filter",
      operation = "select_subset",
      values = c("TRUE"),
      include_NA = FALSE,
      variable = "logical_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_subset filter returns mask for logical including NAs", {
    e <- list(
      kind = "filter",
      operation = "select_subset",
      values = c("TRUE"),
      include_NA = TRUE,
      variable = "logical_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask excluding NAs for Date type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = FALSE,
      variable = "date_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask including NAs for Date type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = TRUE,
      variable = "date_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask excluding NAs for POSIX type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = FALSE,
      variable = "posix_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - select_date filter returns mask including NAs for POSIX type", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      include_NA = TRUE,
      variable = "posix_var",
      dataset = "d"
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("process_dataset_filter_element - dataset filters fail when the current_table_is not the same as the table in the filter", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = FALSE,
      variable = "range_var",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "INCORRECT_D"),
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
      variable = "NON_EXISTING_FIELD",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
      variable = "subset_var",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
      regexp = "Field values must be numerical",
      fixed = TRUE
    )
  })

  test_that("process_dataset_filter_element - select_date filter fail when the field is not date or posix", {
    e <- list(
      kind = "filter",
      operation = "select_date",
      max = 4,
      min = 2,
      include_NA = FALSE,
      variable = "subset_var",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
      variable = "range_var",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
      variable = "posix_var",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
      variable = "posix_var",
      dataset = "d"
    )

    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
          variable = "range_var",
          dataset = "d"
        )
      )
    )

    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
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
          variable = "range_var",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 1,
          include_NA = FALSE,
          variable = "range_var",
          dataset = "d"
        )
      )
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - and filter operation fails when it does not have at least 1 children", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list()
    )
    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
          variable = "range_var",
          dataset = "d"
        )
      )
    )

    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
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
          variable = "range_var",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 1,
          include_NA = FALSE,
          variable = "range_var",
          dataset = "d"
        )
      )
    )
    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - or filter operation fails when it does not have at least 1 children", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list()
    )
    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
          variable = "range_var",
          dataset = "d"
        )
      )
    )

    mask <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d")
    expect_identical(mask, !c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  })

  test_that("process_dataset_filter_element - not filter operation fails when it does not have exactly 1 children", {
    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list()
    )
    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
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
          variable = "range_var",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          variable = "range_var",
          dataset = "d"
        )
      )
    )
    expect_error(
      process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"),
      regexp = "`not` operation requires exactly one child",
      fixed = TRUE
    )
  })

  local({
    dataset_list <- list(
      d1 = data.frame(var1 = c("a", "NOT IN SUBSET")),
      d2 = data.frame(var2 = "b"),
      d3 = data.frame(var3 = "c"),
      d4 = data.frame(var4 = "d")
    )

    e <- list(
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
              variable = "var1",
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

    r <- create_dataset_filter_masks(dataset_list, e)

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
    dataset_list <- list(
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
      create_dataset_filter_masks(dataset_list, e),
      regexp = "a dataset can only appear once inside dataset_filters",
      fixed = TRUE
    )
  })

  test_that("create_datasets_filter_masks fails when a children is not of kind dataset", {
    dataset_list <- list(
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
      create_dataset_filter_masks(dataset_list, e),
      regexp = "dataset_filters children can only be of kind `dataset`",
      fixed = TRUE
    )
  })

  test_that("create_datasets_filter_masks fails when it has more than one children", {
    dataset_list <- list(
      d1 = data.frame(var1 = "a"),
      d2 = data.frame(var2 = "b")
    )

    e <- list(
      children = list(
        list(
          kind = "dataset",
          name = "d1",
          children = list(
            list(),
            list()
          )
        )
      )
    )

    expect_error(
      create_dataset_filter_masks(data_list, e),
      regexp = "`datasets_filter` cannot contain more than children",
      fixed = TRUE
    )
  })

  test_that("create_datasets_filter_masks returns an all TRUE mask when no dataset has no child filters", {
    dataset_list <- list(
      d1 = data.frame(var1 = c("a", "b")),
      d2 = data.frame(var2 = "b")
    )

    e <- list(
      children = list(
        list(
          kind = "dataset",
          name = "d1",
          children = list()
        )
      )
    )

    expect_identical(
      create_dataset_filter_masks(dataset_list, e),
      list(d1 = c(TRUE, TRUE))
    )
  })

  test_that("process_subject_filter_element returns subjects set", {
    e <- list(
      kind = "filter",
      operation = "select_range",
      max = 4,
      min = 2,
      include_NA = FALSE,
      variable = "range_var",
      dataset = "d"
    )
    subject_set <- process_subject_filter_element(dataset_list = dataset_list, filter_element = e, sbj_var = "sbj_var", complete_subject_list = dataset_list[["d"]][["sbj_var"]])
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
          variable = "range_var",
          dataset = "d"
        )
      )
    )

    subject_set <- process_subject_filter_element(dataset_list = dataset_list, filter_element = e, sbj_var = "sbj_var", complete_subject_list = dataset_list[["d"]][["sbj_var"]])
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
          variable = "range_var",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 1,
          include_NA = FALSE,
          variable = "range_var",
          dataset = "d"
        )
      )
    )
    subject_set <- process_subject_filter_element(dataset_list = dataset_list, filter_element = e, sbj_var = "sbj_var", complete_subject_list = dataset_list[["d"]][["sbj_var"]])
    expect_identical(subject_set, c("SBJ-2", "SBJ-3"))
  })

  test_that("process_subject_filter_element - and filter operation fails when it does not have at least 1 children (subject filter)", {
    e <- list(
      kind = "filter_operation",
      operation = "and",
      children = list()
    )
    expect_error(
      process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
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
          variable = "range_var",
          dataset = "d"
        )
      )
    )

    subject_set <- process_subject_filter_element(dataset_list = dataset_list, filter_element = e, sbj_var = "sbj_var", complete_subject_list = dataset_list[["d"]][["sbj_var"]])
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
          variable = "range_var",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 3,
          min = 1,
          include_NA = FALSE,
          variable = "range_var",
          dataset = "d"
        )
      )
    )
    subject_set <- process_subject_filter_element(dataset_list = dataset_list, filter_element = e, sbj_var = "sbj_var", complete_subject_list = dataset_list[["d"]][["sbj_var"]])
    expect_identical(sort(subject_set), c("SBJ-1", "SBJ-2", "SBJ-3", "SBJ-4"))
  })

  test_that("process_subject_filter_element - or filter operation fails when it does not have at least 1 children (subject filter)", {
    e <- list(
      kind = "filter_operation",
      operation = "or",
      children = list()
    )
    expect_error(
      process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
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
          variable = "range_var",
          dataset = "d"
        )
      )
    )

    subject_set <- process_subject_filter_element(dataset_list = dataset_list, filter_element = e, sbj_var = "sbj_var", complete_subject_list = dataset_list[["d"]][["sbj_var"]])
    expect_identical(subject_set, c("SBJ-1", "SBJ-5", "SBJ-6"))
  })

  test_that("process_subject_filter_element - not filter operation fails when it does not have exactly 1 children (subject filter)", {
    e <- list(
      kind = "filter_operation",
      operation = "not",
      children = list()
    )
    expect_error(
      process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
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
          variable = "range_var",
          dataset = "d"
        ),
        list(
          kind = "filter",
          operation = "select_range",
          max = 4,
          min = 2,
          include_NA = FALSE,
          variable = "range_var",
          dataset = "d"
        )
      )
    )
    expect_error(
      process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
      regexp = "`not` operation requires exactly one child",
      fixed = TRUE
    )
  })

  test_that("dataset filter and subject filter fails when a field of element is not present", {
    e <- list()
    expect_error(process_dataset_filter_element(dataset_list = dataset_list, filter_element = e, current_dataset_name = "d"))
    expect_error(process_subject_filter_element(dataset_list = dataset_list, filter_element = e, sbj_var = "sbj_var", complete_subject_list = dataset_list[["d"]][["sbj_var"]]))
  })

  test_that("create_subject_set returns a subject set", {
    dataset_list <- list(
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
          variable = "var1",
          dataset = "d1"
        )
      )
    )

    expect_identical(
      create_subject_set(dataset_list = dataset_list, e, "sbj"),
      "SBJ1"
    )
  })

  test_that("create_subject_set returns full subject list, across datasets, for an empty filter", {
    dataset_list <- list(
      d1 = data.frame(var1 = c("a", "b"), sbj = c("SBJ1", "SBJ2")),
      d2 = data.frame(var1 = "a", sbj = "SBJ3")
    )

    e <- list(
      children = list()
    )

    expect_identical(create_subject_set(dataset_list = dataset_list, e, "sbj"), c("SBJ1", "SBJ2", "SBJ3"))
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
      var = 1 + 2i
    )
    attr(d[["var"]], "label") <- "var_label"
    expect_error(
      get_single_filter_data(d),
      regexp = "variable type unsupported: complex",
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

    expect_length(r[["dataset_lists"]], 2)
    expect_length(r[["dataset_lists"]][[1]][["dataset_list"]], 2)

    expect_identical(r[["dataset_lists"]][[1]][["name"]], jsonlite::unbox("dl1"))
    expect_identical(r[["dataset_lists"]][[1]][["dataset_list"]][[1]][["name"]], jsonlite::unbox("ds1"))
    expect_identical(r[["dataset_lists"]][[1]][["dataset_list"]][[1]][["variables"]][[1]][["name"]], jsonlite::unbox("var1"))
  })
})


# E2E testing

local({
  skip_if_not_running_shiny_tests()
  skip_if_suspect_check()

  date_var <- as.Date("2024-01-01") + c(0L:4L, NA)


  dataset_lists <- list(
    dl1 = list(
      ds1 = data.frame(
        row.names = 1:6,
        range_var = c(1.0:5.0, NA),
        date_var = date_var,
        posix_var = as.POSIXct(date_var),
        subset_var = factor(c(letters[1:5], NA)),
        logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
        sbj_var = paste0("SBJ-", 1:6)
      ),
      ds2 = data.frame(
        row.names = 1:6,
        range_var = c(1.0:5.0, NA),
        date_var = date_var,
        posix_var = as.POSIXct(date_var),
        subset_var = factor(c(letters[1:5], NA)),
        logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
        sbj_var = paste0("SBJ-", 1:6)
      )
    ),
    dl2 = list(
      ds1 = data.frame(
        row.names = 1:6,
        range_var = c(1.0:5.0, NA),
        date_var = date_var,
        posix_var = as.POSIXct(date_var),
        subset_var = factor(c(letters[1:5], NA)),
        logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
        sbj_var = paste0("SBJ-", 1:6)
      ),
      ds2 = data.frame(
        row.names = 1:6,
        range_var = c(1.0:5.0, NA),
        date_var = date_var,
        posix_var = as.POSIXct(date_var),
        subset_var = factor(c(letters[1:5], NA)),
        logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
        sbj_var = paste0("SBJ-", 1:6)
      )
    )
  )

  absolute_state_file <- file.path(getwd(), "./test_data/filter_state.txt")

  test_that("A file state can be loaded in the app|all block types can be included in the application", {
    # The filter includes all possible blocks which effectively test that all can be included
    # Because we are reading back the processed filter we also ensure that all blocks are processed properly

    app <- start_app_driver(rlang::quo({
      message(getwd())
      dv.manager:::run_app_dev_filter(
        data = !!dataset_lists,
        module_list = list(
          Simple3 = dv.listings::mod_listings(
            "mod13",
            dataset_names = "ds1"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        state = !!absolute_state_file
      )
    }))

    state_from_app <- jsonlite::prettify(app$get_value(input = "filter-json"))
    state_from_file <- jsonlite::prettify(paste0(readLines(absolute_state_file), collapse = "\n"))

    expect_identical(state_from_app, state_from_file)
  })

  test_that("A string state can be loaded in the app", {
    string_state <- paste(readLines(absolute_state_file), collapse = "\n")

    app <- start_app_driver(rlang::quo({
      dv.manager:::run_app_dev_filter(
        data = !!dataset_lists,
        module_list = list(
          Simple3 = dv.listings::mod_listings(
            "mod13",
            dataset_names = "ds1"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        state = !!string_state
      )
    }))

    state_from_app <- jsonlite::prettify(app$get_value(input = "filter-json"))
    state_from_file <- jsonlite::prettify(string_state)

    expect_identical(state_from_app, state_from_file)
  })

  test_that("An app with no state has an empty filter", {
    app <- start_app_driver(rlang::quo({
      dv.manager:::run_app_dev_filter(
        data = !!dataset_lists,
        module_list = list(
          Simple3 = dv.listings::mod_listings(
            "mod13",
            dataset_names = "ds1"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var"
      )
    }))

    app_state <- app$get_value(input = "filter-json")
    expect_identical(app_state, '{"filters":{"datasets_filter":{"children":[]},"subject_filter":{"children":[]}},"dataset_list_name":"dl1"}')
  })

  test_that("Filter can be exported", {
    app <- start_app_driver(rlang::quo({
      dv.manager:::run_app_dev_filter(
        data = !!dataset_lists,
        module_list = list(
          Simple3 = dv.listings::mod_listings(
            "mod13",
            dataset_names = "ds1"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        state = !!absolute_state_file
      )
    }))

    tmp_file <- tempfile()
    app$get_download("filter-export_code", tmp_file)
    expect_equal(readLines(tmp_file), readLines(absolute_state_file))
  })

  test_that("Filter can be exported", {
    app <- start_app_driver(rlang::quo({
      dv.manager:::run_app_dev_filter(
        data = !!dataset_lists,
        module_list = list(
          Simple3 = dv.listings::mod_listings(
            "mod13",
            dataset_names = "ds1"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        state = !!absolute_state_file
      )
    }))

    tmp_file <- tempfile()
    app$get_download("filter-export_code", tmp_file)
    expect_equal(readLines(tmp_file), readLines(absolute_state_file))
  })



  test_that("dataset filters are applied", {
    app <- start_app_driver(rlang::quo({
      dv.manager:::run_app_dev_filter(
        data = dataset_lists,
        module_list = list(
          Simple3 = dv.manager:::mod_simple(
            dataset = "ds1",
            module_id = "mod"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        state = '  {
    "filters": {
        "datasets_filter": {
            "children": [
                {
                    "name": "ds1",
                    "kind": "dataset",
                    "children": [
                        {
                            "kind": "filter",
                            "dataset": "ds1",
                            "operation": "select_subset",
                            "variable": "sbj_var",
                            "values" : ["SBJ-1"],
                            "include_NA": false
                        }
                    ]
                }
            ]
        },
        "subject_filter": {
            "children": []
        }
    },
    "dataset_list_name": "dl1"
}'
      )
    }))

    expect_identical(app$get_value(output = "mod-text"), "1")
  })






  test_that("dataset filters are applied", {
    # Filter on ds2 see the effect on ds1

    app <- start_app_driver(rlang::quo({
      dv.manager:::run_app_dev_filter(
        data = dataset_lists,
        module_list = list(
          Simple3 = dv.manager:::mod_simple(
            dataset = "ds1",
            module_id = "mod"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        state = '  {
    "filters": {
        "subject_filter": {
            "children": [
                   {
                            "kind": "filter",
                            "dataset": "ds2",
                            "operation": "select_subset",
                            "variable": "sbj_var",
                            "values" : ["SBJ-1"],
                            "include_NA": false
                        }
            ]
        },
        "dataset_filter": {
            "children": []
        }
    },
    "dataset_list_name": "dl1"
}'
      )
    }))

    expect_identical(app$get_value(output = "mod-text"), "1")
  })

  local({
    root_app <- start_app_driver(rlang::quo({
      dv.manager:::run_app_dev_filter(
        data = dataset_lists,
        module_list = list(
          Simple3 = dv.manager:::mod_simple(
            dataset = "ds1",
            module_id = "mod"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        enableBookmarking = "url",
        state = !!absolute_state_file
      )
    }))

    url <- "?_inputs_&filter-IGNORE_INPUT=null&__tabset_0__=%22mod%22&open_options_modal=0&selector=%22dl1%22&click=true&filter-checkbox=false&filter-log=null&filter-json=%22%7B%5C%22filters%5C%22%3A%7B%5C%22datasets_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%5D%7D%2C%5C%22subject_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%7B%5C%22kind%5C%22%3A%5C%22filter%5C%22%2C%5C%22dataset%5C%22%3A%5C%22ds1%5C%22%2C%5C%22operation%5C%22%3A%5C%22select_subset%5C%22%2C%5C%22variable%5C%22%3A%5C%22sbj_var%5C%22%2C%5C%22values%5C%22%3A%5B%5C%22SBJ-1%5C%22%5D%2C%5C%22include_NA%5C%22%3Afalse%7D%5D%7D%7D%2C%5C%22dataset_list_name%5C%22%3A%5C%22dl1%5C%22%7D%22"


    test_that("Bookmark can be restored | Bookmark overrides state", {
      full_url <- paste0(root_app$get_url(), url)
      app <- shinytest2::AppDriver$new(full_url)
      expect_identical(app$get_value(output = "mod-text"), "1")
    })

    test_that("Bookmark can be restored with no state", {
      root_app <- start_app_driver(rlang::quo({
        dv.manager:::run_app_dev_filter(
          data = dataset_lists,
          module_list = list(
            Simple3 = dv.manager:::mod_simple(
              dataset = "ds1",
              module_id = "mod"
            )
          ),
          filter_data = "ds1",
          filter_key = "sbj_var",
          enableBookmarking = "url"
        )
      }))

      full_url <- paste0(root_app$get_url(), url)
      app <- shinytest2::AppDriver$new(full_url)

      expect_identical(app$get_value(output = "mod-text"), "1")
    })
  })
})

test_that("filter only sends one value when it is updated", {

  # Related to every time we change the filter, including first beat

  skip("skipped until we can set a state programatically")
})

test_that("modules bookmark state works with filter", {
  # Related to the first beat of the app
  # Mainly related to the above, the main requisite is that when starting the app on a bookmarked state
  # a single filtered dataset_list is sent to the modules, otherwise bookmark state is spent on the first filtered
  # dataset_list and in the next one they go to empty/default

  skip("skipped until we can set a state programatically")
  
})
