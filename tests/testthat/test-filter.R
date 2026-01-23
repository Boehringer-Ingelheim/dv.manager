local({
  date_var <- as.Date("2024-01-01") + c(0L:4L, NA)

  dataset_list <- list(
    d1 = data.frame(
      row.names = 1:6,
      range_var = c(1.0:5.0, NA),
      date_var = date_var,
      posix_var = as.POSIXct(date_var),
      subset_var = factor(c(letters[1:5], NA), levels = c(letters[1:5], "LEVEL_WITH_NO_ROWS")),
      subset_var2 = factor(c(letters[6:10], NA)),
      logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
      sbj_var = paste0("SBJ-", 1:6)
    ),
    d2 = data.frame(
      row.names = 1:6,
      subset_var2 = factor(c(letters[6:10], NA)),
      sbj_var = paste0("SBJ-", 1:6)
    )
  )

  # Column filters

  test_that(
    "process_dataset_filter_element - select_range filter returns mask excluding NAs and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_range",
        max = 4,
        min = 2,
        include_NA = FALSE,
        variable = "range_var",
        dataset = "d1"
      )
      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - select_range filter returns mask including NAs and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_range",
        max = 4,
        min = 2,
        include_NA = TRUE,
        variable = "range_var",
        dataset = "d1"
      )
      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - select_subset filter returns mask excluding NAs and selected levels list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_subset",
        values = c("b", "c", "d"),
        include_NA = FALSE,
        variable = "subset_var",
        dataset = "d1"
      )
      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list(subset_var = c("b", "c", "d")))
    }
  )

  test_that(
    "process_dataset_filter_element - select_subset empty vector assigned to the lvls of a variable is unambiguous" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      # When all values are filtered there is an entry in lvls and the entry is an empty vector
      e <- list(
        kind = "filter",
        operation = "select_subset",
        values = c(),
        include_NA = FALSE,
        variable = "subset_var",
        dataset = "d1"
      )
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["lvls"]], list(subset_var = c()))

      # When the variable is not filtered there is no entry
      e <- list(
        kind = "filter",
        operation = "select_range",
        max = 0,
        min = 0,
        include_NA = FALSE,
        variable = "range_var",
        dataset = "d1"
      )
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_true(!"subset_var" %in% names(processed_element[["lvls"]]))

      # When all values are selected the variable appears and contains all selected values
      e <- list(
        kind = "filter",
        operation = "select_subset",
        values = c("a", "b", "c", "d", "e", "LEVEL_WITH_NO_ROWS"),
        include_NA = FALSE,
        variable = "subset_var",
        dataset = "d1"
      )
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["lvls"]], list(subset_var = c("a", "b", "c", "d", "e", "LEVEL_WITH_NO_ROWS")))
    }
  )

  test_that(
    "process_dataset_filter_element - select_subset filter returns mask including NAs and selected levels list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_subset",
        values = c("b", "c", "d"),
        include_NA = TRUE,
        variable = "subset_var",
        dataset = "d1"
      )
      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list(subset_var = c("b", "c", "d")))
    }
  )

  test_that(
    "process_dataset_filter_element - select_subset filter returns mask for logical excluding NAs and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_subset",
        values = c("TRUE"),
        include_NA = FALSE,
        variable = "logical_var",
        dataset = "d1"
      )

      expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - select_subset filter returns mask for logical including NAs and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_subset",
        values = c("TRUE"),
        include_NA = TRUE,
        variable = "logical_var",
        dataset = "d1"
      )

      expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - select_date filter returns mask excluding NAs for Date type and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_date",
        min = "2024-01-02",
        max = "2024-01-04",
        include_NA = FALSE,
        variable = "date_var",
        dataset = "d1"
      )

      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - select_date filter returns mask including NAs for Date type and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_date",
        min = "2024-01-02",
        max = "2024-01-04",
        include_NA = TRUE,
        variable = "date_var",
        dataset = "d1"
      )

      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - select_date filter returns mask excluding NAs for POSIX type and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_date",
        min = "2024-01-02",
        max = "2024-01-04",
        include_NA = FALSE,
        variable = "posix_var",
        dataset = "d1"
      )

      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - select_date filter returns mask including NAs for POSIX type and empty level list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_date",
        min = "2024-01-02",
        max = "2024-01-04",
        include_NA = TRUE,
        variable = "posix_var",
        dataset = "d1"
      )

      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
      expect_identical(processed_element[["lvls"]], list())
    }
  )

  test_that(
    "process_dataset_filter_element - dataset filters fail when the current_table_is not the same as the table in the filter" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      dataset_list[["OTHER_TABLE"]] <- dataset_list[["d1"]]
      e <- list(
        kind = "row_operation",
        operation = "and",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "OTHER_TABLE"
          )
        )
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "Filtering on the wrong dataset",
        fixed = TRUE
      )

      e <- list(
        kind = "row_operation",
        operation = "or",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "OTHER_TABLE"
          )
        )
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "Filtering on the wrong dataset",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - dataset filters fail when the table does not contain the filtered field" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_range",
        max = 4,
        min = 2,
        include_NA = FALSE,
        variable = "NON_EXISTING_FIELD",
        dataset = "d1"
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "data[['d1']] does not contain col `NON_EXISTING_FIELD`",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - select_range filter fail when the field is not numeric" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_range",
        max = 4,
        min = 2,
        include_NA = FALSE,
        variable = "subset_var",
        dataset = "d1"
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "Field values must be numerical",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - select_date filter fail when the field is not date or posix" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_date",
        max = 4,
        min = 2,
        include_NA = FALSE,
        variable = "subset_var",
        dataset = "d1"
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "Field values must be POSIX.ct or Date",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - select_range min must be lower or equal than max" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_range",
        max = 2,
        min = 4,
        include_NA = FALSE,
        variable = "range_var",
        dataset = "d1"
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "min <= max",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - select_date min must be lower or equal than max" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_date",
        min = "2024-01-04",
        max = "2024-01-02",
        include_NA = TRUE,
        variable = "posix_var",
        dataset = "d1"
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "min <= max",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - fails for unknown operations" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "filter",
        operation = "UNKNOWN OPERATION",
        min = "2024-01-04",
        max = "2024-01-02",
        include_NA = TRUE,
        variable = "posix_var",
        dataset = "d1"
      )

      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "Operation unknown: `UNKNOWN OPERATION`",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - and filter operation returns mask for 1" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "and",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
    }
  )

  test_that(
    "process_dataset_filter_element - and filter operation returns mask for n elements" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "and",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 3,
            min = 1,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
    }
  )

  test_that(
    "process_dataset_filter_element - and filter operation fails when it does not have at least 1 children" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "and",
        children = list()
      )
      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "`and` operation requires at least one child",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - and filter operation correctly combines lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "and",
        children = list(
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("b", "c"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("c", "d"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("f", "h"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("h", "j"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d1"
          )
        )
      )

      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["lvls"]], list(subset_var = c("c"), subset_var2 = c("h")))
    }
  )

  test_that(
    "process_dataset_filter_element - or filter operation returns mask for 1 element" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "or",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )
      expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
    }
  )

  test_that(
    "process_dataset_filter_element - or filter operation correctly combines lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "or",
        children = list(
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("b", "c"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("c", "d"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("f", "h"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("h", "j"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d1"
          )
        )
      )

      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["lvls"]], list(subset_var = c("b", "c", "d"), subset_var2 = c("f", "h", "j")))
    }
  )

  test_that(
    "process_dataset_filter_element - or filter operation returns mask for n elements" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "or",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 3,
            min = 1,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
    }
  )

  test_that(
    "process_dataset_filter_element - or filter operation fails when it does not have at least 1 children" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "or",
        children = list()
      )
      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "`or` operation requires at least one child",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_dataset_filter_element - not filter operation returns mask" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "not",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- !c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")
    }
  )

  test_that(
    "process_dataset_filter_element - not filter operation correctly combines lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "not",
        children = list(
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("b", "c", "d", "e"),
            include_NA = TRUE,
            variable = "subset_var",
            dataset = "d1"
          )
        )
      )

      expected <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
      processed_element <- process_dataset_filter_element(dataset_list = dataset_list, filter_element = e)
      expect_identical(processed_element[["mask"]], expected)
      expect_identical(processed_element[["dataset"]], "d1")

      expect_identical(processed_element[["lvls"]], list(subset_var = c("a", "LEVEL_WITH_NO_ROWS")))
    }
  )

  test_that(
    "process_dataset_filter_element - not filter operation fails when it does not have exactly 1 children" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "not",
        children = list()
      )
      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "`not` operation requires exactly one child",
        fixed = TRUE
      )

      e <- list(
        kind = "row_operation",
        operation = "not",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )
      expect_error(
        process_dataset_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "`not` operation requires exactly one child",
        fixed = TRUE
      )
    }
  )

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

    r <- create_dataset_filter_info(dataset_list, e)

    test_that("create_dataset_filter_masks creates a mask per dataset in the filter and none for those not in the filter", {
      expect_identical(names(r), c("d1", "d2"))
      expect_length(r, 2)
    })

    test_that("create_dataset_filter_masks creates a mask for dataset filters with children", {
      expect_identical(r[["d1"]][["mask"]], c(TRUE, FALSE))
    })

    test_that("create_dataset_filter_masks creates a TRUE mask for dataset filters with no children", {
      expect_true(r[["d2"]][["mask"]])
    })
  })

  test_that(
    "create_datasets_filter_masks fails when a dataset appears more than once in the dataset filters" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      dataset_list <- list(
        d1 = data.frame(var1 = "a"),
        d2 = data.frame(var2 = "b")
      )

      e <- list(
        kind = "datasets",
        children = list(
          list(
            kind = "dataset",
            name = "d1",
            children = list()
          ),
          list(
            kind = "dataset",
            name = "d1",
            children = list()
          )
        )
      )

      expect_error(
        create_dataset_filter_info(dataset_list, e),
        regexp = "a dataset can only appear once inside dataset_filters",
        fixed = TRUE
      )
    }
  )

  test_that(
    "create_datasets_filter_masks fails when the dataset is not in the data_list" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      dataset_list <- list(
        d1 = data.frame(var1 = "a"),
        d2 = data.frame(var2 = "b")
      )

      e <- list(
        kind = "datasets",
        children = list(
          list(
            kind = "dataset",
            name = "d3",
            children = list()
          )
        )
      )

      expect_error(
        create_dataset_filter_info(dataset_list, e),
        regexp = "dataset is not inside dataset_list",
        fixed = TRUE
      )
    }
  )

  test_that(
    "create_datasets_filter_masks fails when a children is not of kind dataset" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
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
        create_dataset_filter_info(dataset_list, e),
        regexp = "dataset_filters children can only be of kind `dataset`",
        fixed = TRUE
      )
    }
  )

  test_that(
    "create_datasets_filter_masks fails when it has more than one children" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
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
        create_dataset_filter_info(dataset_list, e),
        regexp = "`datasets_filter` cannot contain more than children",
        fixed = TRUE
      )
    }
  )

  test_that(
    "create_datasets_filter_masks returns an all TRUE mask when no dataset has no child filters" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
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
        create_dataset_filter_info(dataset_list, e),
        list(d1 = list(mask = c(TRUE, TRUE), lvls = list()))
      )
    }
  )

  ######################

  test_that(
    "process_subject_filter_element returns subjects set" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "filter",
        operation = "select_range",
        max = 4,
        min = 2,
        include_NA = FALSE,
        variable = "range_var",
        dataset = "d1"
      )

      expected <- list(subjects = c("SBJ-2", "SBJ-3", "SBJ-4"), dataset_list_lvls = list(d1 = list(), d2 = list()))
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element, expected)
    }
  )

  test_that(
    "process_subject_filter_element - intersect set operation returns subject set for 1 element" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "intersect",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- list(subjects = c("SBJ-2", "SBJ-3", "SBJ-4"), dataset_list_lvls = list(d1 = list(), d2 = list()))
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element, expected)
    }
  )

  test_that(
    "process_subject_filter_element - intersect set operation returns subject set for n elements" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "intersect",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 3,
            min = 1,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- list(subjects = c("SBJ-2", "SBJ-3"), dataset_list_lvls = list(d1 = list(), d2 = list()))
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element, expected)
    }
  )

  test_that(
    "process_dataset_filter_element - intersect set operation correctly combines lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "intersect",
        children = list(
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("b", "c"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("c", "d"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("f", "h"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d2"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("f"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d2"
          )
        )
      )

      expected <- list(d1 = list(subset_var = "c"), d2 = list(subset_var2 = c("f")))
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element[["dataset_list_lvls"]], expected)
    }
  )

  test_that(
    "process_subject_filter_element - intersect set operation fails when it does not have at least 1 children (subject filter)" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "intersect",
        children = list()
      )
      expect_error(
        process_subject_filter_element(
          dataset_list = dataset_list,
          filter_element = e,
          complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
        ),
        regexp = "`intersect` operation requires at least one child",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_subject_filter_element - union set operation returns subject set for 1 element" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "union",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- list(subjects = c("SBJ-2", "SBJ-3", "SBJ-4"), dataset_list_lvls = list(d1 = list(), d2 = list()))
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element, expected)
    }
  )

  test_that(
    "process_subject_filter_element - union set operation returns subject set for n elements" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "union",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 3,
            min = 1,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )
      expected <- list(
        subjects = c("SBJ-2", "SBJ-3", "SBJ-4", "SBJ-1"),
        dataset_list_lvls = list(d1 = list(), d2 = list())
      )
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element, expected)
    }
  )

  test_that(
    "process_dataset_filter_element - union set operation correctly combines lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "union",
        children = list(
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("b", "c"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("c", "d"),
            include_NA = FALSE,
            variable = "subset_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("f", "h"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d2"
          ),
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("f"),
            include_NA = FALSE,
            variable = "subset_var2",
            dataset = "d2"
          )
        )
      )

      expected <- list(d1 = list(subset_var = c("b", "c", "d")), d2 = list(subset_var2 = c("f", "h")))
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element[["dataset_list_lvls"]], expected)
    }
  )

  test_that(
    "process_subject_filter_element - union set operation fails when it does not have at least 1 children (subject filter)" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "union",
        children = list()
      )
      expect_error(
        process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "`union` operation requires at least one child",
        fixed = TRUE
      )
    }
  )

  test_that(
    "process_subject_filter_element - complement set operation returns subject set" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "complement",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )

      expected <- list(subjects = c("SBJ-1", "SBJ-5", "SBJ-6"), dataset_list_lvls = list(d1 = list(), d2 = list()))
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element, expected)
    }
  )

  test_that(
    "process_dataset_filter_element - complement set operation correctly combines lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "complement",
        children = list(
          list(
            kind = "filter",
            operation = "select_subset",
            values = c("b", "c", "d", "e"),
            include_NA = TRUE,
            variable = "subset_var",
            dataset = "d1"
          )
        )
      )

      expected <- list(d1 = list(subset_var = c("a", "LEVEL_WITH_NO_ROWS")), d2 = list())
      processed_element <- process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      )
      expect_identical(processed_element[["dataset_list_lvls"]], expected)
    }
  )

  test_that(
    "process_subject_filter_element - complement set operation fails when it does not have exactly 1 children (subject filter)" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "set_operation",
        operation = "complement",
        children = list()
      )
      expect_error(
        process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "`complement` operation requires exactly one child",
        fixed = TRUE
      )

      e <- list(
        kind = "set_operation",
        operation = "complement",
        children = list(
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          ),
          list(
            kind = "filter",
            operation = "select_range",
            max = 4,
            min = 2,
            include_NA = FALSE,
            variable = "range_var",
            dataset = "d1"
          )
        )
      )
      expect_error(
        process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "`complement` operation requires exactly one child",
        fixed = TRUE
      )
    }
  )

  ######################

  test_that(
    "set operations cannot be child of a row operation or a dataset filter" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list(
        kind = "row_operation",
        operation = "not",
        children = list(
          list(
            kind = "set_operation",
            operation = "complement",
            children = list(
              list(
                kind = "filter",
                operation = "select_range",
                max = 4,
                min = 2,
                include_NA = FALSE,
                variable = "range_var",
                dataset = "d1"
              )
            )
          )
        )
      )

      expect_error(
        process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "Kind unknown: `set_operation`",
        fixed = TRUE
      )

      e <- list(
        kind = "row_operation",
        operation = "and",
        children = list(
          list(
            kind = "set_operation",
            operation = "complement",
            children = list(
              list(
                kind = "filter",
                operation = "select_range",
                max = 4,
                min = 2,
                include_NA = FALSE,
                variable = "range_var",
                dataset = "d1"
              )
            )
          )
        )
      )

      expect_error(
        process_subject_filter_element(dataset_list = dataset_list, filter_element = e),
        regexp = "Kind unknown: `set_operation`",
        fixed = TRUE
      )
    }
  )

  test_that(
    "dataset filter and subject filter fails when a field of element is not present" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      e <- list()
      expect_error(process_dataset_filter_element(dataset_list = dataset_list, filter_element = e))
      expect_error(process_subject_filter_element(
        dataset_list = dataset_list,
        filter_element = e,
        sbj_var = "sbj_var",
        complete_subject_list = dataset_list[["d1"]][["sbj_var"]]
      ))
    }
  )

  test_that(
    "create_subject_set returns a subject set" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
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
        create_subject_filter_info(dataset_list = dataset_list, e, "sbj")[["subjects"]],
        "SBJ1"
      )
    }
  )

  test_that(
    "create_subject_set returns full subject list, across datasets, for an empty filter" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      dataset_list <- list(
        d1 = data.frame(var1 = c("a", "b"), sbj = c("SBJ1", "SBJ2")),
        d2 = data.frame(var1 = "a", sbj = "SBJ3")
      )

      e <- list(
        children = list()
      )

      expect_identical(
        create_subject_filter_info(dataset_list = dataset_list, e, "sbj")[["subjects"]],
        c("SBJ1", "SBJ2", "SBJ3")
      )
    }
  )

  test_that(
    "apply_subject_filter_info applies lvl dropping, reintroduces lvl present still present in the variable, retains not present unfiltered lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      d <- list(
        d = data.frame(
          subset_var = factor(
            c("a", "b", "c"),
            levels = c("a", "b", "c", "NO_ROW_STAY", "NO_ROW_GONE")
          )
        )
      )

      dataset_filter_info <- list(
        d = list(
          mask = c(TRUE, FALSE, TRUE),
          lvls = list(subset_var = c("a", "NO_ROW_STAY"))
        )
      )

      expected <- c(
        "a", # Not filtered
        "c", # In mask but not in lvls, reintroduced
        "NO_ROW_STAY" # Not present but not filtered
      )

      expect_identical(
        expected,
        levels(apply_dataset_filter_info(d, dataset_filter_info)[["d"]][["subset_var"]])
      )
    }
  )

  test_that(
    "apply_subject_filter_info applies lvl dropping, reintroduces lvl present still present in the variable, retains not present unfiltered lvls" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY,
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      d <- list(
        d = data.frame(
          subset_var = factor(
            c("a", "b", "c"),
            levels = c("a", "b", "c", "NO_ROW_STAY", "NO_ROW_GONE")
          ),
          subject_var = factor(
            c("S-1", "S-2", "S-3")
          )
        )
      )

      subject_filter_info <- list(
        subjects = c("S-1", "S-3"),
        dataset_list_lvls = list(
          d = list(subset_var = c("a", "NO_ROW_STAY"))
        )
      )

      expected <- c(
        "a", # Not filtered
        "c", # In mask but not in lvls, reintroduced
        "NO_ROW_STAY" # Not present but not filtered
      )

      expect_identical(
        levels(apply_subject_filter_info(d, subject_filter_info, "subject_var")[["d"]][["subset_var"]]),
        expected
      )
    }
  )
})

local({
  test_that(
    "get_single_filter_data show correct counts and entries for character columns" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
      d <- data.frame(
        var = c("A", "A", "B", NA_character_)
      )
      attr(d[["var"]], "label") <- "var_label"
      r <- get_single_filter_data(d)
      expect_length(r, 1)
      expect_identical(
        r[[1]],
        list(
          name = "var",
          label = "var_label",
          class = class(d[["var"]])[1],
          kind = "categorical",
          NA_count = 1L,
          value = c("A", "B"),
          count = c(2L, 1L)
        )
      )
    }
  )

  test_that(
    "get_single_filter_data show correct counts and entries for factor columns" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
      d <- data.frame(
        var = factor(c("A", "A", "B", NA_character_), levels = c("A", "B", "C"))
      )
      attr(d[["var"]], "label") <- "var_label"
      r <- get_single_filter_data(d)
      expect_length(r, 1)
      expect_identical(
        r[[1]],
        list(
          name = "var",
          label = "var_label",
          class = class(d[["var"]])[1],
          kind = "categorical",
          NA_count = 1L,
          value = c("A", "B", "C"),
          count = c(2L, 1L, 0L)
        )
      )
    }
  )

  test_that(
    "get_single_filter_data show correct max and min for numeric columns" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
      d <- data.frame(
        var = c(1, 1, 2, NA_real_)
      )
      attr(d[["var"]], "label") <- "var_label"
      r <- get_single_filter_data(d)
      expect_length(r, 1)
      expect_identical(
        r[[1]],
        list(
          name = "var",
          label = "var_label",
          class = class(d[["var"]])[1],
          kind = "numerical",
          NA_count = 1L,
          min = 1,
          max = 2,
          density = hist(d[["var"]], plot = FALSE)[["density"]]
        )
      )
    }
  )

  test_that(
    "get_single_filter_data supports all NA columns" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
      d <- data.frame(
        var = c(NA_real_, NA_real_)
      )
      attr(d[["var"]], "label") <- "var_label"
      r <- get_single_filter_data(d)
      expect_length(r, 1)
      expect_identical(
        r[[1]],
        list(
          name = "var",
          label = "var_label",
          class = class(d[["var"]])[1],
          kind = "numerical",
          NA_count = 2L,
          min = Inf,
          max = -Inf,
          density = numeric(0)
        )
      )
    }
  )

  test_that(
    "get_single_filter_data show correct max and min for Date columns" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
      d <- data.frame(
        var = as.Date(c("2024-01-01", "2024-01-02", NA))
      )
      attr(d[["var"]], "label") <- "var_label"
      r <- get_single_filter_data(d)
      expect_length(r, 1)
      expect_identical(
        r[[1]],
        list(
          name = "var",
          label = "var_label",
          class = class(d[["var"]])[1],
          kind = "date",
          NA_count = 1L,
          min = as.numeric(as.Date(c("2024-01-01"))),
          max = as.numeric(as.Date(c("2024-01-02")))
        )
      )
    }
  )

  test_that(
    "get_single_filter_data show correct max and min for POSIXct columns" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
      d <- data.frame(
        var = as.POSIXct(as.Date(c("2024-01-01", "2024-01-02", NA)))
      )
      attr(d[["var"]], "label") <- "var_label"
      r <- get_single_filter_data(d)
      expect_length(r, 1)
      expect_identical(
        r[[1]],
        list(
          name = "var",
          label = "var_label",
          class = class(d[["var"]])[1],
          kind = "date",
          NA_count = 1L,
          min = as.numeric(as.Date(c("2024-01-01"))),
          max = as.numeric(as.Date(c("2024-01-02")))
        )
      )
    }
  )

  test_that(
    "get_single_filter_data returns one entry per column" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
      d <- data.frame(
        var1 = "a",
        var2 = "a"
      )
      r <- get_single_filter_data(d)
      expect_length(r, 2)
    }
  )

  test_that(
    "get_filter_data returns one entry per dataset_list and dataset" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_SUPPORTED_TYPES,
        specs$FILTERING$FILTER_INCLUDE_EXCLUDE_NA
      )),
    {
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

      expect_identical(r[["dataset_lists"]][[1]][["name"]], "dl1")
      expect_identical(r[["dataset_lists"]][[1]][["dataset_list"]][[1]][["name"]], "ds1")
      expect_identical(r[["dataset_lists"]][[1]][["dataset_list"]][[1]][["variables"]][[1]][["name"]], "var1")
    }
  )
})

local({
  test_that(
    "match_set_order matches order of values to reference" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_LEVEL_DROP
      )),
    {
      expect_identical(
        match_set_order(c("A", "B"), c("B", "A")),
        c("A", "B")
      )

      expect_error(
        match_set_order(c("A", "B"), c("B", "A", "C")),
        "All values must be contained in reference",
        fixed = TRUE
      )
    }
  )
})


# E2E testing

local({
  # skip("REMOVE SKIP AFTER FIXING")
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

  test_that(
    "A file state can be loaded in the app|all block types can be included in the application" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_INITIAL_STATE
      )),
    {
      # The filter includes all possible blocks which effectively test that all can be included
      # Because we are reading back the processed filter we also ensure that all blocks are processed properly

      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!dataset_lists,
          module_list = list(
            Simple3 = dv.manager:::mod_simple(
              module_id = "mod13",
              dataset = "ds1",
              from = "filtered_dataset"
            )
          ),
          filter_data = "ds1",
          filter_key = "sbj_var",
          filter_default_state = !!absolute_state_file
        )
      }))

      state_from_app <- jsonlite::prettify(app$get_value(input = "filter-json"))
      state_from_file <- jsonlite::prettify(paste0(readLines(absolute_state_file), collapse = "\n"))

      expect_identical(state_from_app, state_from_file)
    }
  )

  test_that(
    "A string state can be loaded in the app" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_INITIAL_STATE
      )),
    {
      string_state <- paste(readLines(absolute_state_file), collapse = "\n")

      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!dataset_lists,
          module_list = list(
            Simple3 = dv.manager:::mod_simple(
              module_id = "mod13",
              dataset = "ds1",
              from = "filtered_dataset"
            )
          ),
          filter_data = "ds1",
          filter_key = "sbj_var",
          filter_default_state = !!string_state
        )
      }))

      state_from_app <- jsonlite::prettify(app$get_value(input = "filter-json"))
      state_from_file <- jsonlite::prettify(string_state)

      expect_identical(state_from_app, state_from_file)
    }
  )

  test_that(
    "An app with no state has an empty filter" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
      )),
    {
      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!dataset_lists,
          module_list = list(
            Simple3 = dv.manager:::mod_simple(
              module_id = "mod13",
              dataset = "ds1",
              from = "filtered_dataset"
            )
          ),
          filter_data = "ds1",
          filter_key = "sbj_var",
        )
      }))

      app_state <- app$get_value(input = "filter-json")
      expect_identical(
        app_state,
        '{"filters":{"datasets_filter":{"children":[]},"subject_filter":{"children":[]}},"dataset_list_name":"dl1"}'
      )
    }
  )

  test_that("Filter can be exported", {
    app <- start_app_driver(rlang::quo({
      dv.manager:::run_app(
        data = !!dataset_lists,
        module_list = list(
          Simple3 = dv.manager:::mod_simple(
            module_id = "mod13",
            dataset = "ds1",
            from = "filtered_dataset"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        filter_default_state = !!absolute_state_file
      )
    }))

    tmp_file <- tempfile()
    app$get_download("filter-export_code", tmp_file)
    expect_equal(readLines(tmp_file), readLines(absolute_state_file))
  })

  test_that(
    "dataset filters are applied" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
      )),
    {
      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = dataset_lists,
          module_list = list(
            Simple3 = dv.manager:::mod_simple(
              dataset = "ds1",
              module_id = "mod",
              from = "filtered_dataset"
            )
          ),
          filter_data = "ds1",
          filter_key = "sbj_var",
          filter_default_state = '{
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
    }
  )

  test_that(
    "subject filters are applied" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST,
        specs$FILTERING$FILTER_GLOBAL_TABLE,
        specs$FILTERING$FILTER_GLOBAL_KEY
      )),
    {
      # Filter on ds2 see the effect on ds1

      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = dataset_lists,
          module_list = list(
            Simple3 = dv.manager:::mod_simple(
              dataset = "ds1",
              module_id = "mod",
              from = "filtered_dataset"
            )
          ),
          filter_data = "ds1",
          filter_key = "sbj_var",
          filter_default_state = '  {
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
    }
  )

  local({
    root_app <- start_app_driver(rlang::quo({
      dv.manager:::run_app(
        data = dataset_lists,
        module_list = list(
          Simple3 = dv.manager:::mod_simple(
            dataset = "ds1",
            module_id = "mod",
            from = "filtered_dataset"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        enableBookmarking = "url",
        filter_default_state = !!absolute_state_file
      )
    }))

    url <- "?_inputs_&filter-IGNORE_INPUT=null&__tabset_0__=%22mod%22&open_options_modal=0&selector=%22dl1%22&click=true&filter-checkbox=false&filter-log=null&filter-json=%22%7B%5C%22filters%5C%22%3A%7B%5C%22datasets_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%5D%7D%2C%5C%22subject_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%7B%5C%22kind%5C%22%3A%5C%22filter%5C%22%2C%5C%22dataset%5C%22%3A%5C%22ds1%5C%22%2C%5C%22operation%5C%22%3A%5C%22select_subset%5C%22%2C%5C%22variable%5C%22%3A%5C%22sbj_var%5C%22%2C%5C%22values%5C%22%3A%5B%5C%22SBJ-1%5C%22%5D%2C%5C%22include_NA%5C%22%3Afalse%7D%5D%7D%7D%2C%5C%22dataset_list_name%5C%22%3A%5C%22dl1%5C%22%7D%22"

    test_that(
      "Bookmark can be restored | Bookmark overrides state" |>
        vdoc[["add_spec"]](c(
          specs$FILTERING$FILTER_BOOKMARKABLE
        )),
      {
        full_url <- paste0(root_app$get_url(), url)
        app <- shinytest2::AppDriver$new(full_url)
        expect_identical(app$get_value(output = "mod-text"), "1")
      }
    )

    test_that(
      "Bookmark can be restored with no state" |>
        vdoc[["add_spec"]](c(
          specs$FILTERING$FILTER_BOOKMARKABLE
        )),
      {
        root_app <- start_app_driver(rlang::quo({
          dv.manager:::run_app(
            data = dataset_lists,
            module_list = list(
              Simple3 = dv.manager:::mod_simple(
                dataset = "ds1",
                module_id = "mod",
                from = "filtered_dataset"
              )
            ),
            filter_data = "ds1",
            filter_key = "sbj_var",
            enableBookmarking = "url",
            filter = "development"
          )
        }))

        full_url <- paste0(root_app$get_url(), url)
        app <- shinytest2::AppDriver$new(full_url)

        expect_identical(app$get_value(output = "mod-text"), "1")
      }
    )
  })
})

test_that(
  "filter only sends one value when it is updated" |>
    vdoc[["add_spec"]](c(
      specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
    )),
  {
    # Related to every time we change the filter, including first beat

    skip("skipped until we can set a state programatically")
  }
)

test_that(
  "modules bookmark state works with filter" |>
    vdoc[["add_spec"]](c(
      specs$FILTERING$FILTER_BOOKMARKABLE
    )),
  {
    # Related to the first beat of the app
    # Mainly related to the above, the main requisite is that when starting the app on a bookmarked state
    # a single filtered dataset_list is sent to the modules, otherwise bookmark state is spent on the first filtered
    # dataset_list and in the next one they go to empty/default

    skip("skipped until we can set a state programatically")
  }
)

test_that(
  "filter labels are preserved" |>
    vdoc[["add_spec"]](c(
      specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
    )),
  {
    expect_true(FALSE)
  }
)
