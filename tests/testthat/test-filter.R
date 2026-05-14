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
      expect_identical(names(r[["filter_info"]]), c("d1", "d2"))
      expect_length(r[["filter_info"]], 2)
    })

    test_that("create_dataset_filter_masks creates a mask for dataset filters with children", {
      expect_identical(r[["filter_info"]][["d1"]][["mask"]], c(TRUE, FALSE))
    })

    test_that("create_dataset_filter_masks creates a TRUE mask for dataset filters with no children", {
      expect_true(r[["filter_info"]][["d2"]][["mask"]])
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
        list(filter_info = list(d1 = list(mask = c(TRUE, TRUE), lvls = list())))
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

      expected <- list(
        subjects = c("SBJ-2", "SBJ-3", "SBJ-4"),
        dataset_list_lvls = list(d1 = list(lvls = list()), d2 = list(lvls = list()))
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

      expected <- list(
        subjects = c("SBJ-2", "SBJ-3", "SBJ-4"),
        dataset_list_lvls = list(d1 = list(lvls = list()), d2 = list(lvls = list()))
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

      expected <- list(
        subjects = c("SBJ-2", "SBJ-3"),
        dataset_list_lvls = list(d1 = list(lvls = list()), d2 = list(lvls = list()))
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

      expected <- list(d1 = list(lvls = list(subset_var = "c")), d2 = list(lvls = list(subset_var2 = c("f"))))
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

      expected <- list(
        subjects = c("SBJ-2", "SBJ-3", "SBJ-4"),
        dataset_list_lvls = list(d1 = list(lvls = list()), d2 = list(lvls = list()))
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
        dataset_list_lvls = list(d1 = list(lvls = list()), d2 = list(lvls = list()))
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

      expected <- list(
        d1 = list(lvls = list(subset_var = c("b", "c", "d"))),
        d2 = list(lvls = list(subset_var2 = c("f", "h")))
      )
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

      expected <- list(
        subjects = c("SBJ-1", "SBJ-5", "SBJ-6"),
        dataset_list_lvls = list(d1 = list(lvls = list()), d2 = list(lvls = list()))
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

      expected <- list(d1 = list(lvls = list(subset_var = c("a", "LEVEL_WITH_NO_ROWS"))), d2 = list(lvls = list()))
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

  test_that("get_filtered_dataset basic row filtering works", {
    ufd_with_fi <- list(
      unfiltered_dataset_list = local({
        list(ds = data.frame(id = 1:3, x = c("a", "b", "c")))
      }),
      filter_info = list(
        ds = list(mask = c(TRUE, FALSE, TRUE), lvls = list())
      )
    )
    res <- get_filtered_dataset(ufd_with_fi, "ds")
    expect_equal(nrow(res), 2)
    expect_equal(res$id, c(1L, 3L))
    expect_false(lobstr::obj_addr(res$id) == lobstr::obj_addr(ufd_with_fi$unfiltered_dataset_list$ds$id))
  })

  test_that("get_filtered_dataset extra mask is AND-combined with filter mask", {
    ufd_with_fi <- list(
      unfiltered_dataset_list = local({
        list(ds = data.frame(id = 1:3))
      }),
      filter_info = list(
        ds = list(mask = c(TRUE, TRUE, TRUE), lvls = list())
      )
    )
    res <- get_filtered_dataset(ufd_with_fi, "ds", mask = c(TRUE, FALSE, TRUE))
    expect_equal(res$id, c(1L, 3L))
    expect_false(lobstr::obj_addr(res$id) == lobstr::obj_addr(ufd_with_fi$unfiltered_dataset_list$ds$id))
  })

  test_that("get_filtered_dataset vars restricts columns without copying data", {
    ufd_with_fi <- list(
      unfiltered_dataset_list = local({
        list(ds = data.frame(a = 1:2, b = 3:4, c = 5:6))
      }),
      filter_info = list(
        ds = list(mask = c(TRUE, TRUE), lvls = list())
      )
    )

    res <- get_filtered_dataset(ufd_with_fi, "ds", vars = c("a", "c"))
    expect_equal(names(res), c("a", "c"))
    expect_true(lobstr::obj_addr(res$a) == lobstr::obj_addr(ufd_with_fi$unfiltered_dataset_list$ds$a))
    expect_true(lobstr::obj_addr(res$c) == lobstr::obj_addr(ufd_with_fi$unfiltered_dataset_list$ds$c))
  })

  test_that("get_filtered_dataset variable labels are copied from unfiltered to filtered dataset", {
    ufd_with_fi <- list(
      unfiltered_dataset_list = local({
        ds <- data.frame(x = 1:3)
        attr(ds$x, "label") <- "label"
        list(ds = ds)
      }),
      filter_info = list(
        ds = list(mask = c(TRUE, TRUE, TRUE), lvls = list())
      )
    )

    res <- get_filtered_dataset(ufd_with_fi, "ds", vars = c("x"), mask = c(TRUE, FALSE, FALSE))
    expect_equal(attr(res$x, "label"), "label")
  })

  test_that("get_filtered_dataset_ all TRUE mask does not copy data vectors", {
    ufd_with_fi <- list(
      unfiltered_dataset_list = local({
        ds <- data.frame(a = 1:2, b = 3:4, c = 5:6)
        list(ds = ds)
      }),
      filter_info = list(
        ds = list(mask = c(TRUE, TRUE), lvls = list())
      )
    )

    res <- get_filtered_dataset(ufd_with_fi, "ds")
    expect_true(lobstr::obj_addr(res$a) == lobstr::obj_addr(ufd_with_fi$unfiltered_dataset_list$ds$a))
    expect_true(lobstr::obj_addr(res$b) == lobstr::obj_addr(ufd_with_fi$unfiltered_dataset_list$ds$b))
    expect_true(lobstr::obj_addr(res$c) == lobstr::obj_addr(ufd_with_fi$unfiltered_dataset_list$ds$c))
  })

  test_that("get_filtered_dataset_ keeps ds label attribute", {
    ufd_with_fi <- list(
      unfiltered_dataset_list = local({
        ds <- data.frame(a = 1:2, b = 3:4, c = 5:6)
        attr(ds, "label") <- "label"
        list(ds = ds)
      }),
      filter_info = list(
        ds = list(mask = c(TRUE, TRUE), lvls = list())
      )
    )

    res <- attr(get_filtered_dataset(ufd_with_fi, "ds"), "label")
    expect_identical(res, "label")
  })

  test_that("apply_lvls_info_to_ds dropped factor levels are restored when prescribed by ds_lvl", {
    unfiltered <- data.frame(x = factor(c("a", "b", "c")))
    filtered <- unfiltered[1:2, , drop = FALSE] # "c" absent
    ds_lvl <- list(x = c("a", "b", "c"))
    res <- apply_lvls_info_to_ds(unfiltered, filtered, ds_lvl)
    expect_equal(levels(res$x), c("a", "b", "c"))
  })

  test_that("apply_lvls_info_to_ds level order follows the unfiltered variable", {
    unfiltered <- data.frame(x = factor(c("b", "a", "c"), levels = c("c", "b", "a")))
    filtered <- unfiltered[1:2, , drop = FALSE]
    ds_lvl <- list(x = c("a", "b", "c"))
    res <- apply_lvls_info_to_ds(unfiltered, filtered, ds_lvl)
    expect_equal(levels(res$x), c("c", "b", "a"))
  })

  test_that("apply_lvls_info_to_ds non-factor columns are untouched", {
    unfiltered <- data.frame(x = factor(c("a", "b")), n = 1:2)
    filtered <- unfiltered
    res <- apply_lvls_info_to_ds(unfiltered, filtered, ds_lvl = list(x = "a"))
    expect_equal(res$n, 1:2)
  })

  test_that("apply_lvls_info_to_dsempty ds_lvl returns filtered dataset unchanged", {
    unfiltered <- data.frame(x = factor(c("a", "b")))
    filtered <- unfiltered[1, , drop = FALSE]
    res <- apply_lvls_info_to_ds(unfiltered, filtered, ds_lvl = list())
    expect_equal(res, filtered)
  })
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
          "var",
          "var_label",
          class(d[["var"]])[1],
          "categorical",
          1L,
          c("A", "B"),
          c(2L, 1L)
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
          "var",
          "var_label",
          class(d[["var"]])[1],
          "categorical",
          1L,
          c("A", "B", "C"),
          c(2L, 1L, 0L)
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
          "var",
          "var_label",
          class(d[["var"]])[1],
          "numerical",
          1L,
          1,
          2,
          graphics::hist(d[["var"]], plot = FALSE)[["density"]]
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
          "var",
          "var_label",
          class(d[["var"]])[1],
          "numerical",
          2L,
          Inf,
          -Inf,
          numeric(0)
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
          "var",
          "var_label",
          class(d[["var"]])[1],
          "date",
          1L,
          as.numeric(as.Date(c("2024-01-01"))),
          as.numeric(as.Date(c("2024-01-02")))
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
          "var",
          "var_label",
          class(d[["var"]])[1],
          "date",
          1L,
          as.numeric(as.Date(c("2024-01-01"))),
          as.numeric(as.Date(c("2024-01-02")))
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
    "get_filter_data_dataset_lists returns one entry per dataset_list and dataset" |>
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

      r <- get_filter_data_dataset_lists(dataset_lists)

      expect_length(r[[FC$FDF$DATASET_LISTS]], 2)
      expect_length(r[[FC$FDF$DATASET_LISTS]][[1]][[FC$FDF$DATASET_LIST]], 2)

      expect_identical(r[[FC$FDF$DATASET_LISTS]][[1]][[FC$FDF$NAME]], "dl1")
      expect_identical(r[[FC$FDF$DATASET_LISTS]][[1]][[FC$FDF$DATASET_LIST]][[1]][[FC$FDF$NAME]], "ds1")
      expect_identical(r[[FC$FDF$DATASET_LISTS]][[1]][[FC$FDF$DATASET_LIST]][[1]][[FC$FDF$LABEL]], "ds1")
      expect_identical(
        r[[FC$FDF$DATASET_LISTS]][[1]][[FC$FDF$DATASET_LIST]][[1]][[FC$FDF$VARIABLES]][[1]][[FC$FDF$NAME]],
        "var1"
      )
    }
  )

  test_that("combine_filter_info returns combined filter info", {
    # Both in subject and in dataset
    # Subject is copied
    # Datasets are combined (Both and 1 in one and not in the other)
    # Masks are combined (Both and 1 in one and not in the other)
    # vars are combined (Both and 1 in one and not in the other)

    filter_info <- list(
      error_list = new_error_list(),
      "result" = list(
        subject = list(
          subjects = c("sbj1", "sbj2"),
          "filter_info" = list(
            d_both1 = list(
              mask = c(TRUE, TRUE),
              lvls = list(var_both1 = c("a", "b"), var_both2 = c("a", "b"), var_only_subject = c("a", "b"))
            ),
            d_both2 = list(
              mask = c(TRUE, TRUE),
              lvls = list(var_both1 = c("a", "b"), var_both2 = c("a", "b"), var_only_subject = c("a", "b"))
            ),
            d_only_subject = list(mask = c(TRUE, TRUE), lvls = list(var1 = c("a", "b"), var2 = c("a", "b")))
          )
        ),
        dataset = list(
          "filter_info" = list(
            d_both1 = list(
              mask = c(TRUE, FALSE),
              lvls = list(var_both1 = c("a", "b"), var_both2 = c("a", "c"), var_only_dataset = c("a", "b"))
            ),
            d_both2 = list(
              mask = c(TRUE, FALSE),
              lvls = list(var_both1 = c("a", "b"), var_both2 = c("a", "c"), var_only_dataset = c("a", "b"))
            ),
            d_only_dataset = list(mask = c(TRUE, TRUE), lvls = list(var1 = c("a", "b"), var2 = c("a", "b")))
          )
        )
      )
    )

    expected_filter_info <- list(
      "result" = list(
        subjects = c("sbj1", "sbj2"),
        "filter_info" = list(
          d_both1 = list(
            mask = c(TRUE, FALSE),
            lvls = list(
              var_both1 = c("a", "b"),
              var_both2 = c("a"),
              var_only_subject = c("a", "b"),
              var_only_dataset = c("a", "b")
            )
          ),
          d_both2 = list(
            mask = c(TRUE, FALSE),
            lvls = list(
              var_both1 = c("a", "b"),
              var_both2 = c("a"),
              var_only_subject = c("a", "b"),
              var_only_dataset = c("a", "b")
            )
          ),
          d_only_subject = list(mask = c(TRUE, TRUE), lvls = list(var1 = c("a", "b"), var2 = c("a", "b"))),
          d_only_dataset = list(mask = c(TRUE, TRUE), lvls = list(var1 = c("a", "b"), var2 = c("a", "b")))
        )
      ),
      error_list = new_error_list()
    )

    expect_identical(
      combine_filter_info(filter_info),
      expected_filter_info
    )
  })
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
  skip_if_not_running_shiny_tests()

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
              from = "filtered_dataset_list"
            )
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          filter_default_state = !!absolute_state_file
        )
      }))

      state_from_app <- jsonlite::prettify(app$get_value(input = "filter-filter_state_json_input"))
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
              from = "filtered_dataset_list"
            )
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          filter_default_state = !!string_state
        )
      }))

      state_from_app <- jsonlite::prettify(app$get_value(input = "filter-filter_state_json_input"))
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
              from = "filtered_dataset_list"
            )
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
        )
      }))

      app_state <- app$get_value(input = "filter-filter_state_json_input")
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
            from = "filtered_dataset_list"
          )
        ),
        filter_dataset_name = "ds1",
        filter_key = "sbj_var",
        filter_default_state = !!absolute_state_file
      )
    }))

    # We use a manual download because the button imitates but it is not really a shiny button

    download_dir <- tempfile()
    dir.create(download_dir)

    chromote_session <- app$get_chromote_session()
    chromote_session$Browser$setDownloadBehavior(
      behavior = "allowAndName",
      downloadPath = download_dir,
      eventsEnabled = TRUE
    )

    download_done <- FALSE
    chromote_session$Browser$downloadProgress(callback = function(params) {
      if (params$state == "completed") {
        download_done <<- TRUE
      }
    })

    app$click(selector = "#filter-export_code_button_input")
    app$wait_for_idle()

    max_tries <- 10
    try <- 0
    start <- Sys.time()
    while (!download_done && try < max_tries) {
      try <- try + 1
      Sys.sleep(1)
    }
    stopifnot(download_done)
    downloaded_file <- list.files(download_dir, full.names = TRUE)[1]

    expect_equal(readLines(downloaded_file), readLines(absolute_state_file))
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
              dataset = "ds2",
              module_id = "mod",
              from = "filtered_dataset_list"
            )
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          filter_default_state = '{
    "filters": {
        "datasets_filter": {
            "children": [
                {
                    "name": "ds2",
                    "kind": "dataset",
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
              dataset = "ds2",
              module_id = "mod",
              from = "filtered_dataset_list"
            )
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          filter_default_state = '  {
    "filters": {
        "subject_filter": {
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
        },
        "datasets_filter": {
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
            from = "filtered_dataset_list"
          )
        ),
        filter_dataset_name = "ds1",
        filter_key = "sbj_var",
        enableBookmarking = "url",
        filter_default_state = !!absolute_state_file
      )
    }))

    url <- "?_inputs_&filter-IGNORE_INPUT=null&__tabset_0__=%22mod%22&open_options_modal=0&selector=%22dl1%22&click=true&filter-checkbox=false&filter-log=null&filter-filter_state_json_input=%22%7B%5C%22filters%5C%22%3A%7B%5C%22datasets_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%5D%7D%2C%5C%22subject_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%7B%5C%22kind%5C%22%3A%5C%22filter%5C%22%2C%5C%22dataset%5C%22%3A%5C%22ds1%5C%22%2C%5C%22operation%5C%22%3A%5C%22select_subset%5C%22%2C%5C%22variable%5C%22%3A%5C%22sbj_var%5C%22%2C%5C%22values%5C%22%3A%5B%5C%22SBJ-1%5C%22%5D%2C%5C%22include_NA%5C%22%3Afalse%7D%5D%7D%7D%2C%5C%22dataset_list_name%5C%22%3A%5C%22dl1%5C%22%7D%22"

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
  })
})

local({
  skip_if_not_running_shiny_tests()
  # Related to the first beat of the app and subsequent updates
  # Mainly related to the above, the main requisite is that when starting the app on a bookmarked state
  # a single filtered dataset_list is sent to the modules, otherwise bookmark state is spent on the first filtered
  # dataset_list and in the next one they go to empty/default

  dataset_lists <- list(
    dl1 = list(
      ds1 = data.frame(
        row.names = 1:6,
        range_var = c(1.0:5.0, NA),
        sbj_var = paste0("SBJ-", 1:6)
      )
    )
  )

  test_that(
    "filter only sends one value when it starts with no state" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
      )),
    {
      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!dataset_lists,
          module_list = list(
            AFMM = dv.manager:::mod_afmm_export("afmm")
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          enableBookmarking = "url"
        )
      }))

      app$wait_for_idle()

      counter <- app$get_values(export = TRUE)[["export"]][["afmm-filter_counter"]]
      expect_identical(counter, 1)
    }
  )

  test_that(
    "filter only sends one value when it starts with a state" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
      )),
    {
      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!dataset_lists,
          module_list = list(
            AFMM = dv.manager:::mod_afmm_export("afmm")
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          enableBookmarking = "url",
          filter_default_state = '{
    "filters": {
        "datasets_filter": {
            "children": [

            ]
        },
        "subject_filter": {
            "children": [
                {
                    "kind": "row_operation",
                    "operation": "and",
                    "children": [
                        {
                            "kind": "filter",
                            "dataset": "ds1",
                            "operation": "select_subset",
                            "variable": "sbj_var",
                            "values": [
                                "SBJ-1"
                            ],
                            "include_NA": true
                        }
                    ]
                }
            ]
        }
    },
    "dataset_list_name": "dl1"
}

'
        )
      }))

      app$wait_for_idle()

      counter <- app$get_values(export = TRUE)[["export"]][["afmm-filter_counter"]]
      expect_identical(counter, 1)
    }
  )

  test_that(
    "filter only sends one value when it is updated" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
      )),
    {
      set_filter <- function(app) {
        json <- r"--({
    "filters": {
        "datasets_filter": {
            "children": [

            ]
        },
        "subject_filter": {
            "children": [
                {
                    "kind": "row_operation",
                    "operation": "and",
                    "children": [
                        {
                            "kind": "filter",
                            "dataset": "ds1",
                            "operation": "select_subset",
                            "variable": "sbj_var",
                            "values": [
                                "SBJ-1"
                            ],
                            "include_NA": true
                        }
                    ]
                }
            ]
        }
    },
    "dataset_list_name": "dl1"
})--"
        js_fmt <- r"--(dv_filter.request_dataset_filter_state({id:"filter", state:`%s`}))--"

        js <- sprintf(js_fmt, json)

        app$run_js(js)
      }

      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!dataset_lists,
          module_list = list(
            AFMM = dv.manager:::mod_afmm_export("afmm")
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          enableBookmarking = "url"
        )
      }))

      app$wait_for_idle()
      set_filter(app)
      app$wait_for_idle

      counter <- app$get_values(export = TRUE)[["export"]][["afmm-filter_counter"]]
      expect_identical(counter, 2) # One on the start and the second one on the update
    }
  )

  test_that(
    "filter only sends one value when it starts with a bookmark" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_BOOKMARKABLE
      )),
    {
      url <- '?_inputs_&open_options_modal=0&selector="dl1"&click=true&filter-blockly-filter-checkbox=false&nav_header="afmm"&filter-saved_filter_state_json_msg_input="%5B%5D"&filter-filter_state_json_input="%7B%5C"filters%5C"%3A%7B%5C"datasets_filter%5C"%3A%7B%5C"children%5C"%3A%5B%5D%7D%2C%5C"subject_filter%5C"%3A%7B%5C"children%5C"%3A%5B%7B%5C"kind%5C"%3A%5C"row_operation%5C"%2C%5C"operation%5C"%3A%5C"and%5C"%2C%5C"children%5C"%3A%5B%7B%5C"kind%5C"%3A%5C"filter%5C"%2C%5C"dataset%5C"%3A%5C"ds1%5C"%2C%5C"operation%5C"%3A%5C"select_subset%5C"%2C%5C"variable%5C"%3A%5C"sbj_var%5C"%2C%5C"values%5C"%3A%5B%5C"SBJ-1%5C"%5D%2C%5C"include_NA%5C"%3Atrue%7D%5D%7D%5D%7D%7D%2C%5C"dataset_list_name%5C"%3A%5C"dl1%5C"%7D"&_values_&subgroup-subgroups=%5B%5D'

      root_app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!dataset_lists,
          module_list = list(
            AFMM = dv.manager:::mod_afmm_export("afmm")
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          enableBookmarking = "url"
        )
      }))

      full_url <- paste0(root_app$get_url(), url)
      app <- shinytest2::AppDriver$new(full_url)
      app$wait_for_idle()

      counter <- app$get_values(export = TRUE)[["export"]][["afmm-filter_counter"]]
      expect_identical(counter, 1)
    }
  )

  test_that(
    "filter labels are preserved" |>
      vdoc[["add_spec"]](c(
        specs$FILTERING$FILTER_ACTIVE_DATASET_LIST
      )),
    {
      labelled_dataset_lists <- dataset_lists
      labelled_dataset_lists[["dl1"]][["ds1"]] <- dv.manager:::set_lbls(
        labelled_dataset_lists[["dl1"]][["ds1"]],
        list(sbj_var = "Subject Variable", range_var = "Range Variable")
      )

      app <- start_app_driver(rlang::quo({
        dv.manager:::run_app(
          data = !!labelled_dataset_lists,
          module_list = list(
            Labels = dv.manager:::mod_dataset_labels("ds1", "labels")
          ),
          filter_dataset_name = "ds1",
          filter_key = "sbj_var",
          enableBookmarking = "url"
        )
      }))

      app$wait_for_idle()
      labels_before <- dv.manager:::get_lbls(
        app$get_values(export = TRUE)[["export"]][["labels-data"]][["ds1"]]
      )

      set_filter <- function(app) {
        json <- r"--({
    "filters": {
        "datasets_filter": {
            "children": []
        },
        "subject_filter": {
            "children": [
                {
                    "kind": "filter",
                    "dataset": "ds1",
                    "operation": "select_subset",
                    "variable": "sbj_var",
                    "values": ["SBJ-1"],
                    "include_NA": false
                }
            ]
        }
    },
    "dataset_list_name": "dl1"
})--"
        js_fmt <- r"--(dv_filter.request_dataset_filter_state({id:"filter", state:`%s`}))--"
        js <- sprintf(js_fmt, json)
        app$run_js(js)
      }

      set_filter(app)
      app$wait_for_idle()

      labels_after <- dv.manager:::get_lbls(
        app$get_values(export = TRUE)[["export"]][["labels-data"]][["ds1"]]
      )

      expect_identical(labels_before, labels_after)
    }
  )
})
