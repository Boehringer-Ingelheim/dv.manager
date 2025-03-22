# TODO: Include type controls in each of the final filter
# select range check it is an integer or numeric, etc.
# check the required fields in each operation are present

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

  test_that("select_range filter returns mask excluding NAs", {
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

  test_that("select_range filter returns mask including NAs", {
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

  test_that("select_subset filter returns mask excluding NAs", {
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

  test_that("select_subset filter returns mask including NAs", {
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

  test_that("select_date filter returns mask excluding NAs for Date type", {
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

  test_that("select_date filter returns mask including NAs for Date type", {
   e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      values = c("b", "c", "d"),
      include_NA = TRUE,
      field = "date_col",
      dataset = "d"
    )    
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  test_that("select_date filter returns mask excluding NAs for POSIX type", {
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

  test_that("select_date filter returns mask including NAs for POSIX type", {
   e <- list(
      kind = "filter",
      operation = "select_date",
      min = "2024-01-02",
      max = "2024-01-04",
      values = c("b", "c", "d"),
      include_NA = TRUE,
      field = "posix_col",
      dataset = "d"
    )    
    mask <- process_dataset_filter_element(data_list = data_list, element = e, current_table_name = "d")
    expect_identical(mask, c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
  })

  # Filter combine operations

  test_that("and filter operation returns mask for 1 element", {
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

  test_that("and filter operation returns mask for n elements", {
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

  test_that("or filter operation returns mask for 1 element", {
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

  test_that("or filter operation returns mask for n elements", {
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

  test_that("not filter operation returns mask", {
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

  # Subject filter testing

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

  test_that("and filter operation returns mask for 1 element", {
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

  test_that("and filter operation returns mask for n elements", {
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

  test_that("or filter operation returns mask for 1 element", {
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

  test_that("or filter operation returns mask for n elements", {
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

  test_that("not filter operation returns mask", {
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

# Keep working with the errors
  



})
