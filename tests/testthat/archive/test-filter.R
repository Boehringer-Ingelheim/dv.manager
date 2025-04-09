local({
  data <- data.frame(
    row.names = 1:4,
    integer_col = c(1L:3L, NA),
    double_col = c(1.0:3.0, NA),
    date_col = as.Date("2024-01-01") + c(0L:2L, NA),
    category_col = factor(c(letters[1:3], NA))
  )

  test_that("integer type filter is correctly applied without NA", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = 1, max = 2),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, FALSE)
    )
  })

  test_that("integer type filter is correctly applied with NA", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = 1, max = 2),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, TRUE)
    )
  })

  test_that("integer type filter is correctly applied without NA and no max value", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = 2, max = NA),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(FALSE, TRUE, TRUE, FALSE)
    )
  })

  test_that("integer type filter is correctly applied with NA and no max value", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = 2, max = NA),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(FALSE, TRUE, TRUE, TRUE)
    )
  })

  test_that("integer type filter is correctly applied without NA and no min value", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = NA, max = 2),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, FALSE)
    )
  })

  test_that("integer type filter is correctly applied with NA and no min value", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = NA, max = 2),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, TRUE)
    )
  })

  test_that("integer type filter is correctly applied without NA and no max/min value", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = NA, max = NA),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, TRUE, FALSE)
    )
  })

  test_that("integer type filter is correctly applied with NA and no max/min value", {
    filter_parameters <- list(
      type = "integer",
      column = "integer_col",
      value = list(min = NA, max = NA),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, TRUE, TRUE)
    )
  })

  # ----

  test_that("double type filter is correctly applied without NA", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = 1, max = 2),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, FALSE)
    )
  })

  test_that("double type filter is correctly applied with NA", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = 1, max = 2),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, TRUE)
    )
  })

  test_that("double type filter is correctly applied without NA and no max value", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = 2, max = NA),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(FALSE, TRUE, TRUE, FALSE)
    )
  })

  test_that("double type filter is correctly applied with NA and no max value", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = 2, max = NA),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(FALSE, TRUE, TRUE, TRUE)
    )
  })

  test_that("double type filter is correctly applied without NA and no min value", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = NA, max = 2),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, FALSE)
    )
  })

  test_that("double type filter is correctly applied with NA and no min value", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = NA, max = 2),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, TRUE)
    )
  })

  test_that("double type filter is correctly applied without NA and no max/min value", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = NA, max = NA),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, TRUE, FALSE)
    )
  })

  test_that("double type filter is correctly applied with NA and no max/min value", {
    filter_parameters <- list(
      type = "double",
      column = "double_col",
      value = list(min = NA, max = NA),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, TRUE, TRUE)
    )
  })

  # ----

  test_that("date type filter is correctly applied without NA", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date("2024-01-01"), max = as.Date("2024-01-02")),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, FALSE)
    )
  })

  test_that("date type filter is correctly applied with NA", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date("2024-01-01"), max = as.Date("2024-01-02")),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, TRUE)
    )
  })

  test_that("date type filter is correctly applied without NA and no max value", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date("2024-01-02"), max = as.Date(NA)),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(FALSE, TRUE, TRUE, FALSE)
    )
  })

  test_that("date type filter is correctly applied with NA and no max value", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date("2024-01-02"), max = as.Date(NA)),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(FALSE, TRUE, TRUE, TRUE)
    )
  })

  test_that("date type filter is correctly applied without NA and no min value", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date(NA), max = as.Date("2024-01-02")),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, FALSE)
    )
  })

  test_that("date type filter is correctly applied with NA and no min value", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date(NA), max = as.Date("2024-01-02")),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, FALSE, TRUE)
    )
  })

  test_that("date type filter is correctly applied without NA and no max/min value", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date(NA), max = as.Date(NA)),
      NAs = FALSE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, TRUE, FALSE)
    )
  })

  test_that("date type filter is correctly applied with NA and no max/min value", {
    filter_parameters <- list(
      type = "date",
      column = "date_col",
      value = list(min = as.Date(NA), max = as.Date(NA)),
      NAs = TRUE
    )

    expect_identical(
      apply_filter(data, filter_parameters),
      c(TRUE, TRUE, TRUE, TRUE)
    )
  })



  test_that("category type filter is correctly applied without NA", {
    filter_parameters <- list(
      type = "category",
      column = "category_col",
      value = c("a", "b"),
      NAs = FALSE
    )

    expect_identical(apply_filter(data, filter_parameters), c(TRUE, TRUE, FALSE, FALSE))
  })

  test_that("category type filter is correctly applied with NA", {
    filter_parameters <- list(
      type = "category",
      column = "category_col",
      value = c("a", "b"),
      NAs = TRUE
    )

    expect_identical(apply_filter(data, filter_parameters), c(TRUE, TRUE, FALSE, TRUE))
  })

  test_that("category type filter is correctly applied with Inf special case", {
    data <- data.frame(
      row.names = 1:1,
      category_col = factor(c("Inf"))
    )

    filter_parameters <- list(
      type = "category",
      column = "category_col",
      value = c("Inf"),
      NAs = FALSE
    )

    expect_identical(apply_filter(data, filter_parameters), c(TRUE))
  })

  test_that("and type filter is correctly applied", {
    filter_parameters <- list(
      type = "and",
      filter_list = list(
        list(
          type = "category",
          column = "integer_col",
          value = list(min = 1, max = 2),
          NAs = FALSE
        ),
        list(
          type = "category",
          column = "integer_col",
          value = list(min = 2, max = 3),
          NAs = FALSE
        )
      )
    )

    expect_identical(apply_filter(data, filter_parameters), c(FALSE, TRUE, FALSE, FALSE))
  })

  test_that("or type filter is correctly applied", {
    filter_parameters <- list(
      type = "or",
      filter_list = list(
        list(
          type = "category",
          column = "integer_col",
          value = list(min = 1, max = 2),
          NAs = FALSE
        ),
        list(
          type = "category",
          column = "integer_col",
          value = list(min = 2, max = 3),
          NAs = FALSE
        )
      )
    )

    expect_identical(apply_filter(data, filter_parameters), c(TRUE, TRUE, TRUE, FALSE))
  })

  test_that("empty filter returns all TRUE", {
    filter_parameters <- list()

    expect_identical(apply_filter(data, filter_parameters), c(TRUE, TRUE, TRUE, TRUE))
  })

  test_that("not type is correctly applied", {
    filter_parameters <- list(
      type = "not",
      filter = list()
    )

    expect_identical(apply_filter(data, filter_parameters), c(FALSE, FALSE, FALSE, FALSE))
  })
})

local({
  data <- list(
    d1 = data.frame(
      sbj = 1L:3L,
      age = 1:3
    ),
    d2 = data.frame(
      sbj = 1L:3L,
      age = 1:3
    )
  )

  test_that("union is correctly applied", {
    filter_list <- list(
      type = "union",
      filter = list(
        list(
          target = "d1",
          filter = list(
            column = "age",
            type = "integer",
            value = list(min = 1, max = 1),
            NAs = FALSE
          )
        ),
        list(
          target = "d1",
          filter = list(
            column = "age",
            type = "integer",
            value = list(min = 2, max = 2),
            NAs = FALSE
          )
        )
      )
    )

    expect_identical(
      compute_subject_set_from_filter(data, filter_list, "sbj"),
      c(1L, 2L)
    )
  })

  test_that("intersection is correctly applied", {
    filter_list <- list(
      type = "intersect",
      filter = list(
        list(
          target = "d1",
          filter = list(
            column = "age",
            type = "integer",
            value = list(min = 1, max = 2),
            NAs = FALSE
          )
        ),
        list(
          target = "d1",
          filter = list(
            column = "age",
            type = "integer",
            value = list(min = 2, max = 3),
            NAs = FALSE
          )
        )
      )
    )

    expect_identical(
      compute_subject_set_from_filter(data, filter_list, "sbj"),
      c(2L)
    )
  })

  test_that("diff is correctly applied", {
    filter_list <- list(
      type = "diff",
      filter = list(
        list(
          target = "d1",
          filter = list(
            column = "age",
            type = "integer",
            value = list(min = 1, max = 2),
            NAs = FALSE
          )
        ),
        list(
          target = "d1",
          filter = list(
            column = "age",
            type = "integer",
            value = list(min = 2, max = 2),
            NAs = FALSE
          )
        )
      )
    )

    expect_identical(
      compute_subject_set_from_filter(data, filter_list, "sbj"),
      c(1L)
    )
  })
})
