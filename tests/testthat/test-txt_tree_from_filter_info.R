local({
  df1 <- data.frame(
    a = 1,
    b = 2
  )

  df2 <- data.frame(
    c = 1,
    d = 2
  )

  df1 <- set_lbls(df1, stats::setNames(paste0(names(df1), "_label"), names(df1)))
  df2 <- set_lbls(df2, stats::setNames(paste0(names(df2), "_label"), names(df2)))

  udl <- list(
    df1 = df1,
    df2 = df2
  )

  udl <- set_lbls(udl, stats::setNames(paste0(names(udl), "_label"), names(udl)))

  unfiltered_dataset_list_with_filter_info <- list(
    unfiltered_dataset_list = udl,
    filter_info = list(
      df1 = list(mask = c(TRUE, TRUE)),
      df2 = list(mask = c(TRUE, FALSE))
    ),
    dataset_list_filter = list(
      parsed = list(
        filters = list()
      )
    )
  )

  test_that("txt_tree_from_filter_info works with an empty filter", {
    udlwfi <- unfiltered_dataset_list_with_filter_info
    expect_snapshot(txt_tree_from_filter_info(udlwfi))
  })

  test_that("txt_tree_from_filter_info works with all actions", {
    udlwfi <- unfiltered_dataset_list_with_filter_info
    udlwfi[["dataset_list_filter"]][["parsed"]][["filters"]] <- list(
      "subject_filter" = list(
        children = list(
          list(
            kind = "set_operation",
            operation = "union",
            children = list(
              list(
                kind = "set_operation",
                operation = "intersect",
                children = list(
                  list(
                    kind = "set_operation",
                    operation = "complement",
                    children = list(
                      list(
                        kind = "row_operation",
                        operation = "not",
                        children = list(
                          list(
                            kind = "row_operation",
                            operation = "and",
                            children = list(
                              list(
                                kind = "row_operation",
                                operation = "or",
                                children = list(
                                  list(
                                    kind = "filter",
                                    operation = "select_range",
                                    max = 4,
                                    min = 2,
                                    include_NA = FALSE,
                                    variable = "a",
                                    dataset = "df1"
                                  ),
                                  list(
                                    kind = "filter",
                                    operation = "select_subset",
                                    values = letters,
                                    include_NA = FALSE,
                                    variable = "b",
                                    dataset = "df1"
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      datasets_filter = list(
        children = list(
          list(
            "name" = "df1",
            children = list(
              list(
                kind = "row_operation",
                operation = "not",
                children = list(
                  list(
                    kind = "row_operation",
                    operation = "and",
                    children = list(
                      list(
                        kind = "row_operation",
                        operation = "or",
                        children = list(
                          list(
                            kind = "filter",
                            operation = "select_range",
                            max = 4,
                            min = 2,
                            include_NA = FALSE,
                            variable = "a",
                            dataset = "df1"
                          ),
                          list(
                            kind = "filter",
                            operation = "select_subset",
                            values = letters,
                            include_NA = FALSE,
                            variable = "b",
                            dataset = "df1"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    expect_snapshot(txt_tree_from_filter_info(udlwfi)[["txt"]])
    expect_snapshot(txt_tree_from_filter_info(udlwfi)[["txt"]] |> message())
    expect_identical(txt_tree_from_filter_info(udlwfi)[["references"]], list(letters, letters))
  })

  test_that("txt_tree_from_filter_info indents a childless group as a single line", {
    udlwfi <- unfiltered_dataset_list_with_filter_info
    udlwfi[["dataset_list_filter"]][["parsed"]][["filters"]] <- list(
      subject_filter = list(
        children = list(
          list(
            kind = "row_operation",
            operation = "and",
            children = list(
              list(kind = "row_operation", operation = "or", children = list()),
              list(kind = "filter", operation = "select_range", dataset = "df1", variable = "a", min = 1, max = 2)
            )
          ),
          list(kind = "row_operation", operation = "or", children = list())
        )
      )
    )
    expect_snapshot(txt_tree_from_filter_info(udlwfi)[["txt"]] |> message())
  })

  test_that("txt_tree_from_filter_info marks the range bounds it can only print rounded", {
    range_txt <- function(min, max) {
      udlwfi <- unfiltered_dataset_list_with_filter_info
      udlwfi[["dataset_list_filter"]][["parsed"]][["filters"]] <- list(
        subject_filter = list(
          children = list(
            list(kind = "filter", operation = "select_range", dataset = "df1", variable = "a", min = min, max = max)
          )
        )
      )
      txt_tree_from_filter_info(udlwfi)[["txt"]]
    }

    # Printed with 15 significant digits: 59.999999999999993 shows as 60, which the filter does not include
    txt <- range_txt(min = 20L, max = 59.999999999999993)
    expect_match(txt, "Min: 20\n", fixed = TRUE)
    expect_match(txt, paste0("Max: 60 ", ODG$MSG$ROUNDED_BOUND, "\n"), fixed = TRUE)

    # Bounds that read back as the same number are not marked, however many decimals they print
    txt <- range_txt(min = 0.1, max = -1 / 4)
    expect_match(txt, "Min: 0.1\n", fixed = TRUE)
    expect_match(txt, "Max: -0.25\n", fixed = TRUE)
    expect_no_match(txt, ODG$MSG$ROUNDED_BOUND, fixed = TRUE)

    # Dates print as they are
    txt <- range_txt(min = "2020-01-01", max = "2020-12-31")
    expect_no_match(txt, ODG$MSG$ROUNDED_BOUND, fixed = TRUE)
  })
})
