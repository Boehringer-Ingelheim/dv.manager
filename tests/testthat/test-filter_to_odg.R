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

  test_that("filter_to_odg works with an empty filter", {
    udlwfi <- unfiltered_dataset_list_with_filter_info
    expect_snapshot(filter_to_odg(udlwfi))
  })

  test_that("filter_to_odg works with all actions", {
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
    expect_snapshot(filter_to_odg(udlwfi)[["txt"]])
    expect_snapshot(filter_to_odg(udlwfi)[["txt"]] |> message())
    expect_identical(filter_to_odg(udlwfi)[["references"]], list(letters, letters))
  })

  test_that("filter_to_odg indents a childless group as a single line", {
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
    expect_snapshot(filter_to_odg(udlwfi)[["txt"]] |> message())
  })
})
