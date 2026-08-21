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
    expect_snapshot(filter_to_odg(udlwfi)[["reference_txt"]] |> message())
  })
})

filter_to_odg <- function(unfiltered_dataset_list_with_filter_info) {
  TC <- c(
    h = "\u2500", # ─
    v = "\u2502 ", # │(spc)
    vr = "\u251C\u2500", # ├─
    ur = "\u2514\u2500", # └─
    spc = "  " # double space
  )
  MAX_PRINTED_SUBSET_VALUES <- 5

  f <- unfiltered_dataset_list_with_filter_info[["dataset_list_filter"]][["parsed"]][["filters"]]
  udl <- unfiltered_dataset_list_with_filter_info[["unfiltered_dataset_list"]]

  unicode_filter <- list()
  unicode_filter[["sbj"]] <- "Subject_filter"

  reference_list <- list()

  actions <- local({
    actions <- list()
    actions[[FC$FE$COMB$AND]] <- function(el) {
      curr_unicode_filter <- character(0)
      curr_unicode_filter[[1]] <- paste0(" ", el[[FC$FE$F$OPERATION]])
      children <- el[[FC$FE$F$CHILDREN]]
      children_length <- length(children)

      for (idx in seq_along(children)) {
        child <- children[[idx]]
        if (idx == children_length) {
          tree_char <- TC[["ur"]]
          tree_prefix <- TC[["spc"]]
        } else {
          tree_char <- TC[["vr"]]
          tree_prefix <- TC[["v"]]
        }

        operation <- child[[FC$FE$F$OPERATION]]
        child_unicode <- actions[[operation]](child)
        child_unicode[[1]] <- paste0(" ", tree_char, child_unicode[[1]])
        child_unicode[2:length(child_unicode)] <- paste0(" ", tree_prefix, child_unicode[2:length(child_unicode)])
        curr_unicode_filter <- c(curr_unicode_filter, child_unicode)
      }
      curr_unicode_filter
    }
    actions[[FC$FE$COMB$OR]] <- actions[[FC$FE$COMB$AND]]
    actions[[FC$FE$COMB$NOT]] <- actions[[FC$FE$COMB$AND]]
    actions[[FC$SFE$COMB$UNION]] <- actions[[FC$FE$COMB$AND]]
    actions[[FC$SFE$COMB$INTERSECT]] <- actions[[FC$FE$COMB$AND]]
    actions[[FC$SFE$COMB$COMPLEMENT]] <- actions[[FC$FE$COMB$AND]]
    actions[[FC$FE$OP$SUBSET]] <- function(el) {
      cuf <- character(0)
      var_name <- el[[FC$FE$F$VARIABLE]]
      dataset_name <- el[[FC$FE$F$DATASET]]
      var_label <- get_lbl_robust(udl[[dataset_name]], var_name)
      dataset_label <- get_lbl_robust(udl, dataset_name)
      values <- el[[FC$FE$F$VALUES]]
      values_length <- length(values)

      cuf[[length(cuf) + 1]] <- paste0(" ", "Variable: ", var_label, " [", var_name, "]")
      cuf[[length(cuf) + 1]] <- paste0(" ", TC[["vr"]], " Dataset: ", dataset_label, " [", dataset_name, "]")
      cuf[[length(cuf) + 1]] <- paste0(" ", TC[["vr"]], " Selected ", values_length, " value(s)")

      printed_values <- local({
        if (values_length > MAX_PRINTED_SUBSET_VALUES) {
          nxt_reference_idx <- length(reference_list) + 1
          reference_list[[nxt_reference_idx]] <<- values
          c(
            values[1:MAX_PRINTED_SUBSET_VALUES],
            paste0(
              "... (",
              values_length - MAX_PRINTED_SUBSET_VALUES,
              " values not shown. See filter reference (",
              nxt_reference_idx,
              "))"
            )
          )
        } else {
          values
        }
      })

      printed_values_length <- length(printed_values)
      vu <- character(printed_values_length)
      for (idx in seq_along(printed_values)) {
        if (idx == printed_values_length) {
          tree_char <- TC[["ur"]]
          tree_prefix <- TC[["spc"]]
        } else {
          tree_char <- TC[["vr"]]
          tree_prefix <- TC[["v"]]
        }

        vu[[idx]] <- paste0(" ", TC[["v"]], " ", tree_char, " ", printed_values[idx])
      }
      cuf <- c(cuf, vu)
      cuf[[length(cuf) + 1]] <- paste0(" ", TC[["ur"]], " Include NA: ", el[[FC$FE$F$INCLUDE_NA]])
      cuf[[length(cuf) + 1]] <- ""
      cuf
    }
    actions[[FC$FE$OP$RANGE]] <- function(el) {
      cuf <- character(0)
      var_name <- el[[FC$FE$F$VARIABLE]]
      dataset_name <- el[[FC$FE$F$DATASET]]
      var_label <- get_lbl_robust(udl[[dataset_name]], var_name)
      dataset_label <- get_lbl_robust(udl, dataset_name)
      cuf[[length(cuf) + 1]] <- paste0(" ", "Variable: ", var_label, " [", var_name, "]")
      cuf[[length(cuf) + 1]] <- paste0(" ", TC[["vr"]], " Dataset: ", dataset_label, " [", dataset_name, "]")
      cuf[[length(cuf) + 1]] <- paste0(" ", TC[["vr"]], " Min: ", el[[FC$FE$F$MIN]])
      cuf[[length(cuf) + 1]] <- paste0(" ", TC[["vr"]], " Max: ", el[[FC$FE$F$MAX]])
      cuf[[length(cuf) + 1]] <- paste0(" ", TC[["ur"]], " Include NA: ", el[[FC$FE$F$INCLUDE_NA]])
      cuf[[length(cuf) + 1]] <- ""
      cuf
    }
    actions[[FC$FE$OP$DATE]] <- actions[[FC$FE$OP$RANGE]]
    actions
  })

  create_single_unicode_filter <- function(el, root_el) {
    curr_unicode_filter <- character(0)
    curr_unicode_filter[[1]] <- root_el
    children <- el[[FC$FE$F$CHILDREN]]
    last_child_idx <- length(children)

    for (idx in seq_along(children)) {
      child <- children[[idx]]
      if (idx == last_child_idx) {
        tree_char <- TC[["ur"]]
        tree_prefix <- TC[["spc"]]
      } else {
        tree_char <- TC[["vr"]]
        tree_prefix <- TC[["v"]]
      }
      operation <- child[[FC$FE$F$OPERATION]]
      child_unicode <- actions[[operation]](child)
      child_unicode[[1]] <- paste0(tree_char, child_unicode[[1]])
      child_unicode[2:length(child_unicode)] <- paste0(tree_prefix, child_unicode[2:length(child_unicode)])
      curr_unicode_filter <- c(curr_unicode_filter, child_unicode)
    }
    curr_unicode_filter
  }

  filter_unicode <- paste0(
    create_single_unicode_filter(f[["subject_filter"]], "Subject filter"),
    collapse = "\n"
  )

  ds_unicode <- local({
    finfo <- unfiltered_dataset_list_with_filter_info$filter_info
    res <- character(0)
    for (nm in names(finfo)) {
      dataset_label <- dataset_label <- get_lbl_robust(udl, nm)
      dataset_label <- paste0(dataset_label, " [", nm, "]")
      kept_rows <- sum(finfo[[nm]][["mask"]])
      total_rows <- length(finfo[[nm]][["mask"]])
      dropped_rows <- total_rows - kept_rows
      res[[nm]] <- paste0(
        dataset_label,
        " (",
        kept_rows,
        " out of ",
        total_rows,
        " ",
        TC[["h"]],
        " ",
        dropped_rows,
        " rows dropped",
        ")"
      )
    }
    res
  })

  for (ds_filter in f[["datasets_filter"]][["children"]]) {
    nm <- ds_filter[[FC$FE$F$NAME]]
    ds_unicode[[nm]] <- paste0(
      create_single_unicode_filter(ds_filter, ds_unicode[[nm]]),
      collapse = "\n"
    )
  }

  filter_unicode <- paste0(c(filter_unicode, ds_unicode), "\n", collapse = "\n")
  reference_txt <- local({
    if (length(reference_list) > 0) {
      paste0(
        paste0("Reference (", seq_along(reference_list), "): \n"),
        lapply(reference_list, \(x) paste0('"', x, '"', collapse = "\t")),
        "\n",
        collapse = "\n"
      )
    } else {
      ""
    }
  })
  list(
    txt = filter_unicode,
    reference_txt = reference_txt
  )
}
