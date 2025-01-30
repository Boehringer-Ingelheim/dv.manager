#' Get all labels from a dataframe
#'
#' It will try to read the label attribute from each of the columns.
#'
#' @param df a dataframe.
#'
#' @return A named list in which name is the name of the df column and value is the label of the column.
#' Non-labelled columns will return NULL. In the robust version NULL is replaced by the name of the column.
#' @keywords internal
#' @export

get_lbls <- function(df) {
  purrr::map(df, purrr::attr_getter("label"))
}

#' @describeIn get_lbls robust
#' @export
get_lbls_robust <- function(df) {
  rpl_nulls_name(get_lbls(df))
}

#' Get a single label from a dataframe
#'
#' It will try to read the label attribute from one columns.
#'
#' @param df a dataframe.
#'
#' @param var from which column do we want the label
#'
#' @return A string with the label name
#'
#' @keywords internal
#' @export
get_lbl <- function(df, var) {
  purrr::attr_getter("label")(df[[var]])
}

#' Robust getter for dataframe labels
#'
#' It returns the label of the column of a dataframe if it is not NULL. Otherwise it returns the name of the column.
#'
#' @param df a dataframe.
#' @param var a column of the dataframe
#'
#' @return The label attribute or the name of the column if the label attribute is NULL.
#'
#' @keywords internal
#' @export

get_lbl_robust <- function(df, var) {
  if_not_null(var, rpl_nulls_name(get_lbls(df))[[var]])
}

#' Replace NULL values in a list with the name of the entry
#'
#' If one of the values is NULL it will be replaced with the name of the NULL entry.
#'
#' @details
#'
#' The input of this functions is usually the output of a `get_lbls` call.
#'
#' @param l a named list.
#'
#' @return A named list.
#'
#' @keywords internal

rpl_nulls_name <- function(l) {
  null_idx <- purrr::map_lgl(l, is.null)
  l[null_idx] <- names(l)[null_idx]
  l
}

#' Swap names and values
#'
#' @param l a named vector or list
#'
#' @return A vector or list with swapped names and values
#'
#' @keywords internal

swap_val_names <- function(l) {
  if (any(purrr::map_lgl(l, is.null))) {
    rlang::abort("NULL values are not allowed in l")
  }

  v <- if (is.list(l)) as.list(names(l)) else names(l) # maintain type
  stats::setNames(v, l)
}

#' Set the label attribute of a column in a data frame
#'
#'
#' @param df a dataframe.
#' @param var a column of the dataframe
#' @param lbl the label
#'
#' @return the dataframe with the replaced label
#'
#' @keywords internal
#' @export
set_lbl <- function(df, var, lbl) {
  attr(df[[var]], "label") <- lbl
  df
}

#' Set several labels in a dataframe
#'
#'
#' @param df a dataframe.
#' @param lbls a named list. Each entry will have as name the name of a given column in df and as value the expected
#'  label of the given column.
#'
#' @return A dataframe with the set labels
#' @keywords internal
#' @export

set_lbls <- function(df, lbls) {
  if (length(lbls) < 1) {
    return(df)
  }

  n_l <- names(lbls)
  for (idx in seq_along(lbls)) {
    attr(df[[n_l[[idx]]]], "label") <- lbls[[idx]]
  }

  df
}

#' Set several labels in a dataframe
#'
#'
#' @param df a dataframe.
#' @param lbls a named list. Each entry will have as name the name of a given column in df and as value the expected
#'  label of the given column. If df has no column with the name of the entry, the entry is ignored.
#'
#' @return A dataframe with the set labels
#' @keywords internal

possibly_set_lbls <- function(df, lbls) {
  if (length(lbls) < 1) {
    return(df)
  }
  n_d <- names(df)
  n_l <- names(lbls)

  for (idx in seq_along(lbls)) {
    if (n_l[[idx]] %in% n_d) {
      attr(df[[n_l[[idx]]]], "label") <- lbls[[idx]]
    } else {
      next()
    }
  }

  df
}

possibly_copy_labels <- function(target, origin) {
  lbls <- get_lbls(origin)
  possibly_set_lbls(target, lbls)
}

#' Renames a list to include its value in the name
#'
#' @details
#' If value and name are equal the entry name is not modified.
#' These lists usually come from swap_names(rpl_nulls_name(get_lbl(dataset)))
#'
#' @param l a named list
#' @keywords internal

name_label_formatter <- function(l) {
  n <- paste0(" [", names(l), "]")
  n[names(l) == l] <- ""
  names(l) <- paste0(l, n)
  l
}

get_labelled_names <- function(x) {
  unlist(get_lbls_robust(x) |> swap_val_names() |> name_label_formatter())
}
