#' tests if all elements of a list are named
#'
#' @param x a list to be tested
#'
#' @description
#' Is true if all elements have a name or the is of length(0).
#'
#' @keywords internal
has_all_items_named <- function(x) {
  is.list(x) &&
    (
      (length(x) == 0) || # empty list
        (
          !is.null(names(x)) && # Not an unnamed list
            !any("" %in% names(x)) # No unnamed elements
        )
    )
}
