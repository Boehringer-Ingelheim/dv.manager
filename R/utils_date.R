#' Add a date_range attribute to a dataset
#'
#' @description This function will add an attribute `date_range` to a dataset.
#' The source for this dates is the `mtime` (POSIXct) entry from the `meta` attribute as provided by `dv.loader`.
#' As different domains are contained as a list in a single dataset, this attribute will contain the minimum
#' and the maximum modification date from all the domains in the dataset.
#'
#' See `vignette("customizing_application")`
#'
#' @param dataset a dataset loaded with dv.loader
#'
#' @details In case any of the entries in dataset does not have the expected attribute, it will show a warning.
#'
#' @export


add_date_range <- function(dataset) {
  attr(dataset, "date_range") <- get_date_range(purrr::map(dataset, ~ attr(.x, "meta")[["mtime"]]))
  return(dataset)
}

#' Return a range of dates
#'
#' @description Calculates a range of dates from a list of POSIXct objects. If any of the entries is NULL it will show
#' a warning indicating which of the entries
#' was NULL. If any of the entries is not a date object it will throw an error.
#'
#' @param x A list of date objects
#'
#' @return A character vector with two entries, the first and last date in the input vector.
#'
#' @keywords internal

get_date_range <- function(x) {
  with_attr <- purrr::discard(x, is.null)
  without_attr <- purrr::keep(x, is.null)

  purrr::iwalk(
    with_attr,
    ~ if (!lubridate::is.POSIXct(.x)) {
      msg <- glue::glue("{.y} mtime in meta attribute is not a date-object.")
      rlang::abort(msg)
    }
  )

  purrr::iwalk(
    without_attr,
    function(.x, .y) {
      warn_string <- glue::glue("{.y} has no date. no meta attribute or no mtime entry")
      log_warn(warn_string)
    }
  )

  if (length(without_attr) > 0) { # If there is no date in any of the data tables
    date_range <- c(NA, NA)
  } else {
    # Date lists are weird to work with, we need as a vector of dates to apply min and max below
    # unlist does not work fine therefore we must transform in a special way in this case using purrr reduce.
    dates <- purrr::reduce(with_attr, c)
    date_range <- c(min(dates), max(dates))
  }

  return(date_range)
}
