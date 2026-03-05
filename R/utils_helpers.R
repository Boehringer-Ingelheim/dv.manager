#' Include CSS files from path `/inst/css/`
#'
#' @description Imports custom CSS files
#'
#' @param pattern Default search for file pattern
#'
#' @keywords internal

include_css <- function(pattern = "*") {
  css_files <- list.files(
    system.file("css", package = "dv.manager", mustWork = TRUE),
    pattern = pattern,
    full.names = TRUE
  )
  return(htmltools::singleton(lapply(css_files, shiny::includeCSS)))
}

#' Include JS files from path `/inst/js/`
#'
#' @description Imports custom JS files
#'
#' @param pattern Default search for file pattern
#'
#' @keywords internal

include_js <- function(pattern = "*") {
  js_files <- list.files(
    system.file("js", package = "dv.manager", mustWork = TRUE),
    pattern = pattern,
    full.names = TRUE
  )
  return(htmltools::singleton(lapply(js_files, shiny::includeScript)))
}

create_info_modal <- function(session, input, ns) {
  shiny::modalDialog(
    title = "dv.manager info",
    shiny::tags$a(
      shiny::icon("question-circle", class = "fa-lg"),
      "Package documentation",
      href = "", # nolint
      target = "_blank"
    ),
    footer = shiny::tagList(
      shiny::modalButton("OK")
    ),
    easyClose = TRUE
  )
}

get_dataset_list_names <- function(dataset_list) {
  nm <- character(0)
  for (idx in seq_along(dataset_list)) {
    curr_data <- if (is.function(dataset_list[[idx]])) dataset_list[[idx]]() else dataset_list[[idx]]
    nm <- union(nm, names(curr_data))
  }
  return(nm)
}

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

assert <- function(expr, msg) {
  if (!isTRUE(expr)) stop(msg, call. = FALSE)
}
