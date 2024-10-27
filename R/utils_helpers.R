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

create_options_modal <- function(session, input, ns) {
  shiny::modalDialog(
    title = "Display options",
    shiny::checkboxInput(
      inputId = ns("change_theme"),
      label = "Show high-contrast theme",
      value = input$change_theme
    ),
    shiny::tags$a(shiny::icon("question-circle", class = "fa-lg"),
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

get_data_tables_names <- function(data) {
  nm <- character(0)
  for (idx in seq_along(data)) {
    curr_data <- if (is.function(data[[idx]])) data[[idx]]() else data[[idx]]
    nm <- union(nm, names(curr_data))
  }
  return(nm)
}

get_dataset_filters_info <- function(data, filter_data) {
  dataset_filter_names <- setdiff(get_data_tables_names(data), filter_data)
  purrr::map(
    dataset_filter_names,
    function(nm) {
      name <- nm
      hash <- digest::digest(nm, "murmur32")
      id <- sprintf("dataset_filter_%s", hash)
      cont_id <- paste0(id, "_cont")
      list(name = nm, id = id, hash = hash, id_cont = cont_id)
    }
  ) |> purrr::set_names(dataset_filter_names)
}

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

assert <- function(expr, msg) {
  if (!isTRUE(expr)) stop(msg)
}
