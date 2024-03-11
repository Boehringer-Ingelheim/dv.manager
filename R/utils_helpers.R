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
