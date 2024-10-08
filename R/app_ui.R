#' The application User-Interface using an HTML template
#'
#' @param id This parameter can be either an string id if the app_ui is being used as a module or
#' a request if the app_ui is being used as an standalone application then it becomes
#' a Internal parameter for `{shiny}`
#'     DO NOT REMOVE.
#'
#' @keywords internal



app_ui <- function(id) {
  if (is.environment(id)) {
    log_inform("I am the ui of an app")
    ns <- base::identity
  } else if (is.character(id)) {
    ns <- shiny::NS(id)
    log_inform(glue::glue("I am the ui of the module: {ns('')}"))
  } else {
    stop("Unknown value type in id")
  }

  data <- get_config("data")
  module_list <- get_config("module_list")
  filter_data <- get_config("filter_data")

  log_inform("Initializing HTML template UI")
  log_inform(glue::glue(
    "Available modules: {paste(names(module_list), collapse=',')}"
  ))
  log_inform(glue::glue("Available modules (N): {length(module_list)}"))
  log_inform(glue::glue("Dataset options (N): {length(data)}"))


  dataset_filters_ui <- local({
    datasets_filters_info <- get_dataset_filters_info(data, filter_data)
    purrr::map(
      datasets_filters_info,
      function(entry) {
        shiny::div(
          id = entry[["id_cont"]],
          class = "filter-control  filter-filters",
          shiny::tags[["label"]](entry[["name"]]),
          dv.filter::data_filter_ui(ns(entry[["id"]])),
          shiny::hr(style = "border-top: 2px solid gray; height: 10px;")
        )
      }
    )
  })

  collapsable_ui <-
    shiny::div(
      class = "menu-contents",
      shiny::div(
        id = ns("shiny_filter_panel"),
        shinyjs::hidden(shiny::div(
          id = ns("dataset_selector"),
          class = "c-well",
          shiny::tags$label("Dataset Selection",
            class = "text-primary"
          ),
          shiny::selectInput(ns("selector"), label = NULL, choices = names(data))
        )),
        shiny::div(
          class = "c-well shiny_filter",
          shiny::tags$label("Global Filter", class = "text-primary"),           
          shiny::div(
            class = "filter-control  filter-filters",
            dv.filter::data_filter_ui(ns("global_filter"))
          )
        ),
        shiny::div(
          class = "c-well shiny_filter",
          shiny::tags$label("Dataset Filter", class = "text-primary"),
          dataset_filters_ui
        )
      )
    )

  btn_group <- shiny::div(
    id = "btn-group",
    shiny::bookmarkButton("", class = "navbar-btn"),
    # Remove export functionality until new order
    # shiny::actionButton(ns("open_report_modal"), shiny::span(shiny::icon("download")), class = "navbar-btn"), # nolint
    shiny::actionButton(ns("open_options_modal"), shiny::span(shiny::icon("cogs")), class = "navbar-btn")
  )

  dataset_name <-
    shiny::div(
      shiny::tags$span(shiny::icon("info-circle", class = "fa-lg")),
      shiny::textOutput(ns("dataset_name"), container = shiny::tags$span),
      shiny::textOutput(ns("dataset_date"), container = shiny::tags$span),
      class = "grid_page_date"
    )

  sidebar <- shiny::div(
    class = "sidebar-container",
    shiny::tags$input(
      type = "checkbox",
      class = "checkbox",
      id = "click",
      hidden = "",
      checked = ""
    ),
    shiny::div(
      class = "sidebar",
      shiny::tags$span(class = "logo-text", "DaVinci"),
      shiny::tags$label(
        `for` = "click",
        class = "menu-icon",
        shiny::div(class = "line line-1"),
        shiny::div(class = "line line-2"),
        shiny::div(class = "line line-3")
      ),
      collapsable_ui
    ),
    btn_group # Location modified through css check custom.css
  )

  # unnamed because tabset does not admit named list there
  tabs <-
    unname(purrr::imap(module_list, ~ shiny::tabPanel(title = .y, ns_css(.x[["ui"]](
      ns(.x$module_id)
    )))))

  shiny::fluidPage(
    insert_header_add_resources(app_title = get_config("title")),
    theme = get_app_theme(),
    class = "display-grid",
    sidebar,
    do.call(shiny::tabsetPanel, c(
      tabs,
      type = "pills",
      id = ns("main_tab_panel")
    )),
    dataset_name
  )
}
