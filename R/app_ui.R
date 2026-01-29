#' The application User-Interface using an HTML template
#'
#' @param id This parameter can be either an string id if the app_ui is being used as a module or
#' a request if the app_ui is being used as an standalone application then it becomes
#' a Internal parameter for `{shiny}`
#'     DO NOT REMOVE.
#'
#' @keywords internal

app_ui <- function(request_id) {
  if (is.environment(request_id)) {
    log_inform("I am the ui of an app")
    id <- character(0)
  } else if (is.character(id)) {
    id <- request_id
    log_inform(glue::glue("I am the ui of the module: {ns('')}"))
  } else {
    stop("Unknown value type in request_id")
  }

  ns <- shiny::NS(id)

  ######################################

  dataset_lists <- get_config("data")
  module_info <- get_config("module_info")
  subject_filter_dataset_name <- get_config("filter_data")
  filter_info <- get_config("filter_info")

  filter_default_state <- filter_info[["filter_default_state"]]
  enable_subgroup <- get_config("subgroup")[["enable"]]

  log_inform("Initializing HTML template UI")
  log_inform(glue::glue("Available modules (N): {length(module_info[[\"ui_list\"]])}"))
  log_inform(glue::glue("Dataset options (N): {length(data)}"))

  if (enable_subgroup) {
    filter_ui <- shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Filter",
        new_filter_ui(ns(ID$FILTER), subject_filter_dataset_name, state = filter_default_state)
      ),
      shiny::tabPanel(
        title = "Subgroup",
        shiny::div(class = "dv_subgroup_menu", mod_subgroup_ui(ns(ID$SUBGROUP), subject_filter_dataset_name))
      )
    )
  } else {
    filter_ui <- shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Filter",
        new_filter_ui(ns(ID$FILTER), subject_filter_dataset_name, state = filter_default_state)
      )
    )
  }

  collapsable_ui <-
    shiny::div(
      class = "menu-contents",
      shiny::div(
        id = ns("shiny_filter_panel"),
        shinyjs::hidden(shiny::div(
          id = ns("dataset_selector"),
          class = "ps-3 pe-3 pt-3 m-3 bg-light border rounded",
          shiny::selectInput(ns("selector"), label = NULL, choices = names(dataset_lists))
        )),
        filter_ui
      )
    )

  top_buttons <- shiny::div(
    shiny::bookmarkButton("", class = "navbar-btn"),
    # Remove export functionality until new order
    # shiny::actionButton(ns("open_report_modal"), shiny::span(shiny::icon("download")), class = "navbar-btn"), # nolint
    # shiny::actionButton(ns("open_options_modal"), shiny::span(shiny::icon("question")), class = "navbar-btn"),
    class = "dv_top_button_group"
  )

  dataset_name <-
    shiny::div(
      shiny::tags$span(shiny::icon("info-circle", class = "fa-lg")),
      shiny::textOutput(ns("dataset_name"), container = shiny::tags$span),
      shiny::textOutput(ns("dataset_date"), container = shiny::tags$span),
      class = "grid_page_date"
    )

  sidebar <- shiny::div(
    class = "dv-sidebar-container",
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
    )
  )

  shiny::fluidPage(
    class = "dv_main",
    insert_header_add_resources(app_title = get_config("title")),
    theme = get_app_theme(),
    class = "display-grid",
    sidebar,
    module_info[["ui_fn"]](ns, dataset_name, top_buttons),
  )
}
