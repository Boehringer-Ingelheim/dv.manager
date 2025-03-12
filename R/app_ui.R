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

  ## Feature switch for new data filter

  use_new_filter_switch <- isTRUE(getOption("dv.manager.use.blockly.filter"))
  new_filter_state <- getOption("dv.manager.blockly.predefined.filter")

  ######################################

  data <- get_config("data")
  module_info <- get_config("module_info")
  filter_data <- get_config("filter_data")
  enable_dataset_filter <- get_config("enable_dataset_filter")

  log_inform("Initializing HTML template UI")
  log_inform(glue::glue("Available modules (N): {length(module_info[[\"ui_list\"]])}"))
  log_inform(glue::glue("Dataset options (N): {length(data)}"))

  subject_filter_ui <- create_subject_level_ui(ns("global_filter"))

  dataset_filters_ui <- create_dataset_filters_ui(
    get_dataset_filters_info(data, filter_data),
    ns
  )

  if (use_new_filter_switch) {
    filter_ui <- shiny::div(
      class = "c-well shiny_filter",
      shiny::tags$label(
        "Filter",
        shiny::icon("circle-info", title = TT[["SUBJECT_LEVEL_FILTER"]]),
        class = "text-primary"
      ),
      shiny::div(
        class = "filter-control  filter-filters",
        unnamespaced_filter_modal(list(
          new_filter_ui(ns("filter"), data, state = new_filter_state)[["combined_ui"]]
        )),
      )
    )
  } else {
    filter_ui <- list(
      shiny::div(
        class = "c-well shiny_filter",
        shiny::tags$label(
          "Subject Level Filter",
          shiny::icon("circle-info", title = TT[["SUBJECT_LEVEL_FILTER"]]),
          class = "text-primary"
        ),
        shiny::div(
          class = "filter-control  filter-filters",
          subject_filter_ui
        )
      ),
      if (enable_dataset_filter) {
        shiny::div(
          class = "c-well shiny_filter",
          shiny::tags$label(
            "Dataset Filter(s)",
            shiny::icon("circle-info", title = TT[["DATASET_FILTER"]]),
            class = "text-primary"
          ),
          unnamespaced_filter_modal(new_filter_ui("filter", data))
        )
      }
    )
  }

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
        filter_ui
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

  shiny::fluidPage(
    insert_header_add_resources(app_title = get_config("title")),
    theme = get_app_theme(),
    class = "display-grid",
    sidebar,
    module_info[["ui"]](ns),
    dataset_name
  )
}
