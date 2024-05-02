#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#'
#' @noRd
golem_add_external_resources <- function() {
  bundle_path <- app_sys("app/www")
  golem::add_resource_path("www", bundle_path)

  shiny::tags$head(
    golem::bundle_resources(
      path = bundle_path,
      app_title = golem::get_golem_options("title")
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
