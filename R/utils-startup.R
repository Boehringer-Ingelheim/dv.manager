set_config <- function(app, l) {
  app$appOptions[["dv.manager.config"]] <- l
  app
}

get_config <- function(which = NULL) {
  if (is.null(which)) {
    shiny::getShinyOption("dv.manager.config")
  } else {
    shiny::getShinyOption("dv.manager.config")[[which]]
  }
}

get_raw_config <- function(app) {
  app$appOptions[["dv.manager.config"]]
}

insert_header_add_resources <- function(app_title = NULL, ...) {
  shiny::tags$head(
    title = app_title,
    shinyjs::useShinyjs(),
    add_manager_dependency(),
    add_scoper_dependency(),
    ...
  )
}

add_manager_dependency <- function() {
  htmltools::htmlDependency(
    name = "dv.manager",
    version = utils::packageVersion("dv.manager"),
    src = app_sys("www/js"),
    script = "init.js"
  )
}

add_scoper_dependency <- function() {
  htmltools::htmlDependency(
    name = "scoper",
    version = "0.0.0",
    src = app_sys("www/js"),
    script = "scoper.min.js"
  )
}
