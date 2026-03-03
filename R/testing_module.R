## All the functions in this file are used for testing purposes either in testthat
## or to have quick mock apps to test functionality, they are not in their own file as
## they do not have sufficient entity to be included in the modulegallery or to be exported

empty_UI <- function(id) {
  # nolint
  shiny::tagList()
}

empty_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {}
  )
}

mod_empty <- function(mod_id) {
  list(
    ui = empty_UI,
    server = function(afmm) {
      empty_server(id = mod_id)
    },
    module_id = mod_id
  )
}

########### AFMM export module (for testing)

afmm_export_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textOutput(ns("test_text")),
    shiny::textOutput(ns("test_counter")),
    shiny::textInput(ns("target_id"), label = "Target Module"),
    shiny::actionButton(ns("switch_to_target"), label = "Switch")
  )
}

afmm_export_server <- function(id, afmm) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::setBookmarkExclude(c("target_id", "switch_to_target"))
      filter_counter <- shiny::reactiveVal(0)
      shiny::observeEvent(afmm[["filtered_dataset_list"]](), {
        current_counter <- filter_counter()
        filter_counter(current_counter + 1)
      })

      shiny::observeEvent(input[["switch_to_target"]], {
        afmm[["utils"]][["switch2mod"]](input[["target_id"]])
      })

      output[["test_text"]] <- shiny::renderText("test")
      output[["test_counter"]] <- shiny::renderText(filter_counter())
      shiny::exportTestValues(
        afmm = afmm,
        filter_counter = filter_counter()
      )
      return(shiny::reactive(id))
    }
  )
}

#' A testing module that exports the whole afmm object
#'
#' This module is used for testing purposes to access the full afmm object
#' via shinytest2's get_value(export = "module_id-afmm").
#'
#' @param mod_id shiny module ID
#'
#' @return A module list with ui, server, and module_id
#' @keywords internal
mod_afmm_export <- function(mod_id) {
  list(
    ui = afmm_export_UI,
    server = function(afmm) {
      afmm_export_server(id = mod_id, afmm = afmm)
    },
    module_id = mod_id
  )
}

########### Identity module

identity_UI <- function(id) {
  # nolint
  shiny::h1("")
}

identity_server <- function(id, value) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      return(value)
    }
  )
}

mod_identity <- function(value, from = NULL, mod_id) {
  list(
    ui = identity_UI,
    server = function(afmm) {
      identity_server(
        id = mod_id,
        if (!is.null(from)) {
          shiny::reactive(afmm[[from]]()[[value]])
        } else {
          value
        }
      )
    },
    module_id = mod_id
  )
}

########### Simple module

#' @describeIn mod_simple
#' Module UI
#'
#' @param id shiny id
#'
#' @export
simple_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textOutput(ns("text")),
    shiny::verbatimTextOutput(ns("code"))
  )
}

#' @describeIn mod_simple
#' Module server
#'
#' @param dataset input dataset
#'
#' @export
simple_server <- function(id, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$text <- shiny::renderText(
        {
          r <- dataset()
          while (shiny::is.reactive(r)) {
            r <- r()
          }
          log_inform(paste(nrow(r)))
          nrow(r)
        }
      )
    }
  )
}

#' A simple module that counts the number of rows
#'
#' This simple module is used for demonstration purposes in documentation
#'
#' @param module_id shiny module ID
#' @param from name of the dataset_list
#' @param dataset name of the dataset
#'
#' @export
mod_simple <- function(dataset, from, module_id) {
  mod <- list(
    ui = simple_UI,
    server = function(afmm) {
      simple_server(module_id, shiny::reactive(afmm[[from]]()[[dataset]]))
    },
    module_id = module_id
  )
  mod
}

run_mock_app <- function() {
  run_app(
    data = list(
      "D1" = list(
        adsl = get_pharmaverse_data("adsl"),
        adae = get_pharmaverse_data("adae")
      )
    ),
    module_list = list(
      "Simple" = mod_simple("adsl", "filtered_dataset_list", "mod1"),
      "Simple2" = mod_simple("adsl", "unfiltered_dataset_list", "mod2")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

run_mock_app_two_datasets <- function() {
  run_app(
    data = list(
      "D0" = list(
        adsl = get_pharmaverse_data("adsl"),
        adae = get_pharmaverse_data("adae")
      ),
      "D2" = list(
        adsl = get_pharmaverse_data("adsl"),
        adae = get_pharmaverse_data("adae"),
        adlb = get_pharmaverse_data("adlb")
      )
    ),
    module_list = list(
      "Simple" = mod_simple("adsl", "filtered_dataset_list", "mod1"),
      "Simple2" = mod_simple("adsl", "unfiltered_dataset_list", "mod2"),
      "Simple3" = mod_simple("adae", "filtered_dataset_list", "mod3")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

###### Module communication testing
com_test_UI <- function(id, choices = c(1, 2, 3), message) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      ns("select"),
      label = "Select a number",
      choices = choices
    ),
    shiny::p(message),
    shiny::textOutput(ns("output"))
  )
}

com_test_server <- function(id, value) {
  module <- function(input, output, session) {
    output$output <- shiny::renderText({
      shiny::req(value())
    })

    return(shiny::reactive(input$select))
  }

  return(
    shiny::moduleServer(
      id,
      module
    )
  )
}

mod_com_test <- function(choices, message, module_from_id, mod_id) {
  mod <- list(
    ui = function(id) {
      com_test_UI(id, choices, message)
    },
    server = function(afmm) {
      com_test_server(
        id = mod_id,
        value = afmm[["module_output"]]()[[module_from_id]]
      )
    },
    module_id = mod_id
  )
  mod
}

run_mock_com_app <- function() {
  run_app(
    data = list("D1" = list(adsl = get_pharmaverse_data("adsl"))),
    module_list = list(
      "Send and Receive 1" = mod_com_test(
        choices = 1:3,
        message = "The other module has selected",
        module_from_id = "mod_2",
        mod_id = "mod_1"
      ),
      "Send and Receive 2" = mod_com_test(
        choices = c("a", "b", "c"),
        message = "The other module has selected",
        module_from_id = "mod_1",
        mod_id = "mod_2"
      ),
      "Simple" = mod_simple("adsl", "unfiltered_dataset_list", "modSimp")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}


####### Mixing communication and url
table_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("table"))
  )
}

table_server <- function(id, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$table <- DT::renderDT(
        {
          dataset()
        },
        selection = "single"
      )
      returned_rows <- shiny::reactive({
        shiny::req(!is.null(input$table_rows_selected))
        dataset()[[input$table_rows_selected, "USUBJID"]]
      })
      return(returned_rows)
    }
  )
}

mod_table <- function(dataset, from, mod_id) {
  mod <- list(
    ui = function(id) {
      table_UI(id)
    },
    server = function(afmm) {
      table_server(
        id = mod_id,
        dataset = shiny::reactive(afmm[[from]]()[[dataset]])
      )
    },
    module_id = mod_id
  )
  mod
}

run_mock_combined_app <- function() {
  run_app(
    data = list(
      "D1" = list(
        adsl = get_pharmaverse_data("adsl"),
        adae = get_pharmaverse_data("adae")
      ),
      "D2" = list(
        adsl = get_pharmaverse_data("adsl"),
        adae = get_pharmaverse_data("adae")
      )
    ),
    module_list = list(
      "AE Table" = mod_table(
        mod_id = "mod_1",
        from = "filtered_dataset_list",
        dataset = "adae"
      )
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

########### Accessing dataset name

dataset_name_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textOutput(ns("text"))
  )
}

dataset_name_server <- function(id, dataset_name) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$text <- shiny::renderText({
        dataset_name()
      })
    }
  )
}

mod_dataset_name <- function(module_id) {
  mod <- list(
    ui = dataset_name_UI,
    server = function(afmm) {
      dataset_name_server(module_id, afmm[["dataset_metadata"]][["name"]])
    },
    module_id = module_id
  )
  mod
}

run_mock_dataset_name_app <- function() {
  run_app(
    data = list(
      "D1" = list(adsl = get_pharmaverse_data("adsl")),
      "D2" = list(adsl = get_pharmaverse_data("adsl"))
    ),
    module_list = list(
      "Dataset Name" = mod_dataset_name("mod1")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}


############# Switch tab

########### Switch module

#' @describeIn mod_switch
#' Module UI
#'
#' @param name a title to display inside the module
#' @param id shiny id
#'
#'
#' @keywords internal

switch_UI <- function(id, name) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1(name),
    shiny::actionButton(ns("switch"), "Switch tab")
  )
}

#' @describeIn mod_switch
#' Module server
#'
#' @param id the shiny module id+
#' @param selected The name of the tab that we want to switch to
#' @param switch_func a function that when passed the name of a tab switches the focus to it
#'

switch_server <- function(id, selected, switch_func) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observeEvent(input$switch, {
        switch_func(selected)
      })
    }
  )
}

#' A module that allows switching to another module
#'
#' This simple module is used for demonstration purposes in documentation
#'
#' @param name A title that will be shown inside the module
#' @param selected The name of the tab that we want to switch to
#' @param module_id shiny module ID
#'

mod_switch <- function(name, selected, module_id) {
  mod <- list(
    ui = function(module_id) {
      switch_UI(module_id, name)
    },
    server = function(afmm) {
      switch_server(module_id, selected, afmm[["utils"]][["switch2mod"]])
    },
    module_id = module_id
  )
  mod
}

run_mock_switch_app <- function() {
  run_app(
    data = list("D1" = list(adsl = get_pharmaverse_data("adsl"))),
    module_list = list(
      "Mod 1" = mod_switch("Mod 1", "mod2", "mod1"),
      "Mod 2" = mod_switch("Mod 2", "mod1", "mod2")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

##### Module testing afmm

printer_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::verbatimTextOutput(ns("printer"))
}

printer_server <- function(id, value) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$printer <- shiny::renderText(value)
    }
  )
}

mod_print_afmm <- function(mod_id) {
  list(
    ui = printer_UI,
    server = function(afmm) {
      printer_server(
        id = mod_id,
        paste(utils::capture.output(utils::str(afmm)), collapser = "\n")
      )
    },
    module_id = mod_id
  )
}

run_mock_print_afmm <- function() {
  run_app(
    data = list(),
    module_list = list(
      "Send and Receive 1" = mod_com_test(
        choices = 1:3,
        message = "The other module has selected",
        module_from_id = "mod_2",
        mod_id = "mod_1"
      ),
      "Send and Receive 2" = mod_com_test(
        choices = c("a", "b", "c"),
        message = "The other module has selected",
        module_from_id = "mod_1",
        mod_id = "mod_2"
      ),
      "Simple" = mod_simple("adsl", "unfiltered_dataset_list", "modSimp"),
      "afmm" = mod_print_afmm("mod_afmm")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

#### MESSAGE WITH IMPLICIT FUNCTION

msg_impl_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(ns("msg"), label = NULL),
    shiny::actionButton(ns("send_msg"), label = "Send Inform/Message"),
    shiny::actionButton(ns("send_warn"), label = "Send Warning"),
    shiny::actionButton(ns("send_abort"), label = "Send Error/Abort"),
    shiny::actionButton(ns("send_not"), label = "Send Notification")
  )
}

msg_impl_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observeEvent(
        input[["send_msg"]],
        log_inform(
          message = paste("inform", input[["msg"]]),
          log_func = function() message("there goes a function")
        )
      )
      shiny::observeEvent(
        input[["send_warn"]],
        log_warn(paste("warn", input[["msg"]]))
      )
      shiny::observeEvent(
        input[["send_abort"]],
        rlang::abort(paste("error", input[["msg"]]))
      )
      shiny::observeEvent(
        input[["send_not"]],
        shiny::showNotification(input[["msg"]])
      )
    }
  )
}

mod_impl_msg <- function(mod_id) {
  list(
    ui = msg_impl_UI,
    server = function(afmm) {
      msg_impl_server(id = mod_id)
    },
    module_id = mod_id
  )
}

run_mock_app_impl_msg <- function() {
  run_app(
    data = list(),
    module_list = list(
      "msg" = mod_impl_msg("mod1")
    ),
    filter_data = "",
    filter_key = "USUBJID"
  )
}

## Show message

run_mock_startup_msg <- function() {
  run_app(
    data = list(),
    module_list = list(),
    startup_msg = shiny::modalDialog("Sample startup message"),
    filter_data = "",
    filter_key = "USUBJID"
  )
}

## Module printer

########### Accessing dataset name

mod_dataset_name_date_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textOutput(ns("text"))
  )
}

mod_dataset_name_date_server <- function(id, val) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$text <- shiny::renderText({
        paste(
          "dataset_name:",
          val[[1]](),
          "; dataset_date_range:",
          val[[2]]()[[1]],
          val[[2]]()[[2]],
          "; module_name:",
          paste(val[[3]], collapse = ",")
        )
      })
    }
  )
}

mod_dataset_name_date <- function(module_id) {
  mod <- list(
    ui = mod_dataset_name_date_UI,
    server = function(afmm) {
      mod_dataset_name_date_server(
        module_id,
        list(
          afmm[["dataset_metadata"]][["name"]],
          afmm[["dataset_metadata"]][["date_range"]],
          afmm[["module_names"]]
        )
      )
    },
    module_id = module_id
  )
  mod
}

printer_app <- function() {
  run_app(
    data = list(
      "D1" = list(adsl = get_pharmaverse_data("adsl")),
      "D2" = list(adsl = get_pharmaverse_data("adsl"))
    ),
    module_list = list(
      "Dataset Name" = mod_dataset_name_date("mod1")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}


########### Simple module with and without css
# Both button have the same class mybutton
# test.css contains a rule for the mybutton class
# Because the dependency is inside a module and starts with custom- it should only apply to css module

button_UI_css <- function(id) {
  # nolint
  ns <- shiny::NS(id)

  dep <- htmltools::htmlDependency(
    name = "custom-style",
    version = 0,
    src = "test_app/www/css",
    stylesheet = "test.css",
    package = "dv.manager"
  )

  shiny::tagList(
    shiny::actionButton(ns("button"), "I should be red", class = "mybutton"),
    dep
  )
}

button_UI_no_css <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("button"), "I should be black", class = "mybutton")
  )
}

empty_server <- function(id) {
  mod <- function(input, output, session) {}
  shiny::moduleServer(id, mod)
}

mod_button_css <- function(module_id) {
  mod <- list(
    ui = button_UI_css,
    server = function(afmm) {
      empty_server(module_id)
    },
    module_id = module_id
  )
  mod
}

mod_button_no_css <- function(module_id) {
  mod <- list(
    ui = button_UI_no_css,
    server = function(afmm) {
      empty_server(module_id)
    },
    module_id = module_id
  )
  mod
}

run_mock_app_css <- function() {
  run_app(
    data = list(),
    module_list = list(
      "no css" = mod_button_no_css("mod1"),
      "css" = mod_button_css("mod2")
    ),
    filter_data = "",
    filter_key = ""
  )
}

#' A simple module that counts the number of rows
#'
#' This simple module is used for demonstration purposes in documentation
#'
#' It is similar to mod_simple but does not use dispatchers
#'
#' @param module_id shiny module ID
#'
#' @keywords internal
#'
#' @export
mod_simple2 <- function(dataset_name, module_id) {
  mod <- list(
    ui = simple_UI,
    server = function(afmm) {
      simple_server(
        module_id,
        shiny::reactive(afmm[["filtered_dataset_list"]]()[[dataset_name]])
      )
    },
    module_id = module_id,
    meta = list(dataset_info = list(all = dataset_name))
  )
  mod
}


#' A simple module that lists the column labels of all used datasets
#'
#' This simple module is used for demonstration purposes in documentation
#'
#'
#'
#' @param module_id shiny module ID
#'
#' @keywords internal
mod_dataset_labels <- function(dataset_names, module_id) {
  mod <- list(
    ui = dataset_labels_UI,
    server = function(afmm) {
      dataset_labels_server(
        module_id,
        shiny::reactive(afmm[["filtered_dataset_list"]]()[dataset_names])
      )
    },
    module_id = module_id,
    meta = list(dataset_info = list(all = dataset_names))
  )
  mod
}

dataset_labels_UI <- function(id) {
  # nolintr
  ns <- shiny::NS(id)
  list(
    shiny::h1("labels"),
    shiny::uiOutput(ns("labels"))
  )
}

dataset_labels_server <- function(id, data) {
  mod <- function(input, output, session) {
    output[["labels"]] <- shiny::renderUI({
      ds_li <- list()
      nm_ds <- names(data())
      for (ds_idx in seq_along(data())) {
        ds <- data()[[ds_idx]]
        nm_col <- names(ds)
        label_li <- list()
        for (col_idx in seq_along(ds)) {
          label_li[[col_idx]] <- shiny::tags[["li"]](shiny::p(
            nm_col[[col_idx]],
            ": ",
            attr(ds[[col_idx]], "label")
          ))
        }
        ds_li[[(ds_idx * 2) - 1]] <- shiny::tags[["li"]](nm_ds[[ds_idx]])
        ds_li[[(ds_idx * 2)]] <- do.call(shiny::tags[["ul"]], label_li)
      }
      do.call(shiny::tags[["ul"]], ds_li)
    })

    shiny::exportTestValues(
      data = data()
    )

    NULL
  }
  shiny::moduleServer(id, mod)
}

run_mock_app_labels <- function(data) {
  if (missing(data)) {
    add_dummy_labels <- function(ds) {
      nm_cols <- names(ds)
      for (col_idx in seq_along(ds)) {
        attr(ds[[col_idx]], "label") <- paste("Label of", nm_cols[[col_idx]])
      }
      ds
    }
    data <- list(
      "D1" = list(
        mtcars = add_dummy_labels(datasets::mtcars),
        mtcars2 = add_dummy_labels(datasets::mtcars)
      )
    )
  }

  run_app(
    data = data,
    module_list = list(
      "Labels" = mod_dataset_labels(names(data[[1]]), "mod1")
    ),
    filter_data = names(data[[1]])[[1]],
    filter_key = names(data[[1]][[1]])[[1]]
  )
}

########### Multi Simple module

#' @describeIn mod_multi_simple
#' Module UI
#'
#' @param id shiny id
#'
#' @export
#' @keywords internal
multi_simple_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("out"))
}

#' @describeIn mod_multi_simple
#' Module server
#'
#' @param dataset input dataset
#'
#' @export
multi_simple_server <- function(id, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output[["out"]] <- shiny::renderUI({
        ui <- list()
        r_dataset <- dataset()
        for (idx in seq_along(r_dataset)) {
          nm <- names(r_dataset)[[idx]]
          nr <- nrow(r_dataset[[idx]])
          ui[[idx]] <- shiny::p(paste0(nm, ": ", nr))
        }
        ui
      })
    }
  )
}

#' A simple module that counts the number of rows for all datasets loaded in the application
#'
#' This simple module is used for demonstration purposes in documentation, testing and developing
#'
#' @param module_id shiny module ID
#'
#' @export
mod_multi_simple <- function(module_id) {
  mod <- list(
    ui = multi_simple_UI,
    server = function(afmm) {
      multi_simple_server(module_id, afmm[["filtered_dataset_list"]])
    },
    module_id = module_id
  )
  mod
}


mod_CSS_test <- function(module_id) {
  mod <- list(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::tagList(
        # Bootstrap Icons CDN (CSS only - no JS needed)
        shiny::tags$head(
          shiny::tags$link(
            rel = "stylesheet",
            href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.11.3/font/bootstrap-icons.min.css"
          )
        ),

        shiny::div(
          class = "container-fluid",
          shiny::div(
            class = "row",

            # MAIN --------------------------------------------
            shiny::tags$main(
              class = "col-md-10 ms-auto py-4 px-4",
              style = "margin-left:200px;",

              shiny::tags$h1(class = "mb-1", "Bootstrap 5 - Shiny Reference"),
              shiny::tags$p(
                class = "text-secondary mb-5",
                "All BS5 components as a Shiny module with explicit package prefixes."
              ),

              # -- BUTTONS ---------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Buttons"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  shiny::tags$button(class = "btn btn-primary", "Primary"),
                  shiny::tags$button(class = "btn btn-secondary", "Secondary"),
                  shiny::tags$button(class = "btn btn-success", "Success"),
                  shiny::tags$button(class = "btn btn-danger", "Danger"),
                  shiny::tags$button(class = "btn btn-warning", "Warning"),
                  shiny::tags$button(class = "btn btn-info", "Info"),
                  shiny::tags$button(class = "btn btn-light", "Light"),
                  shiny::tags$button(class = "btn btn-dark", "Dark"),
                  shiny::tags$button(class = "btn btn-link", "Link"),
                  shiny::tags$button(class = "btn btn-default", "Default"),
                ),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  shiny::tags$button(class = "btn btn-outline-primary", "Outline Primary"),
                  shiny::tags$button(class = "btn btn-outline-secondary", "Outline Secondary"),
                  shiny::tags$button(class = "btn btn-outline-success", "Outline Success"),
                  shiny::tags$button(class = "btn btn-outline-danger", "Outline Danger"),
                  shiny::tags$button(class = "btn btn-outline-warning", "Outline Warning"),
                  shiny::tags$button(class = "btn btn-outline-info", "Outline Info"),
                  shiny::tags$button(class = "btn btn-outline-light", "Outline Light"),
                  shiny::tags$button(class = "btn btn-outline-dark", "Outline Dark"),
                  shiny::tags$button(class = "btn btn-outline-default", "Outline Default")
                ),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  shiny::tags$button(class = "btn btn-primary btn-lg", "Large"),
                  shiny::tags$button(class = "btn btn-primary", "Default"),
                  shiny::tags$button(class = "btn btn-primary btn-sm", "Small"),
                  shiny::tags$button(class = "btn btn-primary", disabled = NA, "Disabled"),
                  shiny::tags$button(class = "btn btn-primary active", "Active"),
                  shiny::tags$button(class = "btn btn-outline-primary rounded-pill", "Pill"),
                  shiny::tags$button(class = "btn btn-outline-primary rounded-0", "Square"),
                  shiny::tags$button(
                    class = "btn btn-success",
                    shiny::tags$i(class = "bi bi-check-circle me-1"),
                    "With Icon"
                  ),
                  shiny::tags$button(
                    class = "btn btn-primary position-relative",
                    "Alerts",
                    shiny::tags$span(
                      class = "position-absolute top-0 start-100 translate-middle badge rounded-pill bg-danger",
                      "9+"
                    )
                  )
                ),
                shiny::div(
                  class = "d-flex flex-wrap gap-3 align-items-start",
                  shiny::div(
                    class = "btn-group",
                    shiny::tags$button(class = "btn btn-outline-primary", "Left"),
                    shiny::tags$button(class = "btn btn-outline-primary", "Middle"),
                    shiny::tags$button(class = "btn btn-outline-primary", "Right")
                  ),
                  shiny::div(
                    class = "btn-group-vertical",
                    shiny::tags$button(class = "btn btn-outline-secondary", "Top"),
                    shiny::tags$button(class = "btn btn-outline-secondary", "Mid"),
                    shiny::tags$button(class = "btn btn-outline-secondary", "Bot")
                  ),
                  shiny::div(
                    class = "btn-group",
                    shiny::tags$button(class = "btn btn-danger", "Danger Dropdown"),
                    shiny::tags$button(
                      class = "btn btn-danger dropdown-toggle dropdown-toggle-split",
                      `data-bs-toggle` = "dropdown"
                    ),
                    shiny::tags$ul(
                      class = "dropdown-menu",
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Option 1")),
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Option 2"))
                    )
                  )
                )
              ),

              # -- ALERTS ----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Alerts"),
                shiny::div(class = "alert alert-primary", shiny::tags$strong("Primary"), " alert."),
                shiny::div(
                  class = "alert alert-success",
                  shiny::tags$i(class = "bi bi-check-circle me-2"),
                  shiny::tags$strong("Success!"),
                  " Your data was saved."
                ),
                shiny::div(
                  class = "alert alert-danger",
                  shiny::tags$i(class = "bi bi-exclamation-triangle me-2"),
                  shiny::tags$strong("Error!"),
                  " Something went wrong."
                ),
                shiny::div(
                  class = "alert alert-warning",
                  shiny::tags$i(class = "bi bi-exclamation-circle me-2"),
                  "Warning."
                ),
                shiny::div(
                  class = "alert alert-info",
                  shiny::tags$i(class = "bi bi-info-circle me-2"),
                  "Informational."
                ),
                shiny::div(
                  class = "alert alert-success alert-dismissible fade show",
                  shiny::tags$strong("Dismissible!"),
                  " Click x to close.",
                  shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "alert")
                ),
                shiny::div(
                  class = "alert alert-primary",
                  shiny::tags$h5(class = "alert-heading", "Alert with heading"),
                  shiny::tags$p(
                    class = "mb-0",
                    "Extra detail. ",
                    shiny::tags$a(href = "#", class = "alert-link", "Alert link")
                  )
                )
              ),

              # -- BADGES ----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Badges & text-bg-*"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 align-items-center mb-3",
                  shiny::tags$span(class = "badge bg-primary", "bg-primary"),
                  shiny::tags$span(class = "badge bg-secondary", "bg-secondary"),
                  shiny::tags$span(class = "badge bg-success", "bg-success"),
                  shiny::tags$span(class = "badge bg-danger", "bg-danger"),
                  shiny::tags$span(class = "badge bg-warning text-dark", "bg-warning"),
                  shiny::tags$span(class = "badge bg-info text-dark", "bg-info"),
                  shiny::tags$span(class = "badge rounded-pill bg-primary", "Pill"),
                  shiny::tags$span(class = "badge rounded-pill bg-danger", "99+"),
                  shiny::tags$h5(class = "mb-0", "Heading ", shiny::tags$span(class = "badge bg-secondary", "New")),
                  shiny::tags$button(
                    class = "btn btn-primary",
                    "Messages ",
                    shiny::tags$span(class = "badge bg-light text-dark ms-1", "4")
                  )
                ),
                shiny::tags$p(
                  class = "text-secondary small",
                  "text-bg-{variant} - auto contrast (BS 5.2+), preferred for components:"
                ),
                shiny::div(
                  class = "d-flex flex-wrap gap-2",
                  shiny::tags$span(class = "badge text-bg-primary", "text-bg-primary"),
                  shiny::tags$span(class = "badge text-bg-secondary", "text-bg-secondary"),
                  shiny::tags$span(class = "badge text-bg-success", "text-bg-success"),
                  shiny::tags$span(class = "badge text-bg-danger", "text-bg-danger"),
                  shiny::tags$span(class = "badge text-bg-warning", "text-bg-warning"),
                  shiny::tags$span(class = "badge text-bg-info", "text-bg-info"),
                  shiny::tags$span(class = "badge text-bg-light", "text-bg-light"),
                  shiny::tags$span(class = "badge text-bg-dark", "text-bg-dark")
                )
              ),

              # -- MODALS ----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Modals / Dialogs"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2",

                  shiny::tags$button(
                    class = "btn btn-primary",
                    `data-bs-toggle` = "modal",
                    `data-bs-target` = "#m1",
                    "Default"
                  ),
                  shiny::div(
                    class = "modal fade",
                    id = "m1",
                    tabindex = "-1",
                    shiny::div(
                      class = "modal-dialog",
                      shiny::div(
                        class = "modal-content",
                        shiny::div(
                          class = "modal-header",
                          shiny::tags$h5(class = "modal-title", "Default"),
                          shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "modal")
                        ),
                        shiny::div(class = "modal-body", shiny::tags$code("modal-dialog")),
                        shiny::div(
                          class = "modal-footer",
                          shiny::tags$button(class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Close"),
                          shiny::tags$button(class = "btn btn-primary", "Save")
                        )
                      )
                    )
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-primary",
                    `data-bs-toggle` = "modal",
                    `data-bs-target` = "#m2",
                    "Small"
                  ),
                  shiny::div(
                    class = "modal fade",
                    id = "m2",
                    tabindex = "-1",
                    shiny::div(
                      class = "modal-dialog modal-sm",
                      shiny::div(
                        class = "modal-content",
                        shiny::div(
                          class = "modal-header",
                          shiny::tags$h5(class = "modal-title", "Small"),
                          shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "modal")
                        ),
                        shiny::div(class = "modal-body", shiny::tags$code("modal-sm")),
                        shiny::div(
                          class = "modal-footer",
                          shiny::tags$button(class = "btn btn-secondary", `data-bs-dismiss` = "modal", "OK")
                        )
                      )
                    )
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-primary",
                    `data-bs-toggle` = "modal",
                    `data-bs-target` = "#m3",
                    "Large"
                  ),
                  shiny::div(
                    class = "modal fade",
                    id = "m3",
                    tabindex = "-1",
                    shiny::div(
                      class = "modal-dialog modal-lg",
                      shiny::div(
                        class = "modal-content",
                        shiny::div(
                          class = "modal-header",
                          shiny::tags$h5(class = "modal-title", "Large"),
                          shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "modal")
                        ),
                        shiny::div(class = "modal-body", shiny::tags$code("modal-lg")),
                        shiny::div(
                          class = "modal-footer",
                          shiny::tags$button(class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Close")
                        )
                      )
                    )
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-secondary",
                    `data-bs-toggle` = "modal",
                    `data-bs-target` = "#m4",
                    "Fullscreen"
                  ),
                  shiny::div(
                    class = "modal fade",
                    id = "m4",
                    tabindex = "-1",
                    shiny::div(
                      class = "modal-dialog modal-fullscreen",
                      shiny::div(
                        class = "modal-content",
                        shiny::div(
                          class = "modal-header",
                          shiny::tags$h5(class = "modal-title", "Fullscreen"),
                          shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "modal")
                        ),
                        shiny::div(
                          class = "modal-body",
                          shiny::tags$code("modal-fullscreen"),
                          shiny::tags$p(
                            "Also: modal-sm * modal-lg * modal-xl * modal-fullscreen-md-down"
                          )
                        ),
                        shiny::div(
                          class = "modal-footer",
                          shiny::tags$button(class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Close")
                        )
                      )
                    )
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-info",
                    `data-bs-toggle` = "modal",
                    `data-bs-target` = "#m5",
                    "Centered + Scrollable"
                  ),
                  shiny::div(
                    class = "modal fade",
                    id = "m5",
                    tabindex = "-1",
                    shiny::div(
                      class = "modal-dialog modal-dialog-centered modal-dialog-scrollable",
                      shiny::div(
                        class = "modal-content",
                        shiny::div(
                          class = "modal-header",
                          shiny::tags$h5(class = "modal-title", "Centered + Scrollable"),
                          shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "modal")
                        ),
                        shiny::div(
                          class = "modal-body",
                          shiny::tags$code("modal-dialog-centered modal-dialog-scrollable"),
                          lapply(1:8, function(i) {
                            shiny::tags$p(paste("Long content line", i, "..."))
                          })
                        ),
                        shiny::div(
                          class = "modal-footer",
                          shiny::tags$button(class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Close")
                        )
                      )
                    )
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-warning",
                    `data-bs-toggle` = "modal",
                    `data-bs-target` = "#m6",
                    "Static Backdrop"
                  ),
                  shiny::div(
                    class = "modal fade",
                    id = "m6",
                    `data-bs-backdrop` = "static",
                    tabindex = "-1",
                    shiny::div(
                      class = "modal-dialog",
                      shiny::div(
                        class = "modal-content",
                        shiny::div(
                          class = "modal-header",
                          shiny::tags$h5(class = "modal-title", "Static Backdrop"),
                          shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "modal")
                        ),
                        shiny::div(
                          class = "modal-body",
                          "Clicking outside does nothing. ",
                          shiny::tags$code('data-bs-backdrop="static"')
                        ),
                        shiny::div(
                          class = "modal-footer",
                          shiny::tags$button(class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Close")
                        )
                      )
                    )
                  )
                )
              ),

              # -- TOASTS ----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Toasts"),
                # Bootstrap JS is already loaded by Shiny - bootstrap object available globally
                shiny::tags$button(
                  class = "btn btn-primary mb-3",
                  onclick = "new bootstrap.Toast(document.getElementById('liveToast'),{delay:3000}).show();",
                  "Trigger Live Toast"
                ),
                shiny::div(
                  class = "toast-container position-static d-flex flex-column gap-2",
                  shiny::div(
                    class = "toast show",
                    shiny::div(
                      class = "toast-header",
                      shiny::tags$strong(class = "me-auto", "Notification"),
                      shiny::tags$small("just now"),
                      shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "toast")
                    ),
                    shiny::div(class = "toast-body", "Default static toast.")
                  ),
                  shiny::div(
                    class = "toast show text-bg-success",
                    shiny::div(
                      class = "toast-header text-bg-success",
                      shiny::tags$strong(class = "me-auto", "Success"),
                      shiny::tags$small("2s ago"),
                      shiny::tags$button(class = "btn-close btn-close-white", `data-bs-dismiss` = "toast")
                    ),
                    shiny::div(class = "toast-body", "Colored with ", shiny::tags$code("text-bg-success"), ".")
                  )
                ),
                shiny::div(
                  class = "position-fixed top-0 end-0 p-3",
                  style = "z-index:9999;",
                  shiny::div(
                    id = "liveToast",
                    class = "toast text-bg-primary",
                    shiny::div(
                      class = "toast-header text-bg-primary",
                      shiny::tags$strong(class = "me-auto", "Live Toast"),
                      shiny::tags$button(class = "btn-close btn-close-white", `data-bs-dismiss` = "toast")
                    ),
                    shiny::div(class = "toast-body", "Triggered dynamically! Auto-hides in 3s.")
                  )
                )
              ),

              # -- CARDS -----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Cards"),
                shiny::div(
                  class = "row g-3 mb-3",
                  shiny::div(
                    class = "col-md-4",
                    shiny::div(
                      class = "card h-100",
                      shiny::div(class = "card-header", "Card Header"),
                      shiny::div(
                        class = "card-body",
                        shiny::tags$h5(class = "card-title", "Card Title"),
                        shiny::tags$h6(class = "card-subtitle mb-2 text-muted", "Subtitle"),
                        shiny::tags$p(class = "card-text", "Standard card with header and footer."),
                        shiny::tags$a(href = "#", class = "btn btn-primary btn-sm", "Action")
                      ),
                      shiny::div(class = "card-footer text-muted small", "Updated 3m ago")
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::div(
                      class = "card text-bg-primary h-100",
                      shiny::div(
                        class = "card-body text-center",
                        shiny::tags$i(class = "bi bi-bar-chart-fill fs-1"),
                        shiny::tags$h5(class = "card-title mt-2", "Colored Card"),
                        shiny::tags$p(class = "card-text", shiny::tags$code("text-bg-primary on .card"))
                      )
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::div(
                      class = "card border-success h-100",
                      shiny::div(
                        class = "card-body",
                        shiny::tags$h5(class = "card-title text-success", "Border Card"),
                        shiny::tags$p(class = "card-text", shiny::tags$code("border-success")),
                        shiny::plotOutput(ns("cardPlot"), height = "100px")
                      )
                    )
                  )
                ),
                shiny::div(
                  class = "card-group",
                  shiny::div(
                    class = "card",
                    shiny::div(
                      class = "card-body",
                      shiny::tags$h6(class = "card-title", "Group 1"),
                      shiny::tags$p(class = "card-text small", "card-group - equal height")
                    )
                  ),
                  shiny::div(
                    class = "card",
                    shiny::div(
                      class = "card-body",
                      shiny::tags$h6(class = "card-title", "Group 2"),
                      shiny::tags$p(class = "card-text small", "Shared border")
                    )
                  ),
                  shiny::div(
                    class = "card",
                    shiny::div(
                      class = "card-body",
                      shiny::tags$h6(class = "card-title", "Group 3"),
                      shiny::tags$p(class = "card-text small", "Flexbox-based")
                    )
                  )
                )
              ),

              # -- FORMS -----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Form Controls"),
                shiny::div(
                  class = "row g-3",
                  shiny::div(
                    class = "col-md-6",
                    shiny::tags$label(class = "form-label", "Text input"),
                    shiny::tags$input(type = "text", class = "form-control", placeholder = "Default form-control")
                  ),
                  shiny::div(
                    class = "col-md-6",
                    shiny::tags$label(class = "form-label", "Floating label"),
                    shiny::div(
                      class = "form-floating",
                      shiny::tags$input(type = "email", class = "form-control", id = "floatEmail", placeholder = "x"),
                      shiny::tags$label(`for` = "floatEmail", "Email address")
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Large"),
                    shiny::tags$input(
                      type = "text",
                      class = "form-control form-control-lg",
                      placeholder = "form-control-lg"
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Small"),
                    shiny::tags$input(
                      type = "text",
                      class = "form-control form-control-sm",
                      placeholder = "form-control-sm"
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Disabled"),
                    shiny::tags$input(type = "text", class = "form-control", placeholder = "Disabled", disabled = NA)
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Valid"),
                    shiny::tags$input(type = "text", class = "form-control is-valid", value = "Correct"),
                    shiny::div(class = "valid-feedback", "Looks good!")
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Invalid"),
                    shiny::tags$input(type = "text", class = "form-control is-invalid", value = "Bad"),
                    shiny::div(class = "invalid-feedback", "Please fix this.")
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Read-only"),
                    shiny::tags$input(type = "text", class = "form-control", value = "Read only", readonly = NA)
                  ),
                  shiny::div(
                    class = "col-12",
                    shiny::tags$label(class = "form-label", "Textarea"),
                    shiny::tags$textarea(class = "form-control", rows = "3", placeholder = "Textarea...")
                  ),
                  shiny::div(
                    class = "col-md-6",
                    shiny::tags$label(class = "form-label", "File"),
                    shiny::tags$input(type = "file", class = "form-control")
                  ),
                  shiny::div(
                    class = "col-md-3",
                    shiny::tags$label(class = "form-label", "Color"),
                    shiny::tags$input(type = "color", class = "form-control form-control-color", value = "#0d6efd")
                  ),
                  shiny::div(
                    class = "col-md-3",
                    shiny::tags$label(class = "form-label", "Range"),
                    shiny::tags$input(type = "range", class = "form-range", min = "0", max = "100", value = "50")
                  )
                )
              ),

              # -- INPUT GROUPS ----------------------------------
              shiny::tags[["div"]](
                shiny::h1("Input Groups"),
                shiny::div(
                  class = "d-flex flex-column gap-2",
                  shiny::div(
                    class = "input-group",
                    shiny::tags$span(class = "input-group-text", "@"),
                    shiny::tags$input(type = "text", class = "form-control", placeholder = "Username")
                  ),
                  shiny::div(
                    class = "input-group",
                    shiny::tags$input(type = "text", class = "form-control", placeholder = "Search..."),
                    shiny::tags$button(class = "btn btn-primary", shiny::tags$i(class = "bi bi-search"))
                  ),
                  shiny::div(
                    class = "input-group",
                    shiny::tags$span(class = "input-group-text", "$"),
                    shiny::tags$input(type = "number", class = "form-control", placeholder = "0"),
                    shiny::tags$span(class = "input-group-text", ".00")
                  ),
                  shiny::div(
                    class = "input-group input-group-lg",
                    shiny::tags$span(class = "input-group-text", "Large"),
                    shiny::tags$input(type = "text", class = "form-control")
                  ),
                  shiny::div(
                    class = "input-group input-group-sm",
                    shiny::tags$span(class = "input-group-text", "Small"),
                    shiny::tags$input(type = "text", class = "form-control")
                  )
                )
              ),

              # -- SELECTS ---------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Select Inputs"),
                shiny::div(
                  class = "row g-3",
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Default"),
                    shiny::tags$select(
                      class = "form-select",
                      shiny::tags$option("Option 1"),
                      shiny::tags$option("Option 2"),
                      shiny::tags$option("Option 3")
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Large"),
                    shiny::tags$select(class = "form-select form-select-lg", shiny::tags$option("Large"))
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$label(class = "form-label", "Small"),
                    shiny::tags$select(class = "form-select form-select-sm", shiny::tags$option("Small"))
                  ),
                  shiny::div(
                    class = "col-md-6",
                    shiny::tags$label(class = "form-label", "Multiple"),
                    shiny::tags$select(
                      class = "form-select",
                      multiple = NA,
                      size = "4",
                      shiny::tags$option("Alpha"),
                      shiny::tags$option("Beta"),
                      shiny::tags$option("Gamma"),
                      shiny::tags$option("Delta")
                    )
                  ),
                  shiny::div(
                    class = "col-md-6",
                    shiny::tags$label(class = "form-label", "Floating label select"),
                    shiny::div(
                      class = "form-floating",
                      shiny::tags$select(
                        class = "form-select",
                        id = "floatSel",
                        shiny::tags$option("Option A"),
                        shiny::tags$option("Option B")
                      ),
                      shiny::tags$label(`for` = "floatSel", "Category")
                    )
                  )
                )
              ),

              # -- CHECKS & RADIOS -------------------------------
              shiny::tags[["div"]](
                shiny::h1("Checkboxes & Radios"),
                shiny::div(
                  class = "row g-4",
                  shiny::div(
                    class = "col-md-3",
                    shiny::tags$p(class = "text-secondary small mb-2", "Checkboxes"),
                    shiny::div(
                      class = "form-check",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "c1", checked = NA),
                      shiny::tags$label(class = "form-check-label", `for` = "c1", "Checked")
                    ),
                    shiny::div(
                      class = "form-check",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "c2"),
                      shiny::tags$label(class = "form-check-label", `for` = "c2", "Unchecked")
                    ),
                    shiny::div(
                      class = "form-check",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "c3", disabled = NA),
                      shiny::tags$label(class = "form-check-label", `for` = "c3", "Disabled")
                    )
                  ),
                  shiny::div(
                    class = "col-md-3",
                    shiny::tags$p(class = "text-secondary small mb-2", "Radios"),
                    shiny::div(
                      class = "form-check",
                      shiny::tags$input(
                        class = "form-check-input",
                        type = "radio",
                        name = "r",
                        id = "r1",
                        checked = NA
                      ),
                      shiny::tags$label(class = "form-check-label", `for` = "r1", "Option A")
                    ),
                    shiny::div(
                      class = "form-check",
                      shiny::tags$input(class = "form-check-input", type = "radio", name = "r", id = "r2"),
                      shiny::tags$label(class = "form-check-label", `for` = "r2", "Option B")
                    ),
                    shiny::div(
                      class = "form-check",
                      shiny::tags$input(
                        class = "form-check-input",
                        type = "radio",
                        name = "r",
                        id = "r3",
                        disabled = NA
                      ),
                      shiny::tags$label(class = "form-check-label", `for` = "r3", "Disabled")
                    )
                  ),
                  shiny::div(
                    class = "col-md-3",
                    shiny::tags$p(class = "text-secondary small mb-2", "Inline"),
                    shiny::div(
                      class = "form-check form-check-inline",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "ci1", checked = NA),
                      shiny::tags$label(class = "form-check-label", `for` = "ci1", "One")
                    ),
                    shiny::div(
                      class = "form-check form-check-inline",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "ci2"),
                      shiny::tags$label(class = "form-check-label", `for` = "ci2", "Two")
                    )
                  ),
                  shiny::div(
                    class = "col-md-3",
                    shiny::tags$p(class = "text-secondary small mb-2", "Button-toggle (btn-check)"),
                    shiny::div(
                      class = "btn-group",
                      shiny::tags$input(type = "checkbox", class = "btn-check", id = "bc1", checked = NA),
                      shiny::tags$label(class = "btn btn-outline-primary", `for` = "bc1", "A"),
                      shiny::tags$input(type = "checkbox", class = "btn-check", id = "bc2"),
                      shiny::tags$label(class = "btn btn-outline-primary", `for` = "bc2", "B"),
                      shiny::tags$input(type = "checkbox", class = "btn-check", id = "bc3"),
                      shiny::tags$label(class = "btn btn-outline-primary", `for` = "bc3", "C")
                    ),
                    shiny::tags$br(),
                    shiny::tags$br(),
                    shiny::div(
                      class = "btn-group",
                      shiny::tags$input(type = "radio", class = "btn-check", name = "br", id = "br1", checked = NA),
                      shiny::tags$label(class = "btn btn-outline-success", `for` = "br1", "Yes"),
                      shiny::tags$input(type = "radio", class = "btn-check", name = "br", id = "br2"),
                      shiny::tags$label(class = "btn btn-outline-success", `for` = "br2", "No")
                    )
                  )
                )
              ),

              # -- SWITCHES --------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Switches"),
                shiny::div(
                  class = "d-flex flex-wrap gap-5",
                  shiny::div(
                    shiny::div(
                      class = "form-check form-switch",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "sw1", checked = NA),
                      shiny::tags$label(class = "form-check-label", `for` = "sw1", "On")
                    ),
                    shiny::div(
                      class = "form-check form-switch",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "sw2"),
                      shiny::tags$label(class = "form-check-label", `for` = "sw2", "Off")
                    ),
                    shiny::div(
                      class = "form-check form-switch",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "sw3", disabled = NA),
                      shiny::tags$label(class = "form-check-label", `for` = "sw3", "Disabled")
                    )
                  ),
                  shiny::div(
                    shiny::tags$p(class = "text-secondary small mb-1", "form-check-reverse (label left)"),
                    shiny::div(
                      class = "form-check form-switch form-check-reverse",
                      shiny::tags$input(class = "form-check-input", type = "checkbox", id = "sw4", checked = NA),
                      shiny::tags$label(class = "form-check-label", `for` = "sw4", "Reversed")
                    )
                  )
                )
              ),

              # -- DROPDOWNS -------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Dropdowns"),
                shiny::div(
                  class = "d-flex flex-wrap gap-3 align-items-start",
                  shiny::div(
                    class = "dropdown",
                    shiny::tags$button(
                      class = "btn btn-primary dropdown-toggle",
                      `data-bs-toggle` = "dropdown",
                      "Basic"
                    ),
                    shiny::tags$ul(
                      class = "dropdown-menu",
                      shiny::tags$li(shiny::tags$h6(class = "dropdown-header", "Section")),
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Action")),
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Another")),
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item active", href = "#", "Active")),
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item disabled", "Disabled")),
                      shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Separated"))
                    )
                  ),
                  shiny::div(
                    class = "dropup",
                    shiny::tags$button(
                      class = "btn btn-secondary dropdown-toggle",
                      `data-bs-toggle` = "dropdown",
                      "Dropup"
                    ),
                    shiny::tags$ul(
                      class = "dropdown-menu",
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Item 1")),
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Item 2"))
                    )
                  ),
                  shiny::div(
                    class = "dropdown",
                    shiny::tags$button(
                      class = "btn btn-outline-primary dropdown-toggle",
                      `data-bs-toggle` = "dropdown",
                      "Align End"
                    ),
                    shiny::tags$ul(
                      class = "dropdown-menu dropdown-menu-end",
                      shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "End-aligned"))
                    )
                  ),
                  shiny::div(
                    class = "dropdown",
                    shiny::tags$button(
                      class = "btn btn-info dropdown-toggle",
                      `data-bs-toggle` = "dropdown",
                      "With Icons"
                    ),
                    shiny::tags$ul(
                      class = "dropdown-menu",
                      shiny::tags$li(shiny::tags$a(
                        class = "dropdown-item",
                        href = "#",
                        shiny::tags$i(class = "bi bi-pencil me-2"),
                        "Edit"
                      )),
                      shiny::tags$li(shiny::tags$a(
                        class = "dropdown-item",
                        href = "#",
                        shiny::tags$i(class = "bi bi-files me-2"),
                        "Duplicate"
                      )),
                      shiny::tags$li(shiny::tags$hr(class = "dropdown-divider")),
                      shiny::tags$li(shiny::tags$a(
                        class = "dropdown-item text-danger",
                        href = "#",
                        shiny::tags$i(class = "bi bi-trash me-2"),
                        "Delete"
                      ))
                    )
                  )
                )
              ),

              # -- NAVS & TABS -----------------------------------
              shiny::tags[["div"]](
                shiny::h1("Navs & Tabs"),
                shiny::tags$p(class = "text-secondary small", "Tabs"),
                shiny::tags$ul(
                  class = "nav nav-tabs",
                  id = "demoTabs",
                  shiny::tags$li(
                    class = "nav-item",
                    shiny::tags$button(
                      class = "nav-link active",
                      `data-bs-toggle` = "tab",
                      `data-bs-target` = "#t1",
                      "Plot"
                    )
                  ),
                  shiny::tags$li(
                    class = "nav-item",
                    shiny::tags$button(class = "nav-link", `data-bs-toggle` = "tab", `data-bs-target` = "#t2", "Table")
                  ),
                  shiny::tags$li(class = "nav-item", shiny::tags$button(class = "nav-link disabled", "Disabled"))
                ),
                shiny::div(
                  class = "tab-content border border-top-0 rounded-bottom p-3 mb-4",
                  shiny::div(
                    class = "tab-pane fade show active",
                    id = "t1",
                    shiny::plotOutput(ns("tabPlot"), height = "180px")
                  ),
                  shiny::div(class = "tab-pane fade", id = "t2", shiny::tableOutput(ns("tabTable")))
                ),
                shiny::tags$p(class = "text-secondary small", "Pills"),
                shiny::tags$ul(
                  class = "nav nav-pills mb-3",
                  shiny::tags$li(class = "nav-item", shiny::tags$a(class = "nav-link active", href = "#", "Active")),
                  shiny::tags$li(class = "nav-item", shiny::tags$a(class = "nav-link", href = "#", "Link")),
                  shiny::tags$li(class = "nav-item", shiny::tags$a(class = "nav-link disabled", "Disabled"))
                ),
                shiny::tags$p(class = "text-secondary small", "Underline (BS 5.3)"),
                shiny::tags$ul(
                  class = "nav nav-underline",
                  shiny::tags$li(class = "nav-item", shiny::tags$a(class = "nav-link active", href = "#", "Active")),
                  shiny::tags$li(class = "nav-item", shiny::tags$a(class = "nav-link", href = "#", "Link"))
                )
              ),

              # -- NAVBAR ----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Navbar"),
                shiny::tags$nav(
                  class = "navbar navbar-expand-lg navbar-light bg-light rounded",
                  shiny::div(
                    class = "container-fluid",
                    shiny::tags$a(class = "navbar-brand", href = "#", "MyApp"),
                    shiny::tags$button(
                      class = "navbar-toggler",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#nb1",
                      shiny::tags$span(class = "navbar-toggler-icon")
                    ),
                    shiny::div(
                      class = "collapse navbar-collapse",
                      id = "nb1",
                      shiny::tags$ul(
                        class = "navbar-nav me-auto",
                        shiny::tags$li(
                          class = "nav-item",
                          shiny::tags$a(class = "nav-link active", href = "#", "Home")
                        ),
                        shiny::tags$li(class = "nav-item", shiny::tags$a(class = "nav-link", href = "#", "About")),
                        shiny::tags$li(
                          class = "nav-item dropdown",
                          shiny::tags$a(
                            class = "nav-link dropdown-toggle",
                            href = "#",
                            `data-bs-toggle` = "dropdown",
                            "More"
                          ),
                          shiny::tags$ul(
                            class = "dropdown-menu",
                            shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Sub 1")),
                            shiny::tags$li(shiny::tags$a(class = "dropdown-item", href = "#", "Sub 2"))
                          )
                        )
                      ),
                      shiny::tags$form(
                        class = "d-flex gap-2",
                        shiny::tags$input(
                          class = "form-control form-control-sm",
                          type = "search",
                          placeholder = "Search"
                        ),
                        shiny::tags$button(class = "btn btn-light btn-sm", "Go")
                      )
                    )
                  )
                )
              ),

              # -- BREADCRUMB ------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Breadcrumb"),
                shiny::tags$nav(
                  shiny::tags$ol(
                    class = "breadcrumb",
                    shiny::tags$li(class = "breadcrumb-item", shiny::tags$a(href = "#", "Home")),
                    shiny::tags$li(class = "breadcrumb-item", shiny::tags$a(href = "#", "Library")),
                    shiny::tags$li(class = "breadcrumb-item active", "Data")
                  )
                ),
                shiny::tags$nav(
                  shiny::tags$ol(
                    class = "breadcrumb",
                    style = "--bs-breadcrumb-divider:'>'",
                    shiny::tags$li(class = "breadcrumb-item", shiny::tags$a(href = "#", "Home")),
                    shiny::tags$li(class = "breadcrumb-item active", "Custom divider (>)")
                  )
                )
              ),

              # -- PAGINATION ------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Pagination"),
                shiny::div(
                  class = "d-flex flex-column gap-3",
                  shiny::tags$nav(
                    shiny::tags$ul(
                      class = "pagination",
                      shiny::tags$li(
                        class = "page-item disabled",
                        shiny::tags$a(class = "page-link", href = "#", "<")
                      ),
                      shiny::tags$li(class = "page-item", shiny::tags$a(class = "page-link", href = "#", "1")),
                      shiny::tags$li(class = "page-item active", shiny::tags$a(class = "page-link", href = "#", "2")),
                      shiny::tags$li(class = "page-item", shiny::tags$a(class = "page-link", href = "#", "3")),
                      shiny::tags$li(class = "page-item", shiny::tags$a(class = "page-link", href = "#", ">"))
                    )
                  ),
                  shiny::tags$nav(
                    shiny::tags$ul(
                      class = "pagination pagination-sm justify-content-end",
                      shiny::tags$li(class = "page-item", shiny::tags$a(class = "page-link", href = "#", "Prev")),
                      shiny::tags$li(class = "page-item active", shiny::tags$a(class = "page-link", href = "#", "1")),
                      shiny::tags$li(class = "page-item", shiny::tags$a(class = "page-link", href = "#", "Next"))
                    )
                  )
                )
              ),

              # -- PROGRESS --------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Progress Bars"),
                shiny::div(
                  class = "d-flex flex-column gap-3",
                  shiny::div(class = "progress", shiny::div(class = "progress-bar", style = "width:25%", "25%")),
                  shiny::div(
                    class = "progress",
                    shiny::div(class = "progress-bar bg-success", style = "width:50%", "50%")
                  ),
                  shiny::div(
                    class = "progress",
                    shiny::div(class = "progress-bar bg-warning text-dark", style = "width:75%", "75%")
                  ),
                  shiny::div(
                    class = "progress progress-bar-striped",
                    shiny::div(class = "progress-bar", style = "width:60%", "Striped")
                  ),
                  shiny::div(
                    class = "progress progress-bar-striped progress-bar-animated",
                    shiny::div(class = "progress-bar bg-info", style = "width:70%", "Animated")
                  ),
                  shiny::div(
                    class = "progress-stacked",
                    shiny::div(class = "progress", style = "width:35%", shiny::div(class = "progress-bar", "35%")),
                    shiny::div(
                      class = "progress",
                      style = "width:20%",
                      shiny::div(class = "progress-bar bg-success", "20%")
                    ),
                    shiny::div(
                      class = "progress",
                      style = "width:15%",
                      shiny::div(class = "progress-bar bg-danger", "15%")
                    )
                  )
                )
              ),

              # -- SPINNERS --------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Spinners"),
                shiny::div(
                  class = "d-flex flex-wrap gap-3 align-items-center",
                  shiny::div(class = "spinner-border text-primary"),
                  shiny::div(class = "spinner-border text-success"),
                  shiny::div(class = "spinner-border text-danger"),
                  shiny::div(class = "spinner-border text-warning"),
                  shiny::div(class = "spinner-border text-info"),
                  shiny::div(class = "spinner-border spinner-border-sm text-primary"),
                  shiny::div(class = "spinner-grow text-primary"),
                  shiny::div(class = "spinner-grow spinner-grow-sm text-success"),
                  shiny::tags$button(
                    class = "btn btn-primary",
                    disabled = NA,
                    shiny::tags$span(class = "spinner-border spinner-border-sm me-2"),
                    "Loading..."
                  ),
                  shiny::tags$button(
                    class = "btn btn-outline-secondary",
                    disabled = NA,
                    shiny::tags$span(class = "spinner-grow spinner-grow-sm me-2"),
                    "Processing"
                  )
                )
              ),

              # -- ACCORDION -------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Accordion"),
                shiny::div(
                  class = "accordion",
                  id = "acc1",
                  shiny::div(
                    class = "accordion-item",
                    shiny::tags$h2(
                      class = "accordion-header",
                      shiny::tags$button(
                        class = "accordion-button",
                        `data-bs-toggle` = "collapse",
                        `data-bs-target` = "#a1",
                        "Item #1 (open)"
                      )
                    ),
                    shiny::div(
                      id = "a1",
                      class = "accordion-collapse collapse show",
                      `data-bs-parent` = "#acc1",
                      shiny::div(
                        class = "accordion-body",
                        "Shiny inputs work inside accordion panels:",
                        shiny::sliderInput(ns("accSlider"), "Value", 1, 100, 50)
                      )
                    )
                  ),
                  shiny::div(
                    class = "accordion-item",
                    shiny::tags$h2(
                      class = "accordion-header",
                      shiny::tags$button(
                        class = "accordion-button collapsed",
                        `data-bs-toggle` = "collapse",
                        `data-bs-target` = "#a2",
                        "Item #2"
                      )
                    ),
                    shiny::div(
                      id = "a2",
                      class = "accordion-collapse collapse",
                      `data-bs-parent` = "#acc1",
                      shiny::div(class = "accordion-body", "Content two.")
                    )
                  ),
                  shiny::div(
                    class = "accordion-item",
                    shiny::tags$h2(
                      class = "accordion-header",
                      shiny::tags$button(
                        class = "accordion-button collapsed",
                        `data-bs-toggle` = "collapse",
                        `data-bs-target` = "#a3",
                        "Item #3"
                      )
                    ),
                    shiny::div(
                      id = "a3",
                      class = "accordion-collapse collapse",
                      `data-bs-parent` = "#acc1",
                      shiny::div(
                        class = "accordion-body",
                        shiny::selectInput(ns("accSel"), "Pick one", c("Alpha", "Beta", "Gamma"))
                      )
                    )
                  )
                )
              ),

              # -- COLLAPSE --------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Collapse"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 align-items-start",
                  shiny::tags$button(
                    class = "btn btn-outline-primary",
                    `data-bs-toggle` = "collapse",
                    `data-bs-target` = "#col1",
                    "Toggle (button)"
                  ),
                  shiny::tags$a(
                    class = "btn btn-outline-secondary",
                    `data-bs-toggle` = "collapse",
                    href = "#col2",
                    "Toggle (<a>)"
                  ),
                  shiny::div(
                    class = "collapse w-100",
                    id = "col1",
                    shiny::div(
                      class = "card card-body mt-2",
                      "Via ",
                      shiny::tags$code('data-bs-toggle="collapse" data-bs-target="#id"')
                    )
                  ),
                  shiny::div(
                    class = "collapse w-100",
                    id = "col2",
                    shiny::div(class = "card card-body mt-2", "Via ", shiny::tags$code('href="#id"'), " on an anchor.")
                  )
                )
              ),

              # -- OFFCANVAS -------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Offcanvas"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2",

                  shiny::tags$button(
                    class = "btn btn-primary",
                    `data-bs-toggle` = "offcanvas",
                    `data-bs-target` = "#oc1",
                    "Start (Left)"
                  ),
                  shiny::div(
                    class = "offcanvas offcanvas-start",
                    id = "oc1",
                    tabindex = "-1",
                    shiny::div(
                      class = "offcanvas-header border-bottom",
                      shiny::tags$h5(class = "offcanvas-title", "Offcanvas Start"),
                      shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "offcanvas")
                    ),
                    shiny::div(
                      class = "offcanvas-body",
                      shiny::tags$p("offcanvas-start - great for Shiny filter panels."),
                      shiny::sliderInput(ns("ocSlider"), "Filter", 1, 100, 50),
                      shiny::selectInput(ns("ocSel"), "Group", c("A", "B", "C"))
                    )
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-primary",
                    `data-bs-toggle` = "offcanvas",
                    `data-bs-target` = "#oc2",
                    "End (Right)"
                  ),
                  shiny::div(
                    class = "offcanvas offcanvas-end",
                    id = "oc2",
                    tabindex = "-1",
                    shiny::div(
                      class = "offcanvas-header border-bottom",
                      shiny::tags$h5(class = "offcanvas-title", "End"),
                      shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "offcanvas")
                    ),
                    shiny::div(class = "offcanvas-body", "offcanvas-end")
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-secondary",
                    `data-bs-toggle` = "offcanvas",
                    `data-bs-target` = "#oc3",
                    "Top"
                  ),
                  shiny::div(
                    class = "offcanvas offcanvas-top",
                    id = "oc3",
                    tabindex = "-1",
                    shiny::div(
                      class = "offcanvas-header border-bottom",
                      shiny::tags$h5(class = "offcanvas-title", "Top"),
                      shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "offcanvas")
                    ),
                    shiny::div(class = "offcanvas-body", "offcanvas-top")
                  ),

                  shiny::tags$button(
                    class = "btn btn-outline-secondary",
                    `data-bs-toggle` = "offcanvas",
                    `data-bs-target` = "#oc4",
                    "Bottom"
                  ),
                  shiny::div(
                    class = "offcanvas offcanvas-bottom",
                    id = "oc4",
                    tabindex = "-1",
                    shiny::div(
                      class = "offcanvas-header border-bottom",
                      shiny::tags$h5(class = "offcanvas-title", "Bottom"),
                      shiny::tags$button(class = "btn-close", `data-bs-dismiss` = "offcanvas")
                    ),
                    shiny::div(class = "offcanvas-body", "offcanvas-bottom")
                  )
                )
              ),

              # -- TOOLTIPS & POPOVERS ---------------------------
              shiny::tags[["div"]](
                shiny::h1("Tooltips & Popovers"),
                shiny::div(
                  class = "d-flex flex-wrap gap-3 align-items-center",
                  shiny::tags$button(
                    class = "btn btn-secondary",
                    `data-bs-toggle` = "tooltip",
                    title = "Default (top)",
                    "Tooltip Top"
                  ),
                  shiny::tags$button(
                    class = "btn btn-secondary",
                    `data-bs-toggle` = "tooltip",
                    `data-bs-placement` = "right",
                    title = "Right",
                    "Tooltip Right"
                  ),
                  shiny::tags$button(
                    class = "btn btn-secondary",
                    `data-bs-toggle` = "tooltip",
                    `data-bs-placement` = "bottom",
                    title = "Bottom",
                    "Tooltip Bottom"
                  ),
                  shiny::tags$button(
                    class = "btn btn-secondary",
                    `data-bs-toggle` = "tooltip",
                    `data-bs-placement` = "left",
                    title = "Left",
                    "Tooltip Left"
                  ),
                  shiny::tags$button(
                    class = "btn btn-info",
                    `data-bs-toggle` = "popover",
                    `data-bs-trigger` = "focus",
                    title = "Popover",
                    `data-bs-content` = "Popover body text.",
                    "Popover (focus)"
                  ),
                  shiny::tags$button(
                    class = "btn btn-outline-info",
                    `data-bs-toggle` = "popover",
                    `data-bs-trigger` = "hover",
                    `data-bs-placement` = "right",
                    title = "Hover",
                    `data-bs-content` = "Triggered on hover.",
                    "Hover Popover"
                  )
                ),
                # Bootstrap JS already loaded by Shiny - just initialise the components
                shiny::tags$script(shiny::HTML(
                  "
              document.addEventListener('DOMContentLoaded', function () {
                document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')
                  .forEach(function(el) { new bootstrap.Tooltip(el); });
                document.querySelectorAll('[data-bs-toggle=\"popover\"]')
                  .forEach(function(el) { new bootstrap.Popover(el); });
              });
            "
                ))
              ),

              # -- LIST GROUPS -----------------------------------
              shiny::tags[["div"]](
                shiny::h1("List Groups"),
                shiny::div(
                  class = "row g-3",
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$p(class = "text-secondary small", "Basic"),
                    shiny::tags$ul(
                      class = "list-group list-group-flush border rounded",
                      shiny::tags$li(class = "list-group-item active", "Active"),
                      shiny::tags$li(class = "list-group-item", "Item 2"),
                      shiny::tags$li(class = "list-group-item", "Item 3"),
                      shiny::tags$li(class = "list-group-item disabled", "Disabled")
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$p(class = "text-secondary small", "With badges"),
                    shiny::tags$ul(
                      class = "list-group list-group-flush border rounded",
                      shiny::tags$li(
                        class = "list-group-item d-flex justify-content-between align-items-center",
                        "Inbox",
                        shiny::tags$span(class = "badge bg-primary rounded-pill", "14")
                      ),
                      shiny::tags$li(
                        class = "list-group-item d-flex justify-content-between align-items-center",
                        "Sent",
                        shiny::tags$span(class = "badge bg-secondary rounded-pill", "2")
                      ),
                      shiny::tags$li(
                        class = "list-group-item d-flex justify-content-between align-items-center",
                        "Drafts",
                        shiny::tags$span(class = "badge bg-danger rounded-pill", "1")
                      )
                    )
                  ),
                  shiny::div(
                    class = "col-md-4",
                    shiny::tags$p(class = "text-secondary small", "Colored"),
                    shiny::tags$ul(
                      class = "list-group",
                      shiny::tags$li(class = "list-group-item list-group-item-primary", "list-group-item-primary"),
                      shiny::tags$li(class = "list-group-item list-group-item-success", "list-group-item-success"),
                      shiny::tags$li(class = "list-group-item list-group-item-danger", "list-group-item-danger"),
                      shiny::tags$li(class = "list-group-item list-group-item-warning", "list-group-item-warning"),
                      shiny::tags$li(class = "list-group-item list-group-item-info", "list-group-item-info")
                    )
                  ),
                  shiny::div(
                    class = "col-12",
                    shiny::tags$p(class = "text-secondary small", "Horizontal + actionable"),
                    shiny::div(
                      class = "list-group list-group-horizontal",
                      shiny::tags$a(href = "#", class = "list-group-item list-group-item-action active", "Active"),
                      shiny::tags$a(href = "#", class = "list-group-item list-group-item-action", "Link 2"),
                      shiny::tags$a(href = "#", class = "list-group-item list-group-item-action", "Link 3")
                    )
                  )
                )
              ),

              # -- TABLES ----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Tables"),
                shiny::div(
                  class = "table-responsive",
                  shiny::tags$table(
                    class = "table table-bordered table-hover table-striped table-sm",
                    shiny::tags$thead(
                      class = "table-primary",
                      shiny::tags$tr(
                        shiny::tags$th("#"),
                        shiny::tags$th("Name"),
                        shiny::tags$th("Status"),
                        shiny::tags$th("Value")
                      )
                    ),
                    shiny::tags$tbody(
                      shiny::tags$tr(
                        shiny::tags$td("1"),
                        shiny::tags$td("Alice"),
                        shiny::tags$td(shiny::tags$span(class = "badge bg-success", "Active")),
                        shiny::tags$td("$1,200")
                      ),
                      shiny::tags$tr(
                        class = "table-warning",
                        shiny::tags$td("2"),
                        shiny::tags$td("Bob (table-warning)"),
                        shiny::tags$td(shiny::tags$span(class = "badge bg-warning text-dark", "Pending")),
                        shiny::tags$td("$800")
                      ),
                      shiny::tags$tr(
                        shiny::tags$td("3"),
                        shiny::tags$td("Carol"),
                        shiny::tags$td(shiny::tags$span(class = "badge bg-danger", "Inactive")),
                        shiny::tags$td("$0")
                      )
                    ),
                    shiny::tags$tfoot(
                      class = "table-secondary",
                      shiny::tags$tr(
                        shiny::tags$td(colspan = "3", shiny::tags$strong("Total")),
                        shiny::tags$td(shiny::tags$strong("$2,000"))
                      )
                    )
                  )
                ),
                shiny::tags$p(class = "text-secondary small mt-2", "renderTable inside table-responsive:"),
                shiny::div(class = "table-responsive", shiny::tableOutput(ns("shinyTable")))
              ),

              # -- TYPOGRAPHY ------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Typography"),
                shiny::div(class = "display-3 lh-1 mb-2", "display-3"),
                shiny::div(class = "display-5 lh-1 mb-3", "display-5"),
                shiny::tags$h1("h1"),
                shiny::tags$h2("h2"),
                shiny::tags$h3("h3"),
                shiny::tags$h4("h4"),
                shiny::tags$h5("h5"),
                shiny::tags$h6("h6"),
                shiny::tags$p(class = "lead", "lead - larger intro paragraph"),
                shiny::tags$p(
                  shiny::tags$strong("fw-bold"),
                  " * ",
                  shiny::tags$em("fst-italic"),
                  " * ",
                  shiny::tags$mark("mark"),
                  " * ",
                  shiny::tags$del("del"),
                  " * ",
                  shiny::tags$ins("ins"),
                  " * ",
                  shiny::tags$small("small"),
                  " * ",
                  shiny::tags$code("code"),
                  " * ",
                  shiny::tags$kbd("Ctrl+C")
                ),
                shiny::tags$p(class = "text-muted", "text-muted"),
                shiny::tags$p(class = "fw-light", "fw-light"),
                shiny::tags$p(class = "fw-semibold", "fw-semibold"),
                shiny::tags$p(class = "fw-bold", "fw-bold"),
                shiny::tags$p(class = "text-uppercase", "text-uppercase"),
                shiny::tags$p(class = "text-capitalize", "text-capitalize each word"),
                shiny::tags$p(class = "text-decoration-underline", "text-decoration-underline"),
                shiny::tags$p(
                  class = "text-truncate",
                  style = "max-width:250px;",
                  "text-truncate clips long text with an ellipsis..."
                ),
                shiny::tags$blockquote(
                  class = "blockquote",
                  shiny::tags$p("A blockquote example."),
                  shiny::tags$footer(class = "blockquote-footer", "Someone ", shiny::tags$cite("Source"))
                )
              ),

              # -- COLORS ----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Colors: text-* / bg-* / text-bg-*"),
                shiny::tags$p(class = "text-secondary small", "Text (foreground)"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  shiny::tags$span(class = "text-primary", "text-primary"),
                  shiny::tags$span(class = "text-secondary", "text-secondary"),
                  shiny::tags$span(class = "text-success", "text-success"),
                  shiny::tags$span(class = "text-danger", "text-danger"),
                  shiny::tags$span(class = "text-warning", "text-warning"),
                  shiny::tags$span(class = "text-info", "text-info"),
                  shiny::tags$span(class = "text-muted", "text-muted")
                ),
                shiny::tags$p(class = "text-secondary small", "Background"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  shiny::tags$span(class = "bg-primary   text-white px-2 py-1 rounded", "bg-primary"),
                  shiny::tags$span(class = "bg-secondary text-white px-2 py-1 rounded", "bg-secondary"),
                  shiny::tags$span(class = "bg-success   text-white px-2 py-1 rounded", "bg-success"),
                  shiny::tags$span(class = "bg-danger    text-white px-2 py-1 rounded", "bg-danger"),
                  shiny::tags$span(class = "bg-warning   text-dark  px-2 py-1 rounded", "bg-warning"),
                  shiny::tags$span(class = "bg-info      text-dark  px-2 py-1 rounded", "bg-info"),
                  shiny::tags$span(class = "bg-dark      text-white px-2 py-1 rounded", "bg-dark"),
                  shiny::tags$span(class = "bg-light     text-dark  px-2 py-1 rounded", "bg-light")
                ),
                shiny::tags$p(
                  class = "text-secondary small",
                  "text-bg-{variant} - sets both bg and fg with auto contrast (BS 5.2+)"
                ),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  shiny::tags$span(class = "text-bg-primary   px-2 py-1 rounded", "text-bg-primary"),
                  shiny::tags$span(class = "text-bg-secondary px-2 py-1 rounded", "text-bg-secondary"),
                  shiny::tags$span(class = "text-bg-success   px-2 py-1 rounded", "text-bg-success"),
                  shiny::tags$span(class = "text-bg-danger    px-2 py-1 rounded", "text-bg-danger"),
                  shiny::tags$span(class = "text-bg-warning   px-2 py-1 rounded", "text-bg-warning"),
                  shiny::tags$span(class = "text-bg-info      px-2 py-1 rounded", "text-bg-info"),
                  shiny::tags$span(class = "text-bg-light     px-2 py-1 rounded", "text-bg-light"),
                  shiny::tags$span(class = "text-bg-dark      px-2 py-1 rounded", "text-bg-dark")
                ),
                shiny::tags$p(class = "text-secondary small", "bg-opacity-*"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  shiny::tags$span(class = "bg-primary bg-opacity-100 text-white px-2 py-1 rounded", "100"),
                  shiny::tags$span(class = "bg-primary bg-opacity-75  text-white px-2 py-1 rounded", "75"),
                  shiny::tags$span(class = "bg-primary bg-opacity-50  px-2 py-1 rounded", "50"),
                  shiny::tags$span(class = "bg-primary bg-opacity-25  px-2 py-1 rounded", "25"),
                  shiny::tags$span(class = "bg-primary bg-opacity-10  px-2 py-1 rounded", "10"),
                  shiny::tags$span(
                    class = "bg-gradient bg-primary text-white px-2 py-1 rounded",
                    "bg-gradient"
                  )
                )
              ),

              # -- SPACING & BORDERS -----------------------------
              shiny::tags[["div"]](
                shiny::h1("Spacing, Borders & Shadows"),
                shiny::tags$p(class = "text-secondary small", shiny::tags$code("{m|p}{t|b|s|e|x|y}-{0-5|auto}")),
                shiny::div(
                  class = "d-flex flex-wrap gap-1 mb-4",
                  lapply(0:5, function(i) {
                    shiny::div(
                      class = paste0("bg-primary text-white small p-", i),
                      style = if (i == 0) "padding:2px 4px!important" else "",
                      paste0("p-", i)
                    )
                  }),
                  shiny::div(class = "bg-success ms-auto p-2 text-white small", "ms-auto")
                ),
                shiny::tags$p(class = "text-secondary small", "Borders"),
                shiny::div(
                  class = "d-flex flex-wrap gap-2 mb-4",
                  shiny::div(class = "border p-2 small", "border"),
                  shiny::div(class = "border border-primary p-2 small", "border-primary"),
                  shiny::div(class = "border border-success border-2 p-2 small", "border-2"),
                  shiny::div(class = "border border-danger  border-3 p-2 small", "border-3"),
                  shiny::div(class = "border-top    p-2 small", "border-top"),
                  shiny::div(class = "border-bottom p-2 small", "border-bottom"),
                  shiny::div(class = "border-start  p-2 small", "border-start"),
                  shiny::div(class = "border-end    p-2 small", "border-end"),
                  shiny::div(class = "border-0 bg-secondary p-2 small text-white", "border-0"),
                  shiny::div(class = "rounded border p-2 small", "rounded"),
                  shiny::div(class = "rounded-pill border px-3 py-2 small", "rounded-pill"),
                  shiny::div(class = "rounded-0 border p-2 small", "rounded-0"),
                  shiny::div(
                    class = "rounded-circle bg-primary text-white small d-flex align-items-center justify-content-center",
                    style = "width:50px;height:50px;",
                    "circle"
                  )
                ),
                shiny::tags$p(class = "text-secondary small", "Shadows"),
                shiny::div(
                  class = "d-flex flex-wrap gap-3",
                  shiny::div(class = "shadow-none border p-3 rounded small", "shadow-none"),
                  shiny::div(class = "shadow-sm   border p-3 rounded small", "shadow-sm"),
                  shiny::div(class = "shadow      border p-3 rounded small", "shadow"),
                  shiny::div(class = "shadow-lg   border p-3 rounded small", "shadow-lg")
                )
              ),

              # -- FLEX ------------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Flexbox Utilities"),
                shiny::div(
                  class = "d-flex flex-column gap-2",
                  shiny::div(
                    class = "d-flex gap-2 border rounded p-2",
                    shiny::div(class = "bg-primary text-white p-2 small", "1"),
                    shiny::div(class = "bg-primary text-white p-2 small", "2"),
                    shiny::div(class = "bg-primary text-white p-2 small", "3"),
                    shiny::tags$small(class = "text-secondary ms-2 align-self-center", "d-flex gap-2")
                  ),
                  shiny::div(
                    class = "d-flex justify-content-between border rounded p-2",
                    shiny::div(class = "bg-info p-2 small", "start"),
                    shiny::div(class = "bg-info p-2 small", "justify-content-between"),
                    shiny::div(class = "bg-info p-2 small", "end")
                  ),
                  shiny::div(
                    class = "d-flex align-items-center gap-2 border rounded p-2",
                    style = "height:80px;",
                    shiny::div(class = "bg-warning p-2 small", "align-items-center"),
                    shiny::div(class = "bg-warning p-4 small", "tall"),
                    shiny::div(class = "align-self-start bg-danger text-white p-1 small", "align-self-start"),
                    shiny::div(class = "align-self-end bg-danger text-white p-1 small", "align-self-end")
                  ),
                  shiny::div(
                    class = "d-flex gap-2 border rounded p-2",
                    shiny::div(class = "flex-fill bg-primary text-white p-2 small text-center", "flex-fill"),
                    shiny::div(class = "flex-fill bg-primary text-white p-2 small text-center", "flex-fill"),
                    shiny::div(class = "flex-grow-1 bg-success text-white p-2 small text-center", "flex-grow-1"),
                    shiny::div(class = "flex-shrink-0 bg-danger text-white p-2 small", "flex-shrink-0")
                  )
                )
              ),

              # -- GRID ------------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Grid System"),
                shiny::div(
                  class = "row g-2 mb-2",
                  shiny::div(
                    class = "col",
                    shiny::div(class = "bg-primary text-white p-2 rounded text-center small", "col")
                  ),
                  shiny::div(
                    class = "col",
                    shiny::div(class = "bg-primary text-white p-2 rounded text-center small", "col")
                  ),
                  shiny::div(
                    class = "col",
                    shiny::div(class = "bg-primary text-white p-2 rounded text-center small", "col")
                  )
                ),
                shiny::div(
                  class = "row g-2 mb-2",
                  shiny::div(
                    class = "col-4",
                    shiny::div(class = "bg-success text-white p-2 rounded text-center small", "col-4")
                  ),
                  shiny::div(
                    class = "col-8",
                    shiny::div(class = "bg-success text-white p-2 rounded text-center small", "col-8")
                  )
                ),
                shiny::div(
                  class = "row g-2 mb-2",
                  shiny::div(
                    class = "col-md-3",
                    shiny::div(class = "bg-info text-dark p-2 rounded text-center small", "col-md-3")
                  ),
                  shiny::div(
                    class = "col-md-6",
                    shiny::div(class = "bg-info text-dark p-2 rounded text-center small", "col-md-6")
                  ),
                  shiny::div(
                    class = "col-md-3",
                    shiny::div(class = "bg-info text-dark p-2 rounded text-center small", "col-md-3")
                  )
                ),
                shiny::div(
                  class = "row g-2",
                  shiny::div(
                    class = "col-3 offset-3",
                    shiny::div(class = "bg-warning text-dark p-2 rounded text-center small", "offset-3")
                  ),
                  shiny::div(
                    class = "col-4 offset-1",
                    shiny::div(class = "bg-warning text-dark p-2 rounded text-center small", "offset-1")
                  )
                )
              ),

              # -- ICONS -----------------------------------------
              shiny::tags[["div"]](
                shiny::h1("Bootstrap Icons"),
                shiny::div(
                  class = "d-flex flex-wrap gap-3 align-items-center mb-3",
                  lapply(
                    c(
                      "house",
                      "person",
                      "gear",
                      "search",
                      "bell",
                      "check-circle",
                      "x-circle",
                      "exclamation-triangle",
                      "info-circle",
                      "bar-chart-fill",
                      "table",
                      "download",
                      "upload",
                      "filter",
                      "funnel",
                      "sliders",
                      "calendar",
                      "clock",
                      "envelope",
                      "chat",
                      "star",
                      "heart",
                      "bookmark",
                      "share",
                      "trash",
                      "pencil",
                      "plus-circle",
                      "dash-circle",
                      "arrow-right",
                      "chevron-down",
                      "three-dots-vertical",
                      "grid",
                      "list",
                      "lock",
                      "eye",
                      "github",
                      "clipboard",
                      "file-earmark-text",
                      "graph-up",
                      "graph-down",
                      "shield-check",
                      "cpu",
                      "database",
                      "cloud-upload"
                    ),
                    function(icon) {
                      shiny::tags$i(class = paste0("bi bi-", icon, " fs-4"), title = paste0("bi-", icon))
                    }
                  )
                ),
                shiny::tags$p(class = "text-secondary small mb-2", "Sizing with fs-*"),
                shiny::div(
                  class = "d-flex gap-3 align-items-end",
                  shiny::tags$i(class = "bi bi-star fs-6"),
                  shiny::tags$i(class = "bi bi-star fs-5"),
                  shiny::tags$i(class = "bi bi-star fs-4"),
                  shiny::tags$i(class = "bi bi-star fs-3"),
                  shiny::tags$i(class = "bi bi-star fs-2"),
                  shiny::tags$i(class = "bi bi-star fs-1")
                )
              ),

              # -- SHINY + BS5 -----------------------------------
              shiny::tags[["div"]](
                shiny::h1("Shiny Inputs & Outputs inside BS5"),
                shiny::div(
                  class = "row g-3",
                  shiny::div(
                    class = "col-md-4",
                    shiny::div(
                      class = "card h-100",
                      shiny::div(class = "card-header", shiny::tags$i(class = "bi bi-sliders me-2"), "Controls"),
                      shiny::div(
                        class = "card-body",
                        shiny::sliderInput(ns("n"), "Sample size", 10, 500, 100),
                        shiny::selectInput(
                          ns("dist"),
                          "Distribution",
                          c("Normal" = "norm", "Uniform" = "unif", "Exponential" = "exp")
                        ),
                        shiny::numericInput(ns("bins"), "Bins", 20, 5, 100),
                        shiny::actionButton(
                          ns("go"),
                          "Generate",
                          class = "btn btn-primary w-100 mt-2",
                          icon = shiny::icon("play")
                        )
                      )
                    )
                  ),
                  shiny::div(
                    class = "col-md-8",
                    shiny::div(
                      class = "card h-100",
                      shiny::div(
                        class = "card-header d-flex justify-content-between align-items-center",
                        shiny::div(shiny::tags$i(class = "bi bi-bar-chart me-2"), "Output"),
                        shiny::tags$span(
                          class = "badge bg-primary rounded-pill",
                          shiny::textOutput(ns("sampleSize"), inline = TRUE)
                        )
                      ),
                      shiny::div(class = "card-body", shiny::plotOutput(ns("mainPlot"), height = "250px")),
                      shiny::div(
                        class = "card-footer d-flex gap-2",
                        shiny::div(
                          class = "alert alert-info mb-0 py-1 px-2 small flex-fill",
                          shiny::tags$i(class = "bi bi-info-circle me-1"),
                          "Mean: ",
                          shiny::textOutput(ns("meanVal"), inline = TRUE)
                        ),
                        shiny::div(
                          class = "alert alert-success mb-0 py-1 px-2 small flex-fill",
                          shiny::tags$i(class = "bi bi-check-circle me-1"),
                          "SD: ",
                          shiny::textOutput(ns("sdVal"), inline = TRUE)
                        )
                      )
                    )
                  )
                )
              ),

              shiny::div(class = "pb-5")
            ) # end main
          ) # end row
        ) # end container-fluid
      )
    },
    server = function(afmm) {},
    module_id = module_id
  )
  mod
}
