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
        mtcars = add_dummy_labels(mtcars),
        mtcars2 = add_dummy_labels(mtcars)
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
