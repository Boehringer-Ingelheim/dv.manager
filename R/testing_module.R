## All the functions in this file are used for testing purposes either in testthat
## or to have quick mock apps to test functionality, they are not in their own file as
## they do not have sufficient entity to be included in the modulegallery or to be exported


empty_UI <- function(id) { # nolint
  shiny::tagList()
}

empty_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
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

########### Identity module

identity_UI <- function(id) { # nolint
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

mod_identity <- function(value, mod_id) {
  list(
    ui = identity_UI,
    server = function(afmm) {
      identity_server(
        id = mod_id,
        mm_resolve_dispatcher(
          value,
          afmm,
          flatten = inherits(value, "mm_dispatcher") && length(value[["selection"]]) == 1
        )
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
simple_UI <- function(id) { # nolint
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
      output$text <- shinymeta::metaRender(
        shiny::renderText,
        {
          log_inform(paste(nrow(dataset())))
          nrow(shinymeta::..(dataset()))
        }
      )

      # nolint start
      # output$code <- shiny::renderPrint({
      #   shinymeta::expandChain(output$text())
      # })
      # nolint end

      return(structure(list(),
        code = output$text
      ))
    }
  )
}

#' A simple module that counts the number of rows
#'
#' This simple module is used for demonstration purposes in documentation
#'
#' @param module_id shiny module ID
#'
#' @export
mod_simple <- function(dataset, module_id) {
  mod <- list(
    ui = simple_UI,
    server = function(afmm) {
      # Add dispatcher support
      simple_server(module_id, mm_resolve_dispatcher(dataset, afmm, flatten = TRUE))
    },
    module_id = module_id,
    meta
  )
  mod
}

run_mock_app <- function() {
  run_app(
    data = list("D1" = list(adsl = get_pharmaverse_data("adsl"), adae = get_pharmaverse_data("adae"))),
    module_list = list(
      "Simple" = mod_simple(mm_dispatch("filtered_dataset", "adsl"), "mod1"),
      "Simple2" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod2")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

run_mock_app_two_datasets <- function() {
  run_app(
    data = list(
      "D1" = list(
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
      "Simple" = mod_simple(mm_dispatch("filtered_dataset", "adsl"), "mod1"),
      "Simple2" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod2"),
      "Simple3" = mod_simple(mm_dispatch("filtered_dataset", "adae"), "mod3")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

########## URL READING

url_reader_UI <- function(id) { # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("browser"), "papo browser"),
    shiny::textOutput(ns("text")),
    shiny::div("AGE: ", shiny::textOutput(ns("age"), inline = TRUE)),
    shiny::div("SEX: ", shiny::textOutput(ns("sex"), inline = TRUE)),
    shiny::div("RACE: ", shiny::textOutput(ns("race"), inline = TRUE)),
    shiny::div("COUNTRY: ", shiny::textOutput(ns("country"), inline = TRUE))
  )
}

url_reader_server <- function(id, text = NULL, init_text = NULL, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!is.anyreactive(text)) {
        r_text <- shiny::reactive(text)
      } else {
        r_text <- text
      }
      if (!is.anyreactive(init_text)) {
        r_init_text <- shiny::reactive(init_text)
      } else {
        r_init_text <- init_text
      }

      shiny::observeEvent(input$browser, {
        browser()
      })

      out_text <- shiny::reactiveVal()

      shiny::observeEvent(r_init_text(), {
        out_text(r_init_text())
      })

      shiny::observeEvent(r_text(), {
        out_text(r_text())
      })

      subject_data <- shiny::reactive({
        shiny::req(!is.null(out_text()))
        dataset() %>% dplyr::filter(.data[["USUBJID"]] == out_text())
      })

      output$text <- shiny::renderText({
        out_text()
      })
      output$age <- shiny::renderText({
        subject_data()[["AGE"]]
      })
      output$sex <- shiny::renderText({
        subject_data()[["SEX"]]
      })
      output$race <- shiny::renderText({
        subject_data()[["RACE"]]
      })
      output$country <- shiny::renderText({
        subject_data()[["COUNTRY"]]
      })
    }
  )
}

mod_url_reader <- function(text = NULL, init_text, mod_id, dataset) {
  mod <- list(
    ui = url_reader_UI,
    server = function(afmm) {
      url_reader_server(
        id = mod_id,
        text = mm_resolve_dispatcher(text, afmm, flatten = TRUE),
        init_text = mm_resolve_dispatcher(init_text, afmm, flatten = TRUE),
        dataset = mm_resolve_dispatcher(dataset, afmm, flatten = TRUE)
      )
    },
    module_id = mod_id
  )
  mod
}

run_mock_url_app <- function() {
  run_app(
    data = list(
      "D1" = list(adsl = get_pharmaverse_data("adsl")),
      "D2" = list(adsl = get_pharmaverse_data("adsl"))
    ),
    module_list = list(
      "Show URL" = mod_url_reader(
        init_text = mm_dispatch("url_parameters", "subjid"),
        dataset = mm_dispatch("filtered_dataset", "adsl"),
        mod_id = "mod_1"
      )
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}


###### Module communication testing
com_test_UI <- function(id, choices = c(1, 2, 3), message) { # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(ns("select"), label = "Select a number", choices = choices),
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

mod_com_test <- function(choices, message, value, mod_id) {
  mod <- list(
    ui = function(id) {
      com_test_UI(id, choices, message)
    },
    server = function(afmm) {
      com_test_server(id = mod_id, value = mm_resolve_dispatcher(value, afmm, flatten = TRUE))
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
        value = mm_dispatch("module_output", "mod_2"),
        mod_id = "mod_1"
      ),
      "Send and Receive 2" = mod_com_test(
        choices = c("a", "b", "c"),
        message = "The other module has selected",
        value = mm_dispatch("module_output", "mod_1"),
        mod_id = "mod_2"
      ),
      "Simple" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "modSimp")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}


####### Mixing communication and url
table_UI <- function(id) { # nolint
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

mod_table <- function(dataset, mod_id) {
  mod <- list(
    ui = function(id) {
      table_UI(id)
    },
    server = function(afmm) {
      table_server(id = mod_id, dataset = mm_resolve_dispatcher(dataset, afmm, flatten = TRUE))
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
        dataset = mm_dispatch("filtered_dataset", "adae")
      ),
      "PAPO" = mod_url_reader(
        text = mm_dispatch("module_output", "mod_1"),
        init_text = mm_dispatch("url_parameters", "subjid"),
        dataset = mm_dispatch("filtered_dataset", "adsl"),
        mod_id = "mod_2"
      )
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

####### Exporting

add_123_UI <- function(id) { # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::verbatimTextOutput(ns("value")),
    shiny::selectInput(ns("add"), "Add:", 1:3),
    shiny::verbatimTextOutput(ns("result"))
  )
}

add_123_server <- function(id, value) {
  module <- function(input, output, session) {
    output$value <- shiny::renderText({
      value()
    })
    result <- shinymeta::metaReactive({
      as.numeric(shinymeta::..(value())) + as.numeric(shinymeta::..(input$add))
    })
    output$result <- shinymeta::metaRender(shiny::renderText, {
      shinymeta::..(result())
    })

    return(
      structure(result,
        code = output$result
      )
    )
  }
  shiny::moduleServer(id, module)
}

mod_add_123 <- function(value, module_id) {
  mod <- list(
    ui = add_123_UI,
    server = function(afmm) {
      add_123_server(module_id, mm_resolve_dispatcher(value, afmm, flatten = TRUE))
    },
    module_id = module_id
  )
  return(mod)
}

export_UI <- function(id) { # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("var"), "Select a variable", NULL),
    shiny::verbatimTextOutput(ns("Summary")),
    shiny::verbatimTextOutput(ns("code"))
  )
}

export_server <- function(id, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observeEvent(dataset(), {
        shiny::updateSelectInput(inputId = "var", choices = names(dataset()))
      })

      output$Summary <- shinymeta::metaRender(shiny::renderPrint, { # nolint
        unique(shinymeta::..(dataset())[[shinymeta::..(input$var)]])
      })
      output$code <- shiny::renderPrint({
        shinymeta::expandChain(output$Summary())
      })

      return(
        structure(
          list(),
          code = output$Summary
        )
      )
    }
  )
}

export_server_2 <- function(id, dataset) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observeEvent(dataset(), {
        shiny::updateSelectInput(inputId = "var", choices = names(dataset()))
      })

      output$Summary <- shiny::renderPrint({ # nolint
        unique(dataset()[[input$var]])
      })

      input_dataset <- shinymeta::metaReactive({
        shinymeta::..(dataset())
      })

      code <- shinymeta::metaReactive2({
        shinymeta::metaExpr({
          unique(shinymeta::..(input_dataset())[[shinymeta::..(input$var)]])
        })
      })

      output$code <- shiny::renderPrint({
        shinymeta::expandChain(code())
      })

      return(
        structure(
          list(),
          code = code
        )
      )
    }
  )
}


mod_export <- function(dataset, module_id) {
  mod <- list(
    ui = export_UI,
    server = function(afmm) {
      export_server(module_id, mm_resolve_dispatcher(dataset, afmm, flatten = TRUE))
    },
    module_id = module_id
  )
  mod
}

mod_export_2 <- function(dataset, module_id) {
  mod <- list(
    ui = export_UI,
    server = function(afmm) {
      export_server_2(module_id, mm_resolve_dispatcher(dataset, afmm, flatten = TRUE))
    },
    module_id = module_id
  )
  mod
}

run_mock_export_app <- function() { # nolint
  get_d1 <- function() {
    list(
      adsl = get_pharmaverse_data("adsl"),
      adae = get_pharmaverse_data("adae")
    )
  }

  d1 <- structure(
    get_d1(),
    code = rlang::expr(!!get_d1)
  )

  run_app(
    data = list("d1" = d1),
    module_list = list(
      "Simple" = mod_export(mm_dispatch("filtered_dataset", "adsl"), "mod1"),
      "Simple_2" = mod_export_2(mm_dispatch("unfiltered_dataset", "adae"), "mod2"),
      "Simple_3" = mod_export_2(mm_dispatch("filtered_dataset", "adae"), "mod3"),
      "Identity_mod" = mod_identity(shiny::reactive(10), "mod_iden"),
      "Adder Identity" = mod_add_123(mm_dispatch("module_output", "mod_iden"), "mod_add_identity"),
      "Adder" = mod_add_123(shiny::reactive(1), "mod_add"),
      "URL Adder" = mod_add_123(mm_dispatch("url_parameters", "add_val"), "mod_add_url")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

########### Accessing dataset name

dataset_name_UI <- function(id) { # nolint
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

switch_UI <- function(id, name) { # nolint
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

mod_switch <- function(name, selected, switch_func, module_id) {
  mod <- list(
    ui = function(module_id) {
      switch_UI(module_id, name)
    },
    server = function(afmm) {
      switch_server(module_id, selected, mm_resolve_dispatcher(switch_func, afmm, flatten = TRUE))
    },
    module_id = module_id
  )
  mod
}

run_mock_switch_app <- function() {
  run_app(
    data = list("D1" = list(adsl = get_pharmaverse_data("adsl"))),
    module_list = list(
      "Mod 1" = mod_switch("Mod 1", "Mod 2", mm_dispatch("utils", "switch2"), "mod1"),
      "Mod 2" = mod_switch("Mod 2", "Mod 1", mm_dispatch("utils", "switch2"), "mod2")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

##### Module testing afmm

printer_UI <- function(id) { # nolint
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
        value = "mod_2",
        mod_id = "mod_1"
      ),
      "Send and Receive 2" = mod_com_test(
        choices = c("a", "b", "c"),
        message = "The other module has selected",
        value = "mod_1",
        mod_id = "mod_2"
      ),
      "Simple" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "modSimp"),
      "afmm" = mod_print_afmm("mod_afmm")
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

#### MESSAGE WITH IMPLICIT FUNCTION

msg_impl_UI <- function(id) { # nolint
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

mod_dataset_name_date_UI <- function(id) { # nolint
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
          "dataset_name:", val[[1]](),
          "; dataset_date_range:", val[[2]]()[[1]], val[[2]]()[[2]],
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

button_UI_css <- function(id) { # nolint
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

button_UI_no_css <- function(id) { # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("button"), "I should be black", class = "mybutton")
  )
}

empty_server <- function(id) {
  mod <- function(input, output, session) {

  }
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

########### Simple module

#' @describeIn mod_simple
#' Module UI
#'
#' @param id shiny id
#'
#' @export
simple_UI <- function(id) { # nolint
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
      output$text <- shinymeta::metaRender(
        shiny::renderText,
        {
          log_inform(paste(nrow(dataset())))
          nrow(shinymeta::..(dataset()))
        }
      )

      # nolint start
      # output$code <- shiny::renderPrint({
      #   shinymeta::expandChain(output$text())
      # })
      # nolint end

      return(structure(list(),
        code = output$text
      ))
    }
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
#' @export
mod_simple2 <- function(dataset_name, module_id) {
  mod <- list(
    ui = simple_UI,
    server = function(afmm) {
      simple_server(module_id, shiny::reactive(afmm[["filtered_dataset"]]()[[dataset_name]]))
    },
    module_id = module_id,
    meta = list(dataset_info = list(all = dataset_name))
  )
  mod
}
