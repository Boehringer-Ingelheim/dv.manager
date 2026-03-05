# How does it work (Technic/Develop)

**dv.manager** has three main goals:

1.  Act as a front end container for **dv.filter** and the modules
    included in the app
2.  Dispatch data from the loaded datasets to each of the modules
    (either filtered or unfiltered)
3.  Manage communication between modules

## Anatomy of DaVinci Modules

Opposed to pure Shiny modules, DaVinci modules are defined in a single
call such as the one created in the main example of this documentation.

``` r

data <- list(adsl = pharmaverseadam::adsl, adae = pharmaverseadam::adae)

module_list <- list(
  "My First listing" = dv.listings::mod_listings(
    dataset_names = "adsl",
    module_id = "mod1"
  )
)

dv.manager::run_app(
  data = list("DS" = data),
  module_list = module_list,
  filter_data = "adsl"
)
```

These modules are simple wrappers around pure Shiny modules that return
a list containing:

1.  A UI function with a single argument, its shiny id.
2.  A function with a single parameter that will call the server
    function.
3.  A module ID, that will act in the same way as the ID in pure [shiny
    modules](https://shiny.rstudio.com/articles/modules.html).

See below an example with a simple module

``` r

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

mod_table <- function(dataset, mod_id, table_name) {
  mod <- list(
    ui = function(id) {
      table_UI(id)
    },
    server = function(afmm) {
      table_server(
        id = mod_id,
        dataset = shiny::reactive(afmm[["filtered_dataset_list"]]())[[table_name]]
      )
    },
    module_id = mod_id
  )
  mod
}
```

#### An explanation of the code above:

The most relevant part of this module is the server entry of the list of
returned by the module function: a function with a single argument. See
[`vignette("arguments_from_module_manager")`](../articles/arguments_from_module_manager.md)
for a longer explanation on this topic.
