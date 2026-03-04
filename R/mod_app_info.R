app_info_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Mem Usage"),
    shiny::h2("Objects"),
    shiny::verbatimTextOutput(ns("object_mem_size")),
    shiny::h2("R session"),
    shiny::verbatimTextOutput(ns("r_session_mem_size")),
    shiny::h1("devtools::session_info"),
    shiny::actionButton(ns("refresh_session_info"), label = "Refresh"),
    shiny::verbatimTextOutput(ns("session_info"))
  )
}

app_info_server <- function(id, afmm) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::setBookmarkExclude(c("refresh_session_info"))

      output[["object_mem_size"]] <- shiny::renderPrint({
        shiny::validate(
          shiny::need(requireNamespace("lobstr"), "lobstr package is required to see memory usage")
        )

        n_afmm <- names(afmm)
        arg_list <- list()
        idx <- 1
        list_nm <- character(0)
        for (nm in n_afmm) {
          if (shiny::is.reactive(afmm[[nm]])) {
            list_nm[[idx]] <- paste0(nm, "(resolved)")
            arg_list[[idx]] <- afmm[[nm]]()
            idx <- idx + 1
          } else {
            list_nm[[idx]] <- paste0(nm, "(not_reactive)")
            arg_list[[idx]] <- afmm[[nm]]
            idx <- idx + 1
          }
        }

        sizes <- do.call(lobstr::obj_sizes, arg_list)
        names(sizes) <- list_nm
        sizes
      })

      output[["r_session_mem_size"]] <- shiny::renderPrint({
        lobstr::mem_used()
      })

      shiny::observeEvent(input[["switch_to_target"]], {
        afmm[["utils"]][["switch2mod"]](input[["target_id"]])
      })

      output[["test_text"]] <- shiny::renderText("test")
      output[["test_counter"]] <- shiny::renderText(filter_counter())
      output[["session_info"]] <- shiny::renderPrint({
        shiny::validate(
          shiny::need(requireNamespace("devtools", quietly = TRUE), "devtools package is required to see session info")
        )
        input[["refresh_session_info"]]
        devtools::session_info()
      })
      return(NULL)
    }
  )
}


#' A module to inspect the app information
#' @param mod_id Shiny module id. Default: ".._app_info.."
#' @export

mod_app_info <- function(mod_id = ".._app_info..") {
  list(
    ui = app_info_UI,
    server = function(afmm) {
      app_info_server(id = mod_id, afmm = afmm)
    },
    module_id = mod_id
  )
}
