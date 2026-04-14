app_info_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  ..t$add_period(ns("app_ui"), TRUE)
  on.exit(..t$add_period(ns("app_ui"), FALSE))
  shiny::tagList(
    shiny::h1("Mem Usage"),
    shiny::h2("Objects"),
    shiny::actionButton(ns("refresh_object_mem_size"), label = "Refresh"),
    shiny::verbatimTextOutput(ns("object_mem_size")),
    shiny::h2("R session"),
    shiny::actionButton(ns("refresh_session_mem_size"), label = "Refresh"),
    shiny::verbatimTextOutput(ns("r_session_mem_size")),
    shiny::h1("Server init time"),
    shiny::actionButton(ns("refresh_server_init"), label = "Refresh"),
    shiny::div(id = ns("server_init_time_detail"), class = "mt-1"),
    shiny::div(id = ns("server_init_time"), class = "mt-1"),
    shiny::h1("devtools::session_info"),
    shiny::actionButton(ns("refresh_session_info"), label = "Refresh"),
    shiny::verbatimTextOutput(ns("session_info"))
  )
}

app_info_server <- function(id, afmm) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::setBookmarkExclude(c(
        "refresh_session_info",
        "refresh_server_init",
        "refresh_object_mem_size",
        "refresh_session_mem_size"
      ))

      # output[["object_mem_size"]] <- shiny::renderPrint({
      #   ..t$add_period("object_mem_size", TRUE)
      #   on.exit(..t$add_period("object_mem_size", FALSE))
      #   shiny::req(input[["refresh_object_mem_size"]] > 0)
      #   shiny::validate(
      #     shiny::need(requireNamespace("lobstr", quietly = TRUE), "lobstr package is required to see memory usage")
      #   )

      #   sizes <- shiny::isolate({
      #     n_afmm <- names(afmm)
      #     arg_list <- list()
      #     idx <- 1
      #     list_nm <- character(0)
      #     for (nm in n_afmm) {
      #       if (shiny::is.reactive(afmm[[nm]])) {
      #         list_nm[[idx]] <- paste0(nm, "(resolved)")
      #         arg_list[[idx]] <- afmm[[nm]]()
      #       } else {
      #         list_nm[[idx]] <- paste0(nm, "(not_reactive)")
      #         arg_list[[idx]] <- afmm[[nm]]
      #       }

      #       if (
      #         inherits(
      #           try(
      #             lobstr::obj_sizes(arg_list[[idx]]),
      #             silent = TRUE
      #           ),

      #           "try-error"
      #         )
      #       ) {
      #         arg_list[[idx]] <- character(0)
      #         list_nm[[idx]] <- paste0(list_nm[[idx]], "(not_reactive)(ERROR ESTIMATING SIZE)")
      #       }
      #       idx <- idx + 1
      #     }

      #     sizes <- do.call(lobstr::obj_sizes, arg_list)
      #     names(sizes) <- list_nm
      #     sizes
      #   })
      #   sizes
      # })

      # output[["r_session_mem_size"]] <- shiny::renderPrint({
      #   shiny::req(input[["refresh_session_mem_size"]] > 0)
      #   ..t$add_period("mem_used", TRUE)
      #   on.exit(..t$add_period("mem_used", FALSE))
      #   lobstr::mem_used()
      # })

      shiny::observeEvent(list(input[["refresh_server_init"]]), {
        shiny::req(input[["refresh_server_init"]] > 0)
        df <- ..t$time_list_as_df()
        browser()
        session_mask <- df["session"] == shiny::getDefaultReactiveDomain()[["token"]] | df["session"] == ..t$DUMMY_TOKEN
        df <- df[session_mask, , drop = FALSE]
        msg <- list(
          id = session[["ns"]]("server_init_time"),
          data = df
        )
        session[["sendCustomMessage"]](
          "dv_manager_draw_flame_graph",
          msg
        )
      })

      # output[["session_info"]] <- shiny::renderPrint({
      #   ..t$add_period("session_info", TRUE)
      #   on.exit(..t$add_period("session_info", FALSE))
      #   shiny::validate(
      #     shiny::need(requireNamespace("devtools", quietly = TRUE), "devtools package is required to see session info")
      #   )
      #   shiny::req(input[["refresh_session_info"]] > 0)
      #   devtools::session_info()
      # })
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

..t <- list(
  add_period = function(...) {},
  add_event = function(...) {},
  get_members = function(...) {},
  time_list_to_df = function(...) {},
  DUMMY_TOKEN = ""
)
