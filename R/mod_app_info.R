app_info_UI <- function(id) {
  # nolint
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Mem Usage"),
    shiny::h2("Objects"),
    shiny::verbatimTextOutput(ns("object_mem_size")),
    shiny::h2("R session"),
    shiny::verbatimTextOutput(ns("r_session_mem_size")),
    shiny::h1("Server init time"),
    shiny::actionButton(ns("refresh_server_init"), label = "Refresh"),
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
      shiny::setBookmarkExclude(c("refresh_session_info"))

      output[["object_mem_size"]] <- shiny::renderPrint({
        shiny::validate(
          shiny::need(requireNamespace("lobstr", quietly = TRUE), "lobstr package is required to see memory usage")
        )

        n_afmm <- names(afmm)
        arg_list <- list()
        idx <- 1
        list_nm <- character(0)
        for (nm in n_afmm) {
          if (shiny::is.reactive(afmm[[nm]])) {
            list_nm[[idx]] <- paste0(nm, "(resolved)")
            arg_list[[idx]] <- afmm[[nm]]()
          } else {
            list_nm[[idx]] <- paste0(nm, "(not_reactive)")
            arg_list[[idx]] <- afmm[[nm]]
          }

          if (
            inherits(
              try(
                lobstr::obj_sizes(arg_list[[idx]]),
                silent = TRUE
              ),

              "try-error"
            )
          ) {
            arg_list[[idx]] <- character(0)
            list_nm[[idx]] <- paste0(list_nm[[idx]], "(not_reactive)(ERROR ESTIMATING SIZE)")
          }
          idx <- idx + 1
        }

        sizes <- do.call(lobstr::obj_sizes, arg_list)
        names(sizes) <- list_nm
        sizes
      })

      output[["r_session_mem_size"]] <- shiny::renderPrint({
        lobstr::mem_used()
      })

      shiny::observeEvent(list(input[["refresh_server_init"]], ..timing..get_time_cont()), {
        df <- time_env_to_df(..timing..get_time_cont())
        msg <- list(
          id = session[["ns"]]("server_init_time"),
          data = df
        )
        session[["sendCustomMessage"]](
          "dv_manager_draw_flame_graph",
          msg
        )
      })

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


##### TIMING

time_env_to_df <- function(time_env) {
  overflow <- time_env[["period_idx"]] > 1e6
  max_idx <- ceiling(time_env[["period_idx"]] - 1) / 2
  incomplete_tags <- sum(time_env[["period_start"]][time_env[["period_idx"]] - 1]) !=
    sum(!time_env[["period_start"]][time_env[["period_idx"]] - 1])

  start_vec <- time_env[["period_start"]][seq_len(time_env[["period_idx"]] - 1)]
  stack_depth <- integer(max_idx)
  st_stack <- integer(max_idx)
  st_idx <- integer(max_idx)
  et_idx <- integer(max_idx)

  st_et_ptr <- 0
  st_stack_ptr <- 0

  for (vec_idx in seq_along(start_vec)) {
    cs <- start_vec[vec_idx]
    if (cs) {
      # push in the stack
      st_stack_ptr <- st_stack_ptr + 1
      st_stack[[st_stack_ptr]] <- vec_idx
    } else {
      st_et_ptr <- st_et_ptr + 1
      st_idx[[st_et_ptr]] <- st_stack[[st_stack_ptr]]
      stack_depth[[st_et_ptr]] <- st_stack_ptr
      st_stack_ptr <- st_stack_ptr - 1
      et_idx[[st_et_ptr]] <- vec_idx
    }
  }
  origin <- as.numeric(min(time_env[["period_time"]][st_idx]))

  df <- data.frame(
    label_st = time_env[["period_label"]][st_idx],
    label_et = time_env[["period_label"]][et_idx],
    st = as.numeric(time_env[["period_time"]][st_idx]) - origin,
    et = as.numeric(time_env[["period_time"]][et_idx]) - origin,
    depth = factor(stack_depth),
    imputed = FALSE
  )
  df <- df[order(df[["st"]]), , drop = FALSE]
  df[["duration"]] <- df[["et"]] - df[["st"]]
  yyjsonr::write_json_file(df, "data.json")
  return(df)
}

# This could be lists with the correct closures
MAX_IDX <- 1e6
TIMING_OPTION <- "dv.manager.timing"

if (!isFALSE(getOption(TIMING_OPTION))) {
  times_container <- new.env(parent = emptyenv()) # Consider

  ..timing..init_time_cont <- function(session_token = shiny::getDefaultReactiveDomain()[["token"]]) {
    if (!is.null(session_token)) {
      times_container[[session_token]] <- new.env(parent = emptyenv())
      times_container[[session_token]][["event_time"]] <- .POSIXct(numeric(MAX_IDX))
      times_container[[session_token]][["event_label"]] <- vector(mode = "character", length = MAX_IDX)
      times_container[[session_token]][["event_idx"]] <- 1L # Next idx

      times_container[[session_token]][["period_time"]] <- .POSIXct(numeric(MAX_IDX))
      times_container[[session_token]][["period_label"]] <- vector(mode = "character", length = MAX_IDX)
      times_container[[session_token]][["period_start"]] <- rep(as.logical(NA), MAX_IDX)
      times_container[[session_token]][["period_idx"]] <- 1L
    }
  }

  ..timing..dispose_time_cont <- function(session_token = shiny::getDefaultReactiveDomain()[["token"]]) {
    if (!is.null(session_token)) {
      times_container[[session_token]] <- NULL
    }
  }

  ..timing..get_time_cont <- function(session_token = shiny::getDefaultReactiveDomain()[["token"]]) {
    if (!is.null(session_token)) {
      times_container[[session_token]]
    }
  }

  ..timing..add_period <- function(label, start, session_token = shiny::getDefaultReactiveDomain()[["token"]]) {
    if (!is.null(session_token)) {
      next_idx <- times_container[[session_token]][["period_idx"]]
      if (next_idx > 1e6) {
        message("Period timings container is full. No more times will be recorded")
        return()
      }
      times_container[[session_token]][["period_time"]][[next_idx]] <- Sys.time()
      times_container[[session_token]][["period_label"]][[next_idx]] <- label
      times_container[[session_token]][["period_start"]][[next_idx]] <- start
      times_container[[session_token]][["period_idx"]] <- next_idx + 1
    }
  }

  ..timing..add_event <- function(label, session_token = shiny::getDefaultReactiveDomain()[["token"]]) {
    if (!is.null(session_token)) {
      next_idx <- times[[session_token]][["event_idx"]]
      if (next_idx > 1e6) {
        message("Event timings container is full. No more times will be recorded")
        return()
      }
      times_container[[session_token]][["event_time"]][[next_idx]] <- Sys.time()
      times_container[[session_token]][["event_label"]][[next_idx]] <- label
      times_container[[session_token]][["event_idx"]] <- next_idx + 1
    }
  }
} else {
  times_container <- NULL

  ..timing..init_time_cont <- function(session_token = NULL) {}

  ..timing..dispose_time_cont <- function(session_token = NULL) {}

  ..timing..get_time_cont <- function(session_token = NULL) {
    NULL
  }

  ..timing..add_period <- function(label, start, session_token = NULL) {}

  ..timing..add_event <- function(label, session_token = NULL) {}
}
