EEF <- local({
  ID_SHOW_UI <- "EEF_show_ui"

  app_creator_feedback_ui <- function(id, ui) {
    id <- paste(c(id, "validator"), collapse = "-")
    ns <- shiny::NS(id)

    hide <- function(e) shiny::tags[["div"]](e, style = "display: none")

    res <- list(
      shiny::uiOutput(ns("ui")),
      hide(shiny::checkboxInput(inputId = ns(ID_SHOW_UI), label = NULL)),
      shiny::conditionalPanel(
        condition = sprintf("input.%s == true", ID_SHOW_UI),
        ui,
        ns = ns
      )
    )
    return(res)
  }

  app_creator_feedback_server <- function(id, error_messages) {
    id <- paste(c(id, "validator"), collapse = "-")
    module <- shiny::moduleServer(
      id,
      function(input, output, session) {
        output[["ui"]] <- shiny::renderUI({
          res <- list()

          if (length(error_messages)) {
            app_creator_disclaimer <- htmltools::p(htmltools::HTML(
              paste("<i>Configuration errors prevent this module from running.",
                    "<b>The following diagnostic information is meant for the app creator</b>.</i>")
            ), style = "font-size: small;")

            message_well <- function(title, contents, color = "f5f5f5") {
              style <- sprintf(
                paste0(
                  "padding: 0.5rem;",
                  "padding-left: 1rem;",
                  "margin-bottom: 20px;",
                  "background-color: %s;",
                  "border: 1px solid #e3e3e3;",
                  "border-radius: 4px;",
                  "-webkit-box-shadow: inset 0 1px 1px rgba(0,0,0,.05);",
                  "box-shadow: inset 0 1px 1px rgba(0,0,0,.05);"
                ),
                color
              )

              res <- list(shiny::h3(title))
              if (length(contents)) res <- append(res, list(shiny::tags[["div"]](contents, style = style)))
              return(res)
            }

            error_messages <- append(list(app_creator_disclaimer), error_messages)
            res[[length(res) + 1]] <- message_well("Module configuration errors", error_messages, color = "#f4d7d7")
          }

          return(res)
        })
        shiny::outputOptions(output, "ui", suspendWhenHidden = FALSE)

        if (length(error_messages) == 0) {
          shiny::updateCheckboxInput(inputId = ID_SHOW_UI, value = TRUE)
        }
      }
    )

    return(module)
  }

  run_check_mod_fn <- function(check_mod_fn, afmm) {
    res <- NULL

    dl_count <- length(afmm[["data"]])        # NOTE: `dl_*` stands for dataset_list_*

    res_by_dl <- list()
    error_count_by_dl <- integer(0)
    error_count <- 0
    for (i_dl in seq_len(dl_count)) {
      dl <- afmm[["data"]][[i_dl]]
      if (is.function(dl)) dl <- dl()
      error_messages <- check_mod_fn(afmm, dl)
      res_by_dl[[i_dl]] <- error_messages
      error_count_by_dl[[i_dl]] <- length(error_messages)
      error_count <- error_count + length(error_messages)
    }

    as_items <- function(x) htmltools::p(htmltools::HTML(paste("\u2022", x)))

    if (error_count == 0) NULL
    else if (dl_count == 1) {
      # single dataset_list
      res <- Map(as_items, res_by_dl[[1]], USE.NAMES = FALSE)
    } else {
      # multiple dataset_lists
      dl_names <- names(afmm[["data"]])

      errors <- list(htmltools::p(htmltools::HTML(
        "Issues have been grouped by input dataset list. Expand/collapse the elements below to inspect them:"
      )))

      details_collapse_expand_status <- "open"
      for (i_dl in seq_len(dl_count)){
        if (error_count_by_dl[[i_dl]] == 0) next

        errors <- c(
          errors,
          list(
            htmltools::HTML(paste0(
              sprintf('<details %s><summary style="display:list-item"><b>%s</b></summary>',
                      details_collapse_expand_status, dl_names[[i_dl]]),
              "<div style='padding: 0.5rem; margin-bottom: 1rem; background-color: #FFFFFF55;",
              "border: 1px solid #AAAAAA; border-radius: 4px;'>"
            ))
          ),
          Map(as_items, res_by_dl[[i_dl]], USE.NAMES = FALSE),
          list(htmltools::HTML("</div></details>"))
        )

        details_collapse_expand_status <- "" # collapse all but first
      }
      res <- errors
    }

    return(res)
  }

  return(
    list(
      app_creator_feedback_ui = app_creator_feedback_ui,
      app_creator_feedback_server = app_creator_feedback_server,
      run_check_mod_fn = run_check_mod_fn
    )
  )
})
