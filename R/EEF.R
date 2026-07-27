# TODO: Transform afmm in a safe list that is declared fully in run app or in a constructor function, that way all names should be aligned from the beginning and we minimize
# possible errors

EEF_collect <- function(eef_errors, module_info, afmm_static, dataset_list, dataset_list_name) {
  for (nm in names(afmm_static[["module_names"]])) {
    check_mod_fn <- module_info[["meta"]][[nm]][["check_mod_fn"]]
    if (!is.null(check_mod_fn)) {
      log_inform(paste0("------- (S) Running checker for `", nm, "` and dataset list", dataset_list_name, " ---------"))
      eef_errors[[nm]][[dataset_list_name]] <- check_mod_fn(afmm_static, dataset_list, dataset_list_name)
      log_inform(paste0("------- (E) Running checker for `", nm, "` and dataset list", dataset_list_name, " ---------"))
    } else {
      log_inform(paste0("No checker found for `", nm, "`"))
    }
  }
  eef_errors
}

EEF_report <- local({
  bold_html <- function(s) {
    s <- gsub("<b>", "\033[1m", s)
    s <- gsub("</b>", "\033[22m", s)
    # strip any remaining tags
    gsub("<[^>]+>", "", s)
  }

  errors_to_console <- function(errors_by_dl, module_id) {
    parts <- character(0)

    for (dl in names(errors_by_dl)) {
      msgs <- errors_by_dl[[dl]]
      parts <- c(parts, paste0("Dataset: <b>`", dl, "`</b>\n  - ", paste(msgs, collapse = "\n  - ")))
    }
    parts <- paste(parts, collapse = "\n\n")

    bold_html(paste0(
      "EEF errors found for module with id: <b>`",
      module_id,
      "`</b>\n",
      parts
    ))
  }

  errors_to_html <- function(errors_by_dl, module_id) {
    dl_count <- length(errors_by_dl)

    as_items <- function(x) htmltools::p(htmltools::HTML(paste("\u2022", x)))

    res <- list()
    app_creator_disclaimer <- htmltools::p(
      htmltools::HTML(
        paste(
          "<i>Configuration errors prevent the module with id <b>`",
          module_id,
          "`</b> from running.",
          "<b>The following diagnostic information is meant for the app creator</b>.</i>"
        )
      ),
      style = "font-size: small;"
    )

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
      if (length(contents)) {
        res <- append(res, list(shiny::tags[["div"]](contents, style = style)))
      }
      return(res)
    }

    if (dl_count == 1) {
      # single dataset_list
      error_messages <- Map(as_items, errors_by_dl[[1]], USE.NAMES = FALSE)
    } else {
      # multiple dataset_lists
      dl_names <- names(errors_by_dl)

      error_messages <- list(htmltools::p(htmltools::HTML(
        "Issues have been grouped by input dataset list. Expand/collapse the elements below to inspect them:"
      )))

      details_collapse_expand_status <- "open"
      for (i_dl in seq_len(dl_count)) {
        if (length(errors_by_dl[[i_dl]]) == 0) {
          next
        }

        error_messages <- c(
          error_messages,
          list(
            htmltools::HTML(paste0(
              sprintf(
                '<details %s><summary style="display:list-item"><b>%s</b></summary>',
                details_collapse_expand_status,
                dl_names[[i_dl]]
              ),
              "<div style='padding: 0.5rem; margin-bottom: 1rem; background-color: #FFFFFF55;",
              "border: 1px solid #AAAAAA; border-radius: 4px;'>"
            ))
          ),
          Map(as_items, errors_by_dl[[i_dl]], USE.NAMES = FALSE),
          list(htmltools::HTML("</div></details>"))
        )

        details_collapse_expand_status <- "" # collapse all but first
      }
    }
    error_messages <- append(list(app_creator_disclaimer), error_messages)
    res[[length(res) + 1]] <- message_well("Module configuration errors", error_messages, color = "#f4d7d7")

    return(res)
  }

  EEF_report <- function(module_info, eef_errors_by_mod_and_dl) {
    for (idx in seq_along(eef_errors_by_mod_and_dl)) {
      mod_id <- names(eef_errors_by_mod_and_dl)[[idx]]
      dl_errors <- eef_errors_by_mod_and_dl[[idx]]
      error_count <- sum(unlist(sapply(dl_errors, length)))
      if (error_count > 0) {
        # Replace uis and servers
        log_warn(errors_to_console(dl_errors, mod_id))
        module_info[["ui"]][[mod_id]] <- local({
          local_mod_id <- mod_id
          local_dl_errors <- dl_errors
          function(...) {
            errors_to_html(local_dl_errors, local_mod_id)
          }
        })
        module_info[["server"]][[mod_id]] <- function(...) {}
      } else {
        log_inform(paste0("No EEF errors found for `", mod_id, "`"))
      }
    }
    return(module_info)
  }

  EEF_report
})
