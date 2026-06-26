check_EEF <- local({
  # TODO: Transform afmm in a safe list that is declared fully in run app or in a constructor function, that way all names should be aligned from the beginning and we minimize
  # possible errors

  run_check_mod_fn <- function(check_mod_fn, afmm) {
    dl_count <- length(afmm[["data"]]) # NOTE: `dl_*` stands for dataset_list_*

    errors_by_dl <- vector(mode = "list", length = dl_count)
    error_count_by_dl <- integer(0)
    error_count <- 0
    for (i_dl in seq_len(dl_count)) {
      dl <- afmm[["data"]][[i_dl]]
      if (is.function(dl)) {
        dl <- dl()
      }
      # TODO: Unsure of this signature, we break the fact that the check_mod_fn requires afmm
      # Maybe afmm should not be the proper and element passed and breakin into afmm_static and reactive is not necessary
      # Maybe just checking agains the config?

      error_messages <- check_mod_fn(afmm, dl)
      errors_by_dl[[i_dl]] <- error_messages
      error_count_by_dl[[i_dl]] <- length(error_messages)
      error_count <- error_count + length(error_messages)
    }

    names(errors_by_dl) <- names(afmm[["data"]])

    list(
      error_count = error_count,
      errors_by_dl = errors_by_dl,
      error_count_by_dl = error_count_by_dl
    )
  }

  errors_to_html <- function(errors, module_id) {
    error_count <- errors[["error_count"]]
    errors_by_dl <- errors[["errors_by_dl"]]
    error_count_by_dl <- errors[["error_count_by_dl"]]
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
        if (error_count_by_dl[[i_dl]] == 0) {
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

  bold_html <- function(s) {
    s <- gsub("<b>", "\033[1m", s)
    s <- gsub("</b>", "\033[22m", s)
    # strip any remaining tags
    gsub("<[^>]+>", "", s)
  }

  errors_to_console <- function(errors, module_id) {
    parts <- character(0)
    for (dl in names(errors$errors_by_dl)) {
      msgs <- errors$errors_by_dl[[dl]]
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

  check_EEF <- function(module_info, afmm_static) {
    errors <- list()
    for (nm in names(afmm_static[["module_names"]])) {
      check_mod_fn <- module_info[["meta"]][[nm]][["check_mod_fn"]]
      if (!is.null(check_mod_fn)) {
        log_inform(paste0("Running checker for `", nm, "`"))
        log_inform(paste0("------- (S) Running checker for `", nm, "` ---------"))
        errors[[nm]] <- run_check_mod_fn(check_mod_fn = check_mod_fn, afmm = afmm_static)
        if (errors[[nm]][["error_count"]] > 0) {
          # Replace uis and servers
          log_warn(errors_to_console(errors[[nm]], nm))
          module_info[["ui"]][[nm]] <- local({
            local_nm <- nm
            function(...) {
              errors_to_html(errors[[local_nm]], local_nm)
            }
          })
          module_info[["server"]][[nm]] <- function(...) {}
        } else {
          log_inform(paste0("No EEF errors found for `", nm, "`"))
        }
        log_inform(paste0("------- (E) Running checker for `", nm, "` ---------"))
      } else {
        log_inform(paste0("No checker found for `", nm, "`"))
      }
    }
    module_info
  }

  check_EEF
})
