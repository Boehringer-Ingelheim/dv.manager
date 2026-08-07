# This file contains all export related code. Remove it if required.

#' Render an export Rmd to a zip file
#'
#' Runs in a fresh `callr::r()` subprocess (or can be called directly, e.g. in
#' tests) — everything needed must arrive as an argument, nothing carries over
#' from the caller's session.
#'
#' @param export_rmd Path to the .Rmd file to render.
#' @param export_dir Directory containing `export_rmd`; becomes the working
#'   directory, and everything left in it ends up in the zip.
#' @param header_file Path to a file (e.g. `header.tex`) copied into
#'   `export_dir` before rendering.
#' @param pdf_attach_function `function(pdf, attachment)` used to attach files
#'   to a rendered PDF. Injected so it can be tested independently.
#' @param filename Path the resulting zip file is written to.
#' @return `filename`, with an `error_msg` attribute (character(0) on success,
#'   the error message on failure).
#' @keywords internal
render_export_document <- function(export_rmd, export_dir, header_file, pdf_attach_function, filename) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(export_dir)

  error_msg <- character(0)
  tryCatch(
    {
      file.copy(header_file, ".")
      output_file <- rmarkdown::render(input = export_rmd, output_dir = export_dir)
      if (endsWith(output_file, "pdf")) {
        session_info_file <- "session_info.txt"
        writeLines(capture.output(devtools::session_info()), session_info_file)
        pdf_attach_function(output_file, export_rmd)
        pdf_attach_function(output_file, session_info_file)
        unlink(session_info_file)
      }
    },
    error = function(e) {
      error_msg <<- e$message
      warning(sprintf("Error rendering export in %s\n%s", export_dir, error_msg))
      writeLines(c("Error rendering export", error_msg), file.path(export_dir, "error.txt"))
    }
  )

  zip_filename <- utils::zip(filename, list.files(export_dir))
  structure(zip_filename, error_msg = error_msg)
}

#' Attach a file to a PDF in place
#'
#' @param pdf Path to an existing PDF, overwritten in place. Requires qpdf on PATH.
#' @param attachment Path to the file to embed, keyed by its basename.
#' @return `pdf`, invisibly.
#' @keywords internal
pdf_attach <- function(pdf, attachment) {
  stopifnot(file.exists(pdf), file.exists(attachment))
  if (!nzchar(Sys.which("qpdf"))) {
    stop("qpdf command-line tool not found on PATH")
  }

  tmp <- tempfile(tmpdir = dirname(pdf), fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)

  status <- system2("qpdf", shQuote(c(pdf, "--add-attachment", attachment, "--", tmp)))
  if (status != 0) {
    stop("qpdf failed with status ", status)
  }
  if (!file.rename(tmp, pdf)) {
    stop("could not overwrite ", pdf)
  }

  invisible(pdf)
}

if (isTRUE(getOption("dv.export_enabled"))) {
  warning("Export has been enabled. This is an experimental feature.")
  # Code for exporting versions

  EXPORT <- local({
    EXPORT <- poc(
      ID = poc(
        EXPORT_CODE = "export_code",
        EXPORT_CODE_MENU = "export_code_menu",
        EXPORT_MENU_SELECTION = "export_menu_selection",
        OUTPUT_FORMAT = "output_format"
      ),
      ATTR = "export_element_type",
      ELEMENT_KIND = poc(
        ERROR = "error",
        TABLE = "table",
        DEFAULT = "default"
      ),
      OUTPUT_FORMAT = poc(
        HTML = "html",
        PDF = "pdf"
      ),
      MSG = poc(
        EXPORT_BUTTON = "Export",
        EXPORT_CUSTOM_BUTTON = "Select Exported Outputs"
      ),
      VAL = poc(
        EXPORT_ALL = "all",
        EXPORT_CURRENT = "current"
      )
    )

    EXPORT[["TEMPLATES"]] <- list()

    EXPORT[["TEMPLATES"]][["HEADER"]] <- local({
      templates <- character(0)
      templates[[EXPORT$OUTPUT_FORMAT$PDF]] <- r"--(
---
title: "A export"
author: "A user"
date: "`r format(Sys.Date(), '%B %d, %Y')`"
output:
  pdf_document:
    keep_tex: true
    toc: true
    toc_depth: 3
    number_sections: false
    df_print: kable
    latex_engine: xelatex
    pandoc_args: ["-V", "monofont:DejaVu Sans Mono"]
    extra_dependencies: ["pdflscape"]
    includes:
      in_header: header.tex
geometry: margin=2cm
papersize: a4
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  tidy = TRUE,
  fig.width = 10,
  fig.height = 6,
  out.width = "90%",
  dpi = 300,
  dev = "cairo_pdf"     # vector output, sharp text
)
```

)--"

      templates[[EXPORT$OUTPUT_FORMAT$HTML]] <- r"--(
---
title: "A export"
author: "A user"
date: "`r format(Sys.Date(), '%B %d, %Y')`"
output:
  html_document:
    code_folding: "hide"
    code_download: true
    toc: true
    toc_float: true
    toc_depth: 3
    number_sections: false
    df_print: paged    
    self_contained: true
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(
  out.width = "100%", 
  tidy = TRUE
)
```
        
```{css, echo=FALSE}
body::before {
  content: "UNVALIDATED CONTENT";
  position: fixed;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) rotate(-45deg);
  font-size: 8rem;
  font-weight: 700;
  color: rgba(0, 0, 0, 0.08);
  white-space: nowrap;
  pointer-events: none;   /* clicks/selection pass through */
  z-index: 9999;
  user-select: none;
}
        
@media print {
  body::before { position: fixed; }  /* most browsers repeat fixed bg per page */
}
```

)--"

      templates
    })

    EXPORT[["TEMPLATES"]][["SESSION_INFO"]] <- local({
      templates <- character(0)
      templates[[EXPORT$OUTPUT_FORMAT$PDF]] <- r"--(
\begin{landscape}
        
\section{Session Info}
        
\begin{verbatim}
        
```{r session_info, results = 'asis'}
  devtools::session_info()
```
\end{verbatim}        
\end{landscape}
)--"

      templates[[EXPORT$OUTPUT_FORMAT$HTML]] <- r"--(        
# Session Info
        
```{r session_info}
  devtools::session_info()
```

)--"

      templates
    })

    EXPORT[["TEMPLATES"]][["FOOTER"]] <- local({
      templates <- character(0)
      templates[[EXPORT$OUTPUT_FORMAT$PDF]] <- ""

      # Currently code is attached to the PDF file
      #   r"--(
      # # Annex: Code

      # ```{r show-code, ref.label = setdiff(knitr::all_labels(), c("setup", "show-code")), echo=TRUE, eval=FALSE}
      # ```
      # )--"

      templates[[EXPORT$OUTPUT_FORMAT$HTML]] <- ""

      templates
    })

    EXPORT
  })

  append_export_button <- function(x, ns) {
    log_inform("Attaching export button")

    input_id <- ns(EXPORT$ID$EXPORT_CODE_MENU)
    onclick_fmt <- "Shiny.setInputValue('%s', '%s', {priority: 'event'})"

    export_button <- local({
      t <- shiny::tags
      dd_div <- shiny::div(
        class = "btn-group",
        role = "group",
        t[["button"]](type = "button", class = "btn btn-default dropdown-toggle btn-sm", "data-bs-toggle" = "dropdown"),
        t[["ul"]](
          class = "dropdown-menu",
          t[["li"]](
            t[["a"]](class = "dropdown-item", href = "#", EXPORT$MSG$EXPORT_CUSTOM_BUTTON),
            onclick = sprintf(onclick_fmt, input_id, EXPORT$VAL$EXPORT_ALL)
          ),
        )
      )

      bg_d <- shiny::div(
        class = "btn-group",
        role = "group",
        t[["button"]](
          type = "button",
          class = "btn btn-default",
          EXPORT$MSG$EXPORT_BUTTON,
          onclick = sprintf(onclick_fmt, input_id, EXPORT$VAL$EXPORT_CURRENT)
        ),
        dd_div
      )

      bg_d
    })

    top_buttons <- c(
      x,
      list(export_button)
    )

    top_buttons
  }

  export_server_quote <- quote({
    local({
      log_inform("Running export server")
      # Flatten exportable elements and set defaults for missing entries
      exportable_elements <- local({
        res <- list()
        for (idx in seq_along(module_output)) {
          mo <- module_output[[idx]]

          if ("to_export" %in% names(mo)) {
            to_export_elements <- mo[["to_export"]]
            module_id <- names(module_output)[[idx]]
            module_name <- module_names[[module_id]]

            for (jdx in seq_along(to_export_elements)) {
              if (!checkmate::test_named(to_export_elements[[jdx]], type = "unique")) {
                stop(sprintf("Exported elements for module %s are not named or names are not unique", module_id))
              }

              current_element_list_nm <- names(to_export_elements)[[jdx]]
              current_element <- to_export_elements[[jdx]]

              first_module_element <- jdx == 1
              current_element[["label"]] <- current_element[["label"]] %||% current_element_list_nm
              current_element[["info"]] <- current_element[["info"]] %||% current_element[["label"]]
              current_element[["id"]] <- paste0(module_id, "-", current_element_list_nm)
              current_element[["is_first_module_element"]] <- first_module_element
              current_element[["module_name"]] <- module_name
              current_element[["module_id"]] <- module_id
              res[[current_element[["id"]]]] <- current_element
            }
          }
        }

        res
      })

      export_modal_ui <- function(show_tab = NA) {
        # It supports either NA -> Show all tabs
        # Or the id of one tab -> Show only that tab
        log_inform(paste("Showing", show_tab, "tab in menu"))

        card_ui <- list(shiny::h3("Export menu"))
        card_items <- NULL
        selected <- rep(FALSE, length(exportable_elements))
        names(selected) <- names(exportable_elements)

        for (idx in seq_along(exportable_elements)) {
          curr_el <- exportable_elements[[idx]]

          if (!is.na(show_tab) && curr_el[["module_id"]] != show_tab) {
            next
          }

          if (curr_el[["is_first_module_element"]]) {
            if (!is.null(card_items)) {
              card_ui[[length(card_ui) + 1]] <- do.call(bslib::card, card_items)
            }
          }

          if (curr_el[["is_first_module_element"]]) {
            card_items <- list(bslib::card_header(curr_el[["module_name"]]))
          }
          card_items[[length(card_items) + 1]] <-
            shiny::div(
              class = "form-check form-switch",
              shiny::tags[["label"]](
                class = "form-check-label",
                title = curr_el[["info"]],
                shiny::tags[["input"]](
                  class = "form-check-input",
                  type = "checkbox",
                  role = "switch",
                  checked = NA,
                  onchange = sprintf(
                    "Shiny.setInputValue('%s', {value: this.checked, id: '%s'});",
                    ns(EXPORT$ID$EXPORT_MENU_SELECTION),
                    curr_el[["id"]]
                  )
                ),
                curr_el[["label"]]
              )
            )

          selected[[curr_el[["id"]]]] <- TRUE
        }

        # Last card is done post loop
        if (!is.null(card_items)) {
          card_ui[[length(card_ui) + 1]] <- do.call(bslib::card, card_items)
        }

        if (length(card_ui) > 0) {
          card_ui[[length(card_ui) + 1]] <- bslib::card(
            bslib::card_header("Output format"),
            shiny::radioButtons(
              ns(EXPORT$ID$OUTPUT_FORMAT),
              label = NULL,
              choices = EXPORT$OUTPUT_FORMAT
            )
          )

          download_button <- shiny::downloadButton(ns(EXPORT$ID$EXPORT_CODE), "Export")
        } else {
          card_ui[[length(card_ui) + 1]] <- bslib::card(
            bslib::card_header("No elements available for export")
          )

          download_button <- NULL
        }

        res <- list(
          modal_dialog = shiny::modalDialog(
            shiny::div(
              class = "d-flex flex-column vh-25",
              style = "max-height: 90vh",
              shiny::div(
                class = "overflow-auto flex-grow-1 p-3 min-h-0",
                list(
                  card_ui
                )
              ),
              download_button
            ),
            easyClose = TRUE,
            footer = NULL
          ),
          selected = selected
        )
        res
      }

      shiny::observeEvent(input[[EXPORT$ID$EXPORT_MENU_SELECTION]], {
        is_output_selected_to_export[[input[[EXPORT$ID$EXPORT_MENU_SELECTION]][["id"]]]] <<- input[[
          EXPORT$ID$EXPORT_MENU_SELECTION
        ]][["value"]]
        log_inform(
          paste(
            "Selected outputs to export",
            paste(names(is_output_selected_to_export), is_output_selected_to_export, collapse = ", ")
          )
        )
      })

      is_output_selected_to_export <- NULL # TODO replace by reactiveValue so it can be used in the testServer?
      shiny::observeEvent(input[[EXPORT$ID$EXPORT_CODE_MENU]], {
        if (is.null(attr(selected_dataset_list(), "load_fn"))) {
          dataset_list_name <- attr(selected_dataset_list(), "dataset_list_name")
          user_msg <- sprintf("Current dataset list `%s` is not configured for exporting.", dataset_list_name)
          dev_msg <- sprintf("Export not possible for `%s`. No `load_fn` attribute found. ", dataset_list_name)
          log_warn(dev_msg)
          shiny::showNotification(user_msg, type = "error")
          shiny::req(FALSE)
        }

        visible_export_tabs <- NA

        if (input[[EXPORT$ID$EXPORT_CODE_MENU]] == EXPORT$VAL$EXPORT_CURRENT) {
          visible_export_tabs <- input[[ID$NAV_HEADER]]
        }

        x <- export_modal_ui(visible_export_tabs)
        is_output_selected_to_export <<- x[["selected"]]

        log_inform(
          paste(
            "Selected outputs to export by default",
            paste(names(is_output_selected_to_export), is_output_selected_to_export, collapse = ", ")
          )
        )

        shiny::showModal(x[["modal_dialog"]])
      })

      output[[EXPORT$ID$EXPORT_CODE]] <- shiny::downloadHandler(
        filename = "export.zip",
        content = function(filename) {
          shiny::withProgress(message = "Rendering export", expr = {
            output_format <- input[[EXPORT$ID$OUTPUT_FORMAT]]

            RATTR <- EXPORT$ATTR
            REK <- EXPORT$ELEMENT_KIND

            ec <- shiny::isolate({
              .ec <- shinymeta::newExpansionContext()
              fn <- body(attr(selected_dataset_list(), "load_fn"))
              dln <- attr(selected_dataset_list(), "dataset_list_name")
              .ec$substituteMetaReactive(selected_dataset_list, function() {
                shinymeta::metaExpr({
                  df <- ..(fn)
                  df <- add_date_range(df)
                  attr(df, "dataset_list_name") <- ..(dln)
                  df
                })
              })
              .ec
            })

            get_code_in_context <- function(x) {
              shinymeta::expandChain(
                x,
                .expansionContext = ec
              ) |>
                shinymeta::formatCode(formatter = format_with_air, width = 400L) |>
                as.character() |>
                paste(collapse = "\n")
            }

            preprocess_export_element <- function(export_element, output_format) {
              el_processed <- export_element
              log_inform(paste0("Processing:", el_processed[["id"]]))
              checkmate::assert_subset(output_format, as.character(unclass(EXPORT$OUTPUT_FORMAT)))

              if (identical(output_format, EXPORT$OUTPUT_FORMAT$HTML)) {
                reactive <- el_processed[["metareactive"]][["html"]]
              } else if (identical(output_format, EXPORT$OUTPUT_FORMAT$PDF)) {
                reactive <- el_processed[["metareactive"]][["pdf"]]
              }

              char_width <- NULL
              resolved <- try(reactive(), silent = TRUE)

              if (is.null(reactive)) {
                code <- paste("Error creating", el_processed[["id"]], "Not avaliable in", output_format, "format")
                kind <- REK$ERROR
              } else if (inherits(resolved, "try-error")) {
                code <- local({
                  msg <- attr(resolved, "condition")$message
                  paste("Error creating", el_processed[["id"]], msg)
                })
                kind <- REK$ERROR
              } else if (identical(output_format, EXPORT$OUTPUT_FORMAT$HTML)) {
                code <- get_code_in_context(reactive())
                kind <- REK$DEFAULT
              } else if (identical(output_format, EXPORT$OUTPUT_FORMAT$PDF)) {
                # TODO: This could be moved to the formatter section
                if (is.data.frame(reactive())) {
                  reactive_ <- shinymeta::metaReactive(
                    {
                      ..(reactive()) |>
                        gt::gt() |>
                        gt::tab_options(
                          latex.use_longtable = TRUE,
                          table.font.size = gt::px(9),
                          latex.header_repeat = TRUE
                        ) |>
                        gt::as_latex()
                    },
                    inline = TRUE
                  )

                  code <- get_code_in_context(reactive_())
                  # Rough method for estimating an upper limit of table width
                  # Code should never be narrower than the table, but it can overshoot by large sometimes
                  char_width <- max(nchar(unlist(strsplit(reactive_(), "\n"))))
                  kind <- REK$TABLE
                } else if (inherits(reactive(), "gt_tbl")) {
                  reactive_ <- shinymeta::metaReactive(
                    {
                      ..(reactive()) |> gt::as_latex()
                    },
                    inline = TRUE
                  )
                  code <- get_code_in_context(reactive_())
                  char_width <- max(nchar(unlist(strsplit(reactive_(), "\n"))))
                  kind <- REK$TABLE
                } else {
                  code <- get_code_in_context(reactive())
                  kind <- REK$DEFAULT
                }
              }

              el_processed[["metareactive"]] <- NULL
              el_processed[["code"]] <- code
              el_processed[["kind"]] <- kind
              el_processed[["char_width"]] <- char_width

              log_inform(paste0("Preprocessed:", el_processed[["id"]]))

              return(el_processed)
            }

            data_code <- get_code_in_context(invisible(unfiltered_dataset_list_with_filter_info()))

            log_inform("Preprocessing export elements")
            elements_to_export <- local({
              selected_elements <- names(is_output_selected_to_export)[is_output_selected_to_export]
              selected_exportable_elements <- exportable_elements[selected_elements]

              res <- list()
              for (idx in seq_along(selected_exportable_elements)) {
                log_inform(sprintf("Preprocessing element (%d)", idx))
                curr_el <- selected_exportable_elements[[idx]]
                if (is_output_selected_to_export[[curr_el[["id"]]]]) {
                  element <- preprocess_export_element(curr_el, output_format)
                  res <- c(res, list(element))
                }
              }
              res
            })

            log_inform("Creating hardcoded dataset hash section")
            hardcoded_hash_section <- local({
              dataset_list_hash <- vector(mode = "list", length = length(selected_dataset_list()))
              for (idx in seq_along(selected_dataset_list())) {
                dataset_list_hash[[idx]] <- digest::digest(selected_dataset_list()[[idx]])
              }
              names(dataset_list_hash) <- names(selected_dataset_list())

              section <- "## Hardcoded Data hash:"
              for (idx in seq_along(dataset_list_hash)) {
                section <- sprintf(
                  "%s\n\n **name**: `%s` **hash**: %s",
                  section,
                  names(dataset_list_hash)[[idx]],
                  dataset_list_hash[[idx]]
                )
              }
              note <- "These hashes are calculated in-app, they correspond to the data loaded in the app that created the export."
              sprintf("%s\n\n%s\n\n", section, note)
            })

            log_inform("Creating dynamic dataset hash section")
            dynamic_hash_section <- local({
              section <- "## Dynamic Data hash:"
              for (idx in seq_along(selected_dataset_list())) {
                section <- sprintf(
                  "%s\n\n **name**: ``r names(selected_dataset_list)[[%d]]`` **hash**: `r digest::digest(selected_dataset_list[[%d]])`",
                  section,
                  idx,
                  idx
                )
              }
              note <- "These hashes are calculated during export rendering, and should match those in the **Hardcoded Data hash** section."
              sprintf("%s\n\n%s\n\n", section, note)
            })

            log_inform("Creating date section")
            date_section <- local({
              section <- "## Data Modification Dates:"
              section <- sprintf(
                "%s\n\n **Date range**:\n\n`r %s`",
                section,
                get_code_in_context(date_range())
              )

              for (idx in seq_along(selected_dataset_list())) {
                section <- sprintf(
                  "%s\n\n **name**: ``r names(selected_dataset_list)[[%d]]`` **modification time**: `r attr(selected_dataset_list[[%d]], \"meta\")[[\"mtime\"]]`",
                  section,
                  idx,
                  idx
                )
              }
              note <- "These dates are calculated during export rendering."
              sprintf("%s\n\n%s\n\n", section, note)
            })

            log_inform("Creating filter txt section")

            filter_txt_section <- local({
              section <- "## Filters:"

              if (output_format == EXPORT$OUTPUT_FORMAT$HTML) {
                verbatim_tags <- c("<pre>", "</pre>")
              } else if (output_format == EXPORT$OUTPUT_FORMAT$PDF) {
                verbatim_tags <- c("\\begin{verbatim}", "\\end{verbatim}")
              }

              cat_mr <- sm_mr(
                {
                  cat(..(filter_txt()))
                },
                inline = TRUE,
                varname = "cat_filter_txt"
              )

              note <- "An explicit call to the filter and parameters used can be found in the code that accompanies this export."
              section <- sprintf(
                "%s\n\n%s\n\n```{r filter_export_txt, echo = FALSE, results='asis'}\n\n%s\n\n```\n\n%s\n\n%s",
                section,
                verbatim_tags[[1]],
                get_code_in_context(cat_mr()),
                verbatim_tags[[2]],
                note
              )

              section
            })

            filter_reference_list_txt_section <- local({
              section <- "# Filter references:"

              if (nchar(filter_reference_list_txt()) > 0) {
                cat_mr <- sm_mr(
                  {
                    cat(..(filter_reference_list_txt()))
                  },
                  inline = TRUE,
                  varname = "cat_filter_reference_list_txt"
                )
              } else {
                cat_mr <- sm_mr(
                  {
                    cat("No references found")
                  },
                  inline = TRUE,
                  varname = "cat_filter_reference_list_txt"
                )
              }

              section <- sprintf(
                "%s\n\n```{r filter_export_reference_list, echo = FALSE, results='asis'}\n\n%s\n\n```",
                section,
                get_code_in_context(cat_mr())
              )

              section
            })

            log_inform("Creating rmarkdown")
            rmarkdown <- local({
              output_rmd <- ""

              for (idx in seq_along(elements_to_export)) {
                curr_el <- elements_to_export[[idx]]
                log_inform(paste0("Processing idx: ", idx))

                element_formatters <- local({
                  res <- list()
                  res[[EXPORT$OUTPUT_FORMAT$PDF]] <- list()
                  res[[EXPORT$OUTPUT_FORMAT$PDF]][["header"]] <- function(x) {
                    if (x[["is_first_module_element"]]) {
                      sprintf(
                        "\\section{%s}\n\n\\subsection{%s}\n\n",
                        escape_latex(x[["module_name"]]),
                        escape_latex(x[["label"]])
                      )
                    } else {
                      sprintf("\\subsection{%s}\n\n", escape_latex(x[["label"]]))
                    }
                  }
                  res[[EXPORT$OUTPUT_FORMAT$PDF]][[REK$ERROR]] <- function(x) {
                    fmt <- "\n%s\n\n\\alertwarning{%s}\n\n"
                    sprintf(
                      fmt,
                      res[[EXPORT$OUTPUT_FORMAT$PDF]][["header"]](x),
                      x[["code"]]
                    )
                  }

                  res[[EXPORT$OUTPUT_FORMAT$PDF]][[REK$TABLE]] <- function(x) {
                    fmt <- "\n\n\\beginwidepage{%d}\n\n\\begingroup\n\n\\wptabfont\n\n%s\n\n\\endgroup\n\n\\stopwidepage\n\n"
                    sprintf(
                      fmt,
                      x[["char_width"]],
                      res[[EXPORT$OUTPUT_FORMAT$PDF]][[REK$DEFAULT]](x)
                    )
                  }

                  res[[EXPORT$OUTPUT_FORMAT$PDF]][[REK$DEFAULT]] <- function(x) {
                    fmt <- "\n%s\n\n```{r %s}\n%s\n```\n\n"
                    sprintf(
                      fmt,
                      res[[EXPORT$OUTPUT_FORMAT$PDF]][["header"]](x),
                      x[["id"]],
                      x[["code"]]
                    )
                  }

                  res[[EXPORT$OUTPUT_FORMAT$HTML]] <- list()
                  res[[EXPORT$OUTPUT_FORMAT$HTML]][["header"]] <- function(x) {
                    if (x[["is_first_module_element"]]) {
                      sprintf("# %s\n\n## %s\n\n", x[["module_name"]], x[["label"]])
                    } else {
                      sprintf("## %s\n\n", x[["label"]])
                    }
                  }
                  res[[EXPORT$OUTPUT_FORMAT$HTML]][[REK$ERROR]] <- function(x) {
                    fmt <- "\n%s\n\n<div class = \"alert alert-warning\" role = \"alert\">%s</div>\n\n"

                    sprintf(
                      fmt,
                      res[[EXPORT$OUTPUT_FORMAT$HTML]][["header"]](x),
                      x[["code"]]
                    )
                  }

                  res[[EXPORT$OUTPUT_FORMAT$HTML]][[REK$TABLE]] <- function(x) {
                    res[[EXPORT$OUTPUT_FORMAT$HTML]][[REK$DEFAULT]](x)
                  }
                  res[[EXPORT$OUTPUT_FORMAT$HTML]][[REK$DEFAULT]] <- function(x) {
                    fmt <- "\n%s\n\n```{r %s}\n%s\n```\n\n"
                    sprintf(
                      fmt,
                      res[[EXPORT$OUTPUT_FORMAT$HTML]][["header"]](x),
                      x[["id"]],
                      x[["code"]]
                    )
                  }
                  res
                })

                output_rmd <- sprintf(
                  "%s\n%s\n",
                  output_rmd,
                  element_formatters[[output_format]][[curr_el[["kind"]]]](curr_el)
                )
              }

              rmd <- sprintf(
                "%s\n\n```{r data_source}\n%s\n```\n\n%s\n\n# Data source\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n",
                EXPORT$TEMPLATES$HEADER[[output_format]],
                data_code,
                output_rmd,
                date_section,
                hardcoded_hash_section,
                dynamic_hash_section,
                filter_txt_section,
                filter_reference_list_txt_section,
                EXPORT$TEMPLATES$SESSION_INFO[[output_format]],
                EXPORT$TEMPLATES$FOOTER[[output_format]]
              )

              rmd
            })

            log_inform("Rendering rmarkdown")
            rendered_filename <- local({
              export_dir <- tempfile(pattern = "export")
              log_inform(sprintf("Creating export in %s", export_dir))
              dir.create(export_dir)
              curr_dir <- getwd()
              on.exit(
                {
                  if (dir.exists(export_dir)) {
                    unlink(export_dir, recursive = TRUE)
                    log_inform(sprintf("Removing dir %s", export_dir))
                  }
                  setwd(curr_dir)
                },
                add = TRUE
              )

              export_rmd <- file.path(export_dir, "export.Rmd")
              writeLines(rmarkdown, export_rmd)

              zip_filename <- callr::r(
                render_export_document,
                args = list(
                  export_rmd = export_rmd,
                  export_dir = export_dir,
                  header_file = system.file("export_files/header.tex", package = "dv.manager", mustWork = TRUE),
                  pdf_attach_function = pdf_attach,
                  filename = filename
                ),
                show = TRUE
              )
              if (length(attr(zip_filename, "error_msg")) > 0) {
                log_warn(sprintf("Error while rendering export: %s", attr(zip_filename, "error_msg")))
              }
              attr(zip_filename, "error_msg") <- NULL
              zip_filename
            })
          })
        }
      )
    })
  })

  # shinymeta::metaReactive2
  sm_mr2 <- shinymeta::metaReactive2

  # shinymeta::metaReactive
  sm_mr <- shinymeta::metaReactive

  # shinymeta::metaExpr
  sm_me <- shinymeta::metaExpr
}
