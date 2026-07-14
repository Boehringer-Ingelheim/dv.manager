if (isTRUE(getOption("dv.export_enabled"))) {
  log_warn("Export has been enabled. This is an experimental feature.")
  # Code for exporting versions

  EXPORT <- poc(
    ID = poc(
      EXPORT_CODE = "export_code",
      EXPORT_CODE_MENU = "export_code_menu"
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
    extra_dependencies: ["pdflscape"]
header-includes:
  - \usepackage{atbegshi}
  - \usepackage{graphicx}
  - \usepackage{xcolor}
  - \AtBeginShipout{\AtBeginShipoutUpperLeft{\put(25,-420){\rotatebox{90}{\normalfont\color{red}\fontsize{20pt}{24pt}\selectfont UNVALIDATED CONTENT}}\put(570,-420){\rotatebox{90}{\normalfont\color{red}\fontsize{20pt}{24pt}\selectfont UNVALIDATED CONTENT}}}}
  - \newcommand{\alertwarning}[1]{\par\vspace{4pt}\noindent\fcolorbox{yellow!70!black}{yellow!20}{\parbox{\dimexpr\linewidth-2\fboxsep-2\fboxrule}{\color{yellow!60!black}\detokenize{#1}}}\par\vspace{4pt}}

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

  session_info_section <-
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

  append_export_button <- function(x) {
    log_inform("Attaching export button")
    export_button <- local({
      t <- shiny::tags
      dd_div <- shiny::div(
        class = "btn-group",
        role = "group",
        t[["button"]](type = "button", class = "btn btn-primary dropdown-toggle", "data-bs-toggle" = "dropdown"),
        t[["ul"]](
          class = "dropdown-menu",
          t[["li"]](t[["a"]](class = "dropdown-item", href = "#", "Custom current")),
          t[["li"]](t[["a"]](class = "dropdown-item", href = "#", "Custom All")),
        )
      )

      bg_d <- shiny::div(
        class = "btn-group",
        role = "group",
        t[["button"]](type = "button", class = "btn btn-primary", "Export"),
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
              res[[current_element[["id"]]]] <- current_element
            }
          }
        }
        res
      })

      export_modal_ui <- local({
        card_ui <- list(shiny::h3("Export menu"))

        if (length(exportable_elements) > 0) {
          card_items <- NULL

          for (idx in seq_along(exportable_elements)) {
            curr_el <- exportable_elements[[idx]]

            if (curr_el[["is_first_module_element"]]) {
              if (!is.null(card_items)) {
                card_ui[[length(card_ui) + 1]] <- do.call(bslib::card, card_items)
              }
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
                      ns("export_menu_input"),
                      curr_el[["id"]]
                    )
                  ),
                  curr_el[["label"]]
                )
              )
          }
        } else {
          card_ui[[length(card_ui) + 1]] <- bslib::card(
            bslib::card_header("No elements available for export")
          )
        }

        card_ui[[length(card_ui) + 1]] <- bslib::card(
          bslib::card_header("Output format"),
          shiny::radioButtons(
            ns("output_format"),
            label = NULL,
            choices = EXPORT$OUTPUT_FORMAT
          )
        )

        res <- shiny::modalDialog(
          shiny::div(
            class = "d-flex flex-column vh-25",
            style = "max-height: 90vh",
            shiny::div(
              class = "overflow-auto flex-grow-1 p-3 min-h-0",
              list(
                card_ui
              )
            ),
            shiny::downloadButton(ns(EXPORT$ID$EXPORT_CODE), "Export")
          ),
          easyClose = TRUE,
          footer = NULL
        )
        res
      })

      selected <- stats::setNames(
        rep(TRUE, length(exportable_elements)),
        names(exportable_elements)
      )

      shiny::observeEvent(input[["export_menu_input"]], {
        selected[[input[["export_menu_input"]][["id"]]]] <<- input[["export_menu_input"]][["value"]]
      })

      shiny::observeEvent(input[[EXPORT$ID$EXPORT_CODE_MENU]], {
        shiny::showModal(
          export_modal_ui
        )
      })

      output[[EXPORT$ID$EXPORT_CODE]] <- shiny::downloadHandler(
        filename = "export.zip",
        content = function(filename) {
          shiny::withProgress(message = "Rendering export", expr = {
            output_format <- input[["output_format"]]

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

            # Replaces reactives that return no htmlwidgets with PDF compatible options
            replace_if_htmlwidget <- function(x) {
              if (!inherits(x(), "htmlwidget")) {
                return(x)
              }

              supported_htmlwidgets <- list(
                "datatables" = function(x) {
                  shinymeta::metaReactive(
                    {
                      ..(x())$x$data
                    },
                    inline = TRUE
                  )
                }
              )

              supported_type <- inherits(x(), names(supported_htmlwidgets))

              if (!identical(supported_type, 0L)) {
                replaced_x <- supported_htmlwidgets[[supported_type[[1]]]](x)
              } else {
                replaced_x <- shinymeta::metaReactive(
                  {
                    stop("Unsupported htmlwidget", paste(class("`", x()), "`", collapse = ", "))
                  },
                  inline = TRUE
                )
              }

              return(replaced_x)
            }

            process_export_element <- function(export_element, output_format) {
              el_processed <- export_element
              log_inform(paste0("Processing:", el_processed[["id"]]))
              checkmate::assert_subset(output_format, as.character(unclass(EXPORT$OUTPUT_FORMAT)))

              reactive <- el_processed[["reactive"]]
              resolved <- try(reactive(), silent = TRUE)

              if (identical(output_format, EXPORT$OUTPUT_FORMAT$PDF) && !inherits(resolved, "try-error")) {
                reactive <- replace_if_htmlwidget(reactive)
              }

              if (inherits(resolved, "try-error")) {
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
                        )
                    },
                    inline = TRUE
                  )

                  code <- get_code_in_context(reactive_())
                  kind <- REK$TABLE
                } else {
                  code <- get_code_in_context(reactive())
                  kind <- REK$DEFAULT
                }
              }

              el_processed[["reactive"]] <- NULL
              el_processed[["code"]] <- code
              el_processed[["kind"]] <- kind

              log_inform(paste0("Processed:", el_processed[["id"]]))

              return(el_processed)
            }

            data_code <- get_code_in_context(invisible(unfiltered_dataset_list_with_filter_info()))

            log_inform("Processing export elements")
            export_elements <- local({
              selected_exportable_elements <- exportable_elements[names(selected)[selected]]

              res <- list()
              for (idx in seq_along(selected_exportable_elements)) {
                log_inform(sprintf("Processing element (%d)", idx))
                curr_el <- selected_exportable_elements[[idx]]
                if (selected[[curr_el[["id"]]]]) {
                  element <- process_export_element(
                    curr_el,
                    output_format
                  )
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

            log_inform("Creating rmarkdown")
            rmarkdown <- local({
              rmd <- EXPORT$TEMPLATES$HEADER[[output_format]]
              rmd <- sprintf(
                "%s\n# Data source\n\n```{r}\n%s\n```\n\n%s\n\n%s\n\n%s\n\n",
                rmd,
                data_code,
                date_section,
                hardcoded_hash_section,
                dynamic_hash_section
              )

              for (idx in seq_along(export_elements)) {
                curr_el <- export_elements[[idx]]
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
                    fmt <- "\n\\newpage\n\\begin{landscape}\n\n%s\n\n\\end{landscape}\n\\newpage\n\n"
                    sprintf(
                      fmt,
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

                rmd <- sprintf(
                  "%s\n%s\n",
                  rmd,
                  element_formatters[[output_format]][[curr_el[["kind"]]]](curr_el)
                )
              }

              rmd <- sprintf("%s\n%s", rmd, EXPORT$TEMPLATES$SESSION_INFO[[output_format]])

              rmd <- sprintf("%s\n%s", rmd, EXPORT$TEMPLATES$FOOTER[[output_format]])

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
                function(export_rmd, export_dir, filename) {
                  # All file writing happens in export_dir
                  # Directory is  removed after returning so there is no need of intermediate cleaning
                  setwd(export_dir)

                  error_msg <- character(0)
                  output_file <- tryCatch(
                    rmarkdown::render(input = export_rmd, output_dir = export_dir),
                    error = function(e) {
                      error_msg <<- e$message
                      warning(sprintf("Error rendering export in %s", export_dir))
                      error_file_name <- file.path(export_dir, "error.txt")
                      writeLines("Error rendering export", error_file_name)
                      error_file_name
                    }
                  )
                  if (endsWith(output_file, "pdf")) {
                    attach_file <- function(attachment_file, destiny_file) {
                      preattach_file <- paste0("preattach_", basename(destiny_file))
                      file.copy(destiny_file, preattach_file)
                      unlink(destiny_file)
                      system2("pdfattach", args = c(preattach_file, attachment_file, destiny_file))
                      unlink(preattach_file)
                    }

                    session_info_file <- "session_info.txt"

                    writeLines(
                      capture.output(devtools::session_info()),
                      session_info_file
                    )

                    attach_file(export_rmd, output_file)
                    attach_file(session_info_file, output_file)
                    unlink(session_info_file)
                  }
                  zip_filename <- utils::zip(filename, list.files(export_dir))
                  structure(
                    zip_filename,
                    error_msg = error_msg
                  )
                },
                args = list(export_rmd = export_rmd, export_dir = export_dir, filename = filename)
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
} else {
  append_export_button <- identity
  export_server_quote <- NULL

  evaluate_in_caller <- function(expr) {
    eval(substitute(expr), envir = parent.frame())
  }

  # shinymeta::metaReactive2
  sm_mr2 <- shiny::reactive

  # shinymeta::metaReactive
  sm_mr <- shiny::reactive

  # shinymeta::metaExpr
  sm_me <- evaluate_in_caller

  .. <- identity
}
