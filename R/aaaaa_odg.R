# This file contains all output document generation (ODG) related code. Remove it if required.

#' Render an ODG Rmd to a zip file
#'
#' Runs in a fresh `callr::r()` subprocess (or can be called directly, e.g. in
#' tests) — everything needed must arrive as an argument, nothing carries over
#' from the caller's session.
#'
#' @param rmarkdown The full .Rmd document content, as a single string.
#' @param header The `header.tex` content, as a single string.
#' @param pdf_attach_function `function(pdf, attachment)` used to attach files
#'   to a rendered PDF. Injected so it can be tested independently.
#' @param filename Path the resulting zip file is written to.
#' @param quiet Silences both `rmarkdown::render()` and the zip step's output.
#' @return `filename`, with an `error_msg` attribute (character(0) on success,
#'   the error message on failure).
#' @keywords internal
#' @noRd
render_odg_document <- function(rmarkdown, header, pdf_attach_function, filename, quiet = TRUE) {
  odg_dir <- tempfile(pattern = "odg")
  if (!quiet) {
    message(sprintf("Generating output documentation in %s", odg_dir))
  }
  dir.create(odg_dir)

  old_wd <- getwd()
  on.exit(
    {
      if (dir.exists(odg_dir)) {
        unlink(odg_dir, recursive = TRUE)
        if (!quiet) {
          message(sprintf("Removing dir %s", odg_dir))
        }
      }
      setwd(old_wd)
    },
    add = TRUE
  )
  setwd(odg_dir)

  writeLines(rmarkdown, "odg.Rmd")
  writeLines(header, "header.tex")

  error_msg <- character(0)
  tryCatch(
    {
      output_file <- rmarkdown::render(input = "odg.Rmd", output_dir = odg_dir, quiet = quiet)
      if (endsWith(output_file, "pdf")) {
        session_info_file <- "session_info.txt"
        writeLines(capture.output(devtools::session_info()), session_info_file)
        pdf_attach_function(output_file, "odg.Rmd")
        pdf_attach_function(output_file, session_info_file)
        unlink(session_info_file)
      }
    },
    error = function(e) {
      error_msg <<- e$message
      warning(sprintf("Error generating output documentation in %s\n%s", odg_dir, error_msg))
      writeLines(c("Error generating output documentation", error_msg), "error.txt")
    }
  )

  zip_command_path <- local({
    default_zip <- Sys.getenv("R_ZIPCMD")
    if (is.null(default_zip) || !is.character(default_zip) || nchar(default_zip) == 0) {
      warning("`R_ZIPCMD` environment variable is not set\nAttempting to locate zip command it automatically")
      zip_path_via_which <- system2("which", "zip", stdout = TRUE)
      if (length(zip_path_via_which) == 1 && file.exists(zip_path_via_which)) {
        message(sprintf("Found zip command in %s", zip_path_via_which))
        zip_path <- zip_path_via_which
      } else {
        stop(sprintf("Failed to locate zip command automatically"))
      }
    } else {
      zip_path <- default_zip
    }
    zip_path
  })
  zip_filename <- utils::zip(
    filename,
    list.files(odg_dir),
    flags = if (quiet) "-r9Xq" else "-r9X",
    zip = zip_command_path
  )
  structure(zip_filename, error_msg = error_msg)
}

#' Attach a file to a PDF in place
#'
#' @param pdf Path to an existing PDF, overwritten in place. Requires qpdf on PATH.
#' @param attachment Path to the file to embed, keyed by its basename.
#' @return `pdf`, invisibly.
#' @keywords internal
#' @noRd
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

ODG <- local({
  ODG <- poc(
    ID = poc(
      ODG_CODE = "odg_code",
      ODG_CODE_MENU = "odg_code_menu",
      ODG_MENU_SELECTION = "odg_menu_selection",
      OUTPUT_FORMAT = "output_format"
    ),
    ATTR = "odg_element_type",
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
      ODG_BUTTON = "Generate Output Documentation",
      OUTPUTS_CARD = "Outputs",
      FORMAT_CARD = "Format",
      NOTHING_TO_GENERATE = "No outputs available"
    ),
    VAL = poc(
      ODG_ALL = "all"
    )
  )

  ODG[["TEMPLATES"]] <- list()

  ODG[["TEMPLATES"]][["HEADER"]] <- local({
    templates <- character(0)
    templates[[ODG$OUTPUT_FORMAT$PDF]] <- r"--(
---
title: "Output Documentation"
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
    extra_dependencies: ["lscape"]
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

    templates[[ODG$OUTPUT_FORMAT$HTML]] <- r"--(
---
title: "Output Documentation"
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

  ODG[["TEMPLATES"]][["SESSION_INFO"]] <- local({
    templates <- character(0)
    templates[[ODG$OUTPUT_FORMAT$PDF]] <- r"--(

```{r session_info, results = 'asis'}
  session_lines <- capture.output(devtools::session_info())   
  max_char_width <- max(nchar(session_lines))
  
  session_lines <- paste0(session_lines, collapse = "\n")
    
  fmt <- "\n\n\\newpage\n\n\\beginwidepage{%d}\n\n\\begingroup\n\n\\wptabfont\n\n\\section{Session Info}\n\n\\begin{verbatim}\n\n%s\n\n\\end{verbatim}\n\n\\endgroup\n\n\\stopwidepage\n\n"
  cat(sprintf(fmt, max_char_width, session_lines))
    
```

)--"

    templates[[ODG$OUTPUT_FORMAT$HTML]] <- r"--(
# Session Info

```{r session_info}
  devtools::session_info()
```

)--"

    templates
  })

  ODG[["TEMPLATES"]][["FOOTER"]] <- local({
    templates <- character(0)
    templates[[ODG$OUTPUT_FORMAT$PDF]] <- ""

    # Currently code is attached to the PDF file
    #   r"--(
    # # Annex: Code

    # ```{r show-code, ref.label = setdiff(knitr::all_labels(), c("setup", "show-code")), echo=TRUE, eval=FALSE}
    # ```
    # )--"

    templates[[ODG$OUTPUT_FORMAT$HTML]] <- ""

    templates
  })

  ODG
})

#' Formatters mapping (output format, element kind) to a chunk of Rmd markup
#'
#' `ODG_ELEMENT_FORMATTERS[[output_format]][[kind]]` is a `function(x)`
#' taking a processed ODG element and returning markup. A `safe_list`
#' rather than a `poc()` since its entries are behavior (formatter functions),
#' not named constants — but the same fail-loudly-on-a-typo motivation applies
#' to `[[output_format]][[kind]]` lookups.
#' @keywords internal
#' @noRd
ODG_ELEMENT_FORMATTERS <- local({
  res <- safe_list()
  res[[ODG$OUTPUT_FORMAT$PDF]] <- safe_list()
  res[[ODG$OUTPUT_FORMAT$PDF]][["header"]] <- function(x) {
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
  res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$ERROR]] <- function(x) {
    fmt <- "\n%s\n\n\\alertwarning{%s}\n\n"
    sprintf(fmt, res[[ODG$OUTPUT_FORMAT$PDF]][["header"]](x), x[["code"]])
  }
  res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$TABLE]] <- function(x) {
    fmt <- "\n\n\\beginwidepage{%d}\n\n\\begingroup\n\n\\wptabfont\n\n%s\n\n\\endgroup\n\n\\stopwidepage\n\n"
    sprintf(fmt, x[["char_width"]], res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$DEFAULT]](x))
  }
  res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$DEFAULT]] <- function(x) {
    fmt <- "\n%s\n\n```{r %s}\n%s\n```\n\n"
    sprintf(fmt, res[[ODG$OUTPUT_FORMAT$PDF]][["header"]](x), x[["id"]], x[["code"]])
  }

  res[[ODG$OUTPUT_FORMAT$HTML]] <- safe_list()
  res[[ODG$OUTPUT_FORMAT$HTML]][["header"]] <- function(x) {
    if (x[["is_first_module_element"]]) {
      sprintf("# %s\n\n## %s\n\n", x[["module_name"]], x[["label"]])
    } else {
      sprintf("## %s\n\n", x[["label"]])
    }
  }
  res[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$ERROR]] <- function(x) {
    fmt <- "\n%s\n\n<div class = \"alert alert-warning\" role = \"alert\">%s</div>\n\n"
    sprintf(fmt, res[[ODG$OUTPUT_FORMAT$HTML]][["header"]](x), x[["code"]])
  }
  res[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$TABLE]] <- function(x) {
    res[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$DEFAULT]](x)
  }
  res[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$DEFAULT]] <- function(x) {
    fmt <- "\n%s\n\n```{r %s}\n%s\n```\n\n"
    sprintf(fmt, res[[ODG$OUTPUT_FORMAT$HTML]][["header"]](x), x[["id"]], x[["code"]])
  }
  res
})

#' Assemble the final ODG .Rmd from preprocessed elements and precomputed sections
#'
#' @param selected_odg_elements List of preprocessed ODG elements (each with
#'   `kind`, `id`, `code`, `label`, `module_name`, `is_first_module_element`,
#'   `char_width`), in the order they should appear.
#' @param output_format One of `ODG$OUTPUT_FORMAT` (`"html"`/`"pdf"`).
#' @param data_code Rmd code chunk describing how the source data was loaded.
#' @param sections Named list with `date`, `hardcoded_hash`, `dynamic_hash`,
#'   `filter_txt`, `filter_reference` markdown sections.
#' @param templates Named list with `header`, `session_info`, `footer`
#'   templates, already resolved for `output_format`.
#' @return The full .Rmd document, as a single string.
#' @keywords internal
#' @noRd
build_odg_rmd <- function(selected_odg_elements, output_format, data_code, sections, templates) {
  output_rmd <- ""
  for (idx in seq_along(selected_odg_elements)) {
    curr_el <- selected_odg_elements[[idx]]
    log_inform(paste0("Processing idx: ", idx))
    output_rmd <- sprintf(
      "%s\n%s\n",
      output_rmd,
      ODG_ELEMENT_FORMATTERS[[output_format]][[curr_el[["kind"]]]](curr_el)
    )
  }

  sprintf(
    "%s\n\n```{r data_source}\n%s\n```\n\n%s\n\n# Data source\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n%s\n\n",
    templates[["header"]],
    data_code,
    output_rmd,
    sections[["date"]],
    sections[["hardcoded_hash"]],
    sections[["dynamic_hash"]],
    sections[["filter_txt"]],
    sections[["filter_reference"]],
    templates[["session_info"]],
    templates[["footer"]]
  )
}

#' Markdown section listing a content hash per dataset, computed now
#'
#' @param dataset_list Named list of data.frames (the resolved dataset list).
#' @return The markdown section, as a single string.
#' @keywords internal
#' @noRd
build_hardcoded_hash_section <- function(dataset_list) {
  dataset_list_hash <- vector(mode = "list", length = length(dataset_list))
  for (idx in seq_along(dataset_list)) {
    dataset_list_hash[[idx]] <- digest::digest(dataset_list[[idx]])
  }
  names(dataset_list_hash) <- names(dataset_list)

  section <- "## Hardcoded Data hash:"
  for (idx in seq_along(dataset_list_hash)) {
    section <- sprintf(
      "%s\n\n **name**: `%s` **hash**: %s",
      section,
      names(dataset_list_hash)[[idx]],
      dataset_list_hash[[idx]]
    )
  }
  note <- paste(
    "These hashes are calculated in-app, they correspond to the data loaded in the app",
    "that created this output documentation."
  )
  sprintf("%s\n\n%s\n\n", section, note)
}

#' Markdown section recomputing each dataset's hash at render time
#'
#' Emits one `asis` chunk looping over the dataset list, so the section adapts to
#' whatever it holds at render time and never names it or its length itself: the
#' reference comes from `get_code_in_context()`. It collapses to a bare reference
#' (no re-emitted assignment) only because the shared expansion context has
#' already emitted that assignment into the data-source chunk, so this must be
#' called after the data code is generated.
#'
#' @param selected_dataset_list_mr Metareactive resolving to the dataset list.
#' @param get_code_in_context `function(x)` turning a metareactive call into the
#'   R code reproducing it. Injected so it can be tested independently.
#' @return The markdown section, as a single string.
#' @keywords internal
#' @noRd
build_dynamic_hash_section <- function(selected_dataset_list_mr, get_code_in_context) {
  loop <- shinymeta::metaReactive(
    {
      for (idx in seq_along(..(selected_dataset_list_mr()))) {
        cat(sprintf(
          "\n\n **name**: `%s` **hash**: %s",
          names(..(selected_dataset_list_mr()))[[idx]],
          digest::digest(..(selected_dataset_list_mr())[[idx]])
        ))
      }
    },
    inline = TRUE
  )

  note <- paste(
    "These hashes are calculated while generating the output documentation,",
    "and should match those in the **Hardcoded Data hash** section."
  )
  sprintf(
    "## Dynamic Data hash:\n\n```{r dynamic_data_hash, echo = FALSE, results='asis'}\n%s\n```\n\n%s\n\n",
    get_code_in_context(loop()),
    note
  )
}

#' Markdown section with the date range and per-dataset modification times
#'
#' `format()` guards the modification time because a dataset may carry no
#' `meta$mtime`, and a bare `NULL` would collapse the whole `sprintf()` to
#' `character(0)`, dropping that dataset's line entirely.
#'
#' Shares `build_dynamic_hash_section()`'s ordering requirement: call it after
#' the data code has been generated with the same expansion context.
#'
#' @param selected_dataset_list_mr Metareactive resolving to the dataset list.
#' @param date_range_mr Metareactive resolving to the dataset list date range.
#' @param get_code_in_context `function(x)` turning a metareactive call into the
#'   R code reproducing it. Injected so it can be tested independently.
#' @return The markdown section, as a single string.
#' @keywords internal
#' @noRd
build_date_section <- function(selected_dataset_list_mr, date_range_mr, get_code_in_context) {
  loop <- shinymeta::metaReactive(
    {
      for (idx in seq_along(..(selected_dataset_list_mr()))) {
        cat(sprintf(
          "\n\n **name**: `%s` **modification time**: %s",
          names(..(selected_dataset_list_mr()))[[idx]],
          format(attr(..(selected_dataset_list_mr())[[idx]], "meta")[["mtime"]])
        ))
      }
    },
    inline = TRUE
  )

  note <- "These dates are calculated while generating the output documentation."
  sprintf(
    paste0(
      "## Data Modification Dates:\n\n **Date range**:\n\n`r %s`\n\n",
      "```{r data_modification_dates, echo = FALSE, results='asis'}\n%s\n```\n\n%s\n\n"
    ),
    get_code_in_context(date_range_mr()),
    get_code_in_context(loop()),
    note
  )
}

#' Markdown section wrapping the filter description in a verbatim block
#'
#' @param output_format One of `ODG$OUTPUT_FORMAT` (`"html"`/`"pdf"`).
#' @param filter_txt_code R code, as a string, that prints the filter
#'   description at render time.
#' @return The markdown section, as a single string.
#' @keywords internal
#' @noRd
build_filter_txt_section <- function(output_format, filter_txt_code) {
  checkmate::assert_subset(output_format, as.character(unclass(ODG$OUTPUT_FORMAT)))
  section <- "## Filters:"

  if (output_format == ODG$OUTPUT_FORMAT$HTML) {
    verbatim_tags <- c("<pre>", "</pre>")
  } else if (output_format == ODG$OUTPUT_FORMAT$PDF) {
    verbatim_tags <- c("\\begin{verbatim}", "\\end{verbatim}")
  }

  note <- paste(
    "An explicit call to the filter and parameters used can be found in the code",
    "that accompanies this output documentation."
  )
  sprintf(
    "%s\n\n%s\n\n```{r filter_odg_txt, echo = FALSE, results='asis'}\n\n%s\n\n```\n\n%s\n\n%s",
    section,
    verbatim_tags[[1]],
    filter_txt_code,
    verbatim_tags[[2]],
    note
  )
}

#' Markdown section wrapping the filter reference list
#'
#' @param filter_reference_code R code, as a string, that prints the filter
#'   reference list at render time (or a "No references found" fallback).
#' @return The markdown section, as a single string.
#' @keywords internal
#' @noRd
build_filter_reference_section <- function(filter_reference_code) {
  sprintf(
    "%s\n\n```{r filter_odg_reference_list, echo = FALSE, results='asis'}\n\n%s\n\n```",
    "# Filter references:",
    filter_reference_code
  )
}

#' Preprocess every selected ODG element, in declaration order
#'
#' `get_code_in_context()` must receive the *call* to a metareactive, not its
#' value: `shinymeta::expandChain()` forces that argument in meta mode to get
#' code out of it. Resolved values (`resolved`, and the second `latex()` call
#' below) are therefore only ever used for inspection, never handed to it.
#'
#' @param odg_elements Named list of all flattened ODG elements, keyed by id.
#' @param is_selected Named logical vector, keyed by element id.
#' @param output_format One of `ODG$OUTPUT_FORMAT` (`"html"`/`"pdf"`).
#' @param get_code_in_context `function(x)` turning a metareactive call into the
#'   R code reproducing it. Injected so it can be tested independently.
#' @return Unnamed list of preprocessed elements: each with `metareactive`
#'   dropped and `code`, `kind` (one of `ODG$ELEMENT_KIND`) and `char_width` added.
#' @keywords internal
#' @noRd
preprocess_odg_elements <- function(odg_elements, is_selected, output_format, get_code_in_context) {
  checkmate::assert_subset(output_format, as.character(unclass(ODG$OUTPUT_FORMAT)))
  selected <- odg_elements[names(is_selected)[is_selected]]

  res <- vector(mode = "list", length = length(selected))
  for (idx in seq_along(selected)) {
    log_inform(sprintf("Preprocessing element (%d)", idx))
    odg_element <- selected[[idx]]
    log_inform(paste0("Processing:", odg_element[["id"]]))

    # Entries are keyed by the output format values themselves; missing -> NULL
    metareactive <- odg_element[["metareactive"]][[output_format]]
    char_width <- NULL

    if (is.null(metareactive)) {
      code <- paste("Error creating", odg_element[["id"]], "Not avaliable in", output_format, "format")
      kind <- ODG$ELEMENT_KIND$ERROR
    } else if (!inherits(metareactive, "shinymeta_reactive")) {
      code <- paste(
        "Error creating",
        odg_element[["id"]],
        "`metareactive` field is not a metareactive.",
        "Module is not prepared or not activated for generating output documentation."
      )
      kind <- ODG$ELEMENT_KIND$ERROR
    } else {
      resolved <- try(metareactive(), silent = TRUE)
      is_table <- identical(output_format, ODG$OUTPUT_FORMAT$PDF) &&
        (is.data.frame(resolved) || inherits(resolved, "gt_tbl"))

      if (inherits(resolved, "try-error")) {
        code <- paste("Error creating", odg_element[["id"]], attr(resolved, "condition")[["message"]])
        kind <- ODG$ELEMENT_KIND$ERROR
      } else if (is_table) {
        # TODO: This could be moved to the formatter section
        latex <- if (is.data.frame(resolved)) {
          shinymeta::metaReactive(
            {
              ..(metareactive()) |>
                gt::gt() |>
                gt::tab_options(
                  latex.use_longtable = TRUE,
                  table.font.size = gt::px(9)
                  #, latex.header_repeat = TRUE # Replaced by function below
                ) |>
                gt::as_latex() |>
                dv.manager:::latex_header_repeat()
            },
            inline = TRUE
          )
        } else if (inherits(resolved, "gt_tbl")) {
          shinymeta::metaReactive(
            {
              ..(metareactive()) |>
                gt::as_latex() |>
                dv.manager:::latex_header_repeat()
            },
            inline = TRUE
          )
        } else {
          stop("Unknown table type")
        }

        code <- get_code_in_context(latex())
        # Rough method for estimating an upper limit of table width
        # Code should never be narrower than the table, but it can overshoot by large sometimes
        char_width <- max(nchar(unlist(strsplit(latex(), "\n"))))
        kind <- ODG$ELEMENT_KIND$TABLE
      } else {
        code <- get_code_in_context(metareactive())
        kind <- ODG$ELEMENT_KIND$DEFAULT
      }
    }

    odg_element[["metareactive"]] <- NULL
    odg_element[["code"]] <- code
    odg_element[["kind"]] <- kind
    odg_element[["char_width"]] <- char_width

    log_inform(paste0("Preprocessed:", odg_element[["id"]]))
    res[[idx]] <- odg_element
  }
  res
}

#' Build the ODG selection modal
#'
#' @param odg_elements Named list of all flattened ODG elements
#'   (keyed by id), each with `module_id`, `module_name`, `label`, `info`,
#'   `is_first_module_element`.
#' @param ns Namespacing function for input/output ids.
#' @param selected_tab Module id whose outputs are checked by default, or `NA`
#'   (default) to check all of them. Every module is listed either way.
#' @return `list(modal_dialog =, selected =)`: the modal UI, and a named
#'   logical vector (keyed by element id) of which elements are checked by
#'   default. The modal shows one switch per module, keyed by module id, so a
#'   switch event covers every element of that module.
#' @keywords internal
#' @noRd
build_odg_modal_ui <- function(odg_elements, ns, selected_tab = NA) {
  log_inform(paste("Preselecting", selected_tab, "tab elements in menu"))

  switches <- list()
  selected <- logical(length(odg_elements))
  names(selected) <- names(odg_elements)

  for (curr_el in odg_elements) {
    is_selected <- is.na(selected_tab) || curr_el[["module_id"]] == selected_tab
    selected[[curr_el[["id"]]]] <- is_selected

    if (!curr_el[["is_first_module_element"]]) {
      next
    } # a single switch toggles all the outputs of a module

    switches[[length(switches) + 1]] <- shiny::div(
      class = "form-check form-switch",
      shiny::tags[["label"]](
        class = "form-check-label",
        shiny::tags[["input"]](
          class = "form-check-input",
          type = "checkbox",
          role = "switch",
          checked = if (is_selected) NA else NULL,
          onchange = sprintf(
            "Shiny.setInputValue('%s', {value: this.checked, id: '%s'});",
            ns(ODG$ID$ODG_MENU_SELECTION),
            curr_el[["module_id"]]
          )
        ),
        curr_el[["module_name"]]
      )
    )
  }

  if (length(switches) > 0) {
    body_ui <- list(
      do.call(bslib::card, c(list(bslib::card_header(ODG$MSG$OUTPUTS_CARD)), switches)),
      bslib::card(
        bslib::card_header(ODG$MSG$FORMAT_CARD),
        shiny::radioButtons(
          ns(ODG$ID$OUTPUT_FORMAT),
          label = NULL,
          # HTML output is hidden for now, the rest of the pipeline still supports it
          choices = list(PDF = ODG$OUTPUT_FORMAT$PDF)
        )
      )
    )
    download_button <- shiny::downloadButton(ns(ODG$ID$ODG_CODE), ODG$MSG$ODG_BUTTON)
  } else {
    body_ui <- list(bslib::card(bslib::card_header(ODG$MSG$NOTHING_TO_GENERATE)))
    download_button <- NULL
  }

  list(
    modal_dialog = shiny::modalDialog(
      shiny::div(
        class = "d-flex flex-column vh-25",
        style = "max-height: 90vh",
        shiny::div(
          class = "overflow-auto flex-grow-1 p-3 min-h-0",
          body_ui
        ),
        download_button
      ),
      easyClose = TRUE,
      footer = NULL
    ),
    selected = selected
  )
}

ODGA <- list()

ODGA[["append_odg_button"]] <- function(x, ns) {
  log_inform("Attaching ODG button")

  odg_button <- shiny::tags[["button"]](
    type = "button",
    class = "btn btn-default",
    title = ODG$MSG$ODG_BUTTON,
    shiny::icon("file-lines", class = "fa-lg"),
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      ns(ODG$ID$ODG_CODE_MENU),
      ODG$VAL$ODG_ALL
    )
  )

  top_buttons <- c(
    x,
    list(odg_button)
  )

  top_buttons
}

ODGA[["odg_server_quote"]] <- quote({
  local({
    log_inform("Running ODG server")
    # Flatten ODG elements and set defaults for missing entries
    odg_elements <- local({
      res <- list()
      for (idx in seq_along(module_output)) {
        mo <- module_output[[idx]]

        if ("to_odg" %in% names(mo)) {
          module_odg_elements <- mo[["to_odg"]]
          module_id <- names(module_output)[[idx]]
          module_name <- module_names[[module_id]]

          for (jdx in seq_along(module_odg_elements)) {
            if (!checkmate::test_named(module_odg_elements[[jdx]], type = "unique")) {
              stop(sprintf("ODG elements for module %s are not named or names are not unique", module_id))
            }

            current_element_list_nm <- names(module_odg_elements)[[jdx]]
            current_element <- module_odg_elements[[jdx]]

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

    shiny::observeEvent(input[[ODG$ID$ODG_MENU_SELECTION]], {
      # switches are per module, so one event toggles every output of that module
      selection <- input[[ODG$ID$ODG_MENU_SELECTION]]
      module_element_ids <- names(odg_elements)[
        vapply(odg_elements, function(el) el[["module_id"]] == selection[["id"]], logical(1))
      ]
      is_output_selected_for_odg[module_element_ids] <<- selection[["value"]]
      log_inform(
        paste(
          "Selected outputs for ODG",
          paste(names(is_output_selected_for_odg), is_output_selected_for_odg, collapse = ", ")
        )
      )
    })

    is_output_selected_for_odg <- NULL # TODO replace by reactiveValue so it can be used in the testServer?
    shiny::observeEvent(input[[ODG$ID$ODG_CODE_MENU]], {
      if (is.null(attr(selected_dataset_list(), "load_fn"))) {
        dataset_list_name <- attr(selected_dataset_list(), "dataset_list_name")
        user_msg <- sprintf(
          "Current dataset list `%s` is not configured for generating output documentation.",
          dataset_list_name
        )
        dev_msg <- sprintf(
          "Generating output documentation not possible for `%s`. No `load_fn` attribute found. ",
          dataset_list_name
        )
        log_warn(dev_msg)
        shiny::showNotification(user_msg, type = "error")
        shiny::req(FALSE)
      }

      # every ODG output is listed, but only those in the tab in view start out selected
      x <- build_odg_modal_ui(odg_elements, ns, selected_tab = input[[ID$NAV_HEADER]])
      is_output_selected_for_odg <<- x[["selected"]]

      log_inform(
        paste(
          "Selected outputs for ODG by default",
          paste(names(is_output_selected_for_odg), is_output_selected_for_odg, collapse = ", ")
        )
      )

      shiny::showModal(x[["modal_dialog"]])
    })

    output[[ODG$ID$ODG_CODE]] <- shiny::downloadHandler(
      filename = "odg.zip",
      content = function(filename) {
        shiny::withProgress(message = "Generating output documentation", expr = {
          output_format <- input[[ODG$ID$OUTPUT_FORMAT]]

          ec <- shiny::isolate({
            .ec <- shinymeta::newExpansionContext()
            fn <- body(attr(selected_dataset_list(), "load_fn"))
            dln <- attr(selected_dataset_list(), "dataset_list_name")
            .ec$substituteMetaReactive(selected_dataset_list, function() {
              # This metaExpression contains part of the logic of selected dataset list, add date_range and attribute
              # It is not ideal as they may get desynchronized in the future
              ODGE[["A"]][["sm_me"]]({
                df <- ..(fn)
                df <- dv.manager::add_date_range(df)
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

          # Order of calls to get_code_in_context is relevant, once an element has been expanded in a given
          # expansion context, any additional attempt to expand it will return an empty string
          # Therefore we need to make sure that all relevant elements are included in the report and also
          # in the correct order. It is better to resolve those outside of the call as the internal order of resolving
          # due to promises may not be the same.
          #> x <- function() message("x")
          #> y <- function() message("y")
          #> z <- function(a,b){b;a}
          #> z(x(), y())

          data_code <- get_code_in_context(invisible(unfiltered_dataset_list_with_filter_info()))
          date <- build_date_section(selected_dataset_list, date_range, get_code_in_context)
          hardcoded_hash_section <- build_hardcoded_hash_section(selected_dataset_list())
          dynamic_hash_section <- build_dynamic_hash_section(selected_dataset_list, get_code_in_context)

          filter_txt_section <- local({
            cat_mr <- ODGE[["A"]][["sm_mr"]](
              {
                cat(..(filter_txt()))
              },
              inline = TRUE,
              varname = "cat_filter_txt"
            )

            build_filter_txt_section(output_format, get_code_in_context(cat_mr()))
          })

          filter_reference_list_txt_section <- local({
            if (nchar(filter_reference_list_txt()) > 0) {
              cat_mr <- ODGE[["A"]][["sm_mr"]](
                {
                  cat(..(filter_reference_list_txt()))
                },
                inline = TRUE,
                varname = "cat_filter_reference_list_txt"
              )
            } else {
              cat_mr <- ODGE[["A"]][["sm_mr"]](
                {
                  cat("No references found")
                },
                inline = TRUE,
                varname = "cat_filter_reference_list_txt"
              )
            }

            build_filter_reference_section(get_code_in_context(cat_mr()))
          })

          log_inform("Preprocessing ODG elements")
          selected_odg_elements <- preprocess_odg_elements(
            odg_elements,
            is_output_selected_for_odg,
            output_format,
            get_code_in_context
          )

          log_inform("Creating rmarkdown")
          rmarkdown <- build_odg_rmd(
            selected_odg_elements = selected_odg_elements,
            output_format = output_format,
            data_code = data_code,
            sections = list(
              date = date,
              hardcoded_hash = hardcoded_hash_section,
              dynamic_hash = dynamic_hash_section,
              filter_txt = filter_txt_section,
              filter_reference = filter_reference_list_txt_section
            ),
            templates = list(
              header = ODG$TEMPLATES$HEADER[[output_format]],
              session_info = ODG$TEMPLATES$SESSION_INFO[[output_format]],
              footer = ODG$TEMPLATES$FOOTER[[output_format]]
            )
          )

          log_inform("Rendering rmarkdown")
          header <- paste(
            readLines(system.file("odg_files/header.tex", package = "dv.manager", mustWork = TRUE), warn = FALSE),
            collapse = "\n"
          )
          rendered_filename <- callr::r(
            render_odg_document,
            args = list(
              rmarkdown = rmarkdown,
              header = header,
              pdf_attach_function = pdf_attach,
              filename = filename,
              quiet = FALSE # We want to access it in the app log
            ),
            show = TRUE
          )
          if (length(attr(rendered_filename, "error_msg")) > 0) {
            log_warn(sprintf(
              "Error while generating output documentation: %s",
              attr(rendered_filename, "error_msg")
            ))
          }
          attr(rendered_filename, "error_msg") <- NULL
        })
      }
    )
  })
})


# Workaround for gt versions predating the latex.header_repeat tab_options()
# Usage: gt(tbl) |> tab_options(latex.use_longtable = TRUE) |> as_latex() |>
# latex.header_repeat()
# In specific environments we won't have access to gt (>=1.3.0)
# To be removed when the access if granted
latex_header_repeat <- function(x) {
  x <- as.character(x)
  stopifnot(length(x) == 1) # only latex output, gt objects turn into longer vectors
  if (!grepl("\\\\endhead", x)) {
    # the midrule line carries trailing spacing directives in some gt versions
    x <- sub("(\\\\midrule[^\n]*\n)", "\\1\\\\endhead\n", x)
  }
  class(x) <- "knit_asis"
  x
}


..activate_odg <- function() {
  if (requireNamespace("shinymeta", quietly = TRUE)) {
    log_warn("Output documentation generation is under development")

    # These set of functions is declared inside to avoid calling shinymeta out of a function.
    # Doing that would require some extra if statements that would worsen code readability.

    # shinymeta::metaReactive2
    ODGA[["sm_mr2"]] <- shinymeta::metaReactive2

    # shinymeta::metaReactive
    ODGA[["sm_mr"]] <- shinymeta::metaReactive

    # shinymeta::metaExpr
    ODGA[["sm_me"]] <- shinymeta::metaExpr

    ODGE[["A"]] <- ODGA
  } else {
    log_warn("`shinymeta` package is required to activate output documentation generation")
    ODGE[["A"]] <- NODGA
  }
  invisible(NULL)
}

..deactivate_odg <- function() {
  ODGE[["A"]] <- NODGA
}
