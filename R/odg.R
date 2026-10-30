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
#'   the error message on failure). Errors if no zip command is found or zipping fails.
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
      setwd(old_wd) # Move out before unlink
      if (dir.exists(odg_dir)) {
        unlink(odg_dir, recursive = TRUE)
        if (!quiet) {
          message(sprintf("Removing dir %s", odg_dir))
        }
      }
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
        writeLines(capture.output(sessioninfo::session_info()), session_info_file)
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

  zip_command <- Sys.getenv("R_ZIPCMD")
  if (!nzchar(zip_command)) {
    zip_command <- unname(Sys.which("zip"))
  }
  if (!nzchar(zip_command)) {
    stop("zip command not found: set `R_ZIPCMD` or add zip to the PATH")
  }
  # `utils::zip()` returns the command's exit status, not the zip file
  zip_status <- utils::zip(
    filename,
    list.files(odg_dir),
    flags = if (quiet) "-r9Xq" else "-r9X",
    zip = zip_command
  )
  if (zip_status != 0) {
    stop("zip failed with status ", zip_status)
  }
  structure(filename, error_msg = error_msg)
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
      CHUNK = "chunk",
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
      NOTHING_TO_GENERATE = "No outputs available",
      RENDER_FAILED = paste(
        "The output documentation could not be rendered.",
        "The downloaded file includes the error details (error.txt)."
      ),
      GENERATION_FAILED = "The output documentation could not be generated. No file was downloaded."
    ),
    VAL = poc(
      # Currently only all ODG is possible it is kept as a dummy value for the button
      ODG_ALL = "all"
    ),
    USE_TABLE_ESTIMATION_WIDTH = 0L
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
  
```{r condition_catcher, include=FALSE}
# storage for everything we capture
.condition_log <- data.frame(
  chunk   = character(0),
  type    = character(0),
  message = character(0),
  stringsAsFactors = FALSE
)

.log_condition <- function(type, x, options) {
  .condition_log <<- rbind(
    .condition_log,
    data.frame(
      chunk   = options$label,
      type    = type,
      message = trimws(x),
      stringsAsFactors = FALSE
    )
  )
  "" # suppress default inline printing of the warning/message
}

knitr::knit_hooks$set(
  warning = function(x, options) .log_condition("warning", x, options),
  message = function(x, options) .log_condition("message", x, options)
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
  session_lines <- capture.output(sessioninfo::session_info())
  max_char_width <- 0L # Automatic estimation
  
  session_lines <- paste0(session_lines, collapse = "\n")
    
  fmt <- "\n\n\\newpage\n\n\\beginwidepage{%d}\n\n\\begingroup\n\n\\wptabfont\n\n\\section{Session Info}\n\n\\begin{verbatim}\n\n%s\n\n\\end{verbatim}\n\n\\endgroup\n\n\\stopwidepage\n\n"
  cat(sprintf(fmt, max_char_width, session_lines))
    
```

)--"

    templates[[ODG$OUTPUT_FORMAT$HTML]] <- r"--(
# Session Info

```{r session_info}
  sessioninfo::session_info()
```

)--"

    templates
  })

  ODG[["TEMPLATES"]][["FOOTER"]] <- local({
    templates <- character(0)
    templates[[ODG$OUTPUT_FORMAT$PDF]] <- r"--(
```{r warnings-summary, echo=FALSE, results='asis'}
n <- nrow(.condition_log)
color <- if (n == 0) "black" else "red"

cat(sprintf("# Warnings \\& Messages \\texorpdfstring{\\textcolor{%s}{(%d)}}{(%d)}\n\n",color, n, n))

if (n == 0) {
  cat("No warnings or messages were produced during rendering.\n")
} else {
  for (i in seq_len(n)) {
    row <- .condition_log[i, ]
    cat(sprintf("- **[%s]** `%s`: %s\n", toupper(row$type), row$chunk, row$message))
  }
}
```
)--"

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
    fmt <- "\n%s\n\n```{=latex}\n\\alertwarning{%s}\n```\n\n"
    sprintf(fmt, res[[ODG$OUTPUT_FORMAT$PDF]][["header"]](x), escape_latex(x[["code"]]))
  }
  res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$TABLE]] <- function(x) {
    fmt <- "\n\n\\beginwidepage{%d}\n\n\\begingroup\n\n\\wptabfont\n\n%s\n\n\\endgroup\n\n\\stopwidepage\n\n"
    sprintf(fmt, ODG$USE_TABLE_ESTIMATION_WIDTH, res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$DEFAULT]](x))
  }
  # Chunks already hold their header, see `preprocess_odg_elements()`
  res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$CHUNK]] <- function(x) {
    sprintf("\n%s\n\n", x[["code"]])
  }
  res[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$DEFAULT]] <- function(x) {
    fmt <- "\n%s\n\n```{r %s}\n%s\n```\n\n"
    sprintf(fmt, res[[ODG$OUTPUT_FORMAT$PDF]][["header"]](x), x[["chunk_label"]], x[["code"]])
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
  res[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$CHUNK]] <- function(x) {
    sprintf("\n%s\n\n", x[["code"]])
  }
  res[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$DEFAULT]] <- function(x) {
    fmt <- "\n%s\n\n```{r %s}\n%s\n```\n\n"
    sprintf(fmt, res[[ODG$OUTPUT_FORMAT$HTML]][["header"]](x), x[["chunk_label"]], x[["code"]])
  }
  res
})

#' Assemble the final ODG .Rmd from preprocessed elements and precomputed sections
#'
#' @param selected_odg_elements List of preprocessed ODG elements (each with
#'   `kind`, `id`, `chunk_label`, `code`, `label`, `module_name`,
#'   `is_first_module_element`), in the order they should appear.
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
#' A dataset may carry no `meta$mtime`: its modification time prints no available message, as a
#' bare `NULL` would collapse the whole `sprintf()` to `character(0)`, dropping
#' that dataset's line entirely, and `format(NULL)` prints `"NULL"`.
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
        mtime <- attr(..(selected_dataset_list_mr())[[idx]], "meta")[["mtime"]]
        mtime <- if (is.null(mtime)) "Time not available" else format(mtime)
        cat(sprintf(
          "\n\n **name**: `%s` **modification time**: %s",
          names(..(selected_dataset_list_mr()))[[idx]],
          mtime
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
  checkmate::assert_choice(output_format, as.character(unclass(ODG$OUTPUT_FORMAT)))
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
#' @param output_format One of `ODG$OUTPUT_FORMAT` (`"html"`/`"pdf"`).
#' @param filter_reference_code R code, as a string, that prints the filter
#'   reference list at render time (or a "No references found" fallback).
#' @return The markdown section, as a single string.
#' @keywords internal
#' @noRd
build_filter_reference_section <- function(output_format, filter_reference_code) {
  checkmate::assert_choice(output_format, as.character(unclass(ODG$OUTPUT_FORMAT)))

  if (output_format == ODG$OUTPUT_FORMAT$HTML) {
    wrapper_tags <- c("<pre>", "</pre>")
  } else if (output_format == ODG$OUTPUT_FORMAT$PDF) {
    wrapper_tags <- c("```{=latex}", "```")
  }

  sprintf(
    "%s\n\n%s\n\n```{r filter_odg_reference_list, echo = FALSE, results='asis'}\n\n%s\n\n```\n\n%s",
    "# Filter references:",
    wrapper_tags[[1]],
    filter_reference_code,
    wrapper_tags[[2]]
  )
}

#' Preprocess every selected ODG element, in declaration order
#'
#' `get_code_in_context()` must receive the *call* to a metareactive, not its
#' value: `shinymeta::expandChain()` forces that argument in meta mode to get
#' code out of it. Resolved values (`resolved`, and the second `latex()` call
#' below) are therefore only ever used for inspection, never handed to it.
#'
#' A `"chunk"` element is a `function(header)` returning Rmd markup, which is
#' inserted verbatim and must contain `header` (formatted for `output_format`).
#'
#' @param odg_elements Named list of all flattened ODG elements, keyed by id.
#' @param is_selected Named logical vector, keyed by element id.
#' @param output_format One of `ODG$OUTPUT_FORMAT` (`"html"`/`"pdf"`).
#' @param get_code_in_context `function(x)` turning a metareactive call into the
#'   R code reproducing it. Injected so it can be tested independently.
#' @return Unnamed list of preprocessed elements: each with `metareactive`
#'   dropped and `code` and `kind` (one of `ODG$ELEMENT_KIND`) added.
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

    if (is.null(metareactive)) {
      code <- paste("Error creating", odg_element[["id"]], "Not available in", output_format, "format")
      kind <- ODG$ELEMENT_KIND$ERROR
    } else if (inherits(metareactive, "chunk")) {
      # The chunk places the header itself, e.g. after a `\beginwidepage` whose `\clearpage` would otherwise strand it
      code <- metareactive(header = ODG_ELEMENT_FORMATTERS[[output_format]][["header"]](odg_element))
      kind <- ODG$ELEMENT_KIND$CHUNK
    } else if (inherits(metareactive, "shinymeta_reactive")) {
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
                dv.manager::latex_header_repeat()
            },
            inline = TRUE
          )
        } else if (inherits(resolved, "gt_tbl")) {
          shinymeta::metaReactive(
            {
              ..(metareactive()) |>
                gt::as_latex() |>
                dv.manager::latex_header_repeat()
            },
            inline = TRUE
          )
        } else {
          stop("Unknown table type")
        }

        code <- get_code_in_context(latex())
        kind <- ODG$ELEMENT_KIND$TABLE
      } else {
        code <- get_code_in_context(metareactive())
        kind <- ODG$ELEMENT_KIND$DEFAULT
      }
    } else {
      code <- paste(
        "Error creating",
        odg_element[["id"]],
        "`metareactive` field is not a metareactive.",
        "Module is not prepared or not activated for generating output documentation."
      )
      kind <- ODG$ELEMENT_KIND$ERROR
    }

    odg_element[["metareactive"]] <- NULL
    odg_element[["code"]] <- code
    odg_element[["kind"]] <- kind

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
#'   (default) or `NULL` (the tab input is not set yet) to check all of them. Every module is listed either way.
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
  select_all <- is.null(selected_tab) || is.na(selected_tab)

  for (curr_el in odg_elements) {
    is_selected <- select_all || curr_el[["module_id"]] == selected_tab
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
            # selection is reset when modal opens, same value is sent twice
            "Shiny.setInputValue('%s', {value: this.checked, id: '%s'}, {priority: 'event'});",
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

# Workaround for gt versions predating the latex.header_repeat tab_options()
# Usage: gt(tbl) |> tab_options(latex.use_longtable = TRUE) |> as_latex() |>
# latex.header_repeat()
# In specific environments we won't have access to gt (>=1.3.0)
# To be removed when the access if granted
#' Repeat the header of a gt longtable on every page
#'
#' Exported only so the generated output documentation code can call it.
#'
#' @param x A gt LaTeX table, as returned by `gt::as_latex()`.
#' @return `x` with `\endhead` after the header rule, as `knit_asis`.
#' @keywords internal
#' @export
#' @noRd
latex_header_repeat <- function(x) {
  attrs <- attributes(x)
  txt <- as.character(x)
  stopifnot(length(txt) == 1) # only latex output, gt objects turn into longer vectors
  if (!grepl("\\\\endhead", txt)) {
    # the midrule line carries trailing spacing directives in some gt versions
    txt <- sub("(\\\\midrule[^\n]*\n)", "\\1\\\\endhead\n", txt)
  }
  # `as.character()` drops the `knit_meta` attribute carrying gt's latex_dependency
  # list (longtable, booktabs, ...); without it knitr emits no \usepackage lines and
  # the document fails to compile with "Environment longtable undefined"
  attributes(txt) <- attrs
  class(txt) <- "knit_asis"
  txt
}

# Aliases
# shinymeta::metaReactive2
sm_mr2 <- shinymeta::metaReactive2

# shinymeta::metaReactive
sm_mr <- local({
  f <- shinymeta::metaReactive
  fargs <- formals(f)
  fargs$localize <- TRUE
  formals(f) <- fargs
  f
})

# shinymeta::metaExpr
sm_me <- local({
  f <- shinymeta::metaExpr
  fargs <- formals(f)
  fargs$localize <- TRUE
  formals(f) <- fargs
  f
})
