local({
  make_odg_rmd <- function(chunk_body, output = "html_document") {
    paste(
      c(
        "---",
        "title: \"export\"",
        sprintf("output: %s", output),
        "---",
        "",
        "```{r}",
        chunk_body,
        "```"
      ),
      collapse = "\n"
    )
  }

  dummy_header <- "% dummy header"

  spy_pdf_attach <- function() {
    calls <- new.env()
    calls$log <- list()
    fn <- function(pdf, attachment) {
      calls$log[[length(calls$log) + 1]] <- list(pdf = pdf, attachment = attachment)
    }
    list(fn = fn, calls = calls)
  }

  # Rendering the ODG template needs xelatex
  # Reading the PDF back needs pdftotext
  # Attaching files needs qpdf
  stop_if_cannot_render <- function() {
    stopifnot(
      "pandoc not available" = rmarkdown::pandoc_available(),
      "xelatex not available" = nzchar(Sys.which("xelatex"))
    )
  }

  stop_if_no_pdftotext <- function() stopifnot("pdftotext not available" = nzchar(Sys.which("pdftotext")))

  stop_if_no_qpdf <- function() stopifnot("qpdf not available" = nzchar(Sys.which("qpdf")))

  test_that("render_odg_document renders HTML, zips it, and never calls pdf_attach_function", {
    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    spy <- spy_pdf_attach()
    wd <- getwd()

    result <- render_odg_document(make_odg_rmd("1 + 1"), dummy_header, spy$fn, zip_path, "my_report")

    expect_identical(as.vector(result), zip_path)
    expect_identical(getwd(), wd)
    expect_true(file.exists(zip_path))
    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_setequal(contents, c("my_report.Rmd", "my_report.html", "header.tex"))
    expect_length(attr(result, "error_msg"), 0)
    expect_length(spy$calls$log, 0)
  })

  test_that("render_odg_document errors when the zip command fails", {
    zip_path <- tempfile(fileext = ".zip")
    withr::local_envvar(R_ZIPCMD = file.path(tempdir(), "no-such-zip"))

    no_attach <- function(pdf, attachment) NULL
    expect_error(
      suppressWarnings(render_odg_document(make_odg_rmd("1 + 1"), dummy_header, no_attach, zip_path, "odg")),
      "zip failed with status"
    )
    expect_false(file.exists(zip_path))
  })

  test_that("render_odg_document evaluates the document apart from its own variables", {
    zip_path <- tempfile(fileext = ".zip")
    probe_path <- tempfile(fileext = ".txt")
    on.exit(unlink(c(zip_path, probe_path)), add = TRUE)

    # A chunk assigning `absolute_zip_path` would otherwise redirect the zip into the temporary dir deleted on exit
    chunk_body <- c(
      sprintf('writeLines(as.character(exists("odg_dir")), "%s")', probe_path),
      'absolute_zip_path <- "hijacked.zip"'
    )
    result <- render_odg_document(make_odg_rmd(chunk_body), dummy_header, spy_pdf_attach()$fn, zip_path, "odg")

    expect_identical(readLines(probe_path), "FALSE")
    expect_identical(as.vector(result), zip_path)
    expect_true(file.exists(zip_path))
    expect_length(attr(result, "error_msg"), 0)
  })

  test_that("render_odg_document renders PDF and attaches the source Rmd and session info", {
    # A plain `pdf_document` renders with pdflatex, not with the xelatex of the ODG template
    stopifnot(
      "pandoc not available" = rmarkdown::pandoc_available(),
      "pdflatex not available" = nzchar(Sys.which("pdflatex"))
    )

    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    spy <- spy_pdf_attach()

    result <- render_odg_document(
      make_odg_rmd("1 + 1", output = "pdf_document"),
      dummy_header,
      spy$fn,
      zip_path,
      "my_report"
    )

    expect_true(file.exists(zip_path))
    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true("my_report.pdf" %in% contents)
    expect_length(attr(result, "error_msg"), 0)

    # Attaches the rendered Rmd source and a session-info file, both to the same pdf
    expect_length(spy$calls$log, 2)
    expect_true(all(endsWith(vapply(spy$calls$log, `[[`, character(1), "pdf"), ".pdf")))
    attachments <- vapply(spy$calls$log, function(x) basename(x[["attachment"]]), character(1))
    expect_setequal(attachments, c("my_report.Rmd", "session_info.txt"))
  })

  test_that("render_odg_document captures render errors in error.txt without calling pdf_attach_function", {
    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    spy <- spy_pdf_attach()

    result <- NULL
    expect_warning(
      result <- render_odg_document(make_odg_rmd('stop("boom")'), dummy_header, spy$fn, zip_path, "odg"),
      regexp = "Error generating output documentation"
    )

    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true("error.txt" %in% contents)
    expect_match(attr(result, "error_msg"), "boom")
    expect_length(spy$calls$log, 0)
  })

  test_that("render_odg_document works when run inside a callr subprocess", {
    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    result <- callr::r(
      render_odg_document,
      args = list(
        rmarkdown = make_odg_rmd("1 + 1"),
        header = dummy_header,
        pdf_attach_function = function(pdf, attachment) invisible(NULL),
        absolute_zip_path = zip_path,
        document_name = "odg"
      )
    )

    expect_true(file.exists(zip_path))
    expect_length(attr(result, "error_msg"), 0)
  })

  test_that("a PDF error element whose message holds LaTeX and markdown characters renders as a warning box", {
    stop_if_cannot_render()

    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    error_element <- list(
      id = "mod_1-tbl",
      kind = ODG$ELEMENT_KIND$ERROR,
      code = 'Error creating mod_1-tbl could not find function "%>%" {#1} *a*',
      label = "Tbl",
      module_name = "Module 1",
      is_first_module_element = TRUE
    )
    output_format <- ODG$OUTPUT_FORMAT$PDF
    rmd <- build_odg_rmd(
      list(error_element),
      output_format,
      data_code = "NULL",
      filtered_data_code = "NULL",
      sections = list(date = "", hardcoded_hash = "", dynamic_hash = "", filter_txt = "", filter_reference = ""),
      templates = list(header = ODG$TEMPLATES$HEADER[[output_format]], session_info = "", footer = "")
    )
    header <- paste(
      readLines(system.file("odg_files/header.tex", package = "dv.manager", mustWork = TRUE), warn = FALSE),
      collapse = "\n"
    )

    result <- render_odg_document(rmd, header, function(pdf, attachment) invisible(NULL), zip_path, "odg")
    expect_length(attr(result, "error_msg"), 0)

    unzip_dir <- tempfile()
    utils::unzip(zip_path, exdir = unzip_dir)
    tex <- paste(readLines(file.path(unzip_dir, "odg.tex"), warn = FALSE), collapse = "\n")
    # Unparsable raw LaTeX makes pandoc print the whole command as text: `\textbackslash alertwarning\{...`
    expect_match(tex, "\\alertwarning{Error creating mod\\_1-tbl", fixed = TRUE)
    expect_match(tex, "\\%>\\%", fixed = TRUE)
  })

  test_that("the PDF warnings summary prints condition messages verbatim", {
    stop_if_cannot_render()
    stop_if_no_pdftotext()

    zip_path <- tempfile(fileext = ".zip")
    on.exit(unlink(zip_path), add = TRUE)

    # A backslash used to stop the render (`\new` is an undefined control sequence), the rest was read as markup
    msg <- "cannot open C:\\new\\file *bold* _u_ $a^2$ <b> 100% #1 {x} ~y &"
    warning_element <- list(
      id = "mod_1-warn",
      chunk_label = "mod_1-warn",
      kind = ODG$ELEMENT_KIND$DEFAULT,
      code = sprintf("warning(%s)", deparse(msg)),
      label = "Warns",
      module_name = "Module 1",
      is_first_module_element = TRUE
    )
    output_format <- ODG$OUTPUT_FORMAT$PDF
    rmd <- build_odg_rmd(
      list(warning_element),
      output_format,
      data_code = "NULL",
      filtered_data_code = "NULL",
      sections = list(date = "", hardcoded_hash = "", dynamic_hash = "", filter_txt = "", filter_reference = ""),
      templates = list(
        header = ODG$TEMPLATES$HEADER[[output_format]],
        session_info = "",
        footer = ODG$TEMPLATES$FOOTER[[output_format]]
      )
    )
    header <- paste(
      readLines(system.file("odg_files/header.tex", package = "dv.manager", mustWork = TRUE), warn = FALSE),
      collapse = "\n"
    )

    result <- render_odg_document(rmd, header, function(pdf, attachment) invisible(NULL), zip_path, "odg")
    expect_length(attr(result, "error_msg"), 0)

    unzip_dir <- tempfile()
    utils::unzip(zip_path, exdir = unzip_dir)
    expect_true(file.exists(file.path(unzip_dir, "odg.pdf")))

    txt_file <- tempfile(fileext = ".txt")
    system2("pdftotext", c("-raw", shQuote(file.path(unzip_dir, "odg.pdf")), shQuote(txt_file)))
    pdf_txt <- paste(readLines(txt_file, warn = FALSE), collapse = " ")
    expect_match(pdf_txt, msg, fixed = TRUE)
    expect_match(pdf_txt, "[WARNING] mod_1-warn", fixed = TRUE)
  })

  test_that("a PDF table with a cut long cell renders when the element id holds LaTeX special characters", {
    stop_if_cannot_render()

    zip_path <- tempfile(fileext = ".zip")
    on.exit(unlink(zip_path), add = TRUE)

    # The id prefixes the link targets of cut cells: `%` ends the line, `#` is a macro parameter, `(` ends a PDF string
    id <- "mod%1-t#b(l)"
    table_element <- list(
      id = id,
      chunk_label = gsub("[^A-Za-z0-9_-]", "_", id), # as app_server_() sets it
      label = "Long cells",
      module_name = "Module 1",
      is_first_module_element = TRUE,
      codegen = list(pdf = shinymeta::metaReactive(data.frame(txt = strrep("long text ", 25)), varname = "long_src"))
    )
    output_format <- ODG$OUTPUT_FORMAT$PDF
    get_code_in_context <- function(x) paste(as.character(shinymeta::expandChain(x)), collapse = "\n")
    elements <- shiny::isolate(preprocess_odg_elements(list(table_element), output_format, get_code_in_context))
    expect_identical(elements[[1]][["kind"]], ODG$ELEMENT_KIND$TABLE)

    rmd <- build_odg_rmd(
      elements,
      output_format,
      data_code = "NULL",
      filtered_data_code = "NULL",
      sections = list(date = "", hardcoded_hash = "", dynamic_hash = "", filter_txt = "", filter_reference = ""),
      templates = list(header = ODG$TEMPLATES$HEADER[[output_format]], session_info = "", footer = "")
    )
    header <- paste(
      readLines(system.file("odg_files/header.tex", package = "dv.manager", mustWork = TRUE), warn = FALSE),
      collapse = "\n"
    )

    result <- render_odg_document(rmd, header, function(pdf, attachment) invisible(NULL), zip_path, "odg")
    expect_length(attr(result, "error_msg"), 0)

    unzip_dir <- tempfile()
    utils::unzip(zip_path, exdir = unzip_dir)
    expect_true(file.exists(file.path(unzip_dir, "odg.pdf")))
    tex <- paste(readLines(file.path(unzip_dir, "odg.tex"), warn = FALSE), collapse = "\n")
    expect_match(tex, "\\longcellanchor{mod_1-t_b_l_-cell-1}", fixed = TRUE)
    expect_match(tex, "\\longcellmark{mod_1-t_b_l_-note-1}", fixed = TRUE)
  })

  test_that("a PDF table element from a gt_tbl with default options renders as a longtable", {
    stop_if_cannot_render()

    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    # gt's default as_latex() output is a `table`/`tabular*`, where the `\endhead` added for repeated headers is undefined
    gt_element <- list(
      id = "mod_1-gt",
      label = "Gt table",
      module_name = "Module 1",
      is_first_module_element = TRUE,
      codegen = list(pdf = shinymeta::metaReactive(gt::gt(data.frame(a = 1:2, b = c("x", "y"))), varname = "gt_src"))
    )
    output_format <- ODG$OUTPUT_FORMAT$PDF
    get_code_in_context <- function(x) paste(as.character(shinymeta::expandChain(x)), collapse = "\n")
    elements <- shiny::isolate(preprocess_odg_elements(list(gt_element), output_format, get_code_in_context))
    expect_identical(elements[[1]][["kind"]], ODG$ELEMENT_KIND$TABLE)

    rmd <- build_odg_rmd(
      elements,
      output_format,
      data_code = "NULL",
      filtered_data_code = "NULL",
      sections = list(date = "", hardcoded_hash = "", dynamic_hash = "", filter_txt = "", filter_reference = ""),
      templates = list(header = ODG$TEMPLATES$HEADER[[output_format]], session_info = "", footer = "")
    )
    header <- paste(
      readLines(system.file("odg_files/header.tex", package = "dv.manager", mustWork = TRUE), warn = FALSE),
      collapse = "\n"
    )

    result <- render_odg_document(rmd, header, function(pdf, attachment) invisible(NULL), zip_path, "odg")
    expect_length(attr(result, "error_msg"), 0)

    unzip_dir <- tempfile()
    utils::unzip(zip_path, exdir = unzip_dir)
    expect_true(file.exists(file.path(unzip_dir, "odg.pdf")))
    tex <- paste(readLines(file.path(unzip_dir, "odg.tex"), warn = FALSE), collapse = "\n")
    expect_match(tex, "\\begin{longtable}", fixed = TRUE)
    expect_match(tex, "\\endhead", fixed = TRUE)
  })

  test_that("render_odg_document rejects an empty document name or one whose PDF render would overwrite the header", {
    # Errors before writing anything, so nothing is rendered
    for (document_name in c("", "header", "Header", "HEADER")) {
      expect_error(
        render_odg_document("", "", function(pdf, attachment) invisible(NULL), tempfile(), document_name),
        "document_name",
        info = document_name
      )
    }
  })

  test_that("pdf_attach attaches a file into a PDF in place", {
    stop_if_no_qpdf()

    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    pdf_path <- file.path(dir, "doc.pdf")
    grDevices::pdf(pdf_path)
    plot(1)
    grDevices::dev.off()
    attachment_path <- file.path(dir, "attachment.txt")
    writeLines("hello", attachment_path)

    result <- pdf_attach(pdf_path, attachment_path)

    expect_identical(result, pdf_path)
    listing <- system2("qpdf", c("--list-attachments", shQuote(pdf_path)), stdout = TRUE)
    expect_true(any(grepl(basename(attachment_path), listing, fixed = TRUE)))
    # qpdf's --replace-input works through a sibling temp file; none must be left behind
    expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), c("doc.pdf", "attachment.txt"))
  })

  test_that("pdf_attach errors and leaves the input untouched when qpdf fails", {
    stop_if_no_qpdf()

    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    pdf_path <- file.path(dir, "not_a.pdf")
    writeLines("not a pdf", pdf_path)
    attachment_path <- file.path(dir, "attachment.txt")
    writeLines("hello", attachment_path)
    original_md5 <- unname(tools::md5sum(pdf_path))

    expect_error(pdf_attach(pdf_path, attachment_path), "qpdf failed with status")

    expect_identical(unname(tools::md5sum(pdf_path)), original_md5)
    expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), c("not_a.pdf", "attachment.txt"))
  })
})
