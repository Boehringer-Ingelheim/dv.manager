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

    result <- render_odg_document(make_odg_rmd("1 + 1"), dummy_header, spy$fn, zip_path)

    expect_identical(as.vector(result), zip_path)
    expect_identical(getwd(), wd)
    expect_true(file.exists(zip_path))
    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true(any(endsWith(contents, ".html")))
    expect_false("error.txt" %in% contents)
    expect_length(attr(result, "error_msg"), 0)
    expect_length(spy$calls$log, 0)
  })

  test_that("render_odg_document errors when the zip command fails", {
    zip_path <- tempfile(fileext = ".zip")
    withr::local_envvar(R_ZIPCMD = file.path(tempdir(), "no-such-zip"))

    no_attach <- function(pdf, attachment) NULL
    expect_error(
      suppressWarnings(render_odg_document(make_odg_rmd("1 + 1"), dummy_header, no_attach, zip_path)),
      "zip failed with status"
    )
    expect_false(file.exists(zip_path))
  })

  test_that("render_odg_document renders PDF and attaches the source Rmd and session info", {
    skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
    skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not available")

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

    result <- render_odg_document(make_odg_rmd("1 + 1", output = "pdf_document"), dummy_header, spy$fn, zip_path)

    expect_true(file.exists(zip_path))
    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true(any(endsWith(contents, ".pdf")))
    expect_length(attr(result, "error_msg"), 0)

    # Attaches the rendered Rmd source and a session-info file, both to the same pdf
    expect_length(spy$calls$log, 2)
    expect_true(all(endsWith(vapply(spy$calls$log, `[[`, character(1), "pdf"), ".pdf")))
    attachments <- vapply(spy$calls$log, function(x) basename(x[["attachment"]]), character(1))
    expect_setequal(attachments, c("odg.Rmd", "session_info.txt"))
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
      result <- render_odg_document(make_odg_rmd('stop("boom")'), dummy_header, spy$fn, zip_path),
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
        filename = zip_path
      )
    )

    expect_true(file.exists(zip_path))
    expect_length(attr(result, "error_msg"), 0)
  })

  test_that("a PDF error element whose message holds LaTeX and markdown characters renders as a warning box", {
    skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
    skip_if_not(nzchar(Sys.which("xelatex")), "xelatex not available")

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
      sections = list(date = "", hardcoded_hash = "", dynamic_hash = "", filter_txt = "", filter_reference = ""),
      templates = list(header = ODG$TEMPLATES$HEADER[[output_format]], session_info = "", footer = "")
    )
    header <- paste(
      readLines(system.file("odg_files/header.tex", package = "dv.manager", mustWork = TRUE), warn = FALSE),
      collapse = "\n"
    )

    result <- render_odg_document(rmd, header, function(pdf, attachment) invisible(NULL), zip_path)
    expect_length(attr(result, "error_msg"), 0)

    unzip_dir <- tempfile()
    utils::unzip(zip_path, exdir = unzip_dir)
    tex <- paste(readLines(file.path(unzip_dir, "odg.tex"), warn = FALSE), collapse = "\n")
    # Unparsable raw LaTeX makes pandoc print the whole command as text: `\textbackslash alertwarning\{...`
    expect_match(tex, "\\alertwarning{Error creating mod\\_1-tbl", fixed = TRUE)
    expect_match(tex, "\\%>\\%", fixed = TRUE)
  })

  test_that("pdf_attach attaches a file into a PDF", {
    skip_if_not(nzchar(Sys.which("qpdf")), "qpdf not available")

    pdf_path <- tempfile(fileext = ".pdf")
    grDevices::pdf(pdf_path)
    plot(1)
    grDevices::dev.off()
    attachment_path <- tempfile(fileext = ".txt")
    writeLines("hello", attachment_path)
    on.exit(
      {
        if (file.exists(pdf_path)) {
          unlink(pdf_path)
        }
        if (file.exists(attachment_path)) {
          unlink(attachment_path)
        }
      },
      add = TRUE
    )

    result <- pdf_attach(pdf_path, attachment_path)

    expect_identical(result, pdf_path)
    listing <- system2("qpdf", c("--list-attachments", shQuote(pdf_path)), stdout = TRUE)
    expect_true(any(grepl(basename(attachment_path), listing, fixed = TRUE)))
  })
})
