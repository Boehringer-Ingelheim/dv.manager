local({
  write_export_rmd <- function(export_dir, chunk_body, output = "html_document") {
    export_rmd <- file.path(export_dir, "export.Rmd")
    writeLines(
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
      export_rmd
    )
    export_rmd
  }

  spy_pdf_attach <- function() {
    calls <- new.env()
    calls$log <- list()
    fn <- function(pdf, attachment) {
      calls$log[[length(calls$log) + 1]] <- list(pdf = pdf, attachment = attachment)
    }
    list(fn = fn, calls = calls)
  }

  test_that("render_export_document renders HTML, zips it, and never calls pdf_attach_function", {
    export_dir <- tempfile("export_test_")
    dir.create(export_dir)
    header_file <- tempfile(fileext = ".tex")
    writeLines("% dummy header", header_file)
    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (dir.exists(export_dir)) {
          unlink(export_dir, recursive = TRUE)
        }
        if (file.exists(header_file)) {
          unlink(header_file)
        }
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    export_rmd <- write_export_rmd(export_dir, "1 + 1", output = "html_document")
    spy <- spy_pdf_attach()

    result <- render_export_document(export_rmd, export_dir, header_file, spy$fn, zip_path)

    expect_true(file.exists(zip_path))
    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true(any(endsWith(contents, ".html")))
    expect_false("error.txt" %in% contents)
    expect_length(attr(result, "error_msg"), 0)
    expect_length(spy$calls$log, 0)
  })

  test_that("render_export_document renders PDF and attaches the source Rmd and session info", {
    skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
    skip_if_not(nzchar(Sys.which("pdflatex")), "pdflatex not available")

    export_dir <- tempfile("export_test_")
    dir.create(export_dir)
    header_file <- tempfile(fileext = ".tex")
    writeLines("% dummy header", header_file)
    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (dir.exists(export_dir)) {
          unlink(export_dir, recursive = TRUE)
        }
        if (file.exists(header_file)) {
          unlink(header_file)
        }
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    export_rmd <- write_export_rmd(export_dir, "1 + 1", output = "pdf_document")
    spy <- spy_pdf_attach()

    result <- render_export_document(export_rmd, export_dir, header_file, spy$fn, zip_path)

    expect_true(file.exists(zip_path))
    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true(any(endsWith(contents, ".pdf")))
    expect_length(attr(result, "error_msg"), 0)

    # Attaches the rendered Rmd source and a session-info file, both to the same pdf
    expect_length(spy$calls$log, 2)
    expect_true(all(endsWith(vapply(spy$calls$log, `[[`, character(1), "pdf"), ".pdf")))
    attachments <- vapply(spy$calls$log, function(x) basename(x[["attachment"]]), character(1))
    expect_setequal(attachments, c(basename(export_rmd), "session_info.txt"))
  })

  test_that("render_export_document captures render errors in error.txt without calling pdf_attach_function", {
    export_dir <- tempfile("export_test_")
    dir.create(export_dir)
    header_file <- tempfile(fileext = ".tex")
    writeLines("% dummy header", header_file)
    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (dir.exists(export_dir)) {
          unlink(export_dir, recursive = TRUE)
        }
        if (file.exists(header_file)) {
          unlink(header_file)
        }
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    export_rmd <- write_export_rmd(export_dir, 'stop("boom")', output = "html_document")
    spy <- spy_pdf_attach()

    result <- NULL
    expect_warning(
      result <- render_export_document(export_rmd, export_dir, header_file, spy$fn, zip_path),
      regexp = "Error rendering export"
    )

    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true("error.txt" %in% contents)
    expect_match(attr(result, "error_msg"), "boom")
    expect_length(spy$calls$log, 0)
  })

  test_that("render_export_document works when run inside a callr subprocess", {
    export_dir <- tempfile("export_test_")
    dir.create(export_dir)
    header_file <- tempfile(fileext = ".tex")
    writeLines("% dummy header", header_file)
    zip_path <- tempfile(fileext = ".zip")
    on.exit(
      {
        if (dir.exists(export_dir)) {
          unlink(export_dir, recursive = TRUE)
        }
        if (file.exists(header_file)) {
          unlink(header_file)
        }
        if (file.exists(zip_path)) {
          unlink(zip_path)
        }
      },
      add = TRUE
    )

    export_rmd <- write_export_rmd(export_dir, "1 + 1", output = "html_document")

    result <- callr::r(
      render_export_document,
      args = list(
        export_rmd = export_rmd,
        export_dir = export_dir,
        header_file = header_file,
        pdf_attach_function = function(pdf, attachment) invisible(NULL),
        filename = zip_path
      )
    )

    expect_true(file.exists(zip_path))
    expect_length(attr(result, "error_msg"), 0)
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
