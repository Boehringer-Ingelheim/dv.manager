dv.manager:::..activate_export()
on.exit(
  dv.manager:::..deactivate_export(),
  add = TRUE
)

local({
  make_export_rmd <- function(chunk_body, output = "html_document") {
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

  test_that("render_export_document renders HTML, zips it, and never calls pdf_attach_function", {
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

    result <- render_export_document(make_export_rmd("1 + 1"), dummy_header, spy$fn, zip_path)

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

    result <- render_export_document(make_export_rmd("1 + 1", output = "pdf_document"), dummy_header, spy$fn, zip_path)

    expect_true(file.exists(zip_path))
    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true(any(endsWith(contents, ".pdf")))
    expect_length(attr(result, "error_msg"), 0)

    # Attaches the rendered Rmd source and a session-info file, both to the same pdf
    expect_length(spy$calls$log, 2)
    expect_true(all(endsWith(vapply(spy$calls$log, `[[`, character(1), "pdf"), ".pdf")))
    attachments <- vapply(spy$calls$log, function(x) basename(x[["attachment"]]), character(1))
    expect_setequal(attachments, c("export.Rmd", "session_info.txt"))
  })

  test_that("render_export_document captures render errors in error.txt without calling pdf_attach_function", {
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
      result <- render_export_document(make_export_rmd('stop("boom")'), dummy_header, spy$fn, zip_path),
      regexp = "Error rendering export"
    )

    contents <- utils::unzip(zip_path, list = TRUE)[["Name"]]
    expect_true("error.txt" %in% contents)
    expect_match(attr(result, "error_msg"), "boom")
    expect_length(spy$calls$log, 0)
  })

  test_that("render_export_document works when run inside a callr subprocess", {
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
      render_export_document,
      args = list(
        rmarkdown = make_export_rmd("1 + 1"),
        header = dummy_header,
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
