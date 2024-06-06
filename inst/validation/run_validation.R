pkg_name <- read.dcf("DESCRIPTION")[, "Package"]
pkg_version <- read.dcf("DESCRIPTION")[, "Version"]
test_results <- tibble::as_tibble(devtools::test(stop_on_failure = FALSE))

v <- local({
  # This is evaluated inside a local because, otherwise, all the variables created in the chunks of the rendered
  # document leak into the environment

  validation_root <- "./inst/validation"
  validation_report_rmd <- file.path(validation_root, "val_report.Rmd")
  validation_report_html <- "val_report.html"
  validation_results <- file.path(validation_root, "results")
  val_param_rds <- file.path(validation_results, "val_param.rds")

  stopifnot(dir.exists(validation_root))
  stopifnot(file.exists(validation_report_rmd))

  stopifnot(dir.exists(validation_results))
  unlink(list.files(validation_results))

  saveRDS(
    list(
      package = pkg_name,
      tests = test_results,
      version = pkg_version
    ),
    val_param_rds
  )

  rmarkdown::render(
    input = validation_report_rmd,
    params = list(
      package = pkg_name,
      tests = test_results,
      version = pkg_version
    ),
    output_dir = validation_results,
    output_file = validation_report_html
  )

  uncovered_spec
})
