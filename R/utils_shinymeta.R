is.metareactive <- function(x) {
  # nolint
  inherits(x, "shinymeta_reactive")
}

is.anyreactive <- function(x) {
  # nolint
  inherits(x, "reactive") | inherits(x, "shinymeta_reactive")
}

#' Format the R code shown in the ODG report with air
#'
#' air is looked up in the `dv_odg_air_path` option, then in the `DV_ODG_AIR_PATH` environment variable, then on the
#' `PATH`. Meant as the `formatter` of `shinymeta::formatCode()`.
#'
#' @param text `[character]` R code, one or more lines.
#' @return `[character]` Formatted code, one element per line, or `text` unchanged if air is not found or fails.
#' @keywords internal
#' @noRd
format_odg_code <- function(text) {
  candidates <- c(getOption(ODG$AIR_PATH$OPTION, ""), Sys.getenv(ODG$AIR_PATH$ENV_VAR), unname(Sys.which("air")))
  true_candidates <- candidates[file.exists(candidates)]
  if (!length(true_candidates)) {
    return(text)
  }

  air_path <- true_candidates[[1]]

  # A fresh dir with its own air.toml, so air does not pick up any other config
  work_dir <- tempfile("odg_air_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  writeLines(c("[format]", "line-width = 80"), file.path(work_dir, "air.toml"))
  stderr_path <- file.path(work_dir, "stderr")

  formatted <- suppressWarnings(system2(
    air_path,
    c("format", "--stdin-file-path", shQuote(file.path(work_dir, "code.R"))),
    input = text,
    stdout = TRUE,
    stderr = stderr_path
  ))
  status <- attr(formatted, "status")
  if (!is.null(status)) {
    msg <- c(sprintf("air exited with status %d, the code is left unformatted", status), readLines(stderr_path))
    log_warn(paste(msg, collapse = "\n"))
    return(text)
  }
  formatted
}

# TODO: An overkill but for the moment we depend on gt to print latex table
# IN the future we can do our own implementation
escape_latex <- gt::escape_latex
