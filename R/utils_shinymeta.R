is.metareactive <- function(x) {
  # nolint
  inherits(x, "shinymeta_reactive")
}

is.anyreactive <- function(x) {
  # nolint
  inherits(x, "reactive") | inherits(x, "shinymeta_reactive")
}

format_with_air <- function(text, line_width = 80L) {
  air_bin <- system.file("bin/air", package = "dv.manager")
  if (!file.exists(air_bin)) {
    return(text)
  }

  # Write an air.toml in a temp dir so Air picks up the line width
  config_dir <- tempfile("air_config_")
  dir.create(config_dir)
  writeLines(
    c("[format]", sprintf("line-width = %d", line_width)),
    file.path(config_dir, "air.toml")
  )
  on.exit(unlink(config_dir, recursive = TRUE), add = TRUE)

  stdin_path <- file.path(config_dir, "input.R")

  stdout_file <- tempfile("air_stdout_")
  stderr_file <- tempfile("air_stderr_")
  on.exit(unlink(c(stdout_file, stderr_file)), add = TRUE)

  status <- system2(
    air_bin,
    c("format", "--stdin-file-path", stdin_path),
    input = paste(text, collapse = "\n"),
    stdout = stdout_file,
    stderr = stderr_file
  )

  if (status != 0) {
    log_warn("Air formatting failed:\n", paste(readLines(stderr_file), collapse = "\n"))
    return(text)
  }

  readLines(stdout_file)
}

# TODO: An overkill but for the moment we depend on gt to print latex table
# IN the future we can do our own implementation
escape_latex <- gt::escape_latex
