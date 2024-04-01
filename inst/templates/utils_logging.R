log_inform <- function(...) {
  rlang::inform(..., package = utils::packageName())
}

log_warn <- function(...) {
  rlang::warn(..., package = utils::packageName())
}
