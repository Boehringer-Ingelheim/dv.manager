log_inform <- function(...) {
  if (isTRUE(getOption("dv.logging.active"))) rlang::inform(..., package = utils::packageName())
}

log_warn <- function(...) {
  if (isTRUE(getOption("dv.logging.active"))) rlang::warn(..., package = utils::packageName())
}
