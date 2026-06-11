log_inform <- function(...) {
  if (isTRUE(getOption("dv.logging.active"))) {
    cnd_create <- function(message, ...) {
      if (is.null(message)) {
        message <- ""
      }
      x <- list(...)
      x[["message"]] <- message
      x
    }
    cnd <- cnd_create(...)
    cnd[["package"]] <- utils::packageName()
    class(cnd) <- c("message", "dv_log", "condition")
    cnd
    message(cnd)
  }
  invisible(NULL)
}

log_warn <- function(...) {
  cnd_create <- function(message, ...) {
    if (is.null(message)) {
      message <- ""
    }
    x <- list(...)
    x[["message"]] <- message
    x
  }
  cnd <- cnd_create(...)
  cnd[["package"]] <- utils::packageName()
  class(cnd) <- c("warning", "dv_log", "condition")
  cnd
  warning(cnd)
  invisible(NULL)
}
