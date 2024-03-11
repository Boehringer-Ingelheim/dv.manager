gch_debug <- list(
  message = function(cnd) {
    try(
      withCallingHandlers(
        shiny::showNotification(cnd$message, type = "message"),
        error = function(e) {
          rlang::abort(
            "Could not show notification for message:",
            parent = e
          )
        }
      )
    )
  },
  warning = function(cnd) {
    try(
      withCallingHandlers(
        shiny::showNotification(cnd$message, type = "warning"),
        error = function(e) {
          rlang::abort(
            "Could not show notification for warning:",
            parent = e
          )
        }
      )
    )
  },
  error = function(cnd) {
    try(
      withCallingHandlers(
        shiny::showNotification(cnd$message, type = "error"),
        error = function(e) {
          rlang::abort(
            "Could not show notification for error:",
            parent = e
          )
        }
      )
    )
  }
)
