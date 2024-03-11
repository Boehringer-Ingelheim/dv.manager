#' logger
#'
#' This article refers to all the functions that are used by the logger.
#' Please refer to `vignette("logging")`.
#'
#' @details
#' It removes all previous globalCallingHandlers by default
#'
#' @name logging
#'
NULL

#' @describeIn logging Activate logging
#' @param h `[function(0+)]`
#'
#' A named list of functions that will manage the signaled conditions
#'
#' @param .rm_gh `[logical(1)]`
#'
#' Remove the previous globalCallingHandlers?
#' @export
#'
log_activate <- function(h = log_default_handlers(), .rm_gh = TRUE) {
  globalCallingHandlers(NULL)
  cli::cli_h1("dv.manager logging is {.emph ACTIVATED}")
  cli::cli_text("By default dv.manager logging removes all globalCallingHandlers from the environment")
  cli::cli_text("")
  cli::cli_text("If you find an error similar to the one below, this logging feature is probably the responsible:")
  cli::cli_code(
    "Error in globalCallingHandlers(foo = function(c) NULL) : should not be called with handlers on the stack"
  )
  cli::cli_text("")

  # If an error occurs while logging it is captured so the application does not stop because of it
  safe_handler <- function(h) {
    function(e) {
      tryCatch(
        h(e),
        error = function(err) {
          rlang::inform(paste(err[["message"]], "| while logging:", e[["message"]]))
        }
      )
    }
  }

  safe_h <- purrr::map(h, safe_handler)
  options("dv.mm.log_handlers" = safe_h)
  globalCallingHandlers(safe_h)
}

#' @describeIn logging deactivate logging
#' @export
log_deactivate <- function(h, .rm_gh = TRUE) {
  options("dv.mm.log_handlers" = NULL)
  globalCallingHandlers(NULL)
  cli::cli_h1("dv.manager logging is {.emph DEACTIVATED}")
}

#' globalCallinHandlers do not capture the output of UI and Server for some reason therefore
#' specific with handlers are applied to them
#' Because there is no easy way of passing those handlers we use an option to pass those.
#' @keywords internal
log_with_logging_handlers <- function(fnc) {
  function(...) purrr::partial(withCallingHandlers, !!!getOption("dv.mm.log_handlers"))(expr = fnc(...))
}

#' @describeIn logging get default handlers
#' @param level `[character(1)|numeric(1)]`
#'
#' Logging level
#'
#' @export
log_default_handlers <- function(level = 999) {
  format_str <- "[{date}][{package}|{short_sess_id}|{ns}]:{message}"

  cnd_to_str <- function(cnd) {
    cnd %>%
      log_add_date() %>%
      log_add_ns() %>%
      log_add_sess_id() %>%
      log_add_short_sess_id() %>%
      log_format(format_str)
  }

  list(
    message = function(cnd) {
      if (log_test_level(cnd, level)) cli::cli_alert_info(cnd_to_str(cnd))

      rlang::cnd_muffle(cnd)
    },
    warning = function(cnd) {
      if (log_test_level(cnd, level)) cli::cli_alert_warning(cnd_to_str(cnd))
      rlang::cnd_muffle(cnd)
    }
  )
}

#' @describeIn logging add date field to a condition
#' @export
log_add_date <- function(cnd) {
  if (!"date" %in% names(cnd)) {
    cnd[["date"]] <- format(Sys.time(), "%d-%m-%Y %H:%M:%S%z")
  }
  cnd
}

#' @describeIn logging add short_sess_id to a condition
#' @export
log_add_short_sess_id <- function(cnd) {
  if (!"short_sess_id" %in% names(cnd)) {
    cnd[["short_sess_id"]] <- substring(cnd[["sess_id"]], 1, 6)
  }
  cnd
}

#' @describeIn logging Tests if the cnd level is lower than the logger level
#' @param cnd `[condition(1)]`
#'
#'  A condition
#'
#' @param .default `[logical(1)]`
#'
#' default behavior when level is not found in the condition
#'
#' @export
log_test_level <- function(cnd, level, .default = TRUE) {
  checkmate::assert(
    checkmate::check_character(level, null.ok = FALSE),
    checkmate::check_numeric(level, null.ok = FALSE)
  )

  lvl2num <- function(lvl) {
    if (is.character(lvl)) {
      lvl <- do.call(switch, c(list(EXPR = tolower(lvl)), log_get_level_list()))
    }
    lvl
  }

  if ("level" %in% names(cnd)) {
    lvl2num(cnd[["level"]]) <= lvl2num(level)
  } else {
    .default
  }
}

#' @describeIn logging creates a log string according to a format.
#' @param ... parameter passed to glue
#' @export
log_format <- function(cnd, ...) {
  # No need of glue_safe as the problem is providing the string to be formatted itself
  # Nonetheless we provide a transformer to cover the case in which log_format tries to use
  # a non-existent variable.

  safe_transformer <- function(text, envir) {
    t <- if (text %in% names(cnd)) cnd[[text]] else NA_character_
    t <- if (length(t) == 0) NA_character_ else t
    t
  }
  glue::glue(..., .transformer = safe_transformer)
}

#' @describeIn logging add sess_id to a condition
#' @export
log_add_sess_id <- function(cnd) {
  if (!"sess_id" %in% names(cnd)) {
    session <- shiny::getDefaultReactiveDomain()
    if (!is.null(session)) {
      sess_id <- session[["token"]]
    } else {
      sess_id <- NA_character_
    }
    cnd[["sess_id"]] <- sess_id
  }
  cnd
}

#' @describeIn logging add ns to a condition
#' @export
log_add_ns <- function(cnd) {
  if (!"ns" %in% names(cnd)) {
    session <- shiny::getDefaultReactiveDomain()
    if (!is.null(session)) {
      ns <- session[["ns"]]("")
      ns <- substring(ns, 1, nchar(ns) - 1)
      ns <- if (nchar(ns) == 0) "(Root)" else ns
    } else {
      ns <- NA_character_
    }
    cnd[["ns"]] <- ns
  }
  cnd
}

#' @describeIn logging List of levels in the logger
#' @export
log_get_level_list <- function() {
  list(
    debug = 100,
    info = 50,
    warning = 20,
    error = 10,
    critical = 1,
    0
  )
}

#' @describeIn logging Adds the utils_logging file to the package
#' @export
log_use_log <- function() {
  origin <- system.file("templates/utils_logging.R", package = "dv.manager", mustWork = TRUE)
  target <- "R/utils_logging.R"
  if (file.exists(target)) rlang::abort(paste(target, "already exists"))
  file.copy(origin, target)
  rlang::inform(paste("Added file:", target))
}
