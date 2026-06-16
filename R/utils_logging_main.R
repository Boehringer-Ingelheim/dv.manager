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


logdec <- local({
  n_printable_char <- function(x) {
    x <- gsub("\033\\[[0-9;]*m", "", x) # Remove ANSI codes
    x <- gsub("[\n\r\t\v\f]", "", x) # Remove whitespace
    nchar(x)
  }

  cat_ <- function(x) {
    cat(x, file = stderr())
  }

  cat_line <- function(x) {
    cat_(sprintf("%s\n", x))
  }

  h1 <- function(x) {
    cat_line(paste0(rep("-", min(20, n_printable_char(x))), collapse = ""))
    cat_line(x)
    cat_line(paste0(rep("-", min(20, n_printable_char(x))), collapse = ""))
  }

  alert_info <- function(x) {
    cat_line(sprintf("[*] INFO: %s", x))
  }

  alert_warning <- function(x) {
    cat_line(sprintf("[!] WARNING: %s", x))
  }

  code <- function(x) {
    cat_line("-- START CODE BLOCK -----")
    cat_line(x)
    cat_line("-- END  CODE  BLOCK -----")
  }

  text <- cat_line

  safe_list(
    cat = cat_,
    cat_line = cat_line,
    h1 = h1,
    alert_info = alert_info,
    alert_warning = alert_warning,
    code = code,
    text = text
  )
})

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
  logdec$h1("dv.manager logging is ACTIVATED")
  logdec$text("By default dv.manager logging removes all globalCallingHandlers from the environment")
  logdec$text("")
  logdec$text("If you find an error similar to the one below, this logging feature is probably the responsible:")
  logdec$code(
    "Error in globalCallingHandlers(foo = function(c) NULL) : should not be called with handlers on the stack"
  )
  logdec$text("")

  # If an error occurs while logging it is captured so the application does not stop because of it
  safe_handler <- function(h) {
    function(e) {
      tryCatch(
        h(e),
        error = function(err) {
          message(paste(err[["message"]], "| while logging:", e[["message"]]))
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
  logdec$h1("dv.manager logging is DEACTIVATED")
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
  cnd_to_str <- function(cnd) {
    date <- local({
      if (!"date" %in% names(cnd)) {
        format(Sys.time(), "%d-%m-%Y %H:%M:%S%z")
      } else {
        cnd[["date"]]
      }
    })

    ns <- local({
      if (!"ns" %in% names(cnd)) {
        session <- shiny::getDefaultReactiveDomain()
        if (!is.null(session)) {
          ns <- session[["ns"]](NULL)
          ns <- substring(ns, 1, nchar(ns))
          ns <- if (length(ns) == 0) "(Root)" else ns
        } else {
          ns <- NA_character_
        }
      } else {
        ns <- cnd[["ns"]]
      }
      ns
    })

    short_sess_id <- local({
      if (!"sess_id" %in% names(cnd)) {
        session <- shiny::getDefaultReactiveDomain()
        if (!is.null(session)) {
          sess_id <- session[["token"]]
        } else {
          sess_id <- NA_character_
        }
      } else {
        sess_id <- cnd[["sess_id"]]
      }

      substring(sess_id, 1, 6)
    })

    package <- local({
      if ("package" %in% names(cnd)) {
        cnd[["package"]]
      } else {
        NA_character_
      }
    })

    message <- local({
      if ("message" %in% names(cnd)) {
        cnd[["message"]]
      } else {
        NA_character_
      }
    })

    sprintf("[%s][%s|%s|%s]:%s", date, package, short_sess_id, ns, message)
  }

  list(
    message = function(cnd) {
      if (log_test_level(cnd, level)) {
        logdec$alert_info(cnd_to_str(cnd))
      }
      invokeRestart("muffleMessage")
    },
    warning = function(cnd) {
      if (log_test_level(cnd, level)) {
        logdec$alert_warning(cnd_to_str(cnd))
      }
      invokeRestart("muffleWarning")
    }
  )
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
  if (file.exists(target)) {
    stop(paste(target, "already exists"))
  }
  file.copy(origin, target)
  message(paste("Added file:", target))
}
