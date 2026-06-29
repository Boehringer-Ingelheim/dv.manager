# logging

## How to use it

You can start using the logger simply by activating it and sending a
message. Notice we are using internal function `:::` as the loggers are
not exported by dv.manager. See in the sections below how you can use

``` r
dv.manager::log_activate()
message("Logged message")
dv.manager::log_deactivate()
```

In its default state, the logger will capture and decorate any ‘message’
or ‘warning’ condition. Simply put, any message you send using `message`
or `warning` or other packages sending signaling similar conditions
[`rlang::inform`](https://rlang.r-lib.org/reference/abort.html) and
[`rlang::warn`](https://rlang.r-lib.org/reference/abort.html).

## Logging messages sent from your module

A common use case will be using the logger to send messages from inside
your module.

``` r
dv.manager::log_activate()
message("An inform")
dv.manager::log_deactivate()
```

You can use the `packageName` function to dynamically get the name of
your package.

## Using dv.manager convenience logging functions

You can also call `log_use_log` from `dv.manager` in the root of the
package containing the module. This will add a file to your `R/`
directory with two convenience functions. These convenience functions
`log_inform` and `log_warn` create `message` and `warning` conditions
that also automatically add the package name and require the option
`dv.logging.active` to be set to `TRUE`. From now on these function will
be used. Notice in some of the examples we use them with a `:::` as they
are internal to the `dv.manager` and are not exported.

``` r
# Will create an R/utils_logging.R file with two convenience functions
dv.manager::log_use_log()
options("dv.logging.active" = TRUE)
dv.manager::log_activate()
log_inform("An inform with package name")
log_warn("A warning with package name")
dv.manager::log_deactivate()
```

## Using logging levels

The logger can have different print granularities depending on the
logging level selected. A different level can be specified in each of
the logging messages sent by including a logging level field in it.

``` r
options("dv.logging.active" = TRUE)
dv.manager::log_activate(dv.manager:::log_default_handlers(level = "info"))
dv.manager:::log_inform("This will be printed", level = "info")
dv.manager:::log_inform("This will not", level = "debug")
dv.manager::log_deactivate()
```

The logging levels and their numerical value can be checked with:

``` r
dv.manager::log_get_level_list()
```

## Advanced logging

`dv.manager` logging system is based on [R’s condition
system](https://adv-r.hadley.nz/conditions.html). This logging system
simply specifies a set of `globalCallingHandlers` that will capture and
log the messages, warnings and/or errors as specified by the handlers.

### The default handlers

This is a low level logging and that makes it very flexible.

By default, the condition will contain these fields:

- message: \[character(1)\] The message to be logged

It expects that the module developer has added the fields:

- package: \[character(1)\] The name of the package that sent the
  condition (added automatically if `log_info` and `log_warn` were
  used).
- level: \[numeric(1)\] a level for logging granularity.

The handlers will try to add the following fields to the condition
before formatting:

- date: \[character(1)\] A timestamp for the message.
- ns: \[character(1)\] The namespace of the module that sent the
  condition.
- sess_id: \[character(1)\] The Shiny session id that sent the
  condition.
- short_sess_id: \[character(1)\] A truncated version of the sess_id to
  improve human-readibility.

The default handlers present and format this information.

``` r
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
          ns <- session[["ns"]]("")
          ns <- substring(ns, 1, nchar(ns) - 1)
          ns <- if (nchar(ns) == 0) "(Root)" else ns
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
      if (!"package" %in% names(cnd)) {
        cnd[["package"]]
      } else {
        NA_character_
      }
    })

    message <- local({
      if (!"message" %in% names(cnd)) {
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

      rlang::cnd_muffle(cnd)
    },
    warning = function(cnd) {
      if (log_test_level(cnd, level)) {
        logdec$alert_warning(cnd_to_str(cnd))
      }
      rlang::cnd_muffle(cnd)
    }
  )
}
```

### Creating custom handlers

The default handlers are convenient, but the logging behavior can be
modified by using a different set of handlers.

``` r
my_handlers <- function(level = 999) {
  cnd_to_str <- function(cnd) {
    date <- local({
      if (!"date" %in% names(cnd)) {
        format(Sys.time(), "%d-%m-%Y %H:%M:%S%z")
      } else {
        cnd[["date"]]
      }
    })

    message <- local({
      if (!"message" %in% names(cnd)) {
        cnd[["message"]]
      } else {
        NA_character_
      }
    })

    sprintf("Date: %s | %s", date, message)
  }

  list(
    message = function(cnd) {
      if (log_test_level(cnd, level)) {
        logdec$alert_info(cnd_to_str(cnd))
      }

      rlang::cnd_muffle(cnd)
    },
    warning = function(cnd) {
      if (log_test_level(cnd, level)) {
        logdec$alert_warning(cnd_to_str(cnd))
      }
      rlang::cnd_muffle(cnd)
    }
  )
}

options("dv.logging.active" = TRUE)
dv.manager::log_activate(my_handlers())
dv.manager:::log_inform("I will print this")
dv.manager::log_deactivate()
options("dv.logging.active" = NULL)
```

As you can see we can add new fields for logging, we can supress some
messages depending on the fields, etc. This allows filtering logging
messages by package, session, logging level, etc. So it adapts to our
particular need during development.

## Troubleshooting

- When `log_activate` is called, an error similar to the following
  appears:

> Error in globalCallingHandlers(…) : should not be called with handlers
> on the stack

The error above means you are activating the logger from an environment
that already has defined some calling handlers, usually with
`withCallingHandlers`. `shiny` in particular does this when an app
starts, therefore you cannot activate logging once you are inside the
application. The logger is based on setting `globalCallingHandlers` and
setting those is tricky within other functions. The recommendation is to
make the call to `log_activate` in your root session and not within
functions.
