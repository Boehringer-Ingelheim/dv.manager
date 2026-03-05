# logger

This article refers to all the functions that are used by the logger.
Please refer to [`vignette("logging")`](../articles/logging.md).

## Usage

``` r
log_activate(h = log_default_handlers(), .rm_gh = TRUE)

log_deactivate(h, .rm_gh = TRUE)

log_default_handlers(level = 999)

log_add_date(cnd)

log_add_short_sess_id(cnd)

log_test_level(cnd, level, .default = TRUE)

log_format(cnd, ...)

log_add_sess_id(cnd)

log_add_ns(cnd)

log_get_level_list()

log_use_log()
```

## Arguments

- h:

  `[function(0+)]`

  A named list of functions that will manage the signaled conditions

- .rm_gh:

  `[logical(1)]`

  Remove the previous globalCallingHandlers?

- level:

  `[character(1)|numeric(1)]`

  Logging level

- cnd:

  `[condition(1)]`

  A condition

- .default:

  `[logical(1)]`

  default behavior when level is not found in the condition

- ...:

  parameter passed to glue

## Details

It removes all previous globalCallingHandlers by default

## Functions

- `log_activate()`: Activate logging

- `log_deactivate()`: deactivate logging

- `log_default_handlers()`: get default handlers

- `log_add_date()`: add date field to a condition

- `log_add_short_sess_id()`: add short_sess_id to a condition

- `log_test_level()`: Tests if the cnd level is lower than the logger
  level

- `log_format()`: creates a log string according to a format.

- `log_add_sess_id()`: add sess_id to a condition

- `log_add_ns()`: add ns to a condition

- `log_get_level_list()`: List of levels in the logger

- `log_use_log()`: Adds the utils_logging file to the package
