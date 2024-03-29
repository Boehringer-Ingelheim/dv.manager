% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_logging_main.R
\name{logging}
\alias{logging}
\alias{log_activate}
\alias{log_deactivate}
\alias{log_default_handlers}
\alias{log_add_date}
\alias{log_add_short_sess_id}
\alias{log_test_level}
\alias{log_format}
\alias{log_add_sess_id}
\alias{log_add_ns}
\alias{log_get_level_list}
\alias{log_use_log}
\title{logger}
\usage{
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
}
\arguments{
\item{h}{\verb{[function(0+)]}

A named list of functions that will manage the signaled conditions}

\item{.rm_gh}{\verb{[logical(1)]}

Remove the previous globalCallingHandlers?}

\item{level}{\verb{[character(1)|numeric(1)]}

Logging level}

\item{cnd}{\verb{[condition(1)]}

A condition}

\item{.default}{\verb{[logical(1)]}

default behavior when level is not found in the condition}

\item{...}{parameter passed to glue}
}
\description{
This article refers to all the functions that are used by the logger.
Please refer to \code{vignette("logging")}.
}
\details{
It removes all previous globalCallingHandlers by default
}
\section{Functions}{
\itemize{
\item \code{log_activate()}: Activate logging

\item \code{log_deactivate()}: deactivate logging

\item \code{log_default_handlers()}: get default handlers

\item \code{log_add_date()}: add date field to a condition

\item \code{log_add_short_sess_id()}: add short_sess_id to a condition

\item \code{log_test_level()}: Tests if the cnd level is lower than the logger level

\item \code{log_format()}: creates a log string according to a format.

\item \code{log_add_sess_id()}: add sess_id to a condition

\item \code{log_add_ns()}: add ns to a condition

\item \code{log_get_level_list()}: List of levels in the logger

\item \code{log_use_log()}: Adds the utils_logging file to the package

}}
