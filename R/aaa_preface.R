# THIS FILE IS NAMED aaa_preface.R SO IT IS LOADED BEFORE ALL OTHER FILES
# DO NOT CHANGE ITS NAME. IT MUST BE THE FIRST ONE ALPHABETICALLY.

#' Build a collection of named constants
#'
#' @param ... Named parameters to be collected as constants
#' @details
#' Shiny uses strings as IDs to link UI and server elements. E.g:
#'   foo_UI(id = ns("foo")) ...
#'   foo_server(id = "foo")
#'
#' This pattern makes it easy for programmers to fall on the trap of modifying one instance of the string literal "foo"
#' without modifying the rest and be unaware of the problem until a bug is hit. It's also easy to mistakes uses of "foo"
#' as an identifier from other uses (text labels, ...) when, as it's often the case, the parameter is not explicitly
#' named.
#' One easy fix consists in using global variables instead of plain string literals. In the case of the previous
#' example, that would mean:
#'   ID_FOO <- "foo"
#'   foo_UI(ns(ID_FOO)) ...
#'   foo_server(ID_FOO)
#'
#' That simple addition makes the purpose of ID_FOO clear and also fails gracefully when not all ID_FOO instances are
#' updated synchronously along a codebase. It has the drawback of polluting the global namespace with identifier
#' variables. That's easily solved by creating a container of constants, which is the purpose of this pack_of_constants
#' alias.
#'  ID <- pack_of_constants(FOO = "foo", BAR = "bar")
#'  ID$FOO
#'  "foo"
#'  ID$BA
#'  Error in `$.pack_of_constants`(ID, BA) :
#'  Pack of constants "ID" does not contain "BA"
#'
#' The pack of constants is a plain named list that enforces that all elements have unique, non-null names.
#' It is tagged as an S3 object to override its extraction operators.
#'
#' The use of checkmate is unnecessary, but it's a Good Library(TM) and your module should rely on it anyways
#' @keywords internal
pack_of_constants <- function(...) {
  result <- list(...)
  checkmate::assert_list(result, any.missing = FALSE, names = "unique")
  class(result) <- c("pack_of_constants", class(result))
  result
}

#' Extract constant from pack
#'
#' @param pack pack_of_constants
#' @param name target constant
#'
#' This function differs from the base list extraction method in that it avoids partial matching of keys and throws
#' an error if the looked-for constant is not contained within the pack.
#' @keywords internal
`$.pack_of_constants` <- function(pack, name) {
  checkmate::assert_true(name %in% names(pack), .var.name = paste0(deparse(substitute(pack)), "$", name))
  NextMethod()
}

#' @keywords internal
`[[.pack_of_constants` <- `$.pack_of_constants`

#' @keywords internal
`[.pack_of_constants` <- function(pack, name) {
  stop("Invalid pack_of_constants method")
}

poc <- pack_of_constants
