# THIS FILE IS NAMED aaa_preface.R SO IT IS LOADED BEFORE ALL OTHER FILES
# DO NOT CHANGE ITS NAME. IT MUST BE THE FIRST ONE ALPHABETICALLY.

#' @noRd
#' @keywords internal

safe_list_ns <- local({
  # Defines a safe-list that:
  # - Fails when trying to access a non-existant element by name, either by $, [] and [[]]
  # - Disables partial matching when using $
  # - Supports numerical indexing as usual
  #
  # R does not fail when accesing non-existant elements by name, it returns NULL.
  # If we want to be defensive and forbiding access to non-existant elements, we should do that on an element by
  # element basis (!is.null, name %in names(), ...)
  # With this type we avoid including these checks and also forgetting to add a check when accessing a
  # new element.
  #
  # The intention is to use it in specific scopes where this conditions hold true.
  #
  # It exports elements individually and includes a `define_safe_list` that exports all individual elements
  # with the correct names for the operators to work

  #' @keywords internal
  safe_list <- function(...) {
    result <- list(...)
    class(result) <- c("safe_list", class(result))
    return(result)
  }

  #' @keywords internal
  `[[.safe_list` <- function(x, name) {
    if (is.character(name) && !name %in% names(x)) {
      stop(sprintf("Element '%s' not found in safe_list.", name), call. = FALSE)
    }
    NextMethod("[[")
  }

  #' @keywords internal
  `$.safe_list` <- `[[.safe_list`

  #' @keywords internal
  `[.safe_list` <- function(x, i) {
    missing_elements <- setdiff(i, names(x))
    if (is.character(i) && length(missing_elements) > 0) {
      stop(sprintf("Elements '%s' not found in safe_list", paste(missing_elements, collapse = ", ")), call. = FALSE)
    }
    x <- NextMethod("[")
    as_safe_list(x)
  }

  as_safe_list <- function(x) {
    if (!is.list(x)) stop("x must be a list")
    class(x) <- c("safe_list", class(x))
    x
  }

  is_safe_list <- function(x) isTRUE(is.list(x) && inherits(x, "safe_list"))

  test <- function() {
    assert <- function(expr, msg) if (!isTRUE(expr)) stop(msg)
    x <- safe_list(aa = 0, bb = 1)

    assert(is.list(x) && inherits(x, "safe_list"), "Classes are correctly set")

    assert(x[["aa"]] == 0, "Present element 'aa' can be accessed via [[]]")
    assert(x[["bb"]] == 1, "Present element 'bb' can be accessed via [[]]")

    err <- try(x[["c"]], silent = TRUE)
    assert(inherits(err, "try-error"), "Not present element 'c' cannot be accessed via [[]]")
    assert(
      attr(err, "condition")[["message"]] == "Element 'c' not found in safe_list.",
      "Not present element 'c' cannot be accessed via [[]] and throws the correct error"
    )

    assert(x$aa == 0, "Present element 'aa' can be accessed via $")
    assert(x$bb == 1, "Present element 'bb' can be accessed via $")

    err <- try(x$c, silent = TRUE)
    assert(inherits(err, "try-error"), "Not present element 'c' cannot be accessed via $")
    assert(
      attr(err, "condition")[["message"]] == "Element 'c' not found in safe_list.",
      "Not present element 'c' cannot be accessed via $ and throws the correct error"
    )

    err <- try(x$b, silent = TRUE)
    assert(inherits(err, "try-error"), "Partial matching is not possible via $")

    assert(isTRUE(is_safe_list(x)), "safe_list return TRUE when passed a safe_list")
    assert(isFALSE(is_safe_list(list())), "safe_list return FALSE when passed a regular list")

    err <- try(x["c"], silent = TRUE)
    assert(inherits(err, "try-error"), "Not present element 'c' cannot be accessed via $")
    assert(
      attr(err, "condition")[["message"]] == "Elements 'c' not found in safe_list",
      "Not present element 'c' cannot be accessed via $ and throws the correct error"
    )


    assert(identical(x[c("aa")], safe_list(aa = 0)), "[] returns a subset safe_list")

    assert(x[[1]] == 0, "[[]] allows numerical indexing")
    assert(identical(x[1], safe_list(aa = 0)), "[] allows numerical indexing")

    assert(is_safe_list(as_safe_list(list(aa = 0))), "as_safe_list returns a safe_list")

    TRUE
  }

  individual_list <- list(
    safe_list = safe_list,
    "$.safe_list" = `$.safe_list`,
    "[[.safe_list" = `[[.safe_list`,
    "[.safe_list" = `[.safe_list`,
    as_safe_list = as_safe_list,
    is_safe_list = is_safe_list
  )

  c(
    individual_list,
    define_safe_list = function(env = parent.frame()) {
      list2env(individual_list, env)
      invisible(NULL)
    },
    test = test
  )
})

#' @keywords internal
safe_list <- safe_list_ns[["safe_list"]]
#' @keywords internal
`$.safe_list` <- safe_list_ns[["$.safe_list"]]
#' @keywords internal
`[[.safe_list` <- safe_list_ns[["[[.safe_list"]]
#' @keywords internal
`[.safe_list` <- safe_list_ns[["[.safe_list"]]
#' @keywords internal
as_safe_list <- safe_list_ns[["as_safe_list"]]
#' @keywords internal
is_safe_list <- safe_list_ns[["is_safe_list"]]

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
