#' Functional dispatcher helper functions
#'
#' Dispatch is a helper function that allows accessing data and utility functions inside module manager in
#' a dynamic way.
#'
#' @details
#'
#' The parameter `from` can take the following values:
#'   - unfiltered_dataset
#'   - filtered_dataset
#'   - url_parameters
#'   - module_output
#'   - utils
#'
#' @param from a string indicating what structure do we want to access inside module manager
#' @param selection an optional subsetting within the selected data structure
#'
#' @seealso mm_resolve_dispatcher
#'
#' @examples
#'
#' mm_dispatch("unfiltered_dataset", "adsl")
#' mm_dispatch("utils", "switch2")
#' @export

mm_dispatch <- function(from, selection = character(0)) {
  ac <- checkmate::makeAssertCollection()
  checkmate::assert_string(from, min.chars = 1, na.ok = FALSE, add = ac)
  checkmate::assert_character(selection, min.chars = 1, any.missing = FALSE, add = ac)
  checkmate::reportAssertions(ac)
  return(
    structure(
      list(from = from, selection = selection),
      class = "mm_dispatcher"
    )
  )
}

#' Dispatcher resolver
#'
#' @param dispatcher A list of class "mm_dispatcher" to be resolved. If the object is any of other class it
#'  is returned unmodified.
#' @param afmm a list of arguments from module manager
#' @param flatten for dispatcher where selection length is 1 and disppatcher is of class "mm_dispatcher"
#' return the first element instead of a one element list. By default, it will try to flatten them.
#' If selection is greater than 1 flatten must be FALSE, otherwise an error will be thrown.
#'
#' @seealso mm_dispatch
#'
#' @export
#'
#'
mm_resolve_dispatcher <- function(dispatcher, # nolintr excessive cyclomatic complexity
                                  afmm,
                                  flatten = FALSE) {
  ac <- checkmate::makeAssertCollection()
  checkmate::assert_logical(flatten, len = 1, any.missing = FALSE, add = ac)
  checkmate::assert_list(afmm, add = ac)
  checkmate::reportAssertions(ac)

  # If it is not a dispatcher we return as is
  if (!inherits(dispatcher, "mm_dispatcher")) {
    return(dispatcher)
  }

  sel_length <- length(dispatcher[["selection"]])

  checkmate::assert_false(sel_length > 1 && flatten)

  if (dispatcher[["from"]] == "module_output") { # module_output is a special case
    val <- mm_resolve_module_output(dispatcher[["selection"]], afmm, flatten)
  } else {
    from <- afmm[[dispatcher[["from"]]]]

    if (sel_length == 0) {
      val <- from
    } else {
      if (shiny::is.reactive(from)) {
        if (sel_length == 1 && flatten) {
          val <- shiny::reactive({
            from()[[dispatcher[["selection"]]]]
          })
        } else {
          val <- shiny::reactive({
            from()[dispatcher[["selection"]]]
          })
        }
      } else {
        if (is.metareactive(from)) {
          if (sel_length == 1 && flatten) {
            val <- shinymeta::metaReactive({
              shinymeta::..(from())[[shinymeta::..(dispatcher[["selection"]])]]
            })
          } else {
            val <- shinymeta::metaReactive({
              shinymeta::..(from())[shinymeta::..(dispatcher[["selection"]])]
            })
          }
        } else {
          if (sel_length == 1 && flatten) {
            val <- from[[dispatcher[["selection"]]]]
          } else {
            val <- from[dispatcher[["selection"]]]
          }
        }
      }
    }
  }
  return(val)
}

#' A helper function to resolve the output dispatchers
#'
#' @param selection a string indicating which module output we want to access
#' @param afmm a list of arguments from module manager
#'
#' @noRd
#'
mm_resolve_module_output <- function(selection, afmm, flatten) {
  ac <- checkmate::makeAssertCollection()
  checkmate::assert_logical(flatten, len = 1, any.missing = FALSE, add = ac)
  checkmate::assert_character(selection, min.chars = 1, any.missing = FALSE, add = ac)
  checkmate::assert_list(afmm, add = ac)
  checkmate::reportAssertions(ac)

  if (length(selection) > 0) {
    # If we selected only one we do our best to pass non-nested meta/reactive values
    if (length(selection) == 1 && flatten) {
      val <- shinymeta::metaReactive2({
        # If it is any kind of reactive we resolve it and store in meta, regardless of what was before
        if (is.anyreactive(afmm[["module_output"]]()[[selection]])) {
          shinymeta::metaExpr({
            shinymeta::..(afmm[["module_output"]]()[[selection]]())
          })
          # Otherwise we do not resolve and store it as a meta
        } else {
          shinymeta::metaExpr({
            shinymeta::..(afmm[["module_output"]]()[[shinymeta::..(selection)]])
          })
        }
      })
    } else { # if we selected several values we pass as it is inside a meta
      val <- shinymeta::metaReactive2({
        shinymeta::metaExpr({
          shinymeta::..(afmm[["module_output"]]()[selection])
        })
      })
    }
  } else {
    val <- shinymeta::metaReactive({
      afmm[["module_output"]]()
    })
  }
  return(val)
}
