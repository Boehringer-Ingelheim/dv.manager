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
#' @keywords internal

mm_dispatch <- function(from, selection = character(0)) {
  .Defunct(NULL, msg = "please check the module help replacing the dispatchers")
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
#' @keywords internal
mm_resolve_dispatcher <- function(dispatcher, # nolintr excessive cyclomatic complexity
                                  afmm,
                                  flatten = FALSE) {
  .Defunct(NULL, msg = "please check the module help replacing the dispatchers")
}

#' A helper function to resolve the output dispatchers
#'
#' @param selection a string indicating which module output we want to access
#' @param afmm a list of arguments from module manager
#'
#' @noRd
#' 
#' @keywords internal
mm_resolve_module_output <- function(selection, afmm, flatten) {
  .Defunct(NULL, msg = "please check the module help replacing the dispatchers")
}
