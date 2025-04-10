# Modules will access module output list. When they try to access it we want to control that they try to access an
# existing entry or otherwise send a controlled error.
#
# The way we control it is by transforming it in a module_safe_list before it is read by an external module
# This type of object will fail whenever we try to access an element by name that is not present in the list
#
# It will fail by sending a validate error. Why a validate error and not a shiny::req or a regular error?
# Because:
# 1. We expect that in most cases this module_output_safe_list will be called inside a reactive
# 2. A shiny req will fail silently in the application and simply the output will not appear
# 3. An uncontrolled error may make the app crash inside an observe.
# 4. Even if called outside of a reactive the shiny validate will act as a regular error
#
# Why do we not control which entry is called at the application startup?
#
# dv.manager does not know which module outputs will be read by each module (even more they could be dynamic)
#
# Shouldn't module check if the output they want to read is available?
# Yes, this is a fallback system in case a module does not do that. `afmm` contains a list of the available modules
# therefore modules can make this check during startup (preferred always) or even during app time (only if dynamic never
# find a case yet) Without this the error is quite obscure and can make the app crash.
#
# Why a class with accessors?
#
# The object returned to the modules is the full list inside a function mimicking a reactive, 
#
# Could this complexity be avoided? Yes by:a
# - Removing this fallback system and trus modules will do their part
# - Modifying the function returning the module_output_list to take a name/s argument that we can check against
# the names of the list. That would require modifying the current wrappers from module_output[["mod1"]] to 
# module_output("mod1").
# 
#
# Operator are not exported therefore this class has no effect outside of this package namespace

as_module_output_safe_list <- function(x) {
  result <- x
  class(result) <- c("module_output_safe_list", class(result))
  return(result)
}

missing_test <- function() {
  x <- as_module_output_safe_list(list(a=0))
  x[["b"]]
  x
}

#' @keywords internal
`[[.module_output_safe_list` <- function(x, i) {
  if (is.character(i) && !i %in% names(x)) {
    msg <- sprintf("Element '%s' not found as module output", i)
    log_warn(msg)
    shiny::validate(shiny::need(FALSE, msg))    
  }  
  NextMethod("[[")
}

#' @keywords internal
`$.module_output_safe_list` <- `[[.module_output_safe_list`

#' @keywords internal
`[.module_output_safe_list` <- function(x, i) {
  missing_elements <- setdiff(i, names(x))
  if (is.character(i) && length(setdiff(i, names(x))) > 0) {
    msg <- sprintf("Elements '%s' not found in module_output", paste(missing_elements, collapse = ", "))
    log_warn(msg)
    shiny::validate(shiny::need(FALSE, msg))
  }  
  NextMethod("[")
}
