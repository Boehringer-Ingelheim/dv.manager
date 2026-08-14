AEE <- new.env() # Active Export Environment
AEE[["A"]] <- list() # Active Export Environment Aliases

NEA <- list() # Non Export Aliases
NEA[["append_export_button"]] <- function(x, ...) x
NEA[["export_server_quote"]] <- NULL

# shinymeta::metaReactive shinymeta::metaReactive2
NEA[["sm_mr"]] <- function(expr, ...) {
  sb_expr <- substitute(expr)
  shiny::reactive(sb_expr, quoted = TRUE, env = parent.frame())
}
NEA[["sm_mr2"]] <- NEA[["sm_mr"]]

# shinymeta::metaExpr
NEA[["sm_me"]] <- sm_me <- function(expr, ...) {
  eval(substitute(expr), envir = parent.frame())
}

AEE[["A"]] <- NEA

.. <- function(x) x # Out of the environment on purpose, redeclared in the meta... function environment
