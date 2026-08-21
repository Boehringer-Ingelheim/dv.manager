ODGE <- new.env() # Active Output Document Generation Environment
ODGE[["A"]] <- list() # Active Output Document Generation Environment Aliases

NODGA <- list() # Non Output Document Generation Aliases
NODGA[["append_odg_button"]] <- function(x, ...) x
NODGA[["odg_server_quote"]] <- NULL

# shinymeta::metaReactive shinymeta::metaReactive2
NODGA[["sm_mr"]] <- function(expr, ...) {
  sb_expr <- substitute(expr)
  shiny::reactive(sb_expr, quoted = TRUE, env = parent.frame())
}
NODGA[["sm_mr2"]] <- NODGA[["sm_mr"]]

# shinymeta::metaExpr
NODGA[["sm_me"]] <- function(expr, ...) {
  eval(substitute(expr), envir = parent.frame())
}

ODGE[["A"]] <- NODGA

.. <- function(x) x # Out of the environment on purpose, redeclared in the meta... function environment
