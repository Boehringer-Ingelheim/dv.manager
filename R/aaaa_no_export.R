append_export_button <- identity
export_server_quote <- NULL

# shinymeta::metaReactive shinymeta::metaReactive2
sm_mr <- sm_mr2 <- function(expr, ...) {
  sb_expr <- substitute(expr)
  shiny::reactive(sb_expr, quoted = TRUE, env = parent.frame())
}

# shinymeta::metaExpr
sm_me <- function(expr, ...) {
  eval(substitute(expr), envir = parent.frame())
}

.. <- function(x) {
  x
}
