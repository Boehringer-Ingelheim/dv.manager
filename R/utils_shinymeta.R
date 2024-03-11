is.metareactive <- function(x) { # nolint
  inherits(x, "shinymeta_reactive")
}

is.anyreactive <- function(x) { # nolint
  inherits(x, "reactive") | inherits(x, "shinymeta_reactive")
}
