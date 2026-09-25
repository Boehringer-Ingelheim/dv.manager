char_vars_to_factor_vars_dataset <- function(dataset) {
  # nolintr
  var_names <- names(dataset)
  r <- dataset
  for (var_name in var_names) {
    var <- dataset[[var_name]]
    lbl <- attr(var, "label")
    if (is.character(var)) {
      r[[var_name]] <- factor(var)
      if (!is.null(lbl)) {
        attr(r[[var_name]], "label") <- lbl
      }
    }
  }
  r
}

char_vars_to_factor_vars_dataset_list <- function(dataset_list) {
  # nolintr
  dataset_names <- names(dataset_list)
  for (dataset_name in dataset_names) {
    dataset <- dataset_list[[dataset_name]]
    factored_dataset <- char_vars_to_factor_vars_dataset(dataset)
    dataset_list[[dataset_name]] <- factored_dataset
  }
  dataset_list
}

decorate_char_vars_to_factor_vars_dataset_list <- function(f) {
  # nolintr
  function() {
    char_vars_to_factor_vars_dataset_list(f())
  }
}

char_vars_to_factor_vars_dataset_lists <- function(dataset_lists) {
  # nolintr
  lapply(dataset_lists, function(d) {
    if (is.function(d)) {
      decorate_char_vars_to_factor_vars_dataset_list(d)
    } else if (is.list(d)) {
      char_vars_to_factor_vars_dataset_list(d)
    } else {
      stop("Unknown type")
    }
  })
}

decorate_ungroup2df_datasets_dataset_list <- function(f) {
  # nolintr
  function() {
    ungroup2df_datasets_dataset_list(f())
  }
}

ungroup2df_datasets_dataset_list <- function(dataset_list) {
  dataset_names <- names(dataset_list)
  for (dataset_name in dataset_names) {
    d <- dataset_list[[dataset_name]]
    attrs <- attributes(d)
    attrs <- attrs[setdiff(names(attrs), c("names", "row.names", "class", "groups"))]

    d <- dplyr::ungroup(d)
    d <- as.data.frame(d)
    attributes(d)[names(attrs)] <- attrs
    d

    dataset_list[[dataset_name]] <- d
  }
  dataset_list
}

ungroup2df_datasets_dataset_lists <- function(dataset_lists) {
  # nolintr
  lapply(dataset_lists, function(d) {
    if (is.function(d)) {
      decorate_ungroup2df_datasets_dataset_list(d)
    } else if (is.list(d)) {
      ungroup2df_datasets_dataset_list(d)
    } else {
      stop("Unknown type")
    }
  })
}

cache_dataset_list_function <- function(dataset_lists) {
  last_dataset_list_index_requested <- 0L
  last_dataset_list_returned <- NULL

  local_dataset_lists <- dataset_lists

  dataset_list_single_element_cache <- function(dataset_list_idx) {
    if (dataset_list_idx != last_dataset_list_index_requested) {
      last_dataset_list_returned <<- local_dataset_lists[[dataset_list_idx]]()
    }
    last_dataset_list_index_requested <<- dataset_list_idx
    return(last_dataset_list_returned)
  }

  for (idx in seq_along(dataset_lists)) {
    dataset_list <- dataset_lists[[idx]]
    if (is.function(dataset_list)) {
      dataset_lists[[idx]] <- local({
        local_idx <- idx
        function() dataset_list_single_element_cache(local_idx)
      })
    }
  }

  return(dataset_lists)
}
