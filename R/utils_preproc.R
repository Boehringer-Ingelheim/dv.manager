char_vars_to_factor_vars_dataset <- function(dataset) { # nolintr
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

char_vars_to_factor_vars_dataset_list <- function(dataset_list) { # nolintr
  dataset_names <- names(dataset_list)
  for (dataset_name in dataset_names) {
    dataset <- dataset_list[[dataset_name]]
    factored_dataset <- char_vars_to_factor_vars_dataset(dataset)
    dataset_list[[dataset_name]] <- factored_dataset
  }
  dataset_list
}

decorate_char_vars_to_factor_vars_dataset_list <- function(f) { # nolintr
  function() {
    char_vars_to_factor_vars_dataset_list(f())
  }
}

char_vars_to_factor_vars_dataset_lists <- function(dataset_lists) { # nolintr
  dataset_list_names <- names(dataset_lists)
  for (dataset_list_name in dataset_list_names) {
    dataset_list <- dataset_lists[[dataset_list_name]]
    if (is.function(dataset_list)) {
      factored_dataset_list <- decorate_char_vars_to_factor_vars_dataset_list(dataset_list)
    } else if (is.list(dataset_list)) {
      factored_dataset_list <- char_vars_to_factor_vars_dataset_list(dataset_list)
    } else {
      stop("Unknown type")
    }
    dataset_lists[[dataset_list_name]] <- factored_dataset_list
  }
  dataset_lists
}

decorate_ungroup2df_datasets_dataset_list <- function(f) { # nolintr
  function() {
    ungroup2df_datasets_dataset_list(f())
  }
}

ungroup2df_datasets_dataset_list <- function(dataset_list) { # nolintr
  dataset_names <- names(dataset_list)
  for (dataset_name in dataset_names) {
    dataset_list[[dataset_name]] <- as.data.frame( # Transform to base::data.frame, discards tibble related classes...
      dplyr::ungroup(dataset_list[[dataset_name]])
    )
  }
  dataset_list
}

ungroup2df_datasets_dataset_lists <- function(dataset_lists) { # nolintr
  dataset_list_names <- names(dataset_lists)
  for (dataset_list_name in dataset_list_names) {
    dataset_list <- dataset_lists[[dataset_list_name]]
    if (is.function(dataset_list)) {
      ungrouped_dataset_list <- decorate_ungroup2df_datasets_dataset_list(dataset_list)
    } else if (is.list(dataset_list)) {
      ungrouped_dataset_list <- ungroup2df_datasets_dataset_list(dataset_list)
    } else {
      stop("Unknown type")
    }
    dataset_lists[[dataset_list_name]] <- ungrouped_dataset_list
  }
  dataset_lists
}
