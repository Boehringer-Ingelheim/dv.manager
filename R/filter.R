# TODO: review naming

get_dataset_filters_info <- function(data, filter_data) {
  dataset_filter_names <- setdiff(get_data_tables_names(data), filter_data)
  res <- vector(mode = "list", length = length(dataset_filter_names))
  names(res) <- dataset_filter_names

  for (idx in seq_along(dataset_filter_names)) {
    name <- dataset_filter_names[[idx]]
    hash <- digest::digest(name, "murmur32")
    id <- sprintf("dataset_filter_%s", hash)
    cont_id <- paste0(id, "_cont")
    res[[idx]] <- list(name = name, id = id, hash = hash, id_cont = cont_id)
  }

  return(res)
}

create_dataset_filters_ui <- function(dataset_filters_info, ns) {
  if (!isTRUE(getOption("new_filter_switch"))) {
    res <- vector(mode = "list", length = length(dataset_filters_info))
    for (idx in seq_along(dataset_filters_info)) {
      entry <- dataset_filters_info[[idx]]
      res[[idx]] <- shiny::div(
        id = entry[["id_cont"]],
        class = "filter-control  filter-filters",
        shiny::tags[["label"]](entry[["name"]]),
        dv.filter::data_filter_ui(ns(entry[["id"]])),
        shiny::hr(style = "border-top: 2px solid gray; height: 10px;")
      )
    }
  } else {

  }
  return(res)
}

create_subject_level_ui <- function(id) {
  dv.filter::data_filter_ui(id)
}

create_subject_level_server <- function(
    id,
    data) {
  if (!isTRUE(getOption("new_filter_switch"))) dv.filter::data_filter_server(id, data)
}

create_dataset_filters_server <- function(datasets_filters_info, data_list) {
  if (!isTRUE(getOption("new_filter_switch"))) {
    res <- local({
      l <- vector(mode = "list", length = length(datasets_filters_info))
      names(l) <- names(datasets_filters_info)
      for (idx in seq_along(datasets_filters_info)) {
        l[[idx]] <- local({
          curr_dataset_filter_info <- datasets_filters_info[[idx]]
          dv.filter::data_filter_server(
            curr_dataset_filter_info[["id"]],
            shiny::reactive({
              data_list()[[curr_dataset_filter_info[["name"]]]] %||% data.frame()
            })
          )
        })
      }
      l
    })
  } else {

  }

  return(res)
}

# NEW FILTER ----

get_single_filter_data <- function(dataset) {
  nm_var <- names(dataset)
  n_var <- length(nm_var)
  res <- vector(mode = "list", length = n_var)

  inf_to_str <- function(x) {
    if (is.infinite(x)) {
      num_x <- as.numeric(x)
      if (sign(num_x) < 0) {
        res <- "-Inf"
      } else {
        res <- "Inf"
      }
    } else {
      res <- x
    }
    return(res)
  }

  # In R all elements are vectors by default this makes complicated to transform into json as c("a") can be enconded
  # as "a" or ["a"]. To disambiguate this jsonlite offers `unbox`.

  # Why are we inserting NA values in some of the fields?
  # `jsonlite` transforms `NA` `null` by default, seems reasonable as JSON as has no support for NA values
  # Therefore, we remove all NA values from the fields before making our calculations making all `null`
  # values in the JSON intentional.
  # Why not using NULL? NULL is interpreted as an empty object `{}` by default by `jsonlite::toJSON`. We can modify the
  # behavior and making NULL transform into `null`. But this brings two problems, one is that once NULL is gone we
  # cannot create `{}` which although not required now, maybe required in the future. Second `NULL` is very particular
  # in R and although we can `list(a=NULL)`
  # we cannot `x <- list(); x[["a"]] <- NULL` (this has the effect of deleting the entry "a")
  # we must do instead `x <- list(); x["a"] <- list(NULL)`
  # Therefore it seems more reasonable to work with NAs in this case.

  for (idx in seq_len(n_var)) {
    name <- nm_var[[idx]]
    var <- dataset[[name]]
    label <- attr(var, "label") %||% name # FIXME: This is done to maintain the same behavior as jsonlite. Should be reviewed with the js code that uses labels

    l <- list(
      name = yyjsonr::as_scalar(name),
      label = yyjsonr::as_scalar(label),
      class = yyjsonr::as_scalar(class(var)[1])
    )


    # Logical is treated as a factor in the client
    if (is.logical(var)) var <- factor(var)

    if (is.character(var) || is.factor(var)) {
      # FIXME: factor levels are ignored and only the values really present in the dataset are used
      l[["kind"]] <- yyjsonr::as_scalar("categorical")
      l[["NA_count"]] <- yyjsonr::as_scalar(sum(is.na(var)))
      na_clean_var <- var[!is.na(var)]
      count <- sort(table(na_clean_var), decreasing = TRUE)
      values <- names(count)
      count <- unname(count)
      l[["values_count"]] <- vector(mode = "list", length = length(l[["values"]]))
      for (v_idx in seq_along(values)) {
        l[["values_count"]][[v_idx]] <- list(
          value = yyjsonr::as_scalar(values[[v_idx]]),
          count = yyjsonr::as_scalar(count[[v_idx]])
        )
      }
    } else if (is.numeric(var)) {
      l[["kind"]] <- yyjsonr::as_scalar("numerical")
      l[["NA_count"]] <- yyjsonr::as_scalar(sum(is.na(var)))
      na_clean_var <- var[!is.na(var)]

      l[["min"]] <- yyjsonr::as_scalar(inf_to_str(min(Inf, na_clean_var, na.rm = TRUE)))
      l[["max"]] <- yyjsonr::as_scalar(inf_to_str(max(-Inf, na_clean_var, na.rm = TRUE)))

      if (length(na_clean_var) > 0) {
        hist_info <- hist(na_clean_var, plot = FALSE)
      } else {
        hist_info <- list(density = numeric(0))
      }

      l[["density"]] <- hist_info[["density"]]
    } else if (inherits(var, "POSIXct") || inherits(var, "Date")) {
      if (inherits(var, "POSIXct")) {
        var <- as.Date(var)
      }
      l[["kind"]] <- yyjsonr::as_scalar("date")
      l[["NA_count"]] <- yyjsonr::as_scalar(sum(is.na(var)))
      na_clean_var <- var[!is.na(var)]

      l[["min"]] <- yyjsonr::as_scalar(inf_to_str(min(as.Date(Inf), na_clean_var, na.rm = TRUE)))
      l[["max"]] <- yyjsonr::as_scalar(inf_to_str(max(as.Date(-Inf), na_clean_var, na.rm = TRUE)))
    } else {
      stop(paste("variable type unsupported:", typeof(var)))
    }

    res[[idx]] <- l
  }
  return(res)
}

get_filter_data <- function(dataset_lists) {
  nm_dataset_list <- names(dataset_lists)
  n_dataset_list <- length(nm_dataset_list)
  res <- vector(mode = "list", length = n_dataset_list)
  for (idx in seq_len(n_dataset_list)) {
    current_dataset_list <- dataset_lists[[idx]]
    current_dataset_list_name <- nm_dataset_list[[idx]]
    nm_datasets <- names(current_dataset_list)
    n_datasets <- length(nm_datasets)
    current_dataset_res <- vector(mode = "list", length = n_datasets)
    for (jdx in seq_len(n_datasets)) {
      current_dataset <- current_dataset_list[[jdx]]
      current_dataset_name <- nm_datasets[[jdx]]
      current_dataset_res[[jdx]] <- list(
        name = yyjsonr::as_scalar(current_dataset_name),
        nrow = yyjsonr::as_scalar(nrow(current_dataset)),
        variables = get_single_filter_data(current_dataset)
      )
    }
    res[[idx]] <- list(
      name = yyjsonr::as_scalar(current_dataset_list_name),
      dataset_list = current_dataset_res
    )
  }
  res <- list(dataset_lists = res)
  return(res)
}

dataset_filter_operations <- local({

   actions <- list(row_operation = list(), filter = list())

  actions[["row_operation"]][["and"]] <- function(dataset_list, filter_element) {
    filter_dataset <- NA
    assert(length(filter_element[["children"]]) >= 1, "`and` operation requires at least one child")
    mask <- TRUE # Neutral element for &
    lvls <- list()
    for (child in filter_element[["children"]]) {
      processed_element <- process_dataset_filter_element(dataset_list, child)
      mask <- mask & processed_element[["mask"]]
      curr_lvls <- processed_element[["lvls"]]

      if (is.na(filter_dataset)) {
        filter_dataset <- processed_element[["dataset"]]
      } else {
        assert(processed_element[["dataset"]] == filter_dataset, "Filtering on the wrong dataset")
      }

      if (length(curr_lvls) > 0) {
        relevant_factors <- names(curr_lvls)
        for (fct in relevant_factors) {
          lvls[[fct]] <- intersect(
            lvls[[fct]] %||% levels(dataset_list[[filter_dataset]][[fct]]),
            curr_lvls[[fct]]
          )
        }
      }
    }
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions[["row_operation"]][["or"]] <- function(dataset_list, filter_element) {
    filter_dataset <- NA
    assert(length(filter_element[["children"]]) >= 1, "`or` operation requires at least one child")

    mask <- FALSE # Neutral element for |
    lvls <- list()
    for (child in filter_element[["children"]]) {
      processed_element <- process_dataset_filter_element(dataset_list, child)
      mask <- mask | processed_element[["mask"]]
      curr_lvls <- processed_element[["lvls"]]
      if (is.na(filter_dataset)) {
        filter_dataset <- processed_element[["dataset"]]
      } else {
        assert(processed_element[["dataset"]] == filter_dataset, "Filtering on the wrong dataset")
      }
      if (length(curr_lvls) > 0) {
        # This should be an intersection of levels if levels are present for the same factor variable
        relevant_factors <- names(curr_lvls)
        for (fct in relevant_factors) {
          lvls[[fct]] <- union(lvls[[fct]], curr_lvls[[fct]])
        }
      }
    }

    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions[["row_operation"]][["not"]] <- function(dataset_list, filter_element) {
    filter_dataset <- NA
    assert(length(filter_element[["children"]]) == 1, "`not` operation requires exactly one child")
    child <- filter_element[["children"]][[1]]
    processed_element <- process_dataset_filter_element(dataset_list, child)
    mask <- !processed_element[["mask"]]
    filter_dataset <- processed_element[["dataset"]]
    lvls <- list()
    curr_lvls <- processed_element[["lvls"]]
    relevant_factors <- names(curr_lvls)
    for (fct in relevant_factors) {
      lvls[[fct]] <- setdiff(
        levels(dataset_list[[filter_dataset]][[fct]]),
        curr_lvls[[fct]] %||% levels(dataset_list[[filter_dataset]][[fct]])
      )
    }
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions[["filter"]][["select_subset"]] <- function(dataset_list, filter_element) {
    variable <- filter_element[["variable"]]
    include_NA <- filter_element[["include_NA"]]
    filter_dataset <- filter_element[["dataset"]] # TODO: Change for name table
    assert(variable %in% names(dataset_list[[filter_dataset]]), sprintf("data[['%s']] does not contain col `%s`", filter_dataset, if (is.null(variable)) "NULL" else variable))
    variable_values <- dataset_list[[filter_dataset]][[variable]]

    # Logical are treated as factors
    if (is.logical(variable_values)) {
      variable_values <- factor(variable_values)
      is_factor <- FALSE
    } else {
      is_factor <- TRUE
    }

    variable <- filter_element[["variable"]]
    include_NA <- filter_element[["include_NA"]]
    values <- filter_element[["values"]]
    mask <- (variable_values %in% values) | (is.na(variable_values) & include_NA)
    if (is_factor) {
      lvls <- list(values)
      names(lvls) <- variable
    } else {
      lvls <- list()
    }
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions[["filter"]][["select_range"]] <- function(dataset_list, filter_element) {
    variable <- filter_element[["variable"]]
    operation <- filter_element[["operation"]]
    include_NA <- filter_element[["include_NA"]]
    filter_dataset <- filter_element[["dataset"]] # TODO: Change for name table
    assert(variable %in% names(dataset_list[[filter_dataset]]), sprintf("data[['%s']] does not contain col `%s`", filter_dataset, if (is.null(variable)) "NULL" else variable))
    variable_values <- dataset_list[[filter_dataset]][[variable]]

    max <- filter_element[["max"]]
    min <- filter_element[["min"]]
    assert(is.numeric(variable_values), "Field values must be numerical")
    assert(is.numeric(min) && is.numeric(max), "Max and min must be numerical")
    assert(min <= max, "min <= max")
    mask <- (((variable_values <= max) & (variable_values >= min)) & !is.na(variable_values)) | (is.na(variable_values) & include_NA)
    lvls <- list()
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions[["filter"]][["select_date"]] <- function(dataset_list, filter_element) {
    variable <- filter_element[["variable"]]
    operation <- filter_element[["operation"]]
    include_NA <- filter_element[["include_NA"]]
    filter_dataset <- filter_element[["dataset"]] # TODO: Change for name table
    assert(variable %in% names(dataset_list[[filter_dataset]]), sprintf("data[['%s']] does not contain col `%s`", filter_dataset, if (is.null(variable)) "NULL" else variable))
    variable_values <- dataset_list[[filter_dataset]][[variable]]

    if (inherits(variable_values, "POSIXct")) {
      max <- as.POSIXct(filter_element[["max"]], "%Y-%m-%d")
      min <- as.POSIXct(filter_element[["min"]], "%Y-%m-%d")
    } else if (inherits(variable_values, "Date")) {
      max <- as.Date(filter_element[["max"]], "%Y-%m-%d")
      min <- as.Date(filter_element[["min"]], "%Y-%m-%d")
    } else {
      stop("Field values must be POSIX.ct or Date")
    }
    assert(min <= max, "min <= max")
    mask <- (((variable_values <= max) & (variable_values >= min)) & !is.na(variable_values)) | (is.na(variable_values) & include_NA)
    lvls <- list()
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions

})

# nolint start cyclocomp_linter
process_dataset_filter_element <- function(dataset_list, filter_element) { # TODO: replace dataset for dataset_name

  filter_element <- as_safe_list(filter_element)

  kind <- filter_element[["kind"]]
  operation <- filter_element[["operation"]]

  if (!kind %in% names(dataset_filter_operations)) stop(paste0("Kind unknown: `", kind, "`"))
  if (!operation %in% names(dataset_filter_operations[[kind]])) stop(paste0("Operation unknown: `", operation, "`"))

  res <- dataset_filter_operations[[kind]][[operation]](dataset_list, filter_element)
  return(res)
}
# nolint end cyclocomp_linter

create_dataset_filter_info <- function(dataset_list, filter_state) {
  datasets_filter <- as_safe_list(filter_state)

  dataset_filter_info <- list()

  for (dataset_filter_child in datasets_filter[["children"]]) {
    kind <- dataset_filter_child[["kind"]]
    name <- dataset_filter_child[["name"]]
    assert(!(name %in% names(dataset_filter_info)), "a dataset can only appear once inside dataset_filters")
    assert(name %in% names(dataset_list), "dataset is not inside dataset_list")
    assert(kind == "dataset", "dataset_filters children can only be of kind `dataset`")
    if (length(dataset_filter_child[["children"]]) == 1) {
      processed_element <- process_dataset_filter_element(dataset_list, dataset_filter_child[["children"]][[1]])
      assert(processed_element[["dataset"]] == name, "Filter on the wrong dataset")
      dataset_filter_info[[name]][["mask"]] <- processed_element[["mask"]]
      dataset_filter_info[[name]][["lvls"]] <- processed_element[["lvls"]]
    } else if (length(dataset_filter_child[["children"]]) == 0) {
      dataset_filter_info[[name]][["mask"]] <- rep_len(TRUE, nrow(dataset_list[[name]]))
      dataset_filter_info[[name]][["lvls"]] <- list()
    } else {
      assert(FALSE, "`datasets_filter` cannot contain more than children")
    }
  }

  return(dataset_filter_info)
}

apply_dataset_filter_info <- function(dataset_list, dataset_filter_info) {
  filtered_dataset_list <- dataset_list
  for (current_dataset_name in names(dataset_filter_info)) {
    current_mask <- dataset_filter_info[[current_dataset_name]][["mask"]]
    current_lvls <- dataset_filter_info[[current_dataset_name]][["lvls"]]
    current_filtered_dataset <- filtered_dataset_list[[current_dataset_name]]
    lbls <- get_lbls(current_filtered_dataset)
    filtered_dataset <- current_filtered_dataset[current_mask, , drop = FALSE]

    for (var_name in names(current_lvls)) {
      unfiltered_var <- dataset_list[[current_dataset_name]][[var_name]]
      all_possible_lvls <- levels(unfiltered_var)

      filtered_var <- filtered_dataset[[var_name]]
      present_lvls <- levels(droplevels(filtered_var))

      # Applying lvls have the following side case
      # - A factor level may be filtered out using a filter, therefore the lvl should be dropped
      # BUT another filter with an operation, or, may reintroduce a row with a lvl that is supposed to be dropped
      # Therefore we force all levels present in the variable to not be dropped
      new_lvls <- union(present_lvls, current_lvls[[var_name]])

      # Side effect of intersect is that it maintains the order of the first set
      new_lvls <- intersect(all_possible_lvls, new_lvls)
      filtered_dataset[[var_name]] <- factor(filtered_dataset[[var_name]], new_lvls)
    }

    filtered_dataset_list[[current_dataset_name]] <- set_lbls(filtered_dataset, lbls)
  }
  return(filtered_dataset_list)
}

apply_subject_filter_info <- function(dataset_list, subject_filter_info, subj_var) {
  filtered_dataset_list <- dataset_list
  subject_set <- subject_filter_info[["subjects"]]

  for (current_dataset_name in names(dataset_list)) {
    current_mask <- dataset_list[[current_dataset_name]][[subj_var]] %in% subject_set
    current_filtered_dataset <- filtered_dataset_list[[current_dataset_name]]
    current_lvls <- subject_filter_info[["dataset_list_lvls"]][[current_dataset_name]]
    lbls <- get_lbls(current_filtered_dataset)
    filtered_dataset <- current_filtered_dataset[current_mask, , drop = FALSE]

    for (var_name in names(current_lvls)) {
      unfiltered_var <- dataset_list[[current_dataset_name]][[var_name]]
      all_possible_lvls <- levels(unfiltered_var)

      filtered_var <- filtered_dataset[[var_name]]
      present_lvls <- levels(droplevels(filtered_var))

      # Applying lvls have the following side case
      # - A factor level may be filtered out using a filter, therefore the lvl should be dropped
      # BUT another filter with an operation, or, may reintroduce a row with a lvl that is supposed to be dropped
      # Therefore we force all levels present in the variable to not be dropped
      new_lvls <- union(present_lvls, current_lvls[[var_name]])

      # Side effect of intersect is that it maintains the order of the first set
      new_lvls <- intersect(all_possible_lvls, new_lvls)
      filtered_dataset[[var_name]] <- factor(filtered_dataset[[var_name]], new_lvls)
    }

    filtered_dataset_list[[current_dataset_name]] <- set_lbls(filtered_dataset, lbls)
  }
  return(filtered_dataset_list)
}

create_subject_filter_info <- function(dataset_list, subject_filter, sbj_var) {
  subject_filter <- as_safe_list(subject_filter)
  complete_subject_list <- character(0)
  for (current_data in dataset_list) {
    complete_subject_list <- union(complete_subject_list, as.character(unique(current_data[[sbj_var]])))
  }
  children <- subject_filter[["children"]]
  assert(length(children) < 2, "subject filter must have 0 or 1 child")

  if (length(children) == 0) {
    subject_filter_info <- list(subjects = complete_subject_list, dataset_list_lvls = list())
  } else  if (length(children) == 1) {
    subject_filter_info <- process_subject_filter_element(dataset_list, children[[1]], sbj_var, complete_subject_list)
  }

  return(subject_filter_info)
}

subject_filter_operations <- local({

  actions <- list(set_operation = list(), filter = list(), row_operation = list())

  actions[["set_operation"]][["union"]] <- function(dataset_list, filter_element, sbj_var, complete_subject_list) {
    children <- filter_element[["children"]]
    subjects <- character(0)
    assert(length(children) > 0, "`union` operation requires at least one child")
    dataset_list_lvls <-  rep_len(list(list()), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    for (child in children) {
      processed_element <- process_subject_filter_element(dataset_list, child, sbj_var, complete_subject_list)
      subjects <- union(subjects, processed_element[["subjects"]])

      child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
      for (dataset_name in names(child_dataset_list_lvls)) {
        dataset_lvls <- dataset_list_lvls[[dataset_name]]
        child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]]
          relevant_factors <-  names(child_dataset_lvls)
          for (fct in relevant_factors) {
            dataset_lvls[[fct]]  <- union(dataset_lvls[[fct]], child_dataset_lvls[[fct]])
        }
        dataset_list_lvls[[dataset_name]] <- dataset_lvls

      }
    }
    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[["set_operation"]][["intersect"]] <- function(dataset_list, filter_element, sbj_var, complete_subject_list) {
    children <- filter_element[["children"]]
    subjects <- complete_subject_list
    assert(length(children) > 0, "`intersect` operation requires at least one child")
    dataset_list_lvls <-  rep_len(list(list()), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    for (child in children) {
      processed_element <- process_subject_filter_element(dataset_list, child, sbj_var, complete_subject_list)
      subjects <- intersect(subjects, processed_element[["subjects"]])

      child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
      for (dataset_name in names(child_dataset_list_lvls)) {
        dataset_lvls <- dataset_list_lvls[[dataset_name]]
        child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]]
          relevant_factors <- names(child_dataset_lvls)
          for (fct in relevant_factors) {
            dataset_lvls[[fct]] <- intersect(
              dataset_lvls[[fct]] %||% levels(dataset_list[[dataset_name]][[fct]]),
              child_dataset_lvls[[fct]]
            )
        }
        dataset_list_lvls[[dataset_name]] <- dataset_lvls

      }
    }

    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[["set_operation"]][["complement"]] <- function(dataset_list, filter_element, sbj_var, complete_subject_list) {
    children <- filter_element[["children"]]
    assert(length(children) == 1, "`complement` operation requires exactly one child")
    processed_element <- process_subject_filter_element(dataset_list, children[[1]], sbj_var, complete_subject_list)
    subjects <- setdiff(complete_subject_list, processed_element[["subjects"]])
    dataset_list_lvls <-  rep_len(list(list()), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
    for (dataset_name in names(child_dataset_list_lvls)) {
      dataset_lvls <- dataset_list_lvls[[dataset_name]]
      child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]]
        relevant_factors <- names(child_dataset_lvls)
        for (fct in relevant_factors) {
          dataset_lvls[[fct]]  <- setdiff(
          levels(dataset_list[[dataset_name]][[fct]]),
          child_dataset_lvls[[fct]] %||% levels(dataset_list[[dataset_name]][[fct]])
        )
      }
      dataset_list_lvls[[dataset_name]] <- dataset_lvls
    }

    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[["filter"]] <- function(dataset_list, filter_element, sbj_var) {
    processed_element <- process_dataset_filter_element(dataset_list, filter_element)
    mask <- processed_element[["mask"]]
    dataset <- processed_element[["dataset"]]
    subjects <- as.character(dataset_list[[dataset]][[sbj_var]][mask])

    dataset_list_lvls <-  rep_len(list(list()), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)
    dataset_lvls <- processed_element[["lvls"]]
    dataset_name <- processed_element[["dataset"]]
    dataset_list_lvls[[dataset_name]] <- dataset_lvls

    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[["row_operation"]] <- actions[["filter"]]


  actions

})

process_subject_filter_element <- function(dataset_list, filter_element, sbj_var, complete_subject_list) {

  filter_element <- as_safe_list(filter_element)
  kind <- filter_element[["kind"]]
  operation <- filter_element[["operation"]]

  if (!kind %in% names(subject_filter_operations)) stop(paste0("Kind unknown: `", kind, "`"))
  if (!operation %in% names(subject_filter_operations[[kind]]) && !kind %in% c("row_operation", "filter")) stop(paste0("Operation unknown: `", operation, "`"))

  # TODO: This if statement breaks the intention of the upper code of removing ifs. Not relevant now.
  if (!kind %in% c("row_operation", "filter")) {
    subject_filter_info <- subject_filter_operations[[kind]][[operation]](dataset_list, filter_element, sbj_var, complete_subject_list)
  } else {
    subject_filter_info <- subject_filter_operations[[kind]](dataset_list, filter_element, sbj_var)
  }

  return(subject_filter_info)
}

to_filter_validate <- jsonvalidate::json_validator(
  system.file("to_filter_schema.json", package = "dv.manager", mustWork = TRUE),
  engine = "ajv",
  strict = TRUE
)

from_filter_validate <- jsonvalidate::json_validator(
  system.file("from_filter_schema.json", package = "dv.manager", mustWork = TRUE),
  engine = "ajv",
  strict = TRUE
)

yyjsonr_read_json_str_with_options <- function(x) {
  yyjsonr::read_json_str(x, obj_of_arrs_to_df = FALSE, arr_of_objs_to_df = FALSE, num_specials = "special")
}


add_blockly_dependency <- function() {
  htmltools::htmlDependency(
    name = "filter_blockly",
    version = utils::packageVersion("dv.manager"),
    src = app_sys("filter/"),
    script = c("dv_filter_minified.js"),
    style = c("dv_filter.css"),
  )
}

new_filter_ui <- function(id, dataset_lists, subject_dataset_name, state = NULL, saved_states = NULL, strict = FALSE) {
  ns <- shiny::NS(id)

  if (!is.null(state)) {
    if (file.exists(state)) {
      state <- paste0(readLines(state), collapse = "\n")
    }
  } else {
    state <- "null" # Acts as a no filter JSON
  }

  filter_bookmark <- shiny::restoreInput(ns(ID$FILTER_STATE_JSON_INPUT), state)
  filter_bookmark <- filter_bookmark %||% "null"
  saved_states_bookmark <- shiny::restoreInput(ns(ID$SAVED_FILTER_STATE_JSON_MSG_INPUT), saved_states)
  saved_states_bookmark <- saved_states_bookmark %||% "null"

  log_inform(paste("Loading state", filter_bookmark))
  log_inform(paste("Loading saved states", saved_states_bookmark))

  d <- get_filter_data(dataset_lists)

  filter_data <- yyjsonr::write_json_str(d)

  if (strict) assert(to_filter_validate(filter_data), "failed to validate message to filter")

  escape_special_chars <- function(x) {
    y <- gsub("\\\\", "\\\\\\\\", x)
    y <- gsub('"', '\\\\"', y)
    y <- gsub("'", "\\\\'", y)
    y <- gsub("\n", "\\\\n", y)
    y <- gsub("\r", "\\\\r", y)
    y <- gsub("\t", "\\\\t", y)
    y
  }

  init_tag <- shiny::tags[["script"]](
    shiny::HTML(
      sprintf(
        "dv_filter.init('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
        escape_special_chars(ns(ID$FILTER_CONTAINER)),
        escape_special_chars(filter_data),
        escape_special_chars(filter_bookmark),
        escape_special_chars(saved_states_bookmark),
        escape_special_chars(subject_dataset_name),
        escape_special_chars(ns(ID$FILTER_STATE_JSON_INPUT)),
        escape_special_chars(ns(ID$SAVED_FILTER_STATE_JSON_MSG_INPUT)),
        escape_special_chars(ns(ID$EXPORT_CODE_INPUT)),
        escape_special_chars(ns(ID$FILTER_LOG_INPUT))
      )
    )
  )

  tag_dv_filter_wrapper <- function(...) {
    shiny::tag("dv-filter-wrapper", list(...))
  }

  tag_dv_filter_dependencies <- function(...) {
    shiny::tag("dv-filter-dependencies", list(...))
  }

  tag_dv_filter_root <- function(...) {
    shiny::tag("dv-filter-root", list(...))
  }

  dependencies <- shiny::singleton(
    tag_dv_filter_dependencies(
      shiny::span(
        style = "display:none;",
        add_blockly_dependency(),
        # When attaching the dependencies on my own an error occurs when using multiple
        # When including an input perse the error disappears, this should be explored
        shiny::sliderInput(ns("IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES1"), label = NULL, min = 0, max = 0, value = 0),
        shiny::dateRangeInput(ns("IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES2"), label = NULL, start = "2001-01-01", end = "2001-01-01"),
        shinyWidgets::pickerInput(ns("IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES3"), choices = c("A", "B"), multiple = TRUE)
      )
    )
  )

  combined_ui <- local({
    tag_dv_filter_wrapper(
      dependencies,
      tag_dv_filter_root(
        id = ns(ID$FILTER_CONTAINER),
        init_tag
      )
    )
  })

  combined_ui
}

new_filter_server <- function(id, selected_dataset_list_name, subject_filter_dataset_name, after_filter_dataset_list, strict = FALSE) {
  mod <- function(input, output, session) {
    shiny::setBookmarkExclude(
      c(
        "IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES1",
        "IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES2",
        "IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES3"
      )
    )
    ns <- session[["ns"]]

    log_inform(paste("Listening to:", ns(ID$FILTER_STATE_JSON_INPUT)))
    log_inform(paste("Listening to:", ns(ID$SAVED_FILTER_STATE_JSON_MSG_INPUT)))

    shiny::observeEvent(selected_dataset_list_name(), {
      session[["sendCustomMessage"]](
        "init_filter",
        list(
          dataset_list_name = selected_dataset_list_name()
        )
      )
    })

    shiny::observeEvent(input[[ID$SAVED_FILTER_STATE_JSON_MSG_INPUT]], {
      log_inform(
        paste("Received saved states:", input[[ID$SAVED_FILTER_STATE_JSON_MSG_INPUT]])
      )
    })

    shiny::observeEvent(after_filter_dataset_list(), {
      shiny::req(!is.null(after_filter_dataset_list()))

      fd <- after_filter_dataset_list()
      fd_names <- names(fd)
      row_count <- vector("list", length = length(fd))

      for (idx in seq_along(fd)) {
        row_count[[idx]] <- list(
          count = yyjsonr::as_scalar(nrow(fd[[idx]])),
          name = yyjsonr::as_scalar(fd_names[[idx]])
        )
      }

      msg <- list(
        row_count = row_count
      )

      session[["sendCustomMessage"]](
        "update_filter_result",
        list(json = yyjsonr::write_json_str(msg))
      )
    })

    shiny::observeEvent(input[[ID$FILTER_STATE_JSON_INPUT]], {
      log_inform("RECEIVED FILTER")
    })

    shiny::observeEvent(input[[ID$FILTER_LOG_INPUT]], {
      for (msg in input[[ID$FILTER_LOG_INPUT]]) {
        shiny::showNotification(msg, type = "warn", duration = NULL)
      }
    })

    res <- shiny::reactive({
      log_inform("PROCESSING FILTER")
      json_r <- input[[ID$FILTER_STATE_JSON_INPUT]]

      if (checkmate::test_string(json_r, min.chars = 1)) {
        if (strict) assert(from_filter_validate(json_r), "failed to validate message from filter")
        parsed_json <- yyjsonr_read_json_str_with_options(json_r)
        log_inform("PROCESSING FILTER PARSED")
        list(
          parsed = parsed_json %||% NA_character_,
          raw = json_r
        )
      } else {
        log_inform("PROCESSING FILTER NA")
        list(
          parsed = NA_character_,
          raw = NA_character_
        )
      }
    })

    output[[ID$EXPORT_CODE_INPUT]] <- shiny::downloadHandler(
      filename = "filter.txt",
      content = function(file) {
        if (!checkmate::test_string(input[[ID$FILTER_STATE_JSON_INPUT]])) {
          data <- "null"
          shiny::showNotification("Empty filter exported. Please apply filter before exporting.", type = "warn")
        } else {
          data <- jsonlite::prettify(input[[ID$FILTER_STATE_JSON_INPUT]])
        }
        writeLines(
          data,
          con = file
        )
      },
      contentType = "application/json"
    )
    shiny::outputOptions(output, ID$EXPORT_CODE_INPUT, suspendWhenHidden = FALSE)

    return(
      structure(res, raw = shiny::reactive(input[[ID$FILTER_STATE_JSON_INPUT]]))
    )
  }
  shiny::moduleServer(id, mod)
}
