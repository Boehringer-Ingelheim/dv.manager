toJSON <- function(x) unclass(jsonlite::toJSON(x))
fromJSON <- function(x) jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = FALSE)
as_scalar <- jsonlite::unbox
serialize_filter_data_to_client_bin64 <- function(x) {
  jsonlite::base64_enc(binary_serialize_filter_data_C(get_filter_data(x)))
}
deserialize_filter_state_from_client <- fromJSON

FC <- poc(
  KIND = poc(
    CATEGORICAL = "categorical",
    NUMERICAL = "numerical",
    DATE = "date",
    UNKNOWN = "unknown"
  ),
  FDF = poc(
    # Filter Data Field
    NAME = "name",
    NROW = "nrow",
    VARIABLES = "variables",
    DATASET_LIST = "dataset_list",
    DATASET_LISTS = "dataset_lists",
    LABEL = "label",
    CLASS = "class",
    KIND = "kind",
    NA_COUNT = "NA_count",
    VALUE = "value",
    COUNT = "count",
    MIN = "min",
    MAX = "max",
    DENSITY = "density",
    MSG = "msg"
  ),
  FE = poc(
    F = poc(
      NAME = "name",
      KIND = "kind",
      OPERATION = "operation",
      CHILDREN = "children",
      INCLUDE_NA = "include_NA",
      DATASET = "dataset",
      VARIABLE = "variable",
      VALUES = "values",
      MIN = "min",
      MAX = "max"
    ),
    OP = poc(
      OPERATION = "filter",
      SUBSET = "select_subset",
      RANGE = "select_range",
      DATE = "select_date"
    ),
    COMB = poc(
      OPERATION = "row_operation",
      AND = "and",
      OR = "or",
      NOT = "not"
    )
  ),
  SFE = poc(
    F = poc(NAME = "name", KIND = "kind", OPERATION = "operation", CHILDREN = "children"),
    COMB = poc(
      OPERATION = "set_operation",
      UNION = "union",
      INTERSECT = "intersect",
      COMPLEMENT = "complement"
    )
  ),
  ERRORS = poc(
    FILTER_IS_NA = list(
      class = "UNFILTERED_DATASET_LIST_NAME_FILTER_DATASET_LIST_NAME_MISMATCH",
      message = "Mismatch between unfiltered dataset list name and filter dataset list name"
    ),
    UNFILTERED_DATASET_LIST_NAME_FILTER_DATASET_LIST_NAME_MISMATCH = list(
      class = "UNFILTERED_DATASET_LIST_NAME_FILTER_DATASET_LIST_NAME_MISMATCH",
      message = "Mismatch between unfiltered dataset list name and filter dataset list name"
    ),
    GENERIC_FILTER_APPLICATION = list(
      class = "GENERIC_FILTER_APPLICATION",
      message = NA
    )
  )
)

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
  } else {}
  return(res)
}

create_subject_level_ui <- function(id) {
  dv.filter::data_filter_ui(id)
}

create_subject_level_server <- function(
  id,
  data
) {
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
  } else {}

  return(res)
}

# NEW FILTER ----

get_single_filter_data <- function(dataset) {
  nm_var <- names(dataset)
  n_var <- length(nm_var)
  res <- vector(mode = "list", length = n_var)

  FDF <- FC$FDF
  K <- FC$KIND

  for (idx in seq_len(n_var)) {
    name <- nm_var[[idx]]
    var <- dataset[[name]]
    label <- attr(var, "label") %||% name # FIXME: This is done to maintain the same behavior as jsonlite. Should be reviewed with the js code that uses labels

    l <- stats::setNames(
      object = list(name, label, class(var)[1]),
      nm = c(FDF$NAME, FDF$LABEL, FDF$CLASS)
    )

    # Logical is treated as a factor in the client
    if (is.logical(var)) {
      var <- factor(var)
    }

    if (is.character(var) || is.factor(var)) {
      l[[FDF$KIND]] <- K$CATEGORICAL
      l[[FDF$NA_COUNT]] <- sum(is.na(var))
      na_clean_var <- var[!is.na(var)]
      count <- sort(table(na_clean_var), decreasing = TRUE)
      values <- names(count)
      count <- as.integer(count)
      l[["value"]] <- values %||% character(0)
      l[["count"]] <- count
    } else if (is.numeric(var)) {
      var <- as.numeric(var)
      l[[FDF$KIND]] <- K$NUMERICAL
      l[[FDF$NA_COUNT]] <- sum(is.na(var))
      na_clean_var <- var[!is.na(var)]

      l[[FDF$MIN]] <- min(Inf, na_clean_var, na.rm = TRUE)
      l[[FDF$MAX]] <- max(-Inf, na_clean_var, na.rm = TRUE)

      if (length(na_clean_var) > 0 && !all(is.infinite(na_clean_var))) {
        hist_info <- hist(na_clean_var, plot = FALSE)
      } else {
        hist_info <- list(density = numeric(0))
      }

      l[[FDF$DENSITY]] <- hist_info[["density"]]
    } else if (inherits(var, "POSIXct") || inherits(var, "Date")) {
      if (inherits(var, "POSIXct")) {
        var <- as.Date(var)
      }

      var <- as.numeric(var)
      inf_date <- Inf
      minus_inf_date <- -Inf

      l[[FDF$KIND]] <- K$DATE
      l[[FDF$NA_COUNT]] <- sum(is.na(var))
      na_clean_var <- var[!is.na(var)]

      l[[FDF$MIN]] <- min(inf_date, na_clean_var, na.rm = TRUE)
      l[[FDF$MAX]] <- max(minus_inf_date, na_clean_var, na.rm = TRUE)
    } else {
      l[[FDF$KIND]] <- K$UNKNOWN
      l[[FDF$NA_COUNT]] <- NA_integer_
    }

    res[[idx]] <- l
  }
  return(res)
}

get_filter_data <- function(dataset_lists) {
  FDF <- FC$FDF

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
      current_dataset_res[[jdx]] <- stats::setNames(
        object = list(current_dataset_name, nrow(current_dataset), get_single_filter_data(current_dataset)),
        nm = c(FDF$NAME, FDF$NROW, FDF$VARIABLES)
      )
    }
    res[[idx]] <- stats::setNames(
      object = list(current_dataset_list_name, current_dataset_res),
      nm = c(FDF$NAME, FDF$DATASET_LIST)
    )
  }
  res <- stats::setNames(
    object = list(res),
    nm = c(FDF$DATASET_LISTS)
  )
  return(res)
}

subject_filter_operations <- local({
  # Operations partially matches each other and (y4BQ)

  actions <- list(set_operation = list(), filter = list(), row_operation = list())

  actions[[FC$SFE$COMB$OPERATION]][[FC$SFE$COMB$UNION]] <- function(
    dataset_list,
    filter_element,
    sbj_var,
    complete_subject_list
  ) {
    children <- filter_element[["children"]]
    subjects <- character(0)
    assert(length(children) > 0, "`union` operation requires at least one child")
    dataset_list_lvls <- rep_len(list(list()), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    for (child in children) {
      processed_element <- process_subject_filter_element(dataset_list, child, sbj_var, complete_subject_list)
      subjects <- union(subjects, processed_element[["subjects"]])

      child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
      for (dataset_name in names(child_dataset_list_lvls)) {
        dataset_lvls <- dataset_list_lvls[[dataset_name]]
        child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]]
        relevant_factors <- names(child_dataset_lvls)
        for (fct in relevant_factors) {
          dataset_lvls[[fct]] <- union(dataset_lvls[[fct]], child_dataset_lvls[[fct]])
        }
        dataset_list_lvls[[dataset_name]] <- dataset_lvls
      }
    }
    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[[FC$SFE$COMB$OPERATION]][[FC$SFE$COMB$INTERSECT]] <- function(
    dataset_list,
    filter_element,
    sbj_var,
    complete_subject_list
  ) {
    children <- filter_element[["children"]]
    subjects <- complete_subject_list
    assert(length(children) > 0, "`intersect` operation requires at least one child")
    dataset_list_lvls <- rep_len(list(list()), length = length(dataset_list))
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

  actions[[FC$SFE$COMB$OPERATION]][[FC$SFE$COMB$COMPLEMENT]] <- function(
    dataset_list,
    filter_element,
    sbj_var,
    complete_subject_list
  ) {
    children <- filter_element[["children"]]
    assert(length(children) == 1, "`complement` operation requires exactly one child")
    processed_element <- process_subject_filter_element(dataset_list, children[[1]], sbj_var, complete_subject_list)
    subjects <- setdiff(complete_subject_list, processed_element[["subjects"]])
    dataset_list_lvls <- rep_len(list(list()), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
    for (dataset_name in names(child_dataset_list_lvls)) {
      dataset_lvls <- dataset_list_lvls[[dataset_name]]
      child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]]
      relevant_factors <- names(child_dataset_lvls)
      for (fct in relevant_factors) {
        dataset_lvls[[fct]] <- setdiff(
          levels(dataset_list[[dataset_name]][[fct]]),
          child_dataset_lvls[[fct]] %||% levels(dataset_list[[dataset_name]][[fct]])
        )
      }
      dataset_list_lvls[[dataset_name]] <- dataset_lvls
    }

    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[[FC$FE$OP$OPERATION]] <- function(dataset_list, filter_element, sbj_var) {
    processed_element <- process_dataset_filter_element(dataset_list, filter_element)
    mask <- processed_element[["mask"]]
    dataset <- processed_element[["dataset"]]
    subjects <- as.character(dataset_list[[dataset]][[sbj_var]][mask])

    dataset_list_lvls <- rep_len(list(list()), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)
    dataset_lvls <- processed_element[["lvls"]]
    dataset_name <- processed_element[["dataset"]]
    dataset_list_lvls[[dataset_name]] <- dataset_lvls

    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[[FC$FE$COMB$OPERATION]] <- actions[["filter"]]

  actions
})

dataset_filter_operations <- local({
  # Operations partially matches each other and (y4BQ)

  actions <- list(row_operation = list(), filter = list())

  actions[[FC$FE$COMB$OPERATION]][[FC$FE$COMB$AND]] <- function(dataset_list, filter_element) {
    filter_dataset <- NA
    assert(length(filter_element[[FC$FE$F$CHILDREN]]) >= 1, "`and` operation requires at least one child")
    mask <- TRUE # Neutral element for &
    lvls <- list()
    for (child in filter_element[[FC$FE$F$CHILDREN]]) {
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

  actions[[FC$FE$COMB$OPERATION]][[FC$FE$COMB$OR]] <- function(dataset_list, filter_element) {
    filter_dataset <- NA
    assert(length(filter_element[[FC$FE$F$CHILDREN]]) >= 1, "`or` operation requires at least one child")

    mask <- FALSE # Neutral element for |
    lvls <- list()
    for (child in filter_element[[FC$FE$F$CHILDREN]]) {
      processed_element <- process_dataset_filter_element(dataset_list, child)
      mask <- mask | processed_element[["mask"]]
      curr_lvls <- processed_element[["lvls"]]
      if (is.na(filter_dataset)) {
        filter_dataset <- processed_element[[FC$FE$F$DATASET]]
      } else {
        assert(processed_element[[FC$FE$F$DATASET]] == filter_dataset, "Filtering on the wrong dataset")
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

  actions[[FC$FE$COMB$OPERATION]][[FC$FE$COMB$NOT]] <- function(dataset_list, filter_element) {
    filter_dataset <- NA
    assert(length(filter_element[[FC$FE$F$CHILDREN]]) == 1, "`not` operation requires exactly one child")
    child <- filter_element[[FC$FE$F$CHILDREN]][[1]]
    processed_element <- process_dataset_filter_element(dataset_list, child)
    mask <- !processed_element[["mask"]]
    filter_dataset <- processed_element[[FC$FE$F$DATASET]]
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

  actions[[FC$FE$OP$OPERATION]][[FC$FE$OP$SUBSET]] <- function(dataset_list, filter_element) {
    variable <- filter_element[[FC$FE$F$VARIABLE]]
    include_NA <- filter_element[[FC$FE$F$INCLUDE_NA]]
    filter_dataset <- filter_element[[FC$FE$F$DATASET]] # TODO: Change for name table
    assert(
      variable %in% names(dataset_list[[filter_dataset]]),
      sprintf("data[['%s']] does not contain col `%s`", filter_dataset, if (is.null(variable)) "NULL" else variable)
    )
    variable_values <- dataset_list[[filter_dataset]][[variable]]

    # Logical are treated as factors
    if (is.logical(variable_values)) {
      variable_values <- factor(variable_values)
      is_factor <- FALSE
    } else {
      is_factor <- TRUE
    }

    variable <- filter_element[[FC$FE$F$VARIABLE]]
    include_NA <- filter_element[[FC$FE$F$INCLUDE_NA]]
    values <- filter_element[[FC$FE$F$VALUES]]
    mask <- (variable_values %in% values) | (is.na(variable_values) & include_NA)
    if (is_factor) {
      lvls <- list(values)
      names(lvls) <- variable
    } else {
      lvls <- list()
    }
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions[[FC$FE$OP$OPERATION]][[FC$FE$OP$RANGE]] <- function(dataset_list, filter_element) {
    variable <- filter_element[[FC$FE$F$VARIABLE]]
    operation <- filter_element[[FC$FE$F$OPERATION]]
    include_NA <- filter_element[[FC$FE$F$INCLUDE_NA]]
    filter_dataset <- filter_element[[FC$FE$F$DATASET]] # TODO: Change for name table
    assert(
      variable %in% names(dataset_list[[filter_dataset]]),
      sprintf("data[['%s']] does not contain col `%s`", filter_dataset, if (is.null(variable)) "NULL" else variable)
    )
    variable_values <- dataset_list[[filter_dataset]][[variable]]

    max <- filter_element[[FC$FE$F$MAX]]
    min <- filter_element[[FC$FE$F$MIN]]
    assert(is.numeric(variable_values), "Field values must be numerical")
    assert(is.numeric(min) && is.numeric(max), "Max and min must be numerical")
    assert(min <= max, "min <= max")
    mask <- (((variable_values <= max) & (variable_values >= min)) & !is.na(variable_values)) |
      (is.na(variable_values) & include_NA)
    lvls <- list()
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions[[FC$FE$OP$OPERATION]][[FC$FE$OP$DATE]] <- function(dataset_list, filter_element) {
    variable <- filter_element[[FC$FE$F$VARIABLE]]
    operation <- filter_element[[FC$FE$F$OPERATION]]
    include_NA <- filter_element[[FC$FE$F$INCLUDE_NA]]
    filter_dataset <- filter_element[[FC$FE$F$DATASET]] # TODO: Change for name table
    assert(
      variable %in% names(dataset_list[[filter_dataset]]),
      sprintf("data[['%s']] does not contain col `%s`", filter_dataset, if (is.null(variable)) "NULL" else variable)
    )
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
    mask <- (((variable_values <= max) & (variable_values >= min)) & !is.na(variable_values)) |
      (is.na(variable_values) & include_NA)
    lvls <- list()
    return(list(mask = mask, dataset = filter_dataset, lvls = lvls))
  }

  actions
})

process_dataset_filter_element <- function(dataset_list, filter_element) {
  # TODO: replace dataset for dataset_name

  filter_element <- as_safe_list(filter_element)

  kind <- filter_element[[FC$FE$F$KIND]]
  operation <- filter_element[[FC$FE$F$OPERATION]]

  if (!kind %in% names(dataset_filter_operations)) {
    stop(paste0("Kind unknown: `", kind, "`"))
  }
  if (!operation %in% names(dataset_filter_operations[[kind]])) {
    stop(paste0("Operation unknown: `", operation, "`"))
  }

  res <- dataset_filter_operations[[kind]][[operation]](dataset_list, filter_element)
  return(res)
}

process_subject_filter_element <- function(dataset_list, filter_element, sbj_var, complete_subject_list) {
  filter_element <- as_safe_list(filter_element)
  kind <- filter_element[[FC$SFE$F$KIND]]
  operation <- filter_element[[FC$SFE$F$OPERATION]]

  if (!kind %in% names(subject_filter_operations)) {
    stop(paste0("Kind unknown: `", kind, "`"))
  }
  if (!operation %in% names(subject_filter_operations[[kind]]) && !kind %in% c("row_operation", "filter")) {
    stop(paste0("Operation unknown: `", operation, "`"))
  }

  # TODO: This if statement breaks the intention of the upper code of removing ifs. Not relevant now.
  if (!kind %in% c(FC$FE$COMB$OPERATION, FC$FE$OP$OPERATION)) {
    subject_filter_info <- subject_filter_operations[[kind]][[operation]](
      dataset_list,
      filter_element,
      sbj_var,
      complete_subject_list
    )
  } else {
    subject_filter_info <- subject_filter_operations[[kind]](dataset_list, filter_element, sbj_var)
  }

  return(subject_filter_info)
}

create_dataset_filter_info <- function(dataset_list, filter_state) {
  # Code partially matches  (cdU0)
  datasets_filter <- as_safe_list(filter_state)

  dataset_filter_info <- list()

  for (dataset_filter_child in datasets_filter[[FC$FE$F$CHILDREN]]) {
    kind <- dataset_filter_child[[FC$FE$F$KIND]]
    name <- dataset_filter_child[[FC$FE$F$NAME]]
    assert(!(name %in% names(dataset_filter_info)), "a dataset can only appear once inside dataset_filters")
    assert(name %in% names(dataset_list), "dataset is not inside dataset_list")
    assert(kind == "dataset", "dataset_filters children can only be of kind `dataset`")
    if (length(dataset_filter_child[[FC$FE$F$CHILDREN]]) == 1) {
      processed_element <- process_dataset_filter_element(dataset_list, dataset_filter_child[["children"]][[1]])
      assert(processed_element[["dataset"]] == name, "Filter on the wrong dataset")
      dataset_filter_info[[name]][["mask"]] <- processed_element[["mask"]]
      dataset_filter_info[[name]][["lvls"]] <- processed_element[["lvls"]]
    } else if (length(dataset_filter_child[[FC$FE$F$CHILDREN]]) == 0) {
      dataset_filter_info[[name]][["mask"]] <- rep_len(TRUE, nrow(dataset_list[[name]]))
      dataset_filter_info[[name]][["lvls"]] <- list()
    } else {
      assert(FALSE, "`datasets_filter` cannot contain more than children")
    }
  }

  return(dataset_filter_info)
}

create_subject_filter_info <- function(dataset_list, subject_filter, sbj_var) {
  # Code partially matches  (cdU0)
  subject_filter <- as_safe_list(subject_filter)
  complete_subject_list <- character(0)
  for (current_data in dataset_list) {
    complete_subject_list <- union(complete_subject_list, as.character(unique(current_data[[sbj_var]])))
  }
  children <- subject_filter[[FC$SFE$F$CHILDREN]]
  assert(length(children) < 2, "subject filter must have 0 or 1 child")

  if (length(children) == 0) {
    subject_filter_info <- list(subjects = complete_subject_list, dataset_list_lvls = list())
  } else if (length(children) == 1) {
    subject_filter_info <- process_subject_filter_element(dataset_list, children[[1]], sbj_var, complete_subject_list)
  }

  return(subject_filter_info)
}

match_set_order <- function(reference, values) {
  assert(all(values %in% reference), "All values must be contained in reference")
  aligned <- intersect(reference, values)
  aligned
}

apply_dataset_filter_info <- function(dataset_list, dataset_filter_info) {
  # Code partially matches  (EO9M)
  filtered_dataset_list <- dataset_list
  dataset_filter_info <- as_safe_list(dataset_filter_info)
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

      new_lvls <- match_set_order(all_possible_lvls, new_lvls)
      filtered_dataset[[var_name]] <- factor(filtered_dataset[[var_name]], new_lvls)
    }

    filtered_dataset_list[[current_dataset_name]] <- set_lbls(filtered_dataset, lbls)
  }
  return(filtered_dataset_list)
}

apply_subject_filter_info <- function(dataset_list, subject_filter_info, subj_var) {
  # Code partially matches  (EO9M)
  filtered_dataset_list <- dataset_list
  subject_set <- subject_filter_info[["subjects"]]
  subject_filter_info <- as_safe_list(subject_filter_info)

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

      new_lvls <- match_set_order(all_possible_lvls, new_lvls)
      filtered_dataset[[var_name]] <- factor(filtered_dataset[[var_name]], new_lvls)
    }

    filtered_dataset_list[[current_dataset_name]] <- set_lbls(filtered_dataset, lbls)
  }
  return(filtered_dataset_list)
}

apply_filter_to_dataset_list <- (function(unfiltered_dataset_list, dataset_list_filter, filter_key_var) {
  error_list <- new_error_list()
  fd <- NULL

  if (identical(as.character(dataset_list_filter), NA_character_)) {
    error_list$push(FC$ERRORS$FILTER_IS_NA)
  } else if (isTRUE(is.na(dataset_list_filter[["parsed"]]))) {
    fd <- unfiltered_dataset_list
  } else {
    unfiltered_dataset_list_name <- attr(unfiltered_dataset_list, "dataset_list_name")
    filter_dataset_list_name <- dataset_list_filter[["parsed"]][["dataset_list_name"]]

    if (unfiltered_dataset_list_name != filter_dataset_list_name) {
      error_list$push(FC$ERRORS$UNFILTERED_DATASET_LIST_NAME_FILTER_DATASET_LIST_NAME_MISMATCH)
    } else {
      safe_filters <- dataset_list_filter[["parsed"]][["filters"]]
      fd <- tryCatch(
        {
          dataset_filter_info <- create_dataset_filter_info(unfiltered_dataset_list, safe_filters[["datasets_filter"]])
          subject_filter_info <- create_subject_filter_info(
            unfiltered_dataset_list,
            safe_filters[["subject_filter"]],
            filter_key_var
          )

          apply_dataset_filter_info(
            unfiltered_dataset_list,
            dataset_filter_info
          ) |>
            apply_subject_filter_info(
              subject_filter_info,
              filter_key_var
            )
        },
        error = function(e) {
          error <- FC$ERRORS$GENERIC_FILTER_APPLICATION
          error$message <- paste("Filter not applied. Error found:\n", e[["message"]])
          error_list$push(error)
          unfiltered_dataset_list
        }
      )
    }
  }

  res <- list(
    fd = fd,
    error_list = error_list
  )

  return(res)
}) |>
  shiny::maskReactiveContext()

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

add_blockly_dependency <- function() {
  htmltools::htmlDependency(
    name = "filter_blockly",
    version = utils::packageVersion("dv.manager"),
    src = app_sys("filter/"),
    script = c("dv_filter_minified.js"),
    style = c("dv_filter.css"),
  )
}

new_filter_ui <- function(id, subject_dataset_name, state = NULL, saved_states = NULL) {
  ns <- shiny::NS(id)

  # JSONify null
  state <- state %||% "null"
  saved_states <- saved_states %||% "null"
  check_parsable_json_input(state)
  check_parsable_json_input(saved_states)

  filter_bookmark <- shiny::restoreInput(ns(ID$FILTER_STATE_JSON_INPUT), state)
  saved_states_bookmark <- shiny::restoreInput(ns(ID$SAVED_FILTER_STATE_JSON_MSG_INPUT), saved_states)

  log_inform(paste("Loading state", filter_bookmark))
  log_inform(paste("Loading saved states", saved_states_bookmark))

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
        "dv_filter.init('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
        escape_special_chars(id),
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
        shiny::dateRangeInput(
          ns("IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES2"),
          label = NULL,
          start = "2001-01-01",
          end = "2001-01-01"
        ),
        shinyWidgets::pickerInput(ns("IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES3"), choices = c("A", "B"), multiple = TRUE)
      )
    )
  )

  combined_ui <- local({
    tag_dv_filter_wrapper(
      dependencies,
      tag_dv_filter_root(
        id = id,
        init_tag
      )
    )
  })

  combined_ui
}

new_filter_server <- function(
  id,
  selected_dataset_list,
  subject_filter_dataset_name,
  after_filter_dataset_list,
  skip_dataset_filters = FALSE,
  strict = FALSE
) {
  mod <- function(input, output, session) {
    shiny::setBookmarkExclude(
      c(
        "IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES1",
        "IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES2",
        "IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES3"
      )
    )
    ns <- session[["ns"]]
    ns_id <- substr(ns(""), start = 0, stop = nchar(ns("")) - 1)

    log_inform(paste("Listening to:", ns(ID$FILTER_STATE_JSON_INPUT)))
    log_inform(paste("Listening to:", ns(ID$SAVED_FILTER_STATE_JSON_MSG_INPUT)))

    shiny::observeEvent(selected_dataset_list(), {
      log_inform(paste0("Send init message to ", ns_id))
      dataset_list_name <- attr(selected_dataset_list(), "dataset_list_name")
      current_dataset_lists <- stats::setNames(list(selected_dataset_list()), dataset_list_name)

      msg <- list(
        id = ns_id,
        dataset_list_name = attr(selected_dataset_list(), "dataset_list_name"),
        dataset_lists_filter_data = serialize_filter_data_to_client_bin64(current_dataset_lists),
        skip_dataset_filters = skip_dataset_filters
      )

      session[["sendCustomMessage"]](
        "init_filter",
        msg
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
          count = as_scalar(nrow(fd[[idx]])),
          name = as_scalar(fd_names[[idx]])
        )
      }

      payload <- list(
        row_count = row_count
      )

      session[["sendCustomMessage"]](
        "update_filter_result",
        list(
          id = ns_id,
          json = toJSON(payload)
        )
      )
    })

    shiny::observeEvent(input[[ID$FILTER_STATE_JSON_INPUT]], {
      log_inform(paste("RECEIVED FILTER", ns(id)))
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
        if (strict) {
          assert(from_filter_validate(json_r), "failed to validate message from filter")
        }
        parsed_json <- deserialize_filter_state_from_client(json_r)
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

binary_serialize_filter_data <- function(x) {
  C <- list(
    MAGICNUM = "FILTDATA",
    VERSION = 1L,
    ENDIANNESS = "little"
  )

  con <- rawConnection(raw(0), open = "wb")
  on.exit(close(con))

  w <- function(x) {
    writeBin(x, con = con, endian = C$ENDIANNESS)
  }

  w_int <- function(x) {
    x <- as.integer(x)
    writeBin(x, con = con, endian = C$ENDIANNESS)
  }

  w_double <- function(x) {
    x <- as.double(x)
    writeBin(x, con = con, endian = C$ENDIANNESS)
  }

  w_doubles <- function(x) {
    x <- as.double(x)
    writeBin(x, con = con, endian = C$ENDIANNESS)
  }

  w_string <- function(x) {
    string_len <- nchar(x) # length of rawToChar
    w_int(string_len + 1)
    writeBin(x, con = con, endian = C$ENDIANNESS)
  }

  w_strings <- function(x) {
    for (s in x) {
      w_string(s)
    }
  }

  w(C$MAGICNUM)
  w_int(C$VERSION)

  dataset_lists <- x[["dataset_lists"]]
  dataset_lists_len <- length(dataset_lists)
  w_int(dataset_lists_len)

  for (dataset_list_idx in seq_len(dataset_lists_len)) {
    dataset_list_name <- dataset_lists[[dataset_list_idx]][["name"]]
    dataset_list <- dataset_lists[[dataset_list_idx]][["dataset_list"]]
    dataset_list_len <- length(dataset_list)
    w_string(dataset_list_name)
    w_int(dataset_list_len)

    for (dataset_idx in seq_len(dataset_list_len)) {
      dataset_name <- dataset_list[[dataset_idx]][["name"]]
      dataset_var <- dataset_list[[dataset_idx]][["variables"]]
      dataset_nrow <- dataset_list[[dataset_idx]][["nrow"]]
      dataset_nvar <- length(dataset_var)
      w_string(dataset_name)
      w_int(dataset_nrow)
      w_int(dataset_nvar)

      for (var_idx in seq_len(dataset_nvar)) {
        var <- dataset_var[[var_idx]]
        kind <- var[["kind"]]
        w_string(var[["name"]])
        w_string(var[["label"]])
        w_string(var[["class"]])
        w_string(kind)
        w_int(var[["NA_count"]])

        if (kind == "categorical") {
          var_value <- var[["value"]]
          var_count <- var[["count"]]
          w_int(length(var_value))
          w_strings(var_value)
          w_int(var_count)
        } else if (kind == "numerical") {
          w_double(var[["min"]])
          w_double(var[["max"]])
          w_int(length(var[["density"]]))
          w_doubles(var[["density"]])
        } else if (kind == "date") {
          w_double(var[["min"]])
          w_double(var[["max"]])
        } else {
          log_warn(paste("Unknown kind", kind))
        }
      }
    }
  }

  buf <- rawConnectionValue(con)

  return(buf)
}

#' @noRd
#' @useDynLib dv.manager
#' @keywords internal
binary_deserialize_filter_data_C <- function(x) {
  .Call("binary_deserialize_filter_data_C", x, PACKAGE = "dv.manager")
}

#' @noRd
#' @useDynLib dv.manager
#' @keywords internal
binary_serialize_filter_data_C <- function(x) {
  .Call("binary_serialize_filter_data_C", x, PACKAGE = "dv.manager")
}

binary_deserialize_filter_data <- function(x) {
  C <- pack_of_constants(
    MAGICNUM = "FILTDATA",
    VERSION = 1L,
    ENDIANNESS = "little"
  )

  con <- rawConnection(x, open = "rb")
  on.exit(close(con))

  r_int <- function() {
    x <- readBin(con = con, what = integer(0), n = 1L, endian = C$ENDIANNESS)
    x
  }

  r_double <- function() {
    x <- readBin(con = con, what = double(0), n = 1L, endian = C$ENDIANNESS)
    x
  }

  r_doubles <- function() {
    doubles_length <- r_int()
    x <- readBin(con = con, what = double(0), n = doubles_length, endian = C$ENDIANNESS)
    x
  }

  r_ints_n <- function(n) {
    x <- readBin(con = con, what = integer(0), n = n, endian = C$ENDIANNESS)
    x
  }

  r_string <- function() {
    r_int() # readBin uses the NULL character at the end of the string
    x <- readBin(con = con, what = character(), n = 1, size = 1, endian = C$ENDIANNESS)
    x
  }

  magic <- readBin(con = con, what = character(0), n = 1, endian = C$ENDIANNESS)
  stopifnot(identical(magic, C$MAGICNUM))

  version <- r_int()
  stopifnot(identical(version, C$VERSION))

  dataset_lists <- list()
  dataset_lists_len <- r_int()

  for (dataset_list_idx in seq_len(dataset_lists_len)) {
    dataset_list <- list()
    dataset_list_name <- r_string()
    dataset_list_len <- r_int()

    for (dataset_idx in seq_len(dataset_list_len)) {
      dataset <- list()
      dataset[["name"]] <- r_string()
      dataset[["nrow"]] <- r_int()
      dataset_nvar <- r_int()
      dataset_var <- list()

      for (var_idx in seq_len(dataset_nvar)) {
        var <- list()
        var[["name"]] <- r_string()
        var[["label"]] <- r_string()
        var[["class"]] <- r_string()
        kind <- r_string()
        var[["kind"]] <- kind
        var[["NA_count"]] <- r_int()

        if (kind == "categorical") {
          value_len <- r_int()
          var[["value"]] <- vector(mode = "character", length = value_len)
          for (idx in seq_len(value_len)) {
            var[["value"]][[idx]] <- r_string()
          }
          var[["count"]] <- r_ints_n(value_len)
        } else if (kind == "numerical") {
          var[["min"]] <- r_double()
          var[["max"]] <- r_double()
          var[["density"]] <- r_doubles()
        } else if (kind == "date") {
          var[["min"]] <- r_double()
          var[["max"]] <- r_double()
        } else {
          log_warn(paste("Unknown kind", kind))
        }
        dataset_var[[var_idx]] <- var
      }
      dataset[["variables"]] <- dataset_var
      dataset_list[[dataset_idx]] <- dataset
    }
    dataset_lists[[dataset_list_idx]] <- list(
      name = dataset_list_name,
      dataset_list = dataset_list
    )
  }

  x <- list(dataset_lists = dataset_lists)

  return(x)
}
