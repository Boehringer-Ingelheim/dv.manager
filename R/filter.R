toJSON <- function(x) unclass(jsonlite::toJSON(x))
fromJSON <- function(x) jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = FALSE)
as_scalar <- jsonlite::unbox
serialize_filter_data_to_client_bin64 <- function(x) {
  fd <- get_filter_data(x)
  bin <- binary_serialize_filter_data_C(fd)
  ..t$add_period("jsonlite::base64_enc(bin)", TRUE)
  enc <- jsonlite::base64_enc(bin)
  ..t$add_period("jsonlite::base64_enc(bin)", FALSE)
  enc
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
    DATASET_LISTS = 1,
    NAME = 1,
    DATASET_LIST = 2,
    LABEL = 2,
    NROW = 3,
    VARIABLES = 4,
    CLASS = 3,
    KIND = 4,
    NA_COUNT = 5,
    VALUE = 6,
    COUNT = 7,
    MIN = 6,
    MAX = 7,
    DENSITY = 8,
    MSG = "msg" # Unused?
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
      class = "FILTER_IS_NA",
      message = "Filter is not ready or has no data"
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

get_single_filter_data <- function(dataset) {
  nm_var <- names(dataset)
  n_var <- length(nm_var)
  res <- vector(mode = "list", length = n_var)

  FDF <- FC$FDF
  K <- FC$KIND

  for (idx in seq_len(n_var)) {
    start <- Sys.time()
    var <- dataset[[idx]]
    # ..t$add_period(nm_var[[idx]], TRUE)

    # Logical is treated as a factor in the client
    if (is.logical(var)) {
      var <- factor(var)
    }

    if (is.character(var) || is.factor(var)) {
      l <- vector(mode = "list", length = 7)
      l[[FDF$NAME]] <- nm_var[[idx]]
      l[[FDF$LABEL]] <- attr(var, "label") %||% l[[FDF$NAME]] # FIXME: This is done to maintain the same behavior as jsonlite. Should be reviewed with the js code that uses labels
      l[[FDF$CLASS]] <- class(var)[[1]]
      l[[FDF$KIND]] <- K$CATEGORICAL

      if (is.character(var)) {
        var <- factor(var)
      }
      C <- count_factor_C(var)
      l[[FDF$NA_COUNT]] <- C[[1]]

      if (length(C) > 1) {
        C <- C[2:length(C)]
        names(C) <- levels(var)
        # C <- sort(C)
        l[[FDF$COUNT]] <- unname(C)
        l[[FDF$VALUE]] <- names(C)
      } else {
        l[[FDF$COUNT]] <- integer(0)
        l[[FDF$VALUE]] <- character(0)
      }
    } else if (is.numeric(var)) {
      l <- vector(mode = "list", length = 8)
      l[[FDF$NAME]] <- nm_var[[idx]]
      l[[FDF$LABEL]] <- attr(var, "label") %||% l[[FDF$NAME]] # FIXME: This is done to maintain the same behavior as jsonlite. Should be reviewed with the js code that uses labels
      l[[FDF$CLASS]] <- class(var)[[1]]

      var <- as.numeric(var)
      max_min_na <- max_min_count_na_C(var)
      l[[FDF$KIND]] <- K$NUMERICAL
      l[[FDF$NA_COUNT]] <- max_min_na[[3]]

      l[[FDF$MIN]] <- max_min_na[[2]]
      l[[FDF$MAX]] <- max_min_na[[1]]

      if (length(var) > 0 && has_finite_C(var) && l[[FDF$NA_COUNT]] != length(var)) {
        hist_info <- graphics::hist(var, plot = FALSE)
      } else {
        hist_info <- list(density = numeric(0))
      }

      l[[FDF$DENSITY]] <- hist_info[["density"]]
    } else if (inherits(var, "POSIXct") || inherits(var, "Date")) {
      l <- vector(mode = "list", length = 7)
      l[[FDF$NAME]] <- nm_var[[idx]]
      l[[FDF$LABEL]] <- attr(var, "label") %||% l[[FDF$NAME]] # FIXME: This is done to maintain the same behavior as jsonlite. Should be reviewed with the js code that uses labels
      l[[FDF$CLASS]] <- class(var)[[1]]

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
      l <- vector(mode = "list", length = 5)
      l[[FDF$NAME]] <- nm_var[[idx]]
      l[[FDF$LABEL]] <- attr(var, "label") %||% l[[FDF$NAME]] # FIXME: This is done to maintain the same behavior as jsonlite. Should be reviewed with the js code that uses labels
      l[[FDF$CLASS]] <- class(var)[[1]]

      l[[FDF$KIND]] <- K$UNKNOWN
      l[[FDF$NA_COUNT]] <- NA_integer_
    }

    res[[idx]] <- l
    # ..t$add_period(nm_var[[idx]], FALSE)
  }
  return(res)
}

get_filter_data <- function(dataset_lists) {
  ..t$add_period("get_filter_data", TRUE)
  on.exit(..t$add_period("get_filter_data", FALSE), add = TRUE)
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
      current_dataset_label <- attr(current_dataset_list[[jdx]], "label") %||% current_dataset_name

      current_dataset_res[[jdx]] <- vector(mode = "list", length = 4)
      current_dataset_res[[jdx]][[FDF$NAME]] <- nm_datasets[[jdx]]
      current_dataset_res[[jdx]][[FDF$LABEL]] <- attr(current_dataset_list[[jdx]], "label") %||% current_dataset_name
      current_dataset_res[[jdx]][[FDF$NROW]] <- nrow(current_dataset)
      ..t$add_period(sprintf("get_single_filter_data (%s)", nm_datasets[[jdx]]), TRUE)
      current_dataset_res[[jdx]][[FDF$VARIABLES]] <- get_single_filter_data(current_dataset)
      ..t$add_period(sprintf("get_single_filter_data (%s)", nm_datasets[[jdx]]), FALSE)
    }

    res[[idx]] <- vector(mode = "list", length = 2)
    res[[idx]][[FDF$NAME]] <- current_dataset_list_name
    res[[idx]][[FDF$DATASET_LIST]] <- current_dataset_res
  }

  return(list(res))
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
    dataset_list_lvls <- rep_len(list(list(lvls = list())), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    for (child in children) {
      processed_element <- process_subject_filter_element(dataset_list, child, sbj_var, complete_subject_list)
      subjects <- union(subjects, processed_element[["subjects"]])

      child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
      for (dataset_name in names(child_dataset_list_lvls)) {
        dataset_lvls <- dataset_list_lvls[[dataset_name]][["lvls"]]
        child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]][["lvls"]]
        relevant_factors <- names(child_dataset_lvls)
        for (fct in relevant_factors) {
          dataset_lvls[[fct]] <- union(dataset_lvls[[fct]], child_dataset_lvls[[fct]])
        }
        dataset_list_lvls[[dataset_name]][["lvls"]] <- dataset_lvls
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
    dataset_list_lvls <- rep_len(list(list(lvls = list())), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    for (child in children) {
      processed_element <- process_subject_filter_element(dataset_list, child, sbj_var, complete_subject_list)
      subjects <- intersect(subjects, processed_element[["subjects"]])

      child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
      for (dataset_name in names(child_dataset_list_lvls)) {
        dataset_lvls <- dataset_list_lvls[[dataset_name]][["lvls"]]
        child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]][["lvls"]]
        relevant_factors <- names(child_dataset_lvls)
        for (fct in relevant_factors) {
          dataset_lvls[[fct]] <- intersect(
            dataset_lvls[[fct]] %||% levels(dataset_list[[dataset_name]][[fct]]),
            child_dataset_lvls[[fct]]
          )
        }
        dataset_list_lvls[[dataset_name]][["lvls"]] <- dataset_lvls
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
    dataset_list_lvls <- rep_len(list(list(lvls = list())), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)

    child_dataset_list_lvls <- processed_element[["dataset_list_lvls"]]
    for (dataset_name in names(child_dataset_list_lvls)) {
      dataset_lvls <- dataset_list_lvls[[dataset_name]][["lvls"]]
      child_dataset_lvls <- child_dataset_list_lvls[[dataset_name]][["lvls"]]
      relevant_factors <- names(child_dataset_lvls)
      for (fct in relevant_factors) {
        dataset_lvls[[fct]] <- setdiff(
          levels(dataset_list[[dataset_name]][[fct]]),
          child_dataset_lvls[[fct]] %||% levels(dataset_list[[dataset_name]][[fct]])
        )
      }
      dataset_list_lvls[[dataset_name]][["lvls"]] <- dataset_lvls
    }

    return(list(subjects = subjects, dataset_list_lvls = dataset_list_lvls))
  }

  actions[[FC$FE$OP$OPERATION]] <- function(dataset_list, filter_element, sbj_var) {
    processed_element <- process_dataset_filter_element(dataset_list, filter_element)
    mask <- processed_element[["mask"]]
    dataset <- processed_element[["dataset"]]
    subjects <- as.character(dataset_list[[dataset]][[sbj_var]][mask])

    dataset_list_lvls <- rep_len(list(list(lvls = list())), length = length(dataset_list))
    names(dataset_list_lvls) <- names(dataset_list)
    dataset_lvls <- processed_element[["lvls"]]
    dataset_name <- processed_element[["dataset"]]
    dataset_list_lvls[[dataset_name]][["lvls"]] <- dataset_lvls

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

create_dataset_filter_info <- function(dataset_list, dataset_filter) {
  # Code partially matches  (cdU0)
  datasets_filter <- as_safe_list(dataset_filter)

  filter_info <- list()

  for (dataset_filter_child in datasets_filter[[FC$FE$F$CHILDREN]]) {
    kind <- dataset_filter_child[[FC$FE$F$KIND]]
    name <- dataset_filter_child[[FC$FE$F$NAME]]
    assert(!(name %in% names(filter_info)), "a dataset can only appear once inside dataset_filters")
    assert(name %in% names(dataset_list), "dataset is not inside dataset_list")
    assert(kind == "dataset", "dataset_filters children can only be of kind `dataset`")
    if (length(dataset_filter_child[[FC$FE$F$CHILDREN]]) == 1) {
      processed_element <- process_dataset_filter_element(dataset_list, dataset_filter_child[["children"]][[1]])
      assert(processed_element[["dataset"]] == name, "Filter on the wrong dataset")
      filter_info[[name]][["mask"]] <- processed_element[["mask"]]
      filter_info[[name]][["lvls"]] <- processed_element[["lvls"]]
    } else if (length(dataset_filter_child[[FC$FE$F$CHILDREN]]) == 0) {
      filter_info[[name]][["mask"]] <- rep_len(TRUE, nrow(dataset_list[[name]]))
      filter_info[[name]][["lvls"]] <- list()
    } else {
      assert(FALSE, "`datasets_filter` cannot contain more than children")
    }
  }

  # Returned this way so it can has the same structure as the subject_filter_info
  return(list(filter_info = filter_info))
}

create_subject_filter_info <- function(dataset_list, subject_filter, subj_var) {
  # Code partially matches  (cdU0)
  subject_filter <- as_safe_list(subject_filter)
  complete_subject_list <- character(0)

  for (current_dataset in dataset_list) {
    complete_subject_list <- union(complete_subject_list, as.character(unique(current_dataset[[subj_var]])))
  }

  subject_filter_info <- list(
    subjects = complete_subject_list,
    filter_info = stats::setNames(
      rep(list(list(mask = NA, lvls = list())), length(dataset_list)),
      names(dataset_list)
    )
  )

  children <- subject_filter[[FC$SFE$F$CHILDREN]]
  assert(length(children) < 2, "subject filter must have 0 or 1 child")

  if (length(children) > 0) {
    subject_filter_info <- process_subject_filter_element(dataset_list, children[[1]], subj_var, complete_subject_list)
  }

  for (current_ds_name in names(dataset_list)) {
    current_mask <- dataset_list[[current_ds_name]][[subj_var]] %in% subject_filter_info[["subjects"]]
    subject_filter_info[["filter_info"]][[current_ds_name]][["mask"]] <- current_mask
  }

  return(subject_filter_info)
}

# Can be expanded to combine arbitrary filters not necessarily
combine_filter_info <- function(filter_info) {
  if (filter_info[["error_list"]]$any()) {
    return(
      list(result = NULL, error_list = filter_info[["error_list"]])
    )
  }

  #TODO: Review filter_info subtrees mess

  subject_filter_info <- filter_info[["result"]][["subject"]]
  dataset_filter_info <- filter_info[["result"]][["dataset"]]

  res <- list()
  res[["subjects"]] <- subject_filter_info[["subjects"]]
  res[["filter_info"]] <- subject_filter_info[["filter_info"]]

  for (dataset_name in union(
    names(subject_filter_info[["filter_info"]]),
    names(dataset_filter_info[["filter_info"]])
  )) {
    if (dataset_name %in% names(subject_filter_info[["filter_info"]])) {
      subject_mask <- subject_filter_info[["filter_info"]][[dataset_name]][["mask"]]
    } else {
      subject_mask <- TRUE
    }

    if (dataset_name %in% names(dataset_filter_info[["filter_info"]])) {
      dataset_mask <- dataset_filter_info[["filter_info"]][[dataset_name]][["mask"]]
    } else {
      dataset_mask <- TRUE
    }

    res_mask <- subject_mask & dataset_mask
    res[["filter_info"]][[dataset_name]][["mask"]] <- res_mask

    subject_lvls <- subject_filter_info[["filter_info"]][[dataset_name]][["lvls"]]
    dataset_lvls <- dataset_filter_info[["filter_info"]][[dataset_name]][["lvls"]]
    res_lvls <- list()
    for (variable in union(names(subject_lvls), names(dataset_lvls))) {
      if (length(names(subject_lvls)) > 0 && variable %in% names(subject_lvls)) {
        var_subject_lvls <- subject_lvls[[variable]]
      } else {
        var_subject_lvls <- dataset_lvls[[variable]]
      }

      if (length(names(dataset_lvls)) > 0 && variable %in% names(dataset_lvls)) {
        var_dataset_lvls <- dataset_lvls[[variable]]
      } else {
        var_dataset_lvls <- subject_lvls[[variable]]
      }

      res_lvls[[variable]] <- intersect(var_subject_lvls, var_dataset_lvls)
    }
    res[["filter_info"]][[dataset_name]][["lvls"]] <- res_lvls
  }

  return(
    list(result = res, error_list = new_error_list())
  )
}

apply_filter_info <- function(dataset_list, dataset_filter_info) {
  filtered_dataset_list <- dataset_list
  filter_info <- as_safe_list(dataset_filter_info[["filter_info"]])
  for (current_dataset_name in names(filter_info)) {
    current_mask <- filter_info[[current_dataset_name]][["mask"]]
    current_lvls <- filter_info[[current_dataset_name]][["lvls"]]
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

match_set_order <- function(reference, values) {
  assert(all(values %in% reference), "All values must be contained in reference")
  aligned <- intersect(reference, values)
  aligned
}

apply_filter_to_dataset_list <- (function(unfiltered_dataset_list, dataset_list_filter, filter_key_var) {
  filter_info <- get_filter_info(unfiltered_dataset_list, dataset_list_filter, filter_key_var)
  combined_filter_info <- combine_filter_info(filter_info)
  filtered_dataset <- apply_filter_info_to_dataset_list(unfiltered_dataset_list, combined_filter_info)
  filtered_dataset
}) |>
  shiny::maskReactiveContext()

apply_filter_info_to_dataset_list <- (function(
  dataset_list,
  filter_info,
  dataset_names = names(dataset_list)
) {
  res <- list(result = NULL, error_list = NULL)
  dataset_list_subset <- as_safe_list(dataset_list)[dataset_names]
  filter_info_subset <- as_safe_list(filter_info)
  filter_info_subset[["result"]][["filter_info"]] <- filter_info_subset[["result"]][["filter_info"]][dataset_names]

  if (filter_info_subset[["error_list"]]$any()) {
    return(list(result = dataset_list_subset, error_list = filter_info_subset[["error_list"]]))
  } else {
    error_list <- new_error_list()
    fd <- dataset_list_subset

    fd <- tryCatch(
      {
        apply_filter_info(dataset_list_subset, filter_info_subset[["result"]])
      },
      error = function(e) {
        error <- FC$ERRORS$GENERIC_FILTER_APPLICATION
        error$message <- paste("Filter not applied. Error found:\n", e[["message"]])
        error_list$push(error)
        dataset_list_subset
      }
    )
    res <- list(result = fd, error_list = error_list)

    return(res)
  }
}) |>
  shiny::maskReactiveContext()

get_filter_info <- (function(unfiltered_dataset_list, dataset_list_filter, filter_key_var) {
  error_list <- new_error_list()
  fi <- NULL

  if (identical(as.character(dataset_list_filter), NA_character_)) {
    error_list$push(FC$ERRORS$FILTER_IS_NA)
  } else if (isTRUE(is.na(dataset_list_filter[["parsed"]]))) {
    fi <- NULL
  } else {
    unfiltered_dataset_list_name <- attr(unfiltered_dataset_list, "dataset_list_name")
    filter_dataset_list_name <- dataset_list_filter[["parsed"]][["dataset_list_name"]]

    if (unfiltered_dataset_list_name != filter_dataset_list_name) {
      error_list$push(FC$ERRORS$UNFILTERED_DATASET_LIST_NAME_FILTER_DATASET_LIST_NAME_MISMATCH)
    } else {
      safe_filters <- dataset_list_filter[["parsed"]][["filters"]]
      fi <- tryCatch(
        {
          dataset_filter_info <- create_dataset_filter_info(unfiltered_dataset_list, safe_filters[["datasets_filter"]])
          subject_filter_info <- create_subject_filter_info(
            unfiltered_dataset_list,
            safe_filters[["subject_filter"]],
            filter_key_var
          )

          list(
            subject = subject_filter_info,
            dataset = dataset_filter_info
          )
        },
        error = function(e) {
          error <- FC$ERRORS$GENERIC_FILTER_APPLICATION
          error$message <- paste("Filter not applied. Error found:\n", e[["message"]])
          error_list$push(error)
          NA
        }
      )
    }
  }

  res <- list(
    result = fi,
    error_list = error_list
  )

  return(res)
}) |>
  shiny::maskReactiveContext()

json_validator <- function(...) jsonvalidate::json_validator(...)

to_filter_validate <- json_validator(
  system.file("to_filter_schema.json", package = "dv.manager", mustWork = TRUE),
  engine = "ajv",
  strict = TRUE
)

from_filter_validate <- json_validator(
  system.file("from_filter_schema.json", package = "dv.manager", mustWork = TRUE),
  engine = "ajv",
  strict = TRUE
)

add_blockly_dependency <- function() {
  htmltools::htmlDependency(
    name = "filter_blockly",
    version = utils::packageVersion("dv.manager"),
    src = app_sys("filter/"),
    script = c("dv_filter_minified.js")
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
  after_filter_info,
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
    ns_id <- ns(character(0))

    log_inform(paste("Listening to:", ns(ID$FILTER_STATE_JSON_INPUT)))
    log_inform(paste("Listening to:", ns(ID$SAVED_FILTER_STATE_JSON_MSG_INPUT)))

    shiny::observeEvent(selected_dataset_list(), {
      # Not convinced as it is removed somewhere else (app_server) (gvbu)
      session[["sendCustomMessage"]]("dv_manager_show_overlay", list(message = "Setting up filter"))

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

    shiny::observeEvent(after_filter_info(), {
      shiny::req(!is.null(after_filter_info()))

      r_after_filter_info <- after_filter_info()[["filter_info"]][["result"]][["filter_info"]]
      r_after_filter_info_names <- names(r_after_filter_info)

      row_count <- vector("list", length = length(r_after_filter_info))

      for (idx in seq_along(r_after_filter_info)) {
        row_count[[idx]] <- list(
          count = as_scalar(sum(r_after_filter_info[[idx]][["mask"]])),
          name = as_scalar(r_after_filter_info_names[[idx]])
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
      start <- Sys.time()
      json_r <- input[[ID$FILTER_STATE_JSON_INPUT]]

      if (checkmate::test_string(json_r, min.chars = 1)) {
        if (strict) {
          assert(from_filter_validate(json_r), "failed to validate message from filter")
        }
        parsed_json <- deserialize_filter_state_from_client(json_r)
        log_inform("PROCESSING FILTER PARSED")
        res <- list(
          parsed = parsed_json %||% NA_character_,
          raw = json_r
        )
      } else {
        log_inform("PROCESSING FILTER NA")
        res <- NA_character_
      }
      log_inform(paste("Processing filter: ", Sys.time() - start))
      res
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

  FDF <- FC$FDF
  K <- FC$KIND

  dataset_lists <- x[[FDF$DATASET_LISTS]]
  dataset_lists_len <- length(dataset_lists)
  w_int(dataset_lists_len)

  for (dataset_list_idx in seq_len(dataset_lists_len)) {
    dataset_list_name <- dataset_lists[[dataset_list_idx]][[FDF$NAME]]
    dataset_list <- dataset_lists[[dataset_list_idx]][[FDF$DATASET_LIST]]
    dataset_list_len <- length(dataset_list)
    w_string(dataset_list_name)
    w_int(dataset_list_len)

    for (dataset_idx in seq_len(dataset_list_len)) {
      dataset_name <- dataset_list[[dataset_idx]][[FDF$NAME]]
      dataset_label <- dataset_list[[dataset_idx]][[FDF$LABEL]]
      dataset_var <- dataset_list[[dataset_idx]][[FDF$VARIABLES]]
      dataset_nrow <- dataset_list[[dataset_idx]][[FDF$NROW]]
      dataset_nvar <- length(dataset_var)
      w_string(dataset_name)
      w_string(dataset_label)
      w_int(dataset_nrow)
      w_int(dataset_nvar)

      for (var_idx in seq_len(dataset_nvar)) {
        var <- dataset_var[[var_idx]]
        kind <- var[[FDF$KIND]]
        w_string(var[[FDF$NAME]])
        w_string(var[[FDF$LABEL]])
        w_string(var[[FDF$CLASS]])
        w_string(kind)
        w_int(var[[FDF$NA_COUNT]])

        if (kind == K$CATEGORICAL) {
          var_value <- var[[FDF$VALUE]]
          var_count <- var[[FDF$COUNT]]
          w_int(length(var_value))
          w_strings(var_value)
          w_int(var_count)
        } else if (kind == K$NUMERICAL) {
          w_double(var[[FDF$MIN]])
          w_double(var[[FDF$MAX]])
          w_int(length(var[[FDF$DENSITY]]))
          w_doubles(var[[FDF$DENSITY]])
        } else if (kind == K$DATE) {
          w_double(var[[FDF$MIN]])
          w_double(var[[FDF$MAX]])
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
  ..t$add_period("binary_serialize_filter_data_C", TRUE)
  on.exit(..t$add_period("binary_serialize_filter_data_C", FALSE), add = TRUE)
  .Call("binary_serialize_filter_data_C", x, PACKAGE = "dv.manager")
}

binary_deserialize_filter_data <- function(x) {
  C <- pack_of_constants(
    MAGICNUM = "FILTDATA",
    VERSION = 1L,
    ENDIANNESS = "little"
  )

  FDF <- FC$FDF
  K <- FC$KIND

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
      dataset[[FDF$NAME]] <- r_string()
      dataset[[FDF$LABEL]] <- r_string()
      dataset[[FDF$NROW]] <- r_int()
      dataset_nvar <- r_int()
      dataset_var <- list()

      for (var_idx in seq_len(dataset_nvar)) {
        var <- list()
        var[[FDF$NAME]] <- r_string()
        var[[FDF$LABEL]] <- r_string()
        var[[FDF$CLASS]] <- r_string()
        kind <- r_string()
        var[[FDF$KIND]] <- kind
        var[[FDF$NA_COUNT]] <- r_int()

        if (kind == K$CATEGORICAL) {
          value_len <- r_int()
          var[[FDF$VALUE]] <- vector(mode = "character", length = value_len)
          for (idx in seq_len(value_len)) {
            var[[FDF$VALUE]][[idx]] <- r_string()
          }
          var[[FDF$COUNT]] <- r_ints_n(value_len)
        } else if (kind == K$NUMERICAL) {
          var[[FDF$MIN]] <- r_double()
          var[[FDF$MAX]] <- r_double()
          var[[FDF$DENSITY]] <- r_doubles()
        } else if (kind == K$DATE) {
          var[[FDF$MIN]] <- r_double()
          var[[FDF$MAX]] <- r_double()
        } else {
          log_warn(paste("Unknown kind", kind))
        }
        dataset_var[[var_idx]] <- var
      }
      dataset[[FDF$VARIABLES]] <- dataset_var
      dataset_list[[dataset_idx]] <- dataset
    }
    dataset_lists[[dataset_list_idx]] <- list(dataset_list_name, dataset_list)
  }

  x <- list(dataset_lists)

  return(x)
}

#' @noRd
#' @useDynLib dv.manager
#' @keywords internal
has_finite_C <- function(x) {
  .Call("has_finite_C", x, PACKAGE = "dv.manager")
}

#' @noRd
#' @useDynLib dv.manager
#' @keywords internal
count_factor_C <- function(x) {
  .Call("count_factor_C", x, PACKAGE = "dv.manager")
}
#' @noRd
#' @useDynLib dv.manager
#' @keywords internal
max_min_count_na_C <- function(x) {
  .Call("max_min_count_na_C", x, PACKAGE = "dv.manager")
}

#' Get Filtered Data
#'
#' @description
#' Function that applies filter masks and column selections to a named list of
#' datasets. For each requested dataset it: applies the pre-computed boolean mask from
#' `filter_info`, optionally combines it with caller-supplied extra masks, restricts columns to
#' those listed in `dataset_vars`, restores factor levels according to the filter's level
#' information, and copies variable labels back onto the result.
#'
#' @param unfiltered_dataset_list A named list of `data.frame` objects representing the original,
#'   unfiltered datasets.
#' @param filter_info A named list produced by the filtering machinery. Must contain the element
#'   `result$filter_info`, where each entry is keyed by dataset name and holds:
#'   \describe{
#'     \item{`mask`}{A logical vector with one element per row of the corresponding dataset.}
#'     \item{`lvls`}{A named list mapping factor-variable names to the levels that should be
#'       retained after filtering.}
#'   }
#' @param dataset_names A character vector of dataset names to process. Must be a subset of
#'   `names(unfiltered_dataset_list)`. Defaults to all datasets in `unfiltered_dataset_list`.
#' @param dataset_vars A named list of character vectors specifying which columns to keep for each
#'   dataset. Names must be a subset of `names(unfiltered_dataset_list)`. Defaults to all columns
#'   of each dataset.
#' @param dataset_extra_masks A named list of additional logical vectors (one per dataset) that are
#'   AND-combined with the mask from `filter_info`. Defaults to an empty list (no extra masking).
#'
#' @return A named list of filtered `data.frame` objects, one per entry in `dataset_names`, with
#'   rows, columns, factor levels, and variable labels all reconciled against the original data.
#'
get_filtered_dataset_list_ <- function(
  unfiltered_dataset_list,
  filter_info,
  dataset_names,
  dataset_vars,
  dataset_extra_masks
) {
  if (missing(dataset_names)) {
    dataset_names <- names(unfiltered_dataset_list)
  }

  if (missing(dataset_vars)) {
    dataset_vars <- lapply(dataset_names, function(x) {
      colnames(unfiltered_dataset_list[[x]])
    })
    names(dataset_vars) <- dataset_names
  }

  if (missing(dataset_extra_masks)) {
    dataset_extra_masks <- list()
  }

  checkmate::assert_subset(dataset_names, names(unfiltered_dataset_list))
  checkmate::assert_list(dataset_vars, names = "unique")
  checkmate::assert_list(dataset_extra_masks, names = "unique")
  checkmate::assert_subset(names(dataset_vars), names(unfiltered_dataset_list))
  checkmate::assert_subset(names(dataset_extra_masks), names(unfiltered_dataset_list))

  res <- vector(mode = "list", length = length(dataset_names))
  names(res) <- dataset_names

  for (ds_idx in seq_along(dataset_names)) {
    ds_name <- dataset_names[[ds_idx]]
    unfiltered_dataset <- unfiltered_dataset_list[[ds_name]]
    lbls <- get_lbls(unfiltered_dataset)

    ds_lvl <- filter_info[["result"]][["filter_info"]][[ds_name]][["lvls"]]

    ds_mask <- local({
      mask <- filter_info[["result"]][["filter_info"]][[ds_name]][["mask"]]
      if (ds_name %in% names(dataset_extra_masks)) {
        mask <- mask & dataset_extra_masks[[ds_name]]
      }
      mask
    })

    ds_columns <- local({
      if (ds_name %in% names(dataset_vars)) {
        dataset_vars[[ds_name]]
      } else {
        colnames(unfiltered_dataset)
      }
    })

    checkmate::assert_subset(dataset_vars[[ds_name]], names(unfiltered_dataset))

    # Depending on the type of subsetting unrequired extra copies can be made because:
    # (Allocation is always done because we are assigning but are only concerned about the `data` itself not the `pointer` to the data)
    # We have to be careful with row indexing that always triggers copy behavior
    # ds <- data.frame(a = 1:2, b = 3:4, c = 5:6)
    # full_copy <- ds[c(TRUE, TRUE), c("a", "b", "c"), drop = FALSE] # <--- DANGEROUS, copy but all  is the same
    # no_copy <- ds[, c("a", "c"), drop = FALSE] # Takes columns as is but the address is the same
    # required_copy <- ds[c(TRUE, FALSE), c("a", "c"), drop = FALSE] # Copy of all columns is required as the number of rows varies
    # lobstr::ref(ds, full_copy, no_copy, required_copy)

    if (all(ds_mask)) {
      filtered_dataset <- unfiltered_dataset_list[[ds_name]][, ds_columns, drop = FALSE]
    } else {
      # Fastest option:
      # microbenchmark::microbenchmark(iris[mask, cols, drop = FALSE], iris[mask,,drop = FALSE][cols], iris[cols][mask,,drop = FALSE], times = 1e4)
      filtered_dataset <- unfiltered_dataset_list[[ds_name]][ds_mask, ds_columns, drop = FALSE]
      filtered_dataset <- apply_lvls_info_to_ds(unfiltered_dataset, filtered_dataset, ds_lvl)
      filtered_dataset <- copy_labels_from_dataset(unfiltered_dataset, filtered_dataset)
    }

    res[[ds_name]] <- filtered_dataset
  }
  res
}

#' Get Filtered Data
#'
#' @description
#' Wrapper around [get_filtered_dataset_list_()] that accepts the combined`unfiltered_plus_filter_info``.
#'
#' @param unfiltered_plus_filter_info A named list with two elements:
#'   \describe{
#'     \item{`unfiltered_dataset_list`}{A named list of unfiltered `data.frame` objects.}
#'     \item{`filter_info`}{The filter-info structure described in [get_filtered_dataset_list_()].}
#'   }
#' @param dataset_names See [get_filtered_dataset_list_()].
#' @param dataset_vars See [get_filtered_dataset_list_()].
#' @param dataset_extra_masks See [get_filtered_dataset_list_()].
#'
#' @return A named list of filtered `data.frame` objects. See [get_filtered_dataset_list_()] for details.
#'
#' @export
get_filtered_dataset_list <- function(unfiltered_plus_filter_info, dataset_names, dataset_vars, dataset_extra_masks) {
  unfiltered_dataset_list <- as_safe_list(unfiltered_plus_filter_info[["unfiltered_dataset_list"]])
  filter_info <- as_safe_list(unfiltered_plus_filter_info[["filter_info"]])
  get_filtered_dataset_list_(unfiltered_dataset_list, filter_info, dataset_names, dataset_vars, dataset_extra_masks)
}

#' Apply Level Information to a Filtered Dataset
#'
#' @description
#' Reconciles factor levels in a filtered dataset against the level information captured at filter
#' time. After row-filtering, factor levels that were present in the original data may be lost;
#' conversely, OR-type filter operations can re-introduce rows whose levels would otherwise have
#' been dropped.
#'
#' Only variables that appear in both the filtered dataset **and** `ds_lvl` are modified; all other
#' columns are left untouched.
#'
#' Level order is respected
#'
#' @param unfiltered_dataset The original `data.frame` before any row filtering, used as source for the full set
#'  of factor levels and their order.
#' @param filtered_dataset The `data.frame` after row and column filtering whose factor levels need
#'   to be reconciled.
#' @param ds_lvl A namedlist mapping factor-variable names to the character vectors of levels.
#'
#' @return `filtered_dataset` with factor levels of the relevant variables.
#'
#' @keywords internal
apply_lvls_info_to_ds <- function(unfiltered_dataset, filtered_dataset, ds_lvl) {
  ds_names <- names(filtered_dataset)
  ds_lvl_names <- names(ds_lvl)
  candidate_vars <- intersect(ds_names, ds_lvl_names)
  for (var_idx in seq_along(candidate_vars)) {
    var_name <- candidate_vars[[var_idx]]

    unfiltered_var <- unfiltered_dataset[[var_name]]
    all_possible_lvls <- levels(unfiltered_var)

    filtered_var <- filtered_dataset[[var_name]]
    present_lvls <- levels(droplevels(filtered_var))

    # Applying lvls have the following side case
    # - A factor level may be filtered out using a filter, therefore the lvl should be dropped
    # BUT another filter with an operation, or, may reintroduce a row with a lvl that is supposed to be dropped
    # Therefore we force all levels present in the variable to not be dropped
    new_lvls <- union(present_lvls, ds_lvl[[var_name]])

    new_lvls <- match_set_order(all_possible_lvls, new_lvls)
    filtered_dataset[[var_name]] <- factor(filtered_dataset[[var_name]], new_lvls)
  }
  filtered_dataset
}
