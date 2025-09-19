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
    label <- attr(var, "label") %||% name #FIXME: This is done to maintain the same behavior as jsonlite. Should be reviewed with the js code that uses labels

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

# nolint start cyclocomp_linter
process_dataset_filter_element <- function(dataset_list, filter_element) { # TODO: replace dataset for dataset_name

  filter_element <- as_safe_list(filter_element)

  kind <- filter_element[["kind"]]

  if (kind == "row_operation") {
    operation <- filter_element[["operation"]]
    filter_dataset <- NA
    if (operation == "and") {
      assert(length(filter_element[["children"]]) >= 1, "`and` operation requires at least one child")
      mask <- TRUE # Neutral element for &
      for (child in filter_element[["children"]]) {
        processed_element <- process_dataset_filter_element(dataset_list, child)
        mask <- mask & processed_element[["mask"]]
        if (is.na(filter_dataset)) {
          filter_dataset <- processed_element[["dataset"]]
        } else {
          assert(processed_element[["dataset"]] == filter_dataset, "Filtering on the wrong dataset")
        }
      }
    } else if (operation == "or") {
      assert(length(filter_element[["children"]]) >= 1, "`or` operation requires at least one child")

      mask <- FALSE # Neutral element for |
      for (child in filter_element[["children"]]) {
        processed_element <- process_dataset_filter_element(dataset_list, child)
        mask <- mask | processed_element[["mask"]]
        if (is.na(filter_dataset)) {
          filter_dataset <- processed_element[["dataset"]]
        } else {
          assert(processed_element[["dataset"]] == filter_dataset, "Filtering on the wrong dataset")
        }
      }
    } else if (operation == "not") {
      assert(length(filter_element[["children"]]) == 1, "`not` operation requires exactly one child")
      child <- filter_element[["children"]][[1]]
      processed_element <- process_dataset_filter_element(dataset_list, child)
      mask <- !processed_element[["mask"]]
      filter_dataset <- processed_element[["dataset"]]
    } else {
      stop(paste0("Operation unknown: `", operation, "`"))
    }
  } else if (kind == "filter") {
    variable <- filter_element[["variable"]]
    operation <- filter_element[["operation"]]
    include_NA <- filter_element[["include_NA"]]
    filter_dataset <- filter_element[["dataset"]] # TODO: Change for name table
    assert(variable %in% names(dataset_list[[filter_dataset]]), sprintf("data[['%s']] does not contain col `%s`", filter_dataset, if(is.null(variable)) "NULL" else variable))
    variable_values <- dataset_list[[filter_dataset]][[variable]]

    if (operation == "select_subset") {
      # Logical are treated as factors
      if (is.logical(variable_values)) variable_values <- factor(variable_values)

      variable <- filter_element[["variable"]]
      include_NA <- filter_element[["include_NA"]]
      values <- filter_element[["values"]]
      mask <- (variable_values %in% values) | (is.na(variable_values) & include_NA)
    } else if (operation == "select_range") {
      max <- filter_element[["max"]]
      min <- filter_element[["min"]]
      assert(is.numeric(variable_values), "Field values must be numerical")
      assert(is.numeric(min) && is.numeric(max), "Max and min must be numerical")
      assert(min <= max, "min <= max")
      mask <- (((variable_values <= max) & (variable_values >= min)) & !is.na(variable_values)) | (is.na(variable_values) & include_NA)
    } else if (operation == "select_date") {
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
    } else {
      stop(paste0("Operation unknown: `", operation, "`"))
    }
  } else {
    stop(paste("Unknown kind: ", kind))
  }

  return(list(mask = mask, dataset = filter_dataset))
}
# nolint end cyclocomp_linter

create_dataset_filter_masks <- function(dataset_list, filter_state) {
  datasets_filter <- as_safe_list(filter_state)

  dataset_masks <- list()

  for (child in datasets_filter[["children"]]) {
    kind <- child[["kind"]]
    name <- child[["name"]]
    assert(!(name %in% names(dataset_masks)), "a dataset can only appear once inside dataset_filters")
    assert(name %in% names(dataset_list), "dataset is not inside dataset_list")
    assert(kind == "dataset", "dataset_filters children can only be of kind `dataset`")
    if (length(child[["children"]]) == 1) {
      processed_element <- process_dataset_filter_element(dataset_list, child[["children"]][[1]])
      assert(processed_element[["dataset"]] == name, "Filter on the wrong dataset")
      dataset_masks[[name]] <- processed_element[["mask"]]
    } else if (length(child[["children"]]) == 0) {
      dataset_masks[[name]] <- rep_len(TRUE, nrow(dataset_list[[name]]))
    } else {
      assert(FALSE, "`datasets_filter` cannot contain more than children")
    }
  }

  return(dataset_masks)
}

apply_dataset_filter_masks <- function(dataset_list, mask_list) {
  filtered_data_list <- dataset_list
  for (current_mask_name in names(mask_list)) {
    current_mask <- mask_list[[current_mask_name]]
    current_dataset <- filtered_data_list[[current_mask_name]]
    lbls <- get_lbls(current_dataset)
    filtered_dataset <- current_dataset[current_mask, , drop = FALSE]
    filtered_data_list[[current_mask_name]] <- set_lbls(filtered_dataset, lbls)
  }
  return(filtered_data_list)
}

apply_subject_set <- function(dataset_list, subject_set, subj_var) {
  filtered_data_list <- dataset_list
  for (current_ds_name in names(dataset_list)) {
    current_mask <- dataset_list[[current_ds_name]][[subj_var]] %in% subject_set
    current_dataset <- filtered_data_list[[current_ds_name]]
    lbls <- get_lbls(current_dataset)
    filtered_dataset <- current_dataset[current_mask, , drop = FALSE]
    filtered_data_list[[current_ds_name]] <- set_lbls(filtered_dataset, lbls)
  }
  return(filtered_data_list)
}

create_subject_set <- function(dataset_list, subject_filter, sbj_var) {
  subject_filter <- as_safe_list(subject_filter)
  complete_subject_list <- character(0)
  for (current_data in dataset_list) {
    complete_subject_list <- union(complete_subject_list, as.character(unique(current_data[[sbj_var]])))
  }
  children <- subject_filter[["children"]]
  assert(length(children) < 2, "subject filter must have 0 or 1 child")
  if (length(children) == 0) subjects <- return(complete_subject_list)
  if (length(children) == 1) {
    subjects <- process_subject_filter_element(
      dataset_list, children[[1]],
      sbj_var, complete_subject_list
    )
  }
  return(subjects)
}

process_subject_filter_element <- function(dataset_list, filter_element, sbj_var, complete_subject_list) {
  filter_element <- as_safe_list(filter_element)

  kind <- filter_element[["kind"]]

  if (kind == "set_operation") {
    operation <- filter_element[["operation"]]
    if (operation == "union") {
      children <- filter_element[["children"]]
      subjects <- character(0)
      assert(length(children) > 0, "`union` operation requires at least one child")
      for (child in children) {
        current_subjects <- process_subject_filter_element(dataset_list, child, sbj_var, complete_subject_list)
        subjects <- union(subjects, current_subjects)
      }
    } else if (operation == "intersect") {
      children <- filter_element[["children"]]
      subjects <- complete_subject_list
      assert(length(children) > 0, "`intersect` operation requires at least one child")
      for (child in children) {
        current_subjects <- process_subject_filter_element(dataset_list, child, sbj_var, complete_subject_list)
        subjects <- intersect(subjects, current_subjects)
      }
    } else if (operation == "complement") {
      children <- filter_element[["children"]]
      assert(length(children) == 1, "`complement` operation requires exactly one child")
      subjects <- setdiff(
        complete_subject_list,
        process_subject_filter_element(
          dataset_list, children[[1]],
          sbj_var, complete_subject_list
        )
      )
    } else {
      stop(paste("Unknown operation: ", operation))
    }
  } else if (kind == "filter" || kind == "row_operation") {
    # redirect but do not process
    processed_element <- process_dataset_filter_element(dataset_list, filter_element)
    mask <- processed_element[["mask"]]
    dataset <- processed_element[["dataset"]]
    subjects <- as.character(dataset_list[[dataset]][[sbj_var]][mask])
  } else {
    stop(paste("Unknown kind: ", kind))
  }
  return(subjects)
}

to_filter_validate <- function(x) {
  res <- jsonvalidate::json_validate(
    x,
    system.file("to_filter_schema.json", package = "dv.manager", mustWork = TRUE),
    engine = "ajv",
    strict = TRUE,
    verbose = TRUE
  )
  res
}

from_filter_validate <- function(x) {
  jsonvalidate::json_validate(
    x,
    system.file("from_filter_schema.json", package = "dv.manager", mustWork = TRUE),
    engine = "ajv",
    strict = TRUE,
    verbose = TRUE
  )
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

new_filter_ui <- function(id, dataset_lists, subject_dataset_name, state = NULL) {
  ns <- shiny::NS(id)

  if (!is.null(state)) {
    if (file.exists(state)) {
      state <- paste0(readLines(state), collapse = "\n")
    }
  } else {
    state <- "null" # Acts as a no filter JSON
  }

  filter_bookmark <- shiny::restoreInput(ns(ID$FILTER_JSON_INPUT), state)
  d <- get_filter_data(dataset_lists)

  filter_data <- yyjsonr::write_json_str(d) # FIXME: This is SLOOOOOOOOOOOO...OW it is the main bottleneck when starting the app (filter-wise)

  assert(to_filter_validate(filter_data), "failed to validate message to filter")

  init_tag <- shiny::tags[["script"]](
    shiny::HTML(
      sprintf(
        "dv_filter.init('%s', %s, %s, '%s', '%s', '%s', '%s')",
        ns(ID$FILTER_CONTAINER),
        filter_data,
        filter_bookmark,
        subject_dataset_name,
        ns(ID$FILTER_JSON_INPUT),
        ns(ID$EXPORT_CODE_INPUT),
        ns(ID$FILTER_LOG_INPUT)
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
        shiny:::ionRangeSliderDependency(),
        shiny:::datePickerDependency(),
        # When attaching the dependencies on my own an error occurs when using multiple
        # When including an input perse the error disappears, this should be explored
        shinyWidgets::pickerInput(ns("IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES"), choices = c("A", "B"), multiple = TRUE)
      )
    )
  )


  combined_ui <- local({
    tag_dv_filter_wrapper(
      dependencies,
      tag_dv_filter_root(
        id = ns(ID$FILTER_CONTAINER),
        class = "c-well",
        init_tag
      )
    )
  })

  combined_ui
}

new_filter_server <- function(id, selected_dataset_list_name, subject_filter_dataset_name, after_filter_dataset_list, strict = FALSE) {
  mod <- function(input, output, session) {
    shiny::setBookmarkExclude("IGNORE_INPUT_REQUIRED_FOR_DEPENDENCIES")
    ns <- session[["ns"]]

    log_inform(paste("Listening to:", ns(ID$FILTER_JSON_INPUT)))

    shiny::observeEvent(selected_dataset_list_name(), {
      session[["sendCustomMessage"]](
        "init_filter",
        list(
          dataset_list_name = selected_dataset_list_name(),
          simple = list(
            subject_filter_dataset_name = subject_filter_dataset_name
          ),
          datasets = list(
          ),
          blockly = list(
            container_id = ns(ID$BLOCKLY$INNER_CONTAINER),
            gen_code_button_id = ns(ID$BLOCKLY$GEN_CODE)
          ),
          filter_container_id = ns(ID$FILTER_CONTAINER),
          json_input_id = ns(ID$FILTER_JSON_INPUT),
          log_input_id = ns(ID$FILTER_LOG_INPUT)
        )
      )
    })

    shiny::observeEvent(after_filter_dataset_list(), {
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

    shiny::observeEvent(input[[ID$FILTER_JSON_INPUT]], {
      log_inform("RECEIVED FILTER")
    })

    shiny::observeEvent(input[[ID$FILTER_LOG_INPUT]], {
      for (msg in input[[ID$FILTER_LOG_INPUT]]) {
        shiny::showNotification(msg, type = "warn", duration = NULL)
      }
    })

    res <- shiny::reactive({
      log_inform("PROCESSING FILTER")
      json_r <- input[[ID$FILTER_JSON_INPUT]]

      if (checkmate::test_string(json_r, min.chars = 1)) {
        val_res <- from_filter_validate(json_r)
        if (strict) assert(val_res, "failed to validate message from filter")
        parsed_json <- yyjsonr::read_json_str(json_r, obj_of_arrs_to_df = FALSE, arr_of_objs_to_df = FALSE, num_specials = "special")
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
        if (!checkmate::test_string(input[[ID$FILTER_JSON_INPUT]])) {
          data <- "null"
          shiny::showNotification("Empty filter exported. Please apply filter before exporting.", type = "warn")
        } else {
          data <- jsonlite::prettify(input[[ID$FILTER_JSON_INPUT]])
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
      structure(res, raw = shiny::reactive(input[[ID$FILTER_JSON_INPUT]]))
    )
  }
  shiny::moduleServer(id, mod)
}

mock_new_filter <- function(data = list(
                              "D1" = list(
                                adsl = get_pharmaverse_data("adsl"),
                                adae = get_pharmaverse_data("adae")
                              ),
                              "D2" = list(
                                adsl = get_pharmaverse_data("adsl"),
                                adae = get_pharmaverse_data("adae")
                              )
                            ),
                            filter_state = NULL) {
  ui <- function(request) {
    shiny::fluidPage(
      shiny::bookmarkButton(),
      shiny::div(
        new_filter_ui(ID$FILTER, data, state = filter_state)[["combined_ui"]],
        style = "height: 400px"
      ),
      shiny::verbatimTextOutput("raw_json"),
      DT::dataTableOutput("validate_json"),
      shiny::verbatimTextOutput("output_filtered_ds"),
      shiny::verbatimTextOutput("output_filtered_sbj")
    )
  }

  server <- function(input, output, session) {
    selected_data <- "D1"
    x <- new_filter_server(ID$FILTER_JSON_INPUT, selected_dataset = shiny::reactive(selected_data), strict = TRUE)

    output[["raw_json"]] <- shiny::renderPrint({
      json <- x()[["raw"]]
      shiny::req(json)
      jsonlite::prettify(json)
    })

    output[["validate_json"]] <- DT::renderDataTable({
      json <- x()[["raw"]]
      shiny::req(json)
      e <- attr(from_filter_validate(json), "error") |> tibble::as_tibble()
      e
    })

    output[["output_json"]] <- shiny::renderPrint({
      shiny::req(!is.na(x()[["parsed"]]))
      yyjsonr::read_json_str(x()[["parsed"]])
    })

    filtered_datasets <- shiny::reactive({
      shiny::req(!is.na(x()[["parsed"]]))
      ds <- data[[selected_data]]
      mask <- create_dataset_filter_masks(ds, x()[["parsed"]][["filters"]][["datasets_filter"]])
      apply_dataset_filter_masks(ds, mask)
    })

    filtered_subjects <- shiny::reactive({
      shiny::req(!is.na(x()[["parsed"]]))
      ds <- data[[selected_data]]
      subject_set <- create_subject_set(ds, x()[["parsed"]][["filters"]][["subject_filter"]], "USUBJID")
      if (identical(subject_set, NA_character_)) {
        ds
      } else {
        apply_subject_set(ds, subject_set, "USUBJID")
      }
    })

    output[["output_filtered_ds"]] <- shiny::renderPrint({
      filtered_datasets()
    })

    output[["output_filtered_sbj"]] <- shiny::renderPrint({
      filtered_subjects()
    })

    output[["output_ds"]] <- shiny::renderPrint({
      x()
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url"
  )
}
