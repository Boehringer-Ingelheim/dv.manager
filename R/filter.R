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

get_single_filter_data <- function(data) {
  nm_col <- names(data)
  n_col <- length(nm_col)
  res <- vector(mode = "list", length = n_col)

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

  for (idx in seq_len(n_col)) {
    name <- nm_col[[idx]]
    col <- data[[name]]

    l <- list(
      name = jsonlite::unbox(name),
      label = jsonlite::unbox(attr(col, "label"))
    )

    # Logical is treated as a factor in the client
    if (is.logical(col)) col <- factor(col)

    if (is.character(col) || is.factor(col)) {
      l[["kind"]] <- jsonlite::unbox("categorical")
      l[["NA_count"]] <- jsonlite::unbox(sum(is.na(col)))
      na_clean_col <- col[!is.na(col)]

      count <- table(na_clean_col)
      values <- names(count)
      count <- unname(count)
      l[["values_count"]] <- vector(mode = "list", length = length(l[["values"]]))
      for (v_idx in seq_along(values)) {
        l[["values_count"]][[v_idx]] <- list(
          value = jsonlite::unbox(values[[v_idx]]),
          count = jsonlite::unbox(count[[v_idx]])
        )
      }
    } else if (is.numeric(col)) {
      l[["kind"]] <- jsonlite::unbox("numerical")
      l[["NA_count"]] <- jsonlite::unbox(sum(is.na(col)))
      na_clean_col <- col[!is.na(col)]

      l[["min"]] <- jsonlite::unbox(min(Inf, na_clean_col, na.rm = TRUE))
      l[["max"]] <- jsonlite::unbox(max(-Inf, na_clean_col, na.rm = TRUE))
    } else if (inherits(col, "POSIXct") || inherits(col, "Date")) {
      if (inherits(col, "POSIXct")) {
        col <- as.Date(col)
      }
      l[["kind"]] <- jsonlite::unbox("date")
      l[["NA_count"]] <- jsonlite::unbox(sum(is.na(col)))
      na_clean_col <- col[!is.na(col)]

      l[["min"]] <- jsonlite::unbox(min(as.Date(Inf), na_clean_col, na.rm = TRUE))
      l[["max"]] <- jsonlite::unbox(max(as.Date(-Inf), na_clean_col, na.rm = TRUE))
    } else {
      stop(paste("column type unsupported:", typeof(col)))
    }

    res[[idx]] <- l
  }
  return(res)
}

get_filter_data <- function(datasets) {
  nm_dataset <- names(datasets)
  n_dataset <- length(nm_dataset)
  res <- vector(mode = "list", length = n_dataset)
  for (idx in seq_len(n_dataset)) {
    current_dataset <- datasets[[idx]]
    nm_tables <- names(current_dataset)
    n_tables <- length(nm_tables)
    current_dataset_res <- vector(mode = "list", length = n_tables)
    for (jdx in seq_len(n_tables)) {
      current_table <- current_dataset[[jdx]]
      current_dataset_res[[jdx]] <- list(
        name = jsonlite::unbox(nm_tables[[jdx]]),
        fields = get_single_filter_data(current_table)
      )
    }
    res[[idx]] <- list(
      name = jsonlite::unbox(nm_dataset[[idx]]),
      tables = current_dataset_res
    )
  }
  res <- list(datasets = res)
  return(res)
}

# nolint start cyclocomp_linter
process_dataset_filter_element <- function(data_list, element, current_table_name = NULL) { # TODO: replace dataset for dataset_name

  element <- as_safe_list(element)

  kind <- element[["kind"]]

  if (kind == "filter_operation") {
    operation <- element[["operation"]]
    if (operation == "and") {
      assert(length(element[["children"]]) >= 1, "`and` operation requires at least one child")
      mask <- TRUE # Neutral element for &
      for (child in element[["children"]]) {
        mask <- mask & process_dataset_filter_element(data_list, child, current_table_name)
      }
    } else if (operation == "or") {
      assert(length(element[["children"]]) >= 1, "`or` operation requires at least one child")

      mask <- FALSE # Neutral element for |
      for (child in element[["children"]]) {
        mask <- mask | process_dataset_filter_element(data_list, child, current_table_name)
      }
    } else if (operation == "not") {
      assert(length(element[["children"]]) == 1, "`not` operation requires exactly one child")
      mask <- !process_dataset_filter_element(data_list, element[["children"]][[1]], current_table_name)
    } else {
      stop(paste0("Operation unknown: `", operation, "`"))
    }
  } else if (kind == "filter") {
    field <- element[["field"]]
    operation <- element[["operation"]]
    include_NA <- element[["include_NA"]]
    filter_dataset <- element[["dataset"]] # TODO: Change for name table
    assert(is.null(current_table_name) || current_table_name == filter_dataset, "Filtering on the wrong dataset")
    assert(field %in% names(data_list[[filter_dataset]]), sprintf("data[['%s']] does not contain col `%s`", filter_dataset, field))

    field_values <- data_list[[filter_dataset]][[field]]

    if (operation == "select_subset") {
      # Logical are treated as factors
      if (is.logical(field_values)) field_values <- factor(field_values)

      field <- element[["field"]]
      include_NA <- element[["include_NA"]]
      values <- element[["values"]]
      mask <- (field_values %in% values) | (is.na(field_values) & include_NA)
    } else if (operation == "select_range") {
      max <- element[["max"]]
      min <- element[["min"]]
      assert(is.numeric(field_values), "Field values must be numerical")
      assert(is.numeric(min) && is.numeric(max), "Max and min must be numerical")
      assert(min <= max, "min <= max")
      mask <- (((field_values <= max) & (field_values >= min)) & !is.na(field_values)) | (is.na(field_values) & include_NA)
    } else if (operation == "select_date") {
      if (inherits(field_values, "POSIXct")) {
        max <- as.POSIXct(element[["max"]], "%Y-%m-%d")
        min <- as.POSIXct(element[["min"]], "%Y-%m-%d")
      } else if (inherits(field_values, "Date")) {
        max <- as.Date(element[["max"]], "%Y-%m-%d")
        min <- as.Date(element[["min"]], "%Y-%m-%d")
      } else {
        stop("Field values must be POSIX.ct or Date")
      }
      assert(min <= max, "min <= max")
      mask <- (((field_values <= max) & (field_values >= min)) & !is.na(field_values)) | (is.na(field_values) & include_NA)
    } else {
      stop(paste0("Operation unknown: `", operation, "`"))
    }
  } else {
    stop(paste("Unknown kind: ", kind))
  }

  return(mask)
}
# nolint end cyclocomp_linter

create_datasets_filter_masks <- function(data_list, datasets_filter) {
  datasets_filter <- as_safe_list(datasets_filter)

  children <- datasets_filter[["children"]]
  dataset_masks <- list()

  for (child in datasets_filter[["children"]]) {
    kind <- child[["kind"]]
    name <- child[["name"]]
    ; assert(!(name %in% names(dataset_masks)), "a dataset can only appear once inside dataset_filters")
    ; assert(kind == "dataset", "dataset_filters children can only be of kind `dataset`")    
    if (length(child[["children"]]) == 1) {
      dataset_masks[[name]] <- process_dataset_filter_element(data_list, child[["children"]][[1]], name)
    } else if (length(child[["children"]]) == 0) {
      dataset_masks[[name]] <- rep_len(TRUE, nrow(data_list[[name]]))
    } else {
      ; assert(FALSE, "`datasets_filter` cannot contain more than children")
    }
  }

  return(dataset_masks)
}

apply_masks_to_datasets <- function(data_list, mask_list) {
  filtered_data_list <- data_list
  for (current_mask_name in names(mask_list)) {
    current_mask <- mask_list[[current_mask_name]]
    filtered_data_list[[current_mask_name]] <- filtered_data_list[[current_mask_name]][current_mask, , drop = FALSE]
  }
  return(filtered_data_list)
}

apply_subject_set_to_datasets <- function(data_list, subject_set, subj_var) {
  filtered_data_list <- data_list
  for (current_ds_name in names(data_list)) {
    current_mask <- data_list[[current_ds_name]][[subj_var]] %in% subject_set
    filtered_data_list[[current_ds_name]] <- filtered_data_list[[current_ds_name]][current_mask, , drop = FALSE]
  }
  return(filtered_data_list)
}

create_subject_set <- function(data_list, subject_filter, sbj_var) {
  subject_filter <- as_safe_list(subject_filter)
  complete_sbj_list <- character(0)
  for (current_data in data_list) {
    complete_subject_list <- union(complete_sbj_list, as.character(unique(current_data[[sbj_var]])))
  }
  children <- subject_filter[["children"]]
  assert(length(children) < 2, "subject filter must have 0 or 1 child")
  if (length(children) == 0) subjects <- return(NA_character_)
  if (length(children) == 1) {
    subjects <- process_subject_filter_element(
      data_list, children[[1]],
      sbj_var, complete_sbj_list
    )
  }
  return(subjects)
}

process_subject_filter_element <- function(data_list, element, sbj_var, complete_subject_list) {
  element <- as_safe_list(element)

  kind <- element[["kind"]]

  if (kind == "filter_operation") {
    operation <- element[["operation"]]
    if (operation == "or") {
      children <- element[["children"]]
      subjects <- character(0)
      assert(length(children) > 0, "`or` operation requires at least one child")
      for (child in children) {
        current_subjects <- process_subject_filter_element(data_list, child, sbj_var, complete_subject_list)
        subjects <- union(subjects, current_subjects)
      }
    } else if (operation == "and") {
      children <- element[["children"]]
      assert(length(children) > 0, "`and` operation requires at least one child")
      for (child in children) {
        subjects <- complete_subject_list
        current_subjects <- process_subject_filter_element(data_list, child, sbj_var, complete_subject_list)
        subjects <- intersect(subjects, current_subjects)
      }
    } else if (operation == "not") {
      children <- element[["children"]]
      assert(length(children) == 1, "`not` operation requires exactly one child")
      subjects <- setdiff(
        complete_subject_list,
        process_subject_filter_element(
          data_list, children[[1]],
          sbj_var, complete_subject_list
        )
      )
    } else {
      stop(paste("Unknown operation: ", operation))
    }
  } else if (kind == "filter") {
    # redirect but do not process
    mask <- process_dataset_filter_element(data_list, element)
    dataset <- element[["dataset"]] # TODO: Replace by table
    subjects <- as.character(data_list[[dataset]][[sbj_var]][mask])
  } else {
    stop(paste("Unknown kind: ", kind))
  }
  return(subjects)
}

to_filter_validate <- function(x) {
  jsonvalidate::json_validate(
    x,
    system.file("to_filter_schema.json", package = "dv.manager", mustWork = TRUE),
    engine = "ajv",
    strict = FALSE,
    verbose = TRUE
  )
}

from_filter_validate <- function(x) {
  jsonvalidate::json_validate(
    x,
    system.file("from_filter_schema.json", package = "dv.manager", mustWork = TRUE),
    engine = "ajv",
    strict = FALSE,
    verbose = TRUE
  )
}

add_blockly_dependency <- function() {
  htmltools::htmlDependency(
    name = "filter_blockly",
    version = utils::packageVersion("dv.manager"),
    src = app_sys("filter/"),
    script = c("blockly_filter_minified.js"),
    style = c("blockly_filter.css"),
  )
}

new_filter_ui <- function(id, dataset_lists, state = NULL) {
  ns <- shiny::NS(id)

  if (!is.null(state)) {
    if (file.exists(state)) {
      state <- paste0(readLines(state), collapse = "\n")
    }
  } else {
    state <- "null" # Acts as a no filter JSON
  }

  bookmark <- shiny::restoreInput(ns("json"), "null")
  current_filter_data <- jsonlite::toJSON(get_filter_data(dataset_lists))
  assert(to_filter_validate(current_filter_data), "failed to validate message to filter")
  payload <- sprintf("{\"state\": %s, \"data\": %s, \"bookmark\": %s}", state, current_filter_data, bookmark)

  apply_button_ui <- shiny::tags[["button"]](id = ns("gen_code"), "Apply filter", class = "btn btn-primary btn-lg")
  export_button_ui <- shiny::downloadButton(
    outputId = ns("export_code"),
    "Export filter",
    class = "btn btn-primary btn-lg"
  )
  filter_ui <- list(
    add_blockly_dependency(),
    shiny:::ionRangeSliderDependency(),
    shiny:::datePickerDependency(),
    # When attaching the dependencies on my own an error occurs when using multiple
    # When including an input perse the error disappears, this should be explored
    shiny::div(
      style = "display:none;",
      shinyWidgets::pickerInput(ns("IGNORE_INPUT"), choices = c("A", "B"), multiple = TRUE),
    ),
    shiny::div(
      id = ns("filter_container"),
      style = "height: 100%;",
      shiny::tags[["script"]](
        type = "application/json",
        shiny::HTML(payload), # Avoids scaping of > and other HTML special characters
        bookmark = if (bookmark != "null") NA else NULL
      )
    )
  )

  combined_ui <- shiny::div(
    style = "display: grid;
            grid-template-rows: 1fr auto; /* First row takes remaining space, second row based on content */
            height: 100%;
            ",
    shiny::div(
      style = "
            text-align: center;
            padding: 10px;
        ",
      filter_ui
    ),
    shiny::div(
      style = "
            padding: 10px;
            text-align: center;
      ",
      apply_button_ui,
      export_button_ui
    )
  )

  list(
    combined_ui = combined_ui,
    split_ui = list(
      filter_ui = filter_ui,
      apply_button_ui = apply_button_ui,
      export_button_ui = export_button_ui
    )
  )
}

new_filter_server <- function(id, selected_dataset_name, strict = FALSE) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    message(paste("Listening to:", ns("json")))

    shiny::observeEvent(selected_dataset_name(), {
      session[["sendCustomMessage"]](
        "init_blockly_filter",
        list(
          container_id = ns("filter_container"),
          dataset = selected_dataset_name(),
          gen_code_button_id = ns("gen_code"),
          json_input_id = ns("json"),
          log_input_id = ns("log")
        )
      )
    })

    shiny::observeEvent(input[["log"]], {
      for (msg in input[["log"]]) {
        shiny::showNotification(msg, type = "warn", duration = NULL)
      }
    })

    res <- shiny::reactive({
      message(input[["json"]])
      if (checkmate::test_string(input[["json"]], min.chars = 1)) {
        if (strict) assert(from_filter_validate(input[["json"]]), "failed to validate message from filter")
        parsed_json <- jsonlite::fromJSON(input[["json"]], simplifyVector = FALSE)
        list(
          filters = parsed_json %||% NA_character_,
          raw = input[["json"]]
        )
      } else {
        list(
          filters = NA_character_,
          raw = NA_character_
        )
      }
    })

    output[["export_code"]] <- shiny::downloadHandler(
      filename = "filter.txt",
      content = function(file) {
        if (!checkmate::test_string(input[["json"]])) {
          data <- "null"
          shiny::showNotification("Empty filter exported. Please apply filter before exporting.", type = "warn")
        } else {
          data <- jsonlite::prettify(input[["json"]])
        }
        writeLines(
          data,
          con = file
        )
      },
      contentType = "application/json"
    )
    shiny::outputOptions(output, "export_code", suspendWhenHidden = FALSE)

    return(
      structure(res, raw = shiny::reactive(input[["json"]]))
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
        new_filter_ui("filter", data, state = filter_state)[["combined_ui"]],
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
    x <- new_filter_server("filter", selected_dataset = shiny::reactive(selected_data), strict = TRUE)

    output[["raw_json"]] <- shiny::renderPrint({
      json <- attr(x, "raw")()
      shiny::req(json)
      jsonlite::prettify(json)
    })

    output[["validate_json"]] <- DT::renderDataTable({
      json <- attr(x, "raw")()
      shiny::req(json)
      e <- attr(from_filter_validate(json), "error") |> tibble::as_tibble()
      e
    })

    output[["output_json"]] <- shiny::renderPrint({
      shiny::req(!is.na(x()))
      jsonlite::fromJSON(x(), simplifyVector = FALSE)
    })

    filtered_datasets <- shiny::reactive({
      shiny::req(!is.na(x()))
      ds <- data[[selected_data]]
      mask <- create_datasets_filter_masks(ds, x()[["filters"]][["datasets_filter"]])
      apply_masks_to_datasets(ds, mask)
    })

    filtered_subjects <- shiny::reactive({
      shiny::req(!is.na(x()))
      ds <- data[[selected_data]]
      subject_set <- create_subject_set(ds, x()[["filters"]][["subject_filter"]], "USUBJID")
      if (identical(subject_set, NA_character_)) {
        ds
      } else {
        apply_subject_set_to_datasets(ds, subject_set, "USUBJID")
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

unnamespaced_filter_modal <- function(filter_ui) {
  # WARNING: This, as it name implies, is not a module and is not namespaced.
  # This should be adressed before releasing

  warning("Using unnamespaced modal, DO NOT USE IN PRODUCTION")
  shiny::div(
    shiny::tags[["label"]]("Show filter", "for" = "filter-checkbox", class = "btn btn-primary"),
    shiny::tags[["input"]](type = "checkbox", id = "filter-checkbox", style = "display:none;"),
    shiny::div(
      id = "filter_overlay",
      shiny::tags[["style"]](
        '
            /* Overlay style */
        #filter_overlay {
            position: fixed;
            top: 0;
            left: 0;
            width: 100vw;
            height: 100vh;
            background-color: rgba(0, 0, 0, 0.5);
            display: none; /* Hidden by default */
            justify-content: center;
            align-items: center;
            z-index: 999;
        }

        /* Modal content style */
        #filter_modal {
            background-color: #fff;
            padding: 20px;
            border-radius: 5px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
            width: 80vw;
            height: 80vh;
            text-align: center;
        }

        /* Close button style */
        #filter_close-btn {
            display: inline-block;
            margin-top: 10px;
            padding: 5px 10px;
            background-color: #ccc;
            text-decoration: none;
            border-radius: 3px;
        }

        #filter_close-btn:hover {
            background-color: #aaa;
        }

        /* Show the modal when the checkbox is checked */
        input[type="checkbox"]:checked + #filter_overlay {
            display: flex;
        }

            '
      ),
      shiny::div(
        id = "filter_modal",
        style = "display: grid;
            grid-template-rows: auto 1fr auto; /* First row takes remaining space, second row based on content */
            ",
        shiny::h4("Filter"),
        filter_ui,
        shiny::tags[["label"]]("Close filter", "for" = "filter-checkbox", id = "filter_close-btn")
      ),
      shiny::tags[["script"]]("
    $(document).ready(function () {
    document.getElementById('filter_modal').addEventListener('click', function(event) {
            event.stopPropagation(); // Prevent the click event from reaching the overlay
        });
    const overlay = document.getElementById('filter_overlay');
            overlay.addEventListener('click', function(){
            document.getElementById('filter-checkbox').checked = false;
            $('#filter-checkbox').trigger('change');
        });
    $('#filter-checkbox').change(function () {
      window.dispatchEvent(new Event('resize'));
      blockly_filter.chaff();
    });
});
    ")
    )
  )
}
