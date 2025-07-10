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

    l <- list(
      name = jsonlite::unbox(name),
      label = jsonlite::unbox(attr(var, "label"))
    )

    # Logical is treated as a factor in the client
    if (is.logical(var)) var <- factor(var)

    if (is.character(var) || is.factor(var)) {
      l[["kind"]] <- jsonlite::unbox("categorical")
      l[["NA_count"]] <- jsonlite::unbox(sum(is.na(var)))
      na_clean_var <- var[!is.na(var)]

      count <- table(na_clean_var)
      values <- names(count)
      count <- unname(count)
      l[["values_count"]] <- vector(mode = "list", length = length(l[["values"]]))
      for (v_idx in seq_along(values)) {
        l[["values_count"]][[v_idx]] <- list(
          value = jsonlite::unbox(values[[v_idx]]),
          count = jsonlite::unbox(count[[v_idx]])
        )
      }
    } else if (is.numeric(var)) {
      l[["kind"]] <- jsonlite::unbox("numerical")
      l[["NA_count"]] <- jsonlite::unbox(sum(is.na(var)))
      na_clean_var <- var[!is.na(var)]

      l[["min"]] <- jsonlite::unbox(min(Inf, na_clean_var, na.rm = TRUE))
      l[["max"]] <- jsonlite::unbox(max(-Inf, na_clean_var, na.rm = TRUE))
    } else if (inherits(var, "POSIXct") || inherits(var, "Date")) {
      if (inherits(var, "POSIXct")) {
        var <- as.Date(var)
      }
      l[["kind"]] <- jsonlite::unbox("date")
      l[["NA_count"]] <- jsonlite::unbox(sum(is.na(var)))
      na_clean_var <- var[!is.na(var)]

      l[["min"]] <- jsonlite::unbox(min(as.Date(Inf), na_clean_var, na.rm = TRUE))
      l[["max"]] <- jsonlite::unbox(max(as.Date(-Inf), na_clean_var, na.rm = TRUE))
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
        name = jsonlite::unbox(current_dataset_name),
        variables = get_single_filter_data(current_dataset)
      )
    }
    res[[idx]] <- list(
      name = jsonlite::unbox(current_dataset_list_name),
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
    assert(variable %in% names(dataset_list[[filter_dataset]]), sprintf("data[['%s']] does not contain col `%s`", filter_dataset, variable))
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
    filtered_data_list[[current_mask_name]] <- filtered_data_list[[current_mask_name]][current_mask, , drop = FALSE]
  }
  return(filtered_data_list)
}

apply_subject_set <- function(dataset_list, subject_set, subj_var) {
  filtered_data_list <- dataset_list
  for (current_ds_name in names(dataset_list)) {
    current_mask <- dataset_list[[current_ds_name]][[subj_var]] %in% subject_set
    filtered_data_list[[current_ds_name]] <- filtered_data_list[[current_ds_name]][current_mask, , drop = FALSE]
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
    shiny::setBookmarkExclude("IGNORE_INPUT")
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
      json_r <- input[["json"]]

      if (checkmate::test_string(json_r, min.chars = 1)) {
        val_res <- from_filter_validate(json_r)
        if (strict) assert(val_res, "failed to validate message from filter")
        parsed_json <- jsonlite::fromJSON(json_r, simplifyVector = FALSE)
        list(
          parsed = parsed_json %||% NA_character_,
          raw = json_r
        )
      } else {
        list(
          parsed = NA_character_,
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
      jsonlite::fromJSON(x()[["parsed"]], simplifyVector = FALSE)
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
    const overlay = document.getElementById('filter_overlay');
            overlay.addEventListener('click', function(event){
            let $target = $(event.target);
                    if(!$target.closest('#filter_modal').length) {
                      console.log('Inner Hit')
                      document.getElementById('filter-checkbox').checked = false;
            $('#filter-checkbox').trigger('change');

                    }
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
