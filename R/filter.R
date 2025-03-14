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
  # `jsonlite` transforms `NA` `null` by default, seems reasonabel as JSON as has no support for NA values
  # Therefore, we remove all NA values from the fields before making our calculations making all `null`
  # values in the JSON intentional.
  # Why not using NULL? NULL is interpreted as an empty object `{}` by default by `jsonlite::toJSON`. We can modify the
  # behavior and making NULL transform into `null`. But this brings two problems, one is that once NULL is gone we
  # cannot create `{}` which although not required now, maybe required in the future. Second `NULL` is very particular in
  # R and although we can `list(a=NULL)` we cannot `x <- list(); x[["a"]] <- NULL` (this has the effect of deleting the entry "a")
  # we must do instead `x <- list(); x["a"] <- list(NULL)`
  # Therefore it seems more reasonable to work with NAs in this case.

  for (idx in seq_len(n_col)) {
    name <- nm_col[[idx]]
    col <- data[[name]]

    l <- list(
      name = jsonlite::unbox(name),
      label = jsonlite::unbox(attr(col, "label"))
    )
    if (is.character(col) || is.factor(col)) {
      l[["kind"]] <- jsonlite::unbox("categorical")
      l[["NA_count"]] <- jsonlite::unbox(sum(is.na(col)))
      na_clean_col <- col[!is.na(col)]

      values <- unique(na_clean_col)
      l[["values_count"]] <- vector(mode = "list", length = length(l[["values"]]))
      for (v_idx in seq_along(values)) {
        current_value <- values[[v_idx]]
        l[["values_count"]][[v_idx]] <- list(value = jsonlite::unbox(current_value), count = jsonlite::unbox(sum(values == current_value)))
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

process_dataset_filter_element <- function(data_list, element, dataset = NULL) {
  kind <- element[["kind"]]

  if (kind == "dataset") {
    assert(length(element[["children"]]) <= 1, "`dataset` cannot contain more than element")
    name <- element[["name"]]
    if (length(element[["children"]]) == 0) {
      # If no children are found we return a mask with no filter
      mask <- rep_len(TRUE, nrow(data_list[[dataset]]))
    } else {
      mask <- process_dataset_filter_element(data_list, element[["children"]][[1]], dataset)
    }
  } else if (kind == "filter_operation") {
    operation <- element[["operation"]]
    if (operation == "and") {
      assert(length(element[["children"]]) >= 1, "`and` operation requires at least one element")
      mask <- TRUE # Neutral element for &
      for (child in element[["children"]]) {
        mask <- mask & process_dataset_filter_element(data_list, child, dataset)
      }
    } else if (operation == "or") {
      assert(length(element[["children"]]) >= 1, "`or` operation requires at least one element")

      mask <- FALSE # Neutral element for |
      for (child in element[["children"]]) {
        mask <- mask | process_dataset_filter_element(data_list, child, dataset)
      }
    } else if (operation == "not") {
      assert(length(element[["children"]]) == 1, "`not` operation requires exactly one element")
      mask <- !process_dataset_filter_element(data_list, element[["children"]][[1]], dataset)
    } else {
      stop(paste0("Operation unknown: `", operation, "`"))
    }
  } else if (kind == "filter") {
    field <- element[["field"]]
    operation <- element[["operation"]]
    include_NA <- element[["include_NA"]]
    filter_dataset <- element[["dataset"]]
    if (!is.null(dataset)) assert(dataset == filter_dataset, "Filtering on the wrong dataset")

    field_values <- data_list[[filter_dataset]][[field]]

    if (operation == "select_subset") {
      field <- element[["field"]]
      include_NA <- element[["include_NA"]]
      values <- element[["values"]]
      mask <- (field_values %in% values) | (is.na(field_values) & include_NA)
    } else if (operation == "select_range") {
      max <- element[["max"]]
      min <- element[["min"]]
      assert(is.numeric(min) && is.numeric(max), "Max and min must be numerical")
      mask <- field_values <= max & field_values >= min | (is.na(field_values) & include_NA)
    } else if (operation == "select_date") {
      if (inherits(field_values, "POSIXct")) {
        max <- as.POSIXct(element[["max"]], "%Y-%m-%d")
        min <- as.POSIXct(element[["min"]], "%Y-%m-%d")
      } else if (inherits(field_values, "Date")) {
        max <- as.Date(element[["max"]], "%Y-%m-%d")
        min <- as.Date(element[["min"]], "%Y-%m-%d")
      } else {
        stop("Incorrect column type")
      }
      mask <- field_values <= max & field_values >= min | (is.na(field_values) & include_NA)
    } else {
      stop(paste0("Operation unknown: `", operation, "`"))
    }
  } else {
    stop(paste("Unknown kind: ", kind))
  }

  return(mask)
}

create_datasets_filter_masks <- function(data_list, datasets_filter) {
  children <- datasets_filter[["children"]]
  if (length(children) == 0) mask <- list()
  dataset_masks <- list()

  for (child in datasets_filter[["children"]]) {
    kind <- child[["kind"]]
    name <- child[["name"]]
    assert(!(name %in% names(dataset_masks)), "a dataset can only appear once inside dataset_filters")
    assert(kind == "dataset", "dataset_filters children can only be of kind `dataset`")
    dataset_masks[[name]] <- process_dataset_filter_element(data_list, child, name)
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
  for (current_dataset_name in names(data_list)) {
    current_mask <- data_list[[current_dataset_name]][[subj_var]] %in% subject_set
    filtered_data_list[[current_dataset_name]] <- filtered_data_list[[current_dataset_name]][current_mask, , drop = FALSE]
  }
  return(filtered_data_list)
}

create_subject_set <- function(data_list, subject_filter, sbj_var) {
  # Cojunto completo de participantes
  complete_subject_list <- character(0)
  for (current_data in data_list) {
    complete_subject_list <- union(complete_subject_list, as.character(unique(current_data[[sbj_var]])))
  }
  children <- subject_filter[["children"]]
  assert(length(children) < 2, "subject filter must have 0 or 1 child")
  if (length(children) == 0) subjects <- return(NA_character_)
  if (length(children) == 1) subjects <- process_subject_filter_element(data_list, children[[1]], sbj_var, complete_subject_list)
  return(subjects)
}

process_subject_filter_element <- function(data_list, element, sbj_var, complete_subject_list) {
  kind <- element[["kind"]]

  if (kind == "filter_operation") {
    operation <- element[["operation"]]
    if (operation == "or") {
      children <- element[["children"]]
      subjects <- character(0)
      assert(length(children) > 0, "`union` requires at least one child")
      for (child in children) {
        current_subjects <- process_subject_filter_element(data_list, child, sbj_var, complete_subject_list)
        subjects <- union(subjects, current_subjects)
      }
    } else if (operation == "and") {
      children <- element[["children"]]
      subjects <- NA_character_
      assert(length(children) > 0, "`intersection` requires at least one child")
      for (child in children) {
        current_subjects <- process_subject_filter_element(data_list, child, sbj_var, complete_subject_list)
        if (!identical(subjects, NA_character_)) {
          subjects <- intersect(subjects, current_subjects)
        } else {
          subjects <- current_subjects
        }
      }
    } else if (operation == "not") {
      children <- element[["children"]]
      assert(length(children) == 1, "`complement` requires exactly one child")
      subjects <- setdiff(complete_subject_list, process_subject_filter_element(data_list, children[[1]], sbj_var, complete_subject_list))
    } else {
      stop(paste("Unknown operation: ", operation))
    }
  } else if (kind == "filter") {
    # redirect but do not process
    mask <- process_dataset_filter_element(data_list, element)
    dataset <- element[["dataset"]]
    subjects <- as.character(data_list[[dataset]][[sbj_var]][mask])
  } else {
    stop(paste("Unknown kind: ", kind))
  }
  return(subjects)
}

to_filter_validate <- function(x) {
  jsonvalidate::json_validate(x, system.file("to_filter_schema.json", package = "dv.manager", mustWork = TRUE), engine = "ajv", strict = FALSE, verbose = TRUE)
}
from_filter_validate <- function(x) {
  jsonvalidate::json_validate(x, system.file("from_filter_schema.json", package = "dv.manager", mustWork = TRUE), engine = "ajv", strict = FALSE, verbose = TRUE)
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

new_filter_ui <- function(id, data, state = NULL) {
  ns <- shiny::NS(id)

  if (!is.null(state)) {
    if (file.exists(state)) {
      state <- paste0(readLines(state), collapse = "\n")
    }
  } else {
    state <- "null" # Acts as a no filter JSON
  }

  bookmark <- shiny::restoreInput(ns("json"), "null")
  current_filter_data <- jsonlite::toJSON(get_filter_data(data))
  assert(to_filter_validate(current_filter_data), "failed to validate message to filter")
  payload <- sprintf("{\"state\": %s, \"data\": %s, \"bookmark\": %s}", state, current_filter_data, bookmark)

  apply_button_ui <- shiny::tags[["button"]](id = ns("gen_code"), "Apply filter", class = "btn btn-primary btn-lg")
  export_button_ui <- shiny::downloadButton(outputId = ns("export_code"), "Export filter", class = "btn btn-primary btn-lg")
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

new_filter_server <- function(id, selected_dataset, strict = FALSE) {
  mod <- function(input, output, session) {

    ns <- session[["ns"]]

    message(paste("Listening to:", ns("json")))

    shiny::observeEvent(selected_dataset(), {      
      session[["sendCustomMessage"]](
        "init_blockly_filter",
        list(
          container_id = ns("filter_container"),
          dataset = selected_dataset(),
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

# demo_app_filter <- function(data = list(
#                               "Tables" = list(
#                                 adsl = get_pharmaverse_data("adsl"),
#                                 adae = get_pharmaverse_data("adae")
#                               )
#                               # ,
#                               # "D2" = list(
#                               #   adsl = get_pharmaverse_data("adsl"),
#                               #   adae = get_pharmaverse_data("adae")
#                               # )
#                             ),
#                             video_link = "https://www.google.es",
#                             email = "PLACEHOLDER@EMAIL.COM") {
#   ui <- function(request) {
#     shiny::fluidPage(
#       shiny::div(
#         shiny::HTML(paste0("
# <style>
#   .collapsible-container {
#     margin: 10px 0;
#     border: 1px solid #ccc;
#     border-radius: 5px;
#     overflow: hidden;
#   }

#   .collapsible-title {
#     cursor: pointer;
#     background-color: #f1f1f1;
#     padding: 10px;
#     font-size: 18px;f
#     font-weight: bold;
#     border-bottom: 1px solid #ccc;
#     display: flex;
#     justify-content: space-between;
#     align-items: center;
#   }

#   .chevron {
#     transition: transform 0.3s ease;
#   }

#   .chevron.collapsed {
#     transform: rotate(-90deg);
#   }

#   .collapsible-content {
#     display: block; /* Expanded by default */
#     padding: 10px;
#   }
# </style>

# <div class='collapsible-container'>
#   <div class='collapsible-title' onclick='toggleCollapsible()'>
#     Filtering App Information (Click to hide/show)
#     <span class='chevron' id='chevron'>&#9660;</span>
#   </div>
#   <div class='collapsible-content' id='collapsible-content'>
#     <div style='display:flex;gap:20px;'>
#       <!-- Left side content -->
#       <div style='flex:1;'>
#         <h3>About This App</h3>
#         <ul>
#           <li>This is a proof of concept for a new filtering tool within the DaVinCI applications, designed to explore richer options for filtering clinical datasets.</li>
#           <li>As an early-stage prototype, occasional errors or unexpected behavior are expected.</li>
#           <li>Your feedback is crucial in shaping and refining this concept thank you for your support!</li>
#         </ul>
#         <h3>Features</h3>
#         <ul>
#           <li>Filter individual datasets to focus on specific variables or criteria.</li>
#           <li>Create population filters to analyze subgroups based on custom conditions.</li>
#           <li>Save bookmarks to capture and revisit the current state of the application, ensuring quick access to your workflows.</li>
#         </ul>
#         <h3>Initial Steps</h3>
#         <ul>
#           <li>
#             <strong>Dataset Filters:</strong>
#             <ul>
#               <li>Begin by selecting <strong>Dataset Filters</strong> from the <strong>Filter Types</strong> category.</li>
#               <li>Next, choose a table from the <strong>Tables</strong> menu to start working.</li>
#               <li><em>Note:</em> The <strong>Tables</strong> menu may be a bit confusing at this stage, as it requires you to collapse and expand sections to view the available tables.</li>
#               <li>Once a table is included, you must always add an <strong>Operation</strong> under it.</li>
#               <li>After setting up the table and its operation, you can start building your filter logic.</li>
#               <li>You can add multiple tables at the top level to create complex filters.</li>
#               <li>Finally, click <strong>Apply Filter</strong> to execute your filter and see the results.</li>
#               <li>
#                 <strong>Example:</strong>
#                 <p>Imagine you want to filter the <strong>adsl</strong> dataset to find female patients between 55 and 60 years old:</p>
#                 <ol>
#                   <li>First, drag the <strong>Dataset Filters</strong> piece into the workspace.</li>
#                   <li>From the <strong>Tables</strong> menu, drag the <strong>adsl</strong> table into the workspace.</li>
#                   <li>Next, drag an <strong>AND Operation</strong> into the workspace under the <strong>adsl</strong> table.</li>
#                   <li>Under the AND Operation, drag two pieces: <strong>sex</strong> and <strong>age</strong> from the <strong>adsl</strong> menu under the <strong>Tables</strong> section.</li>
#                   <li>For the <strong>sex</strong> filter, select the value <strong>Female</strong>.</li>
#                   <li>For the <strong>age</strong> filter, set the range to <strong>55 to 60 years</strong>.</li>
#                   <li>Finally, click <strong>Apply Filter</strong> to execute your filter and see the results.</li>
#                 </ol>
#                 <p>This setup creates a filter that identifies female patients aged between 55 and 60 from the <strong>adsl</strong> dataset.</p>
#               </li>
#             </ul>
#           </li>
#           <li>
#             <strong>Subject Filters:</strong>
#             <ul>
#               <li>Begin by selecting <strong>Subject Filters</strong> from the <strong>Filter Types</strong> category.</li>
#               <li>Next, include a <strong>Union</strong>, <strong>Intersect</strong>, or <strong>Diff Operation</strong> at the top level.</li>
#               <li>Each of these operations must have <strong>Table</strong> pieces immediately below to define the dataset(s) to compare.</li>
#               <li>Under the table, you can add additional filters and operations as needed, similar to <strong>Dataset Filters</strong>.</li>
#               <li><em>Note:</em> The <strong>Tables</strong> menu may be a bit confusing at this stage, as it requires you to collapse and expand sections to view the available tables.</li>
#               <li>After setting up your filters, click <strong>Apply Filter</strong> to execute and see the results.</li>
#             </ul>
#           </li>
#         </ul>
#         <h3>Bookmarks</h3>
#         <ul>
#           <li>You can save the state of the application using the <strong>Bookmark</strong> button.</li>
#           <li>When you create a bookmark, it will generate a unique link. Opening this link will restore the application to the state of the <strong>last applied filter</strong>.</li>
#           <li><strong>Important:</strong> When restoring a bookmark, remember to press <strong>Apply Filter</strong>. Otherwise, the loaded filter will not be applied.</li>
#         </ul>
#       </div>

#       <!-- Right side content -->
#       <div style='flex:1;border-left:1px solid #ccc;padding-left:20px;'>
#         <h3>Available Datasets and Results</h3>
#         <p>In the current version of the app, two datasets are available for filtering and analysis:</p>
#         <ul>
#           <li><strong>ADSL Dataset:</strong> Typically used for subject-level analysis.</li>
#           <li><strong>ADAE Dataset:</strong> Typically used for event-level analysis.</li>
#         </ul>
#         <p>Below the filtering section, you will find the results of the applied filters:</p>
#         <ul>
#           <li><strong>Dataset Filters Results:</strong> These display the filtered rows of each dataset independently, based on the criteria applied to the specific dataset.</li>
#           <li><strong>Population Filters Results:</strong> These display the selection of a population of patients that meet the criteria of the population filter, which affects both datasets simultaneously.</li>
#         </ul>
#         <p><strong>Important Notes:</strong></p>
#         <ul>
#           <li>The effects of <strong>Dataset Filters</strong> are specific to the dataset they are applied to (e.g., ADSL or ADAE).</li>
#           <li>The effects of <strong>Subject Filters</strong> apply to both datasets and define a population of patients.</li>
#           <li>For demonstration purposes in this app, the results of <strong>Dataset Filters</strong> (row-level filtering) and <strong>Population Filters</strong> (patient population selection) are displayed independently and do not influence one another.</li>
#         </ul>
#         <h3>Known Issues</h3>
#         <p>As this application is a proof of concept, not all functionality is fully implemented. Below are some issues already identified and planned for the roadmap:</p>
#         <ul>
#           <li>Only one <strong>Dataset Filter</strong> and one <strong>Subject Filter</strong> should be allowed at a time.</li>
#           <li>The filter system must prevent incorrect connections, such as:
#             <ul>
#               <li>Using columns from <strong>adae</strong> under <strong>adsl</strong> tables.</li>
#               <li>Adding <strong>Union Operations</strong> in <strong>Dataset Filters</strong>.</li>
#               <li>Other similar incorrect configurations...</li>
#             </ul>
#           </li>
#           <li>Incorrect filter setups currently fail silently. The application must provide meaningful error messages to guide users when filters are configured incorrectly.</li>
#           <li>When restoring a bookmark, the filter is loaded but not automatically applied. Users must press <strong>Apply Filter</strong> to execute the loaded filter.</li>
#         </ul>
#         <h3>What Can You Do For Us?</h3>
#         <p>Your feedback is incredibly valuable and appreciated! The more feedback we receive, the better and more refined the initial version of this filter will be. Help us create a tool that meets your needs and expectations.</p>
#         <p>Write to us at <strong>", email, "</strong> and let us know your thoughts. Here are some examples of how you can contribute:</p>
#         <ul>
#           <li><strong>Suggest a functionality or improvement</strong> to the current setup.</li>
#           <li>
#             <strong>Is there a selection you want to express but can't?</strong>
#             <ul>
#               <li>Try to express it with the available pieces.</li>
#               <li>Take a screenshot of your setup and include a brief description of your desired outcome.</li>
#             </ul>
#           </li>
#           <li>
#             <strong>Does your selection not work as expected?</strong>
#             <ul>
#               <li>Take a screenshot of your selected filter.</li>
#               <li>Include a brief description of the issue and your desired outcome.</li>
#             </ul>
#           </li>
#         </ul>
#         <p>Thank you for helping us make this tool better!</p>
#         <h3>FAQ</h3>
#         <ul>
#           <li>
#             <strong>Q: When I move a piece, all pieces below it are moved too. What can I do?</strong>
#             <br>
#             <strong>A:</strong> Try using <strong>Control + Click</strong> to select only the specific piece you want to move without affecting others.
#           </li>
#           <li>
#             <strong>Q: Can I use keyboard shortcuts like copy and paste?</strong>
#             <br>
#             <strong>A:</strong> Yes, you can use common shortcuts such as <strong>Ctrl + C</strong> to copy and <strong>Ctrl + V</strong> to paste components within the workspace.
#           </li>
#         </ul>
#       </div>
#     </div>
#   </div>
# </div>

# <script>
#   function toggleCollapsible() {
#     const content = document.getElementById('collapsible-content');
#     const chevron = document.getElementById('chevron');
#     const isCollapsed = content.style.display === 'none';

#     content.style.display = isCollapsed ? 'block' : 'none';
#     chevron.classList.toggle('collapsed', !isCollapsed);
#   }
# </script>


# "))
#       ),
#       shiny::a("Open example video", href = video_link, target = "_blank", rel = "noopener", class = "btn btn-primary btn-lg"),
#       shiny::br(),
#       shiny::br(),
#       shiny::bookmarkButton(),
#       new_filter_ui("filter", data)[["combined_ui"]],
#       shiny::h3("Results of applying data filters"),
#       shiny::h4("adsl"),
#       shiny::dataTableOutput("output_filtered_ds_adsl"),
#       shiny::h4("adae"),
#       shiny::dataTableOutput("output_filtered_ds_adae"),
#       shiny::h3("Results of applying subject filters"),
#       shiny::h4("Selected population"),
#       shiny::verbatimTextOutput("selected_subjects"),
#       shiny::h4("adsl"),
#       shiny::dataTableOutput("output_filtered_sbj_adsl"),
#       shiny::h4("adae"),
#       shiny::dataTableOutput("output_filtered_sbj_adae"),
#     )
#   }

#   server <- function(input, output, session) {
#     x <- new_filter_server("filter", shiny::reactive(data))
#     selected_data <- "Tables"

#     output[["output_json"]] <- shiny::renderPrint({
#       shiny::req(!is.na(x()))
#       jsonlite::fromJSON(x(), simplifyVector = FALSE)
#     })

#     filtered_datasets <- shiny::reactive({
#       shiny::validate(
#         shiny::need(
#           !is.null(x()[["filters"]][[selected_data]][["dataset"]]),
#           "Have you applied the filter? Otherwise, Dataset Filter is empty or there was an error when processing it"
#         )
#       )
#       filters <- x()[["filters"]][[selected_data]][["dataset"]]
#       ds <- data[[selected_data]]
#       mask <- create_masks_from_dataset_filters(ds, filters)
#       apply_masks_to_datasets(ds, mask)
#     })

#     filtered_subjects <- shiny::reactive({
#       shiny::validate(
#         shiny::need(
#           !is.null(x()[["filters"]][[selected_data]][["subject"]]),
#           "Have you applied the filter? Otherwise, Subject Filter is empty or there was an error when processing it"
#         )
#       )
#       filters <- x()[["filters"]][[selected_data]][["subject"]]
#       ds <- data[[selected_data]]
#       subjid_set <- as.character(compute_subject_set_from_filter(ds, filters, "USUBJID"))
#       subjid_set
#     })

#     output[["output_filtered_ds_adsl"]] <- shiny::renderDataTable({
#       filtered_datasets()[["adsl"]]
#     })

#     output[["output_filtered_ds_adae"]] <- shiny::renderDataTable({
#       filtered_datasets()[["adae"]]
#     })

#     output[["selected_subjects"]] <- shiny::renderPrint({
#       filtered_subjects()
#     })


#     output[["output_filtered_sbj_adsl"]] <- shiny::renderDataTable({
#       dplyr::filter(data[[selected_data]][["adsl"]], .data[["USUBJID"]] %in% filtered_subjects())
#     })

#     output[["output_filtered_sbj_adae"]] <- shiny::renderDataTable({
#       dplyr::filter(data[[selected_data]][["adae"]], .data[["USUBJID"]] %in% filtered_subjects())
#     })
#   }

#   shiny::shinyApp(
#     ui = ui,
#     server = server,
#     enableBookmarking = "server"
#   )
# }
