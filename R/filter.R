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

# NEW FILTER

get_single_filter_data <- function(data) {
  nm_col <- names(data)
  n_col <- length(nm_col)
  res <- vector(mode = "list", length = n_col)

  # In R all elements are vectors by default this makes complicated to transform into json as c("a") can be enconded
  # as "a" or ["a"]. To disambiguate this jsonlite offers `unbox`. It is undesired that this specific function is
  # coupled as it could purely a transformation of data. It is unlikely that any other package could do this
  # disambiguation automatically but maybe jsonlite is replaced in the future.

  for (idx in seq_len(n_col)) {
    name <- nm_col[[idx]]
    col <- data[[name]]

    l <- list(name = jsonlite::unbox(name))

    if (is.character(col)) {
      l[c("value", "type")] <- list(unique(col), jsonlite::unbox("character"))
    } else if (is.factor(col)) {
      l[c("value", "type")] <- list(unique(as.character(col)), jsonlite::unbox("factor"))
    } else if (is.integer(col)) {
      l[c("value", "type")] <- list(list(min = jsonlite::unbox(min(Inf, col, na.rm = TRUE)), max = jsonlite::unbox(max(-Inf, col, na.rm = TRUE))), jsonlite::unbox("integer"))
    } else if (is.numeric(col)) { # It also detects integers, it must go after is.integer
      l[c("value", "type")] <- list(list(min = jsonlite::unbox(min(Inf, col, na.rm = TRUE)), max = jsonlite::unbox(max(-Inf, col, na.rm = TRUE))), jsonlite::unbox("double"))
    } else if (inherits(col, "POSIXct")) {
      d <- as.Date(col)
      l[c("value", "type")] <- list(list(min = jsonlite::unbox(min(Inf, col, na.rm = TRUE)), max = jsonlite::unbox(max(-Inf, col, na.rm = TRUE))), jsonlite::unbox("Date"))
    } else if (inherits(col, "Date")) { # is.double also detect this one
      l[c("value", "type")] <- list(list(min = jsonlite::unbox(min(Inf, col, na.rm = TRUE)), max = jsonlite::unbox(max(-Inf, col, na.rm = TRUE))), jsonlite::unbox("Date"))
    }
    # remove NA
    l[["value"]] <- l[["value"]][!is.na(l[["value"]])]
    res[[idx]] <- l
  }
  return(res)
}

get_filter_data <- function(datasets) {
  nm_dataset <- names(datasets)
  n_dataset <- length(nm_dataset)
  res <- vector(mode = "list", length = n_dataset)
  names(res) <- nm_dataset
  for (idx in seq_len(n_dataset)) {
    dataset <- datasets[[idx]]
    nm_tables <- names(dataset)
    n_tables <- length(nm_tables)
    dataset_res <- vector(mode = "list", length = n_tables)
    names(dataset_res) <- nm_tables
    for (jdx in seq_len(n_tables)) {
      table <- dataset[[jdx]]
      dataset_res[[jdx]] <- get_single_filter_data(table)
    }
    res[[idx]] <- dataset_res
  }
  return(res)
}

apply_filter <- function(data, filter_parameters) {
  if (length(filter_parameters) == 0) {
    return(rep_len(TRUE, nrow(data)))
  }

  checkmate::assert_subset(c("type"), names(filter_parameters))
  type <- filter_parameters[["type"]]

  if (type %in% c("integer", "double", "date")) {
    checkmate::assert_set_equal(c("type", "column", "value", "NAs"), names(filter_parameters))

    column <- filter_parameters[["column"]]
    value <- filter_parameters[["value"]]
    NAs <- filter_parameters[["NAs"]]

    checkmate::assert_set_equal(names(value), c("min", "max"))

    min_v <- if (!is.na(value[["min"]])) value[["min"]] else -Inf
    max_v <- if (!is.na(value[["max"]])) value[["max"]] else Inf
    mask <- min_v <= data[[column]] & data[[column]] <= max_v

    if (NAs) {
      mask <- mask | is.na(data[[column]])
    } else {
      mask <- mask & !is.na(data[[column]])
    }
  } else if (type == "category") {
    checkmate::assert_set_equal(c("type", "column", "value", "NAs"), names(filter_parameters))

    column <- filter_parameters[["column"]]
    value <- as.character(filter_parameters[["value"]]) # Force as.character in case JSON conversion fails
    NAs <- filter_parameters[["NAs"]]

    mask <- data[[column]] %in% c(value)
    if (NAs) {
      mask <- mask | is.na(data[[column]])
    } else {
      mask <- mask & !is.na(data[[column]])
    }
  } else if (type == "and") {
    checkmate::assert_set_equal(c("type", "filter_list"), names(filter_parameters))
    filter_list <- filter_parameters[["filter_list"]]
    mask <- TRUE # Neutral element for &
    for (this_filter_parameters in filter_list) {
      mask <- mask & apply_filter(data, this_filter_parameters)
    }
  } else if (type == "or") {
    checkmate::assert_set_equal(names(filter_parameters), c("type", "filter_list"))
    filter_list <- filter_parameters[["filter_list"]]
    mask <- FALSE # Neutral element for |
    for (this_filter_parameters in filter_list) {
      mask <- mask | apply_filter(data, this_filter_parameters)
    }
  } else if (type == "not") {
    checkmate::assert_set_equal(names(filter_parameters), c("type", "filter"))
    mask <- !apply_filter(data, filter_parameters[["filter"]])
  } else {
    stop("Filter type unknown")
  }

  return(mask)
}

create_masks_from_dataset_filters <- function(data_list, filter_list) {
  n_filter <- length(filter_list)
  res_mask <- vector(mode = "list", length = n_filter)
  res_names <- character(0)
  for (idx in seq_len(n_filter)) {
    nm <- filter_list[[idx]][["target"]]
    filter <- filter_list[[idx]][["filter"]]
    d <- data_list[[nm]]
    res_mask[[idx]] <- apply_filter(d, filter)
    res_names[[idx]] <- nm
  }
  names(res_mask) <- res_names
  res_mask
}

apply_masks_to_datasets <- function(data_list, mask_list) {
  nm_data <- names(data_list)
  nm_mask <- names(mask_list)
  n_data <- length(nm_data)
  res_data <- vector(mode = "list", length = n_data)
  names(res_data) <- nm_data
  for (idx in seq_len(n_data)) {
    nm <- nm_data[[idx]]
    d <- data_list[[nm]]

    # If no mask is given for a dataset the dataset is considered not filtered and included as is
    if (!nm %in% nm_mask) {
      res_data[[nm]] <- d
    } else {
      res_data[[nm]] <- d[mask_list[[nm]], , drop = FALSE]
    }
  }
  res_data
}

compute_subject_set_from_filter <- function(data_list, filter_list, subjid_var) {  
  if ("target" %in% names(filter_list)) {
    target <- filter_list[["target"]]
    filter <- filter_list[["filter"]]
    d <- data_list[[target]]

    checkmate::test_subset(target, names(data_list))
    mask <- apply_filter(data_list[[target]], filter)
    res_subject_set <- d[[subjid_var]][mask]
  } else if ("type" %in% names(filter_list)) {
    type <- filter_list[["type"]]
    filter <- filter_list[["filter"]]
    if (type == "intersect") {
      checkmate::assert_list(filter, min.len = 1)
      for (idx in seq_along(filter)) {
        if (idx == 1) {
          res_subject_set <- compute_subject_set_from_filter(data_list, filter[[idx]], subjid_var)
        } else {
          res_subject_set <- intersect(res_subject_set, compute_subject_set_from_filter(data_list, filter[[idx]], subjid_var))
        }
      }
    } else if (type == "union") {
      checkmate::assert_list(filter, min.len = 1)
      for (idx in seq_along(filter)) {
        if (idx == 1) {
          res_subject_set <- compute_subject_set_from_filter(data_list, filter[[idx]], subjid_var)
        } else {
          res_subject_set <- union(res_subject_set, compute_subject_set_from_filter(data_list, filter[[idx]], subjid_var))
        }
      }
    } else if (type == "diff") {
      checkmate::assert_list(filter_list, len = 2)
      res_subject_set_1 <- compute_subject_set_from_filter(data_list, filter[[1]], subjid_var)
      res_subject_set_2 <- compute_subject_set_from_filter(data_list, filter[[2]], subjid_var)
      res_subject_set <- setdiff(res_subject_set_1, res_subject_set_2)
    } else {
      stop("Unknown type")
    }
  } else {
    stop("Unknown element")
  }

  return(res_subject_set)
}

add_blockly_dependency <- function() {
  htmltools::htmlDependency(
    name = "filter_blockly",
    version = utils::packageVersion("dv.manager"),
    src = app_sys("filter/"),
    script = c("blockly/package/blockly_compressed.js", "blockly/package/blocks_compressed.js", "blockly/package/msg/en.js", "filter.js")
  )
}

new_filter_ui <- function(id, data) {
  ns <- shiny::NS(id)

  previous_state <- shiny::restoreInput(ns("json"), "null")
  current_filter_data <- jsonlite::toJSON(get_filter_data(data))
  payload <- sprintf("{\"state\": %s, \"data\": %s}", previous_state, current_filter_data)

  shiny::div(
    add_blockly_dependency(),
    shiny::h1("FILTER"),
    shiny::tags[["button"]](id = "gen_code", "Gen Code"),
    shiny::div(
      id = ns("filter_container"),
      style = "height: 480px; width: 100wh;",
      shiny::tags[["script"]](
        type = "application/json",
        payload        
      ),
      shiny::tags[["script"]](
        paste0(
          sprintf("var filter = filterBlockly.init('%s');", ns("filter_container")), "\n",
          "const send_code = function() {
          const code = filterBlockly.get_code(filter.workspace, filter.generator);
          document.getElementById('code').innerText = code;
          Shiny.setInputValue('", {ns("json")}, "', code, {priority: 'event'});
};

 document.getElementById('gen_code').addEventListener('click', send_code);
"
        )
      )
    ),
    shiny::div(
      id = "code"
    )
  )
}

new_filter_server <- function(id, data) {
  mod <- function(input, output, session) {
    
  res <- shiny::reactive({
    if (checkmate::test_string(input[["json"]], min.chars = 1)) {
      parsed_json <- jsonlite::fromJSON(input[["json"]], simplifyVector = FALSE)
      list(
        state = parsed_json[["state"]] %||% NA_character_,
        filters = parsed_json[["filters"]] %||% NA_character_,        
        raw = input[["json"]]
      )
    } else {
      list(
        state = NA_character_,
        dataset_filters = NA_character_,
        subject_filters = NA_character_,
        raw = NA_character_
      )
    }
  })
  return(res)
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
                            )) {
  ui <- function(request) {
    shiny::fluidPage(      
      shiny::bookmarkButton(),
      new_filter_ui("filter", data),

            shiny::verbatimTextOutput("output_filtered_ds"),
            shiny::verbatimTextOutput("output_filtered_sbj")
    )
  }

  server <- function(input, output, session) {
    x <- new_filter_server("filter", shiny::reactive(data))
    selected_data <- "D1"

    output[["output_json"]] <- shiny::renderPrint({
      shiny::req(!is.na(x()))
      jsonlite::fromJSON(x(), simplifyVector = FALSE)
    })

    filtered_datasets <- shiny::reactive({
      shiny::req(!is.null(x()[["filters"]][[selected_data]][["dataset"]]))
      filters <- x()[["filters"]][[selected_data]][["dataset"]]
      ds <- data[[selected_data]]
      mask <- create_masks_from_dataset_filters(ds, filters)
      apply_masks_to_datasets(ds, mask)
    })

    filtered_subjects <- shiny::reactive({
      shiny::req(!is.null(x()[["filters"]][[selected_data]][["subject"]]))
      filters <- x()[["filters"]][[selected_data]][["subject"]]
      ds <- data[[selected_data]]
      subjid_set <- as.character(compute_subject_set_from_filter(ds, filters, "USUBJID"))
      subjid_set
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

  message("A")

  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url"
  )
}

mock_new_filter_modal <- function(data = list(
                              "D1" = list(
                                adsl = get_pharmaverse_data("adsl"),
                                adae = get_pharmaverse_data("adae")
                              ),
                              "D2" = list(
                                adsl = get_pharmaverse_data("adsl"),
                                adae = get_pharmaverse_data("adae")
                              )
                            )) {
  ui <- function(request) {
    shiny::fluidPage(
      #unnamespaced_filter_modal(new_filter_ui("filter", data)),            
      new_filter_ui("filter", data),            
      shiny::verbatimTextOutput("output_filtered_ds")
    )
  }

  server <- function(input, output, session) {
    x <- new_filter_server("filter", shiny::reactive(data))
    selected_data <- "D1"

    output[["output_json"]] <- shiny::renderPrint({
      shiny::req(!is.na(x()))
      jsonlite::fromJSON(x(), simplifyVector = FALSE)
    })

    filtered_datasets <- shiny::reactive({
      shiny::req(!is.na(x()))
      filters <- jsonlite::fromJSON(x(), simplifyVector = FALSE)[[selected_data]][["dataset"]]
      ds <- data[[selected_data]]
      mask <- create_masks_from_dataset_filters(ds, filters)
      apply_masks_to_datasets(ds, mask)
    })

    filtered_subjects <- shiny::reactive({
      shiny::req(!is.na(x()))
      filters <- jsonlite::fromJSON(input[["json"]], simplifyVector = FALSE)[[selected_data]][["subject"]]
      ds <- data[[selected_data]]
      subjid_set <- as.character(compute_subject_set_from_filter(ds, filters, "USUBJID"))
      subjid_set
    })

    output[["output_filtered_ds"]] <- shiny::renderPrint({
      tryCatch(filtered_datasets(), error = function(e)"A")
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
  shiny::div(
        shiny::tags[["label"]]("Show filter", "for" = "filter-checkbox", class = "btn"),
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
            width: 100%;
            height: 100%;
            background-color: rgba(0, 0, 0, 0.5);
            display: none; /* Hidden by default */
            justify-content: center;
            align-items: center;            
        }

        /* Modal content style */
        #filter_modal {
            background-color: #fff;
            padding: 20px;
            border-radius: 5px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
            height: 100vh;
            width: 100vw;
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
          shiny::div(id = "filter_modal",
          shiny::h4("Filter"),
          filter_ui,
          shiny::tags[["label"]]("Close filter", "for" = "filter-checkbox", id = "filter_close-btn")
          )
        )        
      )
}