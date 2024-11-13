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

  for (idx in seq_len(n_col)) {
    name <- nm_col[[idx]]
    col <- data[[name]]

    l <- list(name = name)

    if (is.character(col)) {
      l[c("value", "type")] <- list(unique(col), "character")
    } else if (is.factor(col)) {
      l[c("value", "type")] <- list(levels(col), "factor")
    } else if (is.integer(col)) {
      l[c("value", "type")] <- list(list(min = min(col), max = max(col)), "integer")
    } else if (is.double(col)) {
      l[c("value", "type")] <- list(list(min = min(col), max = max(col)), "integer")
    } else if (inherits(col, "POSIXct")) {
      d <- as.Date(col)
      l[c("value", "type")] <- list(list(min = min(d), max = max(d)), "Date")
    } else if (inherits(col, "Date")) {
      l[c("value", "type")] <- list(list(min = min(col), max = max(col)), "Date")
    }
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

new_filter_ui <- function(id, data) {
  ns <- shiny::NS(id)
  list(
    shiny::h1("FILTER"),
    shiny::div(
      id = ns("filter_container"),
      shiny::tags[["script"]](
        type = "application/json",
        jsonlite::toJSON(get_filter_data(data))
      )
    )
  )
}

new_filter_server <- function(id, data) {
  res <- shiny::reactive({
    data()
  })
  return(res)
}

mock_new_filter <- function() {
  data <- list(
    D1 = list(cars = mtcars, iris = iris)
  )

  ui <- function(request) {
    shiny::fluidPage(
      shiny::bookmarkButton(),
      new_filter_ui("filter", data),
      shiny::verbatimTextOutput("output")
    )
  }

  server <- function(input, output, session) {
    x <- new_filter_server("filter", shiny::reactive(data))
    output[["output"]] <- shiny::renderPrint({
      x()
    })
  }

  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url"
  )
}


apply_filter <- function(data, filter_parameters) {
  if (length(filter_parameters) == 0) {
    return(rep_len(TRUE, nrow(data)))
  }

  checkmate::assert_subset(c("type"), names(filter_parameters))
  type <- filter_parameters[["type"]]

  if (type %in% c("integer", "double", "date")) {
    checkmate::assert_subset(names(filter_parameters), c("type", "column", "value", "NAs"))

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
    checkmate::assert_set_equal(names(filter_parameters), c("type", "column", "value", "NAs"))

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
    checkmate::assert_set_equal(names(filter_parameters), c("type", "filter_list"))
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