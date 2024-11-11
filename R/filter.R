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

get_dataset_filters_ui <- function(dataset_filters_info, ns) {
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
  return(res)
}

get_subject_level_ui <- function(id) {
  dv.filter::data_filter_ui(id)
}

get_subject_level_server <- function(
    id,
    data) {
  if (!isTRUE(getOption("new_filter_switch"))) dv.filter::data_filter_server(id, data)
}

get_dataset_filters_server <- function(datasets_filters_info, data_list) {
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
