#' Server side of the dv.manager
#'
#' @section data:
#' *data* is the fundamental data source for the application launched by the dv.manager
#'   - This data source is a named list of:
#'      - lists of data.frames as loaded by dv.loader
#'      - of functions that when called return a list of data.frames
#'   - Entries of the list will selected and then each of the data.frames will be dispatched to each module according to
#'    the code that defines the application
#'   - The data dispatched to the modules will be filtered according to a global *dv.filter* (see filter_data section)
#'   - The name of the selected dataset will appear in the sidebar, according to the name provided the list
#'   - The date of the dataset will be automatically displayed by selecting the earliest and the latest date of the
#'    composing data.frames.
#'     - *dv.manager* assumes that all data.frames within a dataset have an attribute *meta*, and within it,
#'      it will look for the entry *mtime*
#'     - This assumption is met when the dataset is loaded directly with dv.loader, nonetheless, this attributes may
#'      need to be created for new derived data.frames during preprocessing
#'     It is the programmer obligation to check that all datasets have this attribute and that is correct.
#'
#' @section filter_data:
#' The datasets in *data* will be filtered in the following way:
#'   1. A dataset for global filtering is defined by *filter_data*
#'   1. This dataset is filtered by *dv.filter* and the user input
#'   1. The remaining USUBJIDs (by default, see *filter_key* field in run_app) are used to filter the rest of the
#'    datasets
#'
#' @section module_list:
#' *module_list* will contain a list of the modules to be used in the application. Each entry in the list will contain
#'  the following fields:
#' - The name of each entry of the list (unique), will be used in the selection menus, therefore must be meaningful and
#'  intended for human use.
#' - **ui**: A UI function that will need a single parameter, id. Almost always it will be the other entry module_id
#' - **server**: A function that will receive a single parameter. See `vignette("arguments_from_module_manager")`
#' - **module_id**: A unique alphanumeric string that will identify the module, it will be used internally therefore it
#'  is intended for machine use.
#' Additionally, it will expect a value **input$mod_select** to identify which module is currently selected. Here it is
#'  only used to produce a log with the selection
#' but it will be used for selecting in the UI conditional panels.
#'
#' @param id id when app_server is used as a module
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#'
#' @keywords internal

app_server <- function(input = NULL, output = NULL, session = NULL) {
  opts <- list(
    "module_info" = get_config("module_info"),
    "data" = get_config("data"),
    "filter_data" = get_config("filter_data"),
    "filter_key" = get_config("filter_key"),
    "startup_msg" = get_config("startup_msg"),
    "reload_period" = get_config("reload_period"),
    "enable_dataset_filter" = get_config("enable_dataset_filter")
  )

  app_server_(input, output, session, opts)
}

app_server_module <- function(id) {
  opts <- list(
    "module_info" = get_config("module_info"),
    "data" = get_config("data"),
    "filter_data" = get_config("filter_data"),
    "filter_key" = get_config("filter_key"),
    "startup_msg" = get_config("startup_msg"),
    "reload_period" = get_config("reload_period")
  )
  shiny::moduleServer(id = id, module = function(input, output, session) app_server_(input, output, session, opts))
}

# nolint start cyclocomp_linter
app_server_ <- function(input, output, session, opts) {
  ns <- session[["ns"]]

  # Inject tools available for the rest of modules
  session$userData$manager_utils <- list(
    switch_tab = function(selected) {
      .Defunct(
        "affm[[\"utils\"]][[\"switch_function\"]]",
        msg = "Switch function has been moved to the list of arguments passed to the module"
      )
      shiny::updateTabsetPanel(session, "__tabset_0__", selected)
    }
  )

  module_server <- opts[["module_info"]][["server_list"]]
  module_meta <- opts[["module_info"]][["meta_list"]]
  module_names <- opts[["module_info"]][["module_name_list"]]
  module_hierarchy_list <- opts[["module_info"]][["hierarchy_list"]]
  data <- opts[["data"]]
  filter_data <- opts[["filter_data"]]
  filter_key <- opts[["filter_key"]]
  startup_msg <- opts[["startup_msg"]]
  reload_period <- opts[["reload_period"]]
  enable_dataset_filter <- opts[["enable_dataset_filter"]]


  ## Feature switch for new data filter

  use_new_filter_switch <- isTRUE(get_config("dv.manager.use.blockly.filter"))

  ######################################

  datasets_filters_info <- get_dataset_filters_info(data, filter_data)

  # Check if dataset must be reloaded in the next session
  check_data_reload(reload_period)

  if (!is.null(startup_msg)) {
    shiny::showModal(startup_msg)
  }

  log_inform(glue::glue("Filtering key: {filter_key}"))

  url_parameters <- shiny::reactiveVal()

  if (length(data) > 1) {
    shinyjs::toggle(id = "dataset_selector")
  }

  shiny::observe({
    url_parameters(shiny::parseQueryString(session$clientData$url_search))
    if (!is.null(url_parameters()[["data_name"]])) {
      shiny::updateSelectizeInput(inputId = "selector", selected = url_parameters()[["data_name"]])
    }
  })

  unfiltered_dataset <- shinymeta::metaReactive({
    shiny::req(input$selector)
    if (is.function(data[[input$selector]])) {
      add_date_range(data[[input$selector]]())
    } else {
      add_date_range(data[[input$selector]])
    }
  })


  if (use_new_filter_switch) {
    dataset_filter <- new_filter_server("filter", shiny::reactive({input$selector}))

    filtered_dataset <- shinymeta::metaReactive({

      if (isTRUE(is.na(dataset_filter()[["filters"]]))) {
        return(unfiltered_dataset())
      }

      current_server_dataset_name <- shiny::isolate(input$selector)
      current_client_dataset_name <- dataset_filter()[["filters"]][["dataset_name"]]
      shiny::req(current_server_dataset_name == current_client_dataset_name)

      ds <- unfiltered_dataset()

      # JS client may not fully control that the selected filter is correct.
      # (e.g. blocks that must have children, like `and`,  has them).
      # The server must also control for those (and it actually does it right now). See asserts in the create*_masks functions
      # This is required as the server must NOT trust the client and always assert that the filter is correct.
      # Errors must be caught here as downstream modules may crash when an errors happens inside one of the observes
      # Errors should be controlled inside the observes by modules themselves, unfortunately it is not always the case

      fd <- tryCatch({
        ds_mask <- create_datasets_filter_masks(ds, dataset_filter()[["filters"]][["datasets_filter"]])
        apply_masks_to_datasets(ds, ds_mask)
      },
        error = function(e) {
          msg <- paste("Filter not applied. Error found:\n", e[["message"]])
          warning(msg)
          shiny::showNotification(msg, type = "error")
          ds
        }
      )      


      # Check NA optimization in the future
      subject_set <- tryCatch({
        create_subject_set(ds, dataset_filter()[["filters"]][["subject_filter"]], filter_key)
      },
        error = function(e) {
          msg <- paste("Filter not applied. Error found:\n", e[["message"]])
          warning(msg)
          shiny::showNotification(msg, type = "error")
          NA_character_
      })      

      if (!identical(subject_set, NA_character_)) {
        fd <- apply_subject_set_to_datasets(fd, subject_set, filter_key)
      }

      fd
    })
  } else {
    global_filtered_values <- dv.filter::data_filter_server(
      "global_filter",
      shiny::reactive(unfiltered_dataset()[[filter_data]])
    )


    if (enable_dataset_filter) {
      log_inform("Dataset filter server")

      dataset_filters <- local({
        l <- vector(mode = "list", length = length(datasets_filters_info))
        names(l) <- names(datasets_filters_info)
        for (idx in seq_along(datasets_filters_info)) {
          l[[idx]] <- local({
            curr_dataset_filter_info <- datasets_filters_info[[idx]]
            dv.filter::data_filter_server(
              curr_dataset_filter_info[["id"]],
              shiny::reactive({
                unfiltered_dataset()[[curr_dataset_filter_info[["name"]]]] %||% data.frame()
              })
            )
          })
        }

        l
      })

      filtered_dataset <- shinymeta::metaReactive({
        # dv.filter returns a logical vector. This contemplates the case of empty lists
        shiny::req(is.logical(global_filtered_values()))

        # Depend on all datasets
        purrr::walk(dataset_filters, ~ .x())

        # We do not react to changed in unfiltered dataset, otherwise when a dataset changes
        # We filter the previous dataset which in the best case produces and extra reactive beat
        # and in the worst case produces an error in (mvbc)
        # We don't want to control the error in (mvbc) because filtered dataset only changes when filter changes
        ufds <- shiny::isolate(unfiltered_dataset())

        curr_dataset_filters <- dataset_filters[intersect(names(dataset_filters), names(ufds))]

        # Current dataset must be logical with length above 0
        # Check dataset filters check all datafilters are initialized
        purrr::walk(curr_dataset_filters, ~ shiny::req(checkmate::test_logical(.x(), min.len = 0)))

        filtered_key_values <- ufds[[filter_data]][[filter_key]][global_filtered_values()]

        fds <- ufds

        # Single dataset filtering
        fds[names(curr_dataset_filters)] <- purrr::imap(
          fds[names(curr_dataset_filters)],
          function(val, nm) {
            # (mvbc)
            labels <- get_lbls(fds[[nm]])
            current_fds <- fds[[nm]][dataset_filters[[nm]](), , drop = FALSE]
            set_lbls(current_fds, labels)
          }
        )

        # Global dataset filtering
        global_filtered <- purrr::map(
          fds, function(current_ds) {
            mask <- current_ds[[filter_key]] %in% filtered_key_values
            labels <- get_lbls(current_ds)
            current_ds <- current_ds[mask, , drop = FALSE]
            set_lbls(current_ds, labels)
          }
        )
      })

      tab_ids <- c("__tabset_0__", names(opts[["module_info"]][["tab_group_names"]]))
      shiny::observeEvent(
        {
          purrr::map(tab_ids, ~ input[[.x]])
        },
        {
          current_tab <- "__tabset_0__"
          zero_tabs <- length(input[["__tabset_0__"]]) == 0
          if (!zero_tabs) {
            while (!current_tab %in% opts[["module_info"]][["module_id_list"]]) {
              current_tab <- input[[current_tab]]
            }
          }

          used_ds <- used_datasets[[current_tab]]
          all_nm <- names(datasets_filters_info)
          if (!zero_tabs && !is.null(used_ds)) {
            used_nm <- intersect(used_datasets[[current_tab]], names(datasets_filters_info))
            unused_nm <- setdiff(all_nm, used_nm)
          } else {
            used_nm <- all_nm
            unused_nm <- character(0)
          }

          for (nm in unused_nm) {
            shinyjs::hide(datasets_filters_info[[nm]][["id_cont"]])
          }

          for (nm in used_nm) {
            shinyjs::show(datasets_filters_info[[nm]][["id_cont"]])
          }
        }
      )
    } else {
      log_inform("Single filter server")

      filtered_dataset <- shinymeta::metaReactive({
        # dv.filter returns a logical vector. This contemplates the case of empty lists
        shiny::req(is.logical(global_filtered_values()))
        log_inform("New filter applied")
        filtered_key_values <- unfiltered_dataset()[[filter_data]][[filter_key]][global_filtered_values()] # nolint
        purrr::map(
          unfiltered_dataset(),
          ~ dplyr::filter(.x, .data[[filter_key]] %in% filtered_key_values) # nolint
        )
      })
    }
  }

  # Prepare module_output argument
  module_output_env <- rlang::current_env()
  module_output_func <- function() {
    return(base::get("module_output", envir = module_output_env))
  }

  module_args <- list(
    data = data,
    unfiltered_dataset = unfiltered_dataset,
    filtered_dataset = filtered_dataset,
    url_parameters = url_parameters,
    dataset_name = shiny::reactive({
      rlang::warn("afmm[[\"dataset_name\"]] will be deprecated in future versions. Please replace by afmm[[\"dataset_metadata\"]][[\"name\"]].") # nolintr
      input$selector
    }),
    dataset_metadata = list(
      name = shiny::reactive(input$selector),
      date_range = shiny::reactive(attr(unfiltered_dataset(), "date_range"))
    ),
    module_output = module_output_func,
    module_names = module_names,
    utils = list(
      switch2 = function(selected) {
        .Deprecated(
          "switch2mod",
          "switch2 is being deprecated in favor of switch2mod. switch2mod directly works on module ids and supports switching to nested tabs." # nolint
        )
        if (!checkmate::test_string(selected, min.chars = 1)) {
          log_warn("selected must be a non-empty string")
          return(NULL)
        }

        if (!checkmate::test_string(selected, min.chars = 1)) {
          log_warn("selected must be a non-empty string")
          return(NULL)
        }

        if (!selected %in% module_names) {
          log_warn(
            paste0("\"", selected, "\"", " is not a module name. switch does not support switching to nested tabs")
          )
          return(NULL)
        }

        shiny::updateTabsetPanel(session, "__tabset_0__", names(module_names)[module_names == selected])
      },
      switch2mod = function(selected) {
        if (!checkmate::test_string(selected, min.chars = 1)) {
          log_warn("selected must be a non-empty string")
          return(NULL)
        }

        if (!selected %in% names(module_hierarchy_list)) {
          log_warn("selected must be a module id")
          return(NULL)
        }

        this_hierarchy_value <- module_hierarchy_list[[selected]]
        this_hierarchy_names <- names(this_hierarchy_value)

        for (idx in seq_along(this_hierarchy_value)) {
          tab_value <- this_hierarchy_value[[idx]]
          tabset_id <- this_hierarchy_names[[idx]]
          shiny::updateTabsetPanel(session, tabset_id, tab_value)
        }
      }
    )
  )

  used_datasets <- list()

  module_output <- list()
  for (srv in module_server) {
    mod_id <- srv[["module_id"]]
    srv_fun <- srv[["server"]]

    module_output[[mod_id]] <- srv_fun(module_args)
    used_datasets[[mod_id]] <- module_meta[[mod_id]][["meta"]][["dataset_info"]][["all"]]
  }




  #### Report modal

  # REPORT IS DEACTIVATED

  # nolint start

  # shiny::observeEvent(input$open_report_modal, {
  #   shiny::showModal(create_report_modal(ns = ns))
  # })
  #
  # output$download_script <- shiny::downloadHandler(
  #   filename = glue::glue("report_{format.Date(Sys.time(), \"%d%m%Y-%H%M%S\")}.zip"),
  #   contentType = "application/zip",
  #   content = function(zip_file) {
  #     # We begin by closing the modal that created this event.
  #     shiny::removeModal()
  #
  #     report_create_bundle(
  #       title = input[["report_title"]],
  #       summary = input[["report_summary"]],
  #       module_code = purrr::imap(module_output, ~ attr(.x, "code")),
  #       module_names = purrr::map(module_list, "module_id"),
  #       unfiltered_dataset = unfiltered_dataset,
  #       filtered_dataset = filtered_dataset,
  #       load_function = attr(data[[input[["selector"]]]], "code"),
  #       do_render = input[["report_do_render"]],
  #       include_code = input[["report_include_code"]],
  #       render_format = input[["report_render_format"]],
  #       zip_file = zip_file
  #     )
  #   }
  # )

  # nolint end

  # Dataset name and date

  output$dataset_name <- shiny::renderText({
    paste0("Dataset name: ", input$selector)
  })

  output$dataset_date <- shiny::renderText({
    date_range <- attr(unfiltered_dataset(), "date_range")

    if (!any(is.na(date_range))) {
      date_range <- format(date_range, "%Y-%b-%d (%Z)")
      if (date_range[1] != date_range[2]) {
        date_string <- glue::glue("{date_range[1]} - {date_range[2]}")
      } else {
        date_string <- glue::glue("{date_range[1]}")
      }
    } else {
      date_string <- glue::glue("Date unavailable")
    }
    paste0("Dataset date: ", date_string)
  })

  #### Options modal

  shiny::observeEvent(input$open_options_modal, {
    shiny::showModal(create_options_modal(session = session, input = input, ns = ns))
  })

  shiny::observeEvent(
    input$change_theme,
    session$setCurrentTheme(get_app_theme(input$change_theme))
  )
}

# nolint end cyclocomp_linter

# Convoluted way of having a testable server function
# TestServer reads the caller environment
# Therefore, when running a wrapped function like
# nolint start
# function(opts){
#    function(input,output,session){
#     # This environment is returned not the one inside
#      app_server_(input, output, session, opts)
#   }
# }
# No instropection of the internals of app_server_ is possible
# nolint end

app_server_test <- function(opts) {
  # Remove opts argument. It will be taken from this closure
  f <- rlang::new_function(rlang::exprs(input = , output = , session = ), rlang::fn_body(app_server_))
  f
}
