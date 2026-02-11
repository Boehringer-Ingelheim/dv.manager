app_server <- function(input = NULL, output = NULL, session = NULL) {
  opts <- list(
    "module_info" = get_config("module_info"),
    "data" = get_config("data"),
    "filter_data" = get_config("filter_data"),
    "filter_key" = get_config("filter_key"),
    "startup_msg" = get_config("startup_msg"),
    "reload_period" = get_config("reload_period"),
    "filter_info" = get_config("filter_info"),
    "enable_subgroup" = get_config("subgroup")[["enable"]]
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
    "reload_period" = get_config("reload_period"),
    "filter_info" = get_config("filter_info"),
    "enable_subgroup" = get_config("subgroup")[["enable"]]
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
      session$sendCustomMessage("set_active_tab", list(tab_id = selected))
    }
  )

  module_server <- opts[["module_info"]][["server"]]
  module_meta <- opts[["module_info"]][["meta"]]
  module_names <- opts[["module_info"]][["module_name"]]
  module_hierarchy_list <- opts[["module_info"]][["hierarchy"]]
  dataset_lists <- opts[["data"]]
  subject_filter_dataset_name <- opts[["filter_data"]]
  filter_key_var <- opts[["filter_key"]]
  startup_msg <- opts[["startup_msg"]]
  reload_period <- opts[["reload_period"]]
  filter_info <- opts[["filter_info"]]
  enable_subgroup <- opts[["enable_subgroup"]]

  ######################################

  # Check if dataset must be reloaded in the next session
  check_data_reload(reload_period)

  if (!is.null(startup_msg)) {
    shiny::showModal(startup_msg)
  }

  log_inform(glue::glue("Filtering key: {filter_key_var}"))

  url_parameters <- shiny::reactiveVal()

  if (length(dataset_lists) > 1) {
    shinyjs::toggle(id = "dataset_selector")
  }

  shiny::observe({
    url_parameters(shiny::parseQueryString(session$clientData$url_search))
    if (!is.null(url_parameters()[["data_name"]])) {
      shiny::updateSelectizeInput(inputId = "selector", selected = url_parameters()[["data_name"]])
    }
  })

  selected_dataset_list <- shiny::reactive({
    dataset_list_name <- input$selector
    shiny::req(checkmate::test_string(dataset_list_name, min.chars = 1))
    assert(dataset_list_name %in% names(dataset_lists))

    if (is.function(dataset_lists[[dataset_list_name]])) {
      res <- add_date_range(dataset_lists[[dataset_list_name]]())
    } else {
      res <- add_date_range(dataset_lists[[dataset_list_name]])
    }

    attr(res, "dataset_list_name") <- dataset_list_name
    res
  })

  if (enable_subgroup) {
    apply_subgroups <- mod_subgroup_server(
      ID$SUBGROUP,
      selected_dataset_list,
      subject_filter_dataset_name,
      filter_key_var
    )
  }

  unfiltered_dataset_list <- shiny::reactive({
    r_selected_dataset_list <- selected_dataset_list()

    if (enable_subgroup) {
      r_apply_subgroups <- apply_subgroups()
      res_apply_subgroups <- r_apply_subgroups(r_selected_dataset_list, subject_filter_dataset_name, filter_key_var)

      for (error in res_apply_subgroups[["errors"]]$get_messages()) {
        shiny::showNotification(error, type = "warning")
      }

      subgrouped_dataset_list <- res_apply_subgroups[["dataset_list"]]
    } else {
      subgrouped_dataset_list <- r_selected_dataset_list
    }
    attr(subgrouped_dataset_list, "dataset_list_name") <- attr(r_selected_dataset_list, "dataset_list_name")
    subgrouped_dataset_list
  })

  dataset_list_filter <- new_filter_server(
    ID$FILTER,
    unfiltered_dataset_list,
    subject_filter_dataset_name,
    filtered_dataset_list
  )

  filtered_dataset_list <- shiny::reactive({
    unfiltered_dataset_list_r <- shiny::isolate(unfiltered_dataset_list())
    dataset_list_filter_r <- dataset_list_filter()

    res <- apply_filter_to_dataset_list(unfiltered_dataset_list_r, dataset_list_filter_r, filter_key_var)

    error_list <- res$error_list
    fd <- res$fd

    shiny::req(
      !error_list$any_has_class(FC$ERRORS$FILTER_IS_NA$class) &&
        !error_list$any_has_class(FC$ERRORS$UNFILTERED_DATASET_LIST_NAME_FILTER_DATASET_LIST_NAME_MISMATCH$class)
    )

    for (error_message in error_list$get_messages()) {
      warning(error_message)
      shiny::showNotification(error_message, type = "error")
    }

    fd
  })

  shiny::observeEvent(filtered_dataset_list(), {
    # Not convinced as it is set somewhere else (app_ui and filter) (gvbu)
    session[["sendCustomMessage"]]("dv_manager_hide_overlay", list())
  })

  shiny::observeEvent(
    {
      input[[ID$NAV_HEADER]]
      dataset_list_filter() # FIXME: We depend on this because redrawing the filter replaces the elements on the screen and removes the hidden property
      # We don't want to redraw everytime we switch tabs an alternative to this strategy should be found (hovng)
    },
    {
      all_nm <- names(selected_dataset_list())
      current_tab <- input[[ID$NAV_HEADER]]

      if (!is.null(current_tab)) {
        used_ds <- used_datasets[[current_tab]]
      } else {
        used_ds <- NULL
      }

      if (!is.null(used_ds)) {
        used_nm <- intersect(used_datasets[[current_tab]], all_nm)
        unused_nm <- setdiff(all_nm, used_nm)
      } else {
        used_nm <- all_nm
        unused_nm <- character(0)
      }

      unused_nm <- setdiff(unused_nm, subject_filter_dataset_name) # Subject filter is never hid

      session$sendCustomMessage("show_hide_dataset_filters", list(id = ns(ID$FILTER), hidden = unused_nm))
    },
    ignoreNULL = FALSE
  )

  # This mimicks a reactive, by delaying the access to module_output
  # This is required for the modules to be able to read the output of other modules that are not yet declared

  module_output_fn <- function() {
    as_dv_manager_module_output_safe_list(module_output)
  }

  afmm <- list(
    data = dataset_lists,
    unfiltered_dataset = shiny::reactive({
      # log_warn("(Message for the module developer) afmm[[\"unfiltered_dataset\"]] will be deprecated in future versions. Please replace by afmm[[\"unfiltered_dataset_list\"]].") # nolintr
      unfiltered_dataset_list()
    }),
    unfiltered_dataset_list = unfiltered_dataset_list,
    filtered_dataset = shiny::reactive({
      #log_warn("(Message for the module developer) afmm[[\"filtered_dataset\"]] will be deprecated in future versions. Please replace by afmm[[\"filtered_dataset_list\"]].") # nolintr
      filtered_dataset_list()
    }),
    filtered_dataset_list = filtered_dataset_list,
    url_parameters = url_parameters,
    dataset_name = shiny::reactive({
      # log_warn("(Message for the module developer) afmm[[\"dataset_name\"]] will be deprecated in future versions. Please replace by afmm[[\"dataset_metadata\"]][[\"name\"]].") # nolintr
      input$selector
    }),
    dataset_metadata = list(
      name = shiny::reactive({
        attr(unfiltered_dataset_list(), "dataset_list_name")
      }),
      date_range = shiny::reactive(attr(unfiltered_dataset_list(), "date_range"))
    ),
    module_output = module_output_fn,
    module_names = module_names,
    utils = list(
      switch2 = function(selected) {
        .Defunct(
          "switch2mod",
          "switch2 is no longer available. Please check the documentation in switch2mod as it no longer accepts module names, but module ids." # nolint
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
        session$sendCustomMessage("set_active_tab", list(id = session[["ns"]](ID$NAV_HEADER), tab_id = selected))
      }
    ),
    filter_metadata = list(
      output = shiny::reactive({
        log_warn(
          "You are using afmm[['filter_metadata']][['output']]. This is not a public element and it may disappear or be modified without notice"
        )
        dataset_list_filter()
      })
    )
  )

  used_datasets <- list()

  module_output <- list()
  for (idx in seq_along(module_server)) {
    fn <- module_server[[idx]]
    id <- names(module_server)[[idx]]

    assert(is.character(id), "id must be a character")
    assert(is.function(fn), "fn must be a function")

    module_output[[id]] <- fn(afmm)
    used_datasets[[id]] <- module_meta[[id]][["dataset_info"]][["all"]]
  }

  # Dataset name and date

  output$dataset_name <- shiny::renderText({
    paste0("Dataset name: ", input$selector)
  })

  output[["dataset_date"]] <- shiny::renderText({
    date_range <- attr(unfiltered_dataset_list(), "date_range")

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
    shiny::showModal(create_info_modal(session = session, input = input, ns = ns))
  })
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
