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
    "filter_info" = get_config("filter_info")
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
    "filter_info" = get_config("filter_info")
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

  use_dataset_filter <- filter_info[["filter_type"]] == FILTER$TYPE$DATASETS
  use_blockly_filter <- filter_info[["filter_type"]] == FILTER$TYPE$BLOCKLY

  ######################################

  datasets_filters_info <- get_dataset_filters_info(dataset_lists, subject_filter_dataset_name)

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

  apply_subgroups <- mod_subgroup_server(ID$SUBGROUP, unfiltered_dataset_list, subject_filter_dataset_name, filter_key_var)

  unfiltered_dataset_list <- shiny::reactive({
    dataset_list_name <- input$selector
    r_apply_subgroups <- apply_subgroups()
    shiny::req(checkmate::test_string(dataset_list_name, min.chars = 1))
    assert(dataset_list_name %in% names(dataset_lists))

    if (is.function(dataset_lists[[dataset_list_name]])) {
      selected_dataset_list <- add_date_range(dataset_lists[[dataset_list_name]]())
    } else {
      selected_dataset_list <- add_date_range(dataset_lists[[dataset_list_name]])
    }

    res_apply_subgroups <- r_apply_subgroups(selected_dataset_list, subject_filter_dataset_name, filter_key_var)

    for (error in res_apply_subgroups[["errors"]]) {
      shiny::showNotification(error, type = "warning")
    }

    selected_dataset_list <- res_apply_subgroups[["dataset_list"]]
    attr(selected_dataset_list, "dataset_list_name") <- dataset_list_name
    selected_dataset_list
  })

  if (use_blockly_filter) {
    dataset_filter <- new_filter_server(ID$FILTER, unfiltered_dataset_list, subject_filter_dataset_name, filtered_dataset_list)

    filtered_dataset_list <- shiny::reactive({
      ufd <- unfiltered_dataset_list()

      shiny::req(!is.na(dataset_filter()))

      safe_dsf <- as_safe_list(dataset_filter())

      if (isTRUE(is.na(safe_dsf[["parsed"]]))) {
        return(ufd)
      }

      safe_filters <- as_safe_list(safe_dsf[["parsed"]][["filters"]])

      current_server_dataset_list_name <- attr(ufd, "dataset_list_name")
      current_client_dataset_name <- safe_dsf[["parsed"]][["dataset_list_name"]]
      shiny::req(current_server_dataset_list_name == current_client_dataset_name)

      ds <- ufd

      # JS client may not fully control that the selected filter is correct.
      # (e.g. blocks that must have children, like `and`,  has them).
      # The server must also control for those (and it actually does it right now). See asserts in the create*_masks functions
      # This is required as the server must NOT trust the client and always assert that the filter is correct.
      # Errors must be caught here as downstream modules may crash when an errors happens inside one of the observes
      # Errors should be controlled inside the observes by modules themselves, unfortunately it is not always the case

      fd <- tryCatch(
        {
          dataset_filter_info <- create_dataset_filter_info(ds, safe_filters[["datasets_filter"]])
          apply_dataset_filter_info(ds, dataset_filter_info)
        },
        error = function(e) {
          msg <- paste("Filter not applied. Error found:\n", e[["message"]])
          warning(msg)
          shiny::showNotification(msg, type = "error")
          ds
        }
      )

      # Check NA optimization in the future
      fd <- tryCatch(
        {
          subject_filter_info <- create_subject_filter_info(ds, safe_filters[["subject_filter"]], filter_key_var)
          if (!identical(subject_filter_info, NA_character_)) {
            apply_subject_filter_info(fd, subject_filter_info, filter_key_var)
          } else {
            fd
          }
        },
        error = function(e) {
          msg <- paste("Filter not applied. Error found:\n", e[["message"]])
          warning(msg)
          shiny::showNotification(msg, type = "error")
          ds
        }
      )

      fd
    })

    shiny::observeEvent(
      {
        input[[ID$NAV_HEADER]]
        dataset_filter() # FIXME: We depend on this because redrawing the filter replaces the elements on the screen and removes the hidden property
        # We don't want to redraw everytime we switch tabs an alternative to this strategy should be found (hovng)
      },
      {
        all_nm <- names(datasets_filters_info)
        current_tab <- input[[ID$NAV_HEADER]]

        if (!is.null(current_tab)) {
          used_ds <- used_datasets[[current_tab]]
        } else {
          used_ds <- NULL
        }

        if (!is.null(used_ds)) {
          used_nm <- intersect(used_datasets[[current_tab]], names(datasets_filters_info))
          unused_nm <- setdiff(all_nm, used_nm)
        } else {
          used_nm <- all_nm
          unused_nm <- character(0)
        }

        session$sendCustomMessage("show_hide_dataset_filters", list(id = ns(ID$FILTER), hidden = unused_nm))
      },
      ignoreNULL = FALSE
    )
  } else {
    global_filtered_values <- dv.filter::data_filter_server(
      "global_filter",
      shiny::reactive(unfiltered_dataset_list()[[subject_filter_dataset_name]])
    )


    if (use_dataset_filter) {
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
                unfiltered_dataset_list()[[curr_dataset_filter_info[["name"]]]] %||% data.frame()
              })
            )
          })
        }

        l
      })

      filtered_dataset_list <- shiny::reactive({
        # dv.filter returns a logical vector. This contemplates the case of empty lists
        shiny::req(is.logical(global_filtered_values()))

        # Depend on all datasets
        purrr::walk(dataset_filters, ~ .x())

        # We do not react to changed in unfiltered dataset, otherwise when a dataset changes
        # We filter the previous dataset which in the best case produces and extra reactive beat
        # and in the worst case produces an error in (mvbc)
        # We don't want to control the error in (mvbc) because filtered dataset only changes when filter changes
        ufds <- shiny::isolate(unfiltered_dataset_list())

        curr_dataset_filters <- dataset_filters[intersect(names(dataset_filters), names(ufds))]

        # Current dataset must be logical with length above 0
        # Check dataset filters check all datafilters are initialized
        purrr::walk(curr_dataset_filters, ~ shiny::req(checkmate::test_logical(.x(), min.len = 0)))

        filtered_key_values <- ufds[[subject_filter_dataset_name]][[filter_key_var]][global_filtered_values()]

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
            mask <- current_ds[[filter_key_var]] %in% filtered_key_values
            labels <- get_lbls(current_ds)
            current_ds <- current_ds[mask, , drop = FALSE]
            set_lbls(current_ds, labels)
          }
        )
      })

      shiny::observeEvent(
        {
          input[[ID$NAV_HEADER]]
        },
        {
          all_nm <- names(datasets_filters_info)
          current_tab <- input[[ID$NAV_HEADER]]

          if (!is.null(current_tab)) {
            used_ds <- used_datasets[[current_tab]]
          } else {
            used_ds <- NULL
          }

          if (!is.null(used_ds)) {
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
        },
        ignoreNULL = FALSE
      )
    } else {
      log_inform("Single filter server")

      filtered_dataset_list <- shiny::reactive({
        # dv.filter returns a logical vector. This contemplates the case of empty lists
        shiny::req(is.logical(global_filtered_values()))
        log_inform("New filter applied")
        filtered_key_values <- unfiltered_dataset_list()[[subject_filter_dataset_name]][[filter_key_var]][global_filtered_values()] # nolint
        purrr::map(
          unfiltered_dataset_list(),
          ~ dplyr::filter(.x, .data[[filter_key_var]] %in% filtered_key_values) # nolint
        )
      })
    }
  }

  # This mimicks a reactive, by delaying the access to module_output
  # This is required for the modules to be able to read the output of other modules that are not yet declared

  module_output_fn <- function() {
    as_dv_manager_module_output_safe_list(module_output)
  }

  afmm <- list(
    data = dataset_lists,
    unfiltered_dataset = shiny::reactive({
      log_warn("(Message for the module developer) afmm[[\"unfiltered_dataset\"]] will be deprecated in future versions. Please replace by afmm[[\"unfiltered_dataset_list\"]].") # nolintr
      unfiltered_dataset_list()
    }),
    unfiltered_dataset_list = unfiltered_dataset_list,
    filtered_dataset = shiny::reactive({
      log_warn("(Message for the module developer) afmm[[\"filtered_dataset\"]] will be deprecated in future versions. Please replace by afmm[[\"filtered_dataset_list\"]].") # nolintr
      filtered_dataset_list()
    }),
    filtered_dataset_list = filtered_dataset_list,
    url_parameters = url_parameters,
    dataset_name = shiny::reactive({
      log_warn("(Message for the module developer) afmm[[\"dataset_name\"]] will be deprecated in future versions. Please replace by afmm[[\"dataset_metadata\"]][[\"name\"]].") # nolintr
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
        log_warn("You are using afmm[['filter_metadata']][['output']]. This is not a public element and it may disappear or be modified without notice")
        dataset_filter()
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

  output$dataset_date <- shiny::renderText({
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


MAX_CATEGORIES <- 10

mod_subgroup_ui <- function(id, subject_filter_dataset_name) {
  ns <- shiny::NS(id)
  list(
    shiny::textInput(ns("subgroup_name"), label = NULL, placeholder = "Enter subgroup name"),
    shiny::textInput(ns("subgroup_label"), label = NULL, placeholder = "Enter subgroup label"),
    shiny::div(
      style = "display: flex; align-items:baseline",
      shiny::span("Categories in subgroup", class = "mb-3 pe-1"),
      shiny::selectInput(ns("subgroup_cat_num"), NULL, choices = 2:MAX_CATEGORIES, selected = 2, width = "auto", selectize = FALSE)
    ),
    shiny::uiOutput(ns("subgroup_cat_container")),
    shiny::actionButton(ns("add_subgroup"), label = "Add subgroup", class = "btn-sm", style = "flex: 2;"),
    shiny::uiOutput(ns("subgroups")),
    new_filter_ui(ns("filter"), subject_filter_dataset_name, state = NULL)
  )
}

mod_subgroup_server <- function(id, unfiltered_dataset_list, subject_filter_dataset_name, filter_key_var) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    assign_btn_id <- "assign_button"
    view_btn_id <- "view_button"
    get_cat_label_id <- function(idx) paste0("label_", idx)
    label_others_id <- "label_others"

    subgroups <- shiny::reactiveVal(list())
    DEFAULT_CAT_ASSIGNMENTS <- vector(mode = "list", length = MAX_CATEGORIES + 1)
    cat_assignments <- shiny::reactiveVal(DEFAULT_CAT_ASSIGNMENTS)

    shiny::setBookmarkExclude({
      c(
        "add_subgroup", "subgroup_name", "subgroup_label", "accordion", "subgroup_cat_num", "check_subgroup",
        label_others_id, get_cat_label_id(1:MAX_CATEGORIES)
      )
    })

    subgroup_filter <- new_filter_server("filter", unfiltered_dataset_list, subject_filter_dataset_name, unfiltered_dataset_list, skip_dataset_filters = TRUE) # FIXME: Pass filtered one

    shiny::onBookmark(function(state) {
      state$values$subgroups <- I(subgroups())
    })

    shiny::onRestore(function(state) {
      subgroups(state$values$subgroups)
    })

    output[["subgroup_cat_container"]] <- shiny::renderUI({
      r_subgroup_cat_num <- as.integer(input[["subgroup_cat_num"]])
      shiny::req(checkmate::test_integer(r_subgroup_cat_num, lower = 2, len = 1, upper = MAX_CATEGORIES, any.missing = FALSE))

      ui <- vector(mode = "list", length = r_subgroup_cat_num)

      for (idx in seq_len(r_subgroup_cat_num - 1)) {
        label_id <- get_cat_label_id(idx)

        if (r_subgroup_cat_num > 2) {

          if (checkmate::test_string(cat_assignments()[[idx]], min.chars = 1)) {
            icon_state <- shiny::icon("circle-check")
          } else {
            icon_state <- shiny::icon("circle-xmark")
          }

          ui[[idx]] <- shiny::div(
            shiny::div(
              style = "display: flex; align-items:baseline; gap: 2px",
              shiny::textInput(ns(label_id), label = NULL, placeholder = paste("Label for category", idx), value = shiny::isolate(input[[label_id]])),
              shiny::tags[["button"]](
                shiny::icon("user-plus"),
                class = "btn btn-sm mb-3 btn-default",
                title = "Assign filtered subjects",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%d', { priority: 'event' })",
                  ns(assign_btn_id),
                  idx
                )
              ),
              icon_state
              # ,
              # shiny::tags[["button"]](
              #   shiny::icon("magnifying-glass"),
              #   class = "btn btn-sm mb-3 btn-default",
              #   title = "Load filter for assigned subjects",
              #   onclick = sprintf(
              #     "Shiny.setInputValue('%s', '%d', { priority: 'event' })",
              #     ns(view_btn_id),
              #     idx
              #   )
              # )
            )
          )
        } else {
          ui[[idx]] <- shiny::div(
            shiny::div(
              style = "display: flex",
              shiny::textInput(ns(label_id), label = NULL, placeholder = paste("Label for category", idx), value = shiny::isolate(input[[label_id]]))
            )
          )
        }
      }

      ui[[r_subgroup_cat_num]] <- shiny::div(
        style = "display: flex; justify-content: flex-end;",
        shiny::textInput(ns(label_others_id), label = NULL, placeholder = "Label for other subjects", value = shiny::isolate(input[[label_others_id]]))
      )
      ui
    })

    subgroups <- shiny::reactiveVal(list())

    output[["subgroups"]] <- shiny::renderUI({
      r_subgroups <- subgroups()
      tags <- shiny::tags

      badge_ui <- vector(mode = "list", length = length(r_subgroups))

      for (idx in seq_along(r_subgroups)) {
        subgroup_name <- names(r_subgroups)[[idx]]
        badge_ui[[idx]] <- tags[["span"]](subgroup_name, class = "badge w-auto bg-light text-dark")
      }

      tags[["div"]](
        class = "mt-2",
        style = "display:flex; flex-wrap: wrap; gap: .25rem",
        badge_ui

      )
    })

    shiny::observe({
      subgroup_filter() # FIXME: We depend on this because redrawing the filter replaces the elements on the screen and removes the hidden property
        # We don't want to redraw everytime we switch tabs an alternative to this strategy should be found (hovng)
        # The message sets the a property in the filter with hidden filters, hides and shows directly in the handler by applying the list. When
        # redrawing the property is consulted and the class is applied on draw.

      session$sendCustomMessage(
        "show_hide_dataset_filters",
        list(
          id = ns("filter"),
          hidden = setdiff(names(unfiltered_dataset_list()), subject_filter_dataset_name)
      )
    )
    })

    assert_or_notify_and_early_out <- function(expr, msg) {
      if (!expr) {
        log_warn(msg)
        shiny::showNotification(msg, type = "error", duration = Inf)
        shiny::req(FALSE)
      }
      NULL
    }

    assert_conditions_or_notify_and_early_out <- function(x) {
      for (idx in seq_along(x)) {
        log_warn(x[[idx]]$message)
        shiny::showNotification(x[[idx]]$message, type = "error", duration = Inf)
      }

      if (length(x) > 0) shiny::req(FALSE)

      NULL
    }

    shiny::observeEvent(input[[assign_btn_id]], {
      idx <- as.integer(input[[assign_btn_id]])
      r_new_subgroup_filter <- subgroup_filter()
      assert_or_notify_and_early_out(!isTRUE(is.na(r_new_subgroup_filter[["raw"]])), "Filter is not ready")

      new_assignment <- r_new_subgroup_filter[["raw"]]
      current_assingments <- cat_assignments()
      current_assingments[[idx]] <- new_assignment
      cat_assignments(current_assingments)
    })

    shiny::observeEvent(input[["add_subgroup"]], {
      r_subgroup_cat_num <- as.integer(input[["subgroup_cat_num"]])
      r_subgroup_name <- input[["subgroup_name"]]
      r_subgroup_label <- if (checkmate::test_string(input[["subgroup_label"]], min.chars = 1)) input[["subgroup_label"]] else NULL

      subject_dataset <- unfiltered_dataset_list()[[subject_filter_dataset_name]]
      current_subgroups <- subgroups()
      original_names <- setdiff(names(subject_dataset), names(current_subgroups))
      new_subgroups <- subgroups()

      assert_or_notify_and_early_out(checkmate::test_string(r_subgroup_name, min.chars = 1), "Subgroup name is empty")
      assert_or_notify_and_early_out(
        !r_subgroup_name %in% original_names,
        sprintf(
          "Subgroup name: `%s` is already a column name in the dataset `%s`",
          r_subgroup_name,
          subject_filter_dataset_name
        )
      )


      if (r_subgroup_cat_num == 2) {
        r_new_subgroup_filter <- subgroup_filter()
        assert_or_notify_and_early_out(!isTRUE(is.na(r_new_subgroup_filter[["raw"]])), "Filter is not ready")

        r_true_label <- input[[get_cat_label_id(1)]]
        r_false_label <- input[[label_others_id]]
        if (!checkmate::test_string(r_true_label, min.chars = 1)) r_true_label <- "TRUE"
        if (!checkmate::test_string(r_false_label, min.chars = 1)) r_false_label <- "FALSE"

        # We store the json instead of the parsed value because its representation is inert and does not suffer
        # from jsonlite autounboxing issues
        json_subject_filter <- r_new_subgroup_filter[["raw"]]
        cat_labels <- c(r_true_label, r_false_label)
        cat_filters <- c(json_subject_filter)
        new_subgroup <- I(list(label = r_subgroup_label, cat_labels = cat_labels, cat_filters = cat_filters))
      } else if (r_subgroup_cat_num > 2) {
        cat_labels <- vector(mode = "character", length = r_subgroup_cat_num)
        cat_filters <- vector(mode = "character", length = r_subgroup_cat_num - 1)
        curr_cat_assignments <- cat_assignments()
        for (idx in seq_len(r_subgroup_cat_num - 1)) {
          cat_label <- input[[get_cat_label_id(idx)]]
          assert_or_notify_and_early_out(checkmate::test_string(cat_label, min.chars = 1), sprintf("No label for category %d", idx))
          cat_assign <- curr_cat_assignments[[idx]]
          assert_or_notify_and_early_out(checkmate::test_string(cat_assign, min.chars = 1), sprintf("No filter assignment for category %d", idx))
          cat_labels[[idx]] <- cat_label
          cat_filters[[idx]] <- cat_assign
        }
        r_others_label <- input[[label_others_id]]
        assert_or_notify_and_early_out(checkmate::test_string(r_others_label, min.chars = 1), "No label for last category")
        cat_labels[[length(cat_labels)]] <- r_others_label
        new_subgroup <- I(list(label = r_subgroup_label, cat_labels = cat_labels, cat_filters = cat_filters))
      }

      new_subgroups[[r_subgroup_name]] <- new_subgroup

      # We assume no wrong dataset can be added
      no_subgroup_dataset_list <- unfiltered_dataset_list()
      no_subgroup_dataset_list[[subject_filter_dataset_name]] <- no_subgroup_dataset_list[[subject_filter_dataset_name]][,!names(unfiltered_dataset_list()[[subject_filter_dataset_name]]) %in% names(new_subgroups), drop = FALSE]
      apply_check <- apply_subgroups(
        no_subgroup_dataset_list,
        subject_filter_dataset_name,
        filter_key_var,
        new_subgroups
      )

      assert_conditions_or_notify_and_early_out(apply_check[["errors"]])

      subgroups(new_subgroups)

      # Clean inputs for next subgroup
      for (idx in seq_len(r_subgroup_cat_num - 1)) {
        shiny::updateTextInput(inputId = get_cat_label_id(idx), value = "")
      }

      shiny::updateTextInput(inputId = label_others_id, value = "")
      shiny::updateTextInput(inputId = "subgroup_name", value = "")
      shiny::updateTextInput(inputId = "subgroup_label", value = "")

      cat_assignments(DEFAULT_CAT_ASSIGNMENTS)
    })

    apply_subgroups <- (function(dataset_list, subject_filter_dataset_name, filter_key_var, subgroups) {
      subject_dataset <- dataset_list[[subject_filter_dataset_name]]
      errors <- list()
      push_error <- function(x) {
        errors[[length(errors) + 1]] <<- simpleCondition(message = x)
      }

      for (idx in seq_along(subgroups)) {
        name <- names(subgroups)[[idx]]
        label <- subgroups[[idx]][["label"]]
        cat_labels <- subgroups[[idx]][["cat_labels"]]
        cat_filters <- subgroups[[idx]][["cat_filters"]]
        log_inform(sprintf("Adding subgroup: %s", name))

        categorized_subject_mask <- rep_len(FALSE, nrow(subject_dataset))

        if (name %in% names(subject_dataset)) {
          push_error(sprintf("Skipping subgroup: `%s`. It is already a column name in the dataset `%s`.", name, subject_filter_dataset_name))
        }

        if (length(errors) == 0) {
          new_var <- rep_len(NA_character_, nrow(subject_dataset))
          subgroup_ok <- TRUE

          for (cat_idx in seq_len(length(cat_filters))) { # Others will be treated separately
            category_label <- cat_labels[[cat_idx]]
            category_filter <- cat_filters[[cat_idx]]
            log_inform(sprintf("Adding category: %s", category_label))

            parsed_category_filter <- deserialize_filter_state_from_client(category_filter)[["filters"]][["subject_filter"]]
            category_subjects <- create_subject_filter_info(dataset_list, parsed_category_filter, filter_key_var)[["subjects"]]
            category_mask <- subject_dataset[[filter_key_var]] %in% category_subjects

            if (any(categorized_subject_mask & category_mask)) {
              push_error(sprintf("Subgroup: `%s` has at least one subject in two categories", name))
            } else {
              new_var[category_mask] <- category_label
              categorized_subject_mask <- categorized_subject_mask | category_mask
            }

            if (length(errors) > 0) {
              subgroup_ok <- FALSE
              break
            }
          }

          if (subgroup_ok) {
            other_category_label <- cat_labels[[length(cat_labels)]]
            other_category_mask <- !categorized_subject_mask
            new_var[other_category_mask] <- other_category_label
            subject_dataset[[name]] <- factor(new_var, levels = cat_labels)
            attr(subject_dataset[[name]], "label") <- label
          }
        }
      }
      dataset_list[[subject_filter_dataset_name]] <- subject_dataset
      dataset_list
      return(
        list(
          dataset_list = dataset_list,
          errors = errors
        )
      )
    }) |> shiny::maskReactiveContext()

    res <- shiny::reactive({
      r_subgroups <- subgroups()
      function(...) {
        apply_subgroups(..., subgroups = r_subgroups)
      }
    })

    return(res)
  }
  shiny::moduleServer(id, mod)
}
