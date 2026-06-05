app_server <- function(input = NULL, output = NULL, session = NULL) {
  opts <- list(
    "afmm_static" = get_config("afmm_static"),
    "module_info" = get_config("module_info"),
    "filter_dataset_name" = get_config("filter_dataset_name"),
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
    "afmm_static" = get_config("afmm_static"),
    "module_info" = get_config("module_info"),
    "filter_dataset_name" = get_config("filter_dataset_name"),
    "filter_key" = get_config("filter_key"),
    "startup_msg" = get_config("startup_msg"),
    "reload_period" = get_config("reload_period"),
    "filter_info" = get_config("filter_info"),
    "enable_subgroup" = get_config("subgroup")[["enable"]]
  )
  shiny::moduleServer(id = id, module = function(input, output, session) app_server_(input, output, session, opts))
}

app_server_ <- function(input, output, session, opts) {
  ns <- session[["ns"]]

  ..t$add_period("app_server_", TRUE)
  on.exit(..t$add_period("app_server_", FALSE), add = TRUE)

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

  afmm_static <- opts[["afmm_static"]]
  dataset_lists <- afmm_static[["data"]]
  module_names <- afmm_static[["module_names"]]

  module_server <- opts[["module_info"]][["server"]]
  module_meta <- opts[["module_info"]][["meta"]]
  module_hierarchy_list <- opts[["module_info"]][["hierarchy"]]

  subject_filter_dataset_name <- opts[["filter_dataset_name"]]
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

  log_inform(sprintf("Filtering key: %s", filter_key_var))

  url_parameters <- shiny::reactiveVal()

  shiny::observe({
    url_parameters(shiny::parseQueryString(session$clientData$url_search))
    if (!is.null(url_parameters()[["data_name"]])) {
      shiny::updateSelectizeInput(inputId = "selector", selected = url_parameters()[["data_name"]])
    }
  })

  selected_dataset_list <- shinymeta::metaReactive2(
    {
      dataset_list_name <- input[["selector"]]
      shiny::req(checkmate::test_string(dataset_list_name, min.chars = 1))
      ..t$add_period("selected_dataset_list", TRUE)
      on.exit(..t$add_period("selected_dataset_list", FALSE))
      assert(dataset_list_name %in% names(dataset_lists))

      if (is.function(dataset_lists[[dataset_list_name]])) {
        d <- dataset_lists[[dataset_list_name]]()
      } else {
        d <- dataset_lists[[dataset_list_name]]
      }

      df <- add_date_range(d)
      attr(df, "dataset_list_name") <- dataset_list_name
      df
    }
  )

  dv_manager_ec <- function() {
    shiny::isolate({
      ec <- shinymeta::newExpansionContext()
      fn <- body(attr(selected_dataset_list(), "load_fn"))
      dln <- attr(selected_dataset_list(), "dataset_list_name")
      ec$substituteMetaReactive(selected_dataset_list, function() {
        shinymeta::metaExpr({
          df <- ..(fn)
          attr(df, "dataset_list_name") <- ..(dln)
          df
        })
      })
      ec
    })
  }

  if (enable_subgroup) {
    subgroups <- mod_subgroup_server(
      ID$SUBGROUP,
      selected_dataset_list,
      subject_filter_dataset_name,
      filter_key_var
    )
  } else {
    subgroups <- list(
      set_incorrect_subgroups = function(...) {},
      subgroups = shiny::reactive(list())
    )
  }

  unfiltered_dataset_list_ <- shinymeta::metaReactive2(
    {
      ..t$add_period("unfiltered_dataset_list_", TRUE)
      on.exit(..t$add_period("unfiltered_dataset_list_", FALSE))

      res_apply_subgroups <- shinymeta::metaExpr(
        {
          apply_subgroups(
            ..(selected_dataset_list()),
            ..(subject_filter_dataset_name),
            ..(filter_key_var),
            subgroups = ..(subgroups[["subgroups"]]())
          )
        },
        bindToReturn = TRUE
      )

      res_apply_subgroups
    },
    inline = TRUE
  )

  unfiltered_dataset_list <- shinymeta::metaReactive2({
    ..t$add_period("unfiltered_dataset_list", TRUE)
    on.exit(..t$add_period("unfiltered_dataset_list", FALSE))

    for (error in unfiltered_dataset_list_()[["error_list"]]$get_messages()) {
      shiny::showNotification(error, type = "warning")
    }
    subgroups[["set_incorrect_subgroups"]](unfiltered_dataset_list_()[["result"]][["incorrect_subgroups"]])

    shinymeta::metaExpr(
      {
        ..(unfiltered_dataset_list_())[["result"]][["dataset_list"]]
      },
      localize = TRUE
    )
  })

  unfiltered_dataset_list_with_filter_info_ <- shinymeta::metaReactive2(
    {
      ..t$add_period("unfiltered_dataset_list_with_filter_info_", TRUE)
      on.exit(..t$add_period("unfiltered_dataset_list_with_filter_info_", FALSE))
      # Place reqs here so all elements are synchronized before going forward
      # Consider generation counters (Check current approach)

      res <- shinymeta::metaExpr(
        {
          r_unfiltered_dataset_list <- ..(shiny::isolate(unfiltered_dataset_list()))
          r_dataset_list_filter <- ..(dataset_list_filter()) # List that describes the filter no need of solving it in shinymeta
          filter_key_var <- ..(filter_key_var)
          filter_info <- combine_filter_info(get_filter_info(
            r_unfiltered_dataset_list,
            r_dataset_list_filter,
            filter_key_var
          ))

          list(
            unfiltered_dataset_list = r_unfiltered_dataset_list,
            filter_info = filter_info[["result"]][["filter_info"]],
            get_filtered_dataset = get_filtered_dataset,
            error_list = filter_info[["error_list"]]
          )
        },
        localize = TRUE
      )

      res
    },
    inline = TRUE
  )

  unfiltered_dataset_list_with_filter_info <- shinymeta::metaReactive2(
    {
      ..t$add_period("unfiltered_dataset_list_with_filter_info", TRUE)
      on.exit(..t$add_period("unfiltered_dataset_list_with_filter_info", FALSE))
      # Place reqs here so all elements are synchronized before going forward
      # Consider generation counters (Check current approach)

      shiny::req(
        # Wait until filter info is ready
        !unfiltered_dataset_list_with_filter_info_()[["error_list"]]$any_has_class(FC$ERRORS$FILTER_IS_NA$class) &&
          !unfiltered_dataset_list_with_filter_info_()[["error_list"]]$any_has_class(
            FC$ERRORS$UNFILTERED_DATASET_LIST_NAME_FILTER_DATASET_LIST_NAME_MISMATCH$class
          )
      )

    if (unfiltered_dataset_list_with_filter_info_()[["error_list"]]$any()) {
      msg <- shiny::div(
        shiny::p(paste(filter_info[["error_list"]]$get_messages(), collapse = "; ")),
        shiny::p("Please select a valid filter or clear current filter to continue")
      )
      shiny::showNotification(msg, type = "error", duration = NULL)
      shiny::req(FALSE)
    }


    res <- shinymeta::metaExpr({
      shinymeta::..(unfiltered_dataset_list_with_filter_info_())
    })

      ..t$add_event("received unfiltered_dataset_list_with_filter_info")

      res
    },
  )

  filtered_dataset_list <- shinymeta::metaReactive2({
    ..t$add_period("filtered_dataset_list", TRUE)
    on.exit(..t$add_period("filtered_dataset_list", FALSE))
    fd <- shinymeta::metaExpr({
      r_unfiltered_dataset_list_with_filter_info <- ..(unfiltered_dataset_list_with_filter_info())

      get_filtered_dataset_list(r_unfiltered_dataset_list_with_filter_info)
    })
    ..t$add_event("received filtered_dataset_list")
    fd
  })

  dataset_list_filter <- new_filter_server(
    ID$FILTER,
    unfiltered_dataset_list,
    subject_filter_dataset_name,
    unfiltered_dataset_list_with_filter_info
  )

  shiny::observeEvent(unfiltered_dataset_list_with_filter_info(), {
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

      exists_current_module <- !is.null(current_tab) && current_tab %in% names(module_names)
      if (exists_current_module) {
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

  afmm_reactive <- list(
    unfiltered_dataset = shiny::reactive({
      log_warn(
        "(Message for the module developer) afmm[[\"unfiltered_dataset\"]] will be deprecated in future versions. Please replace by afmm[[\"unfiltered_dataset_list\"]]."
      ) # nolintr
      unfiltered_dataset_list()
    }),
    filtered_dataset = shiny::reactive({
      log_warn(
        "(Message for the module developer) afmm[[\"filtered_dataset\"]] will be deprecated in future versions. Please replace by afmm[[\"filtered_dataset_list\"]]."
      ) # nolintr
      filtered_dataset_list()
    }),
    unfiltered_dataset_list = unfiltered_dataset_list,
    filtered_dataset_list = filtered_dataset_list,
    unfiltered_dataset_list_with_filter_info = unfiltered_dataset_list_with_filter_info,
    url_parameters = url_parameters,
    dataset_name = shiny::reactive({
      log_warn(
        "(Message for the module developer) afmm[[\"dataset_name\"]]() will be deprecated in future versions. Please replace by attr(afmm[[\"unfiltered_dataset_list_with_filter_info\"]]()[[\"unfiltered_dataset_list\"]], \"dataset_list_name\")."
      )
      input$selector
    }),
    dataset_metadata = list(
      name = shiny::reactive({
        log_warn(
          "(Message for the module developer) afmm[[\"dataset_metadata\"]][[\"name\"]]() will be deprecated in future versions. Please replace by attr(afmm[[\"unfiltered_dataset_list_with_filter_info\"]]()[[\"unfiltered_dataset_list\"]], \"dataset_list_name\")."
        ) # nolintr
        attr(unfiltered_dataset_list(), "dataset_list_name")
      }),
      date_range = shiny::reactive({
        "(Message for the module developer) afmm[[\"dataset_metadata\"]][[\"date_range\"]] will be deprecated in future versions. Please replace by attr(afmm[[\"unfiltered_dataset_list_with_filter_info\"]]()[[\"unfiltered_dataset_list\"]], \"date_range\")."
        attr(unfiltered_dataset_list(), "date_range")
      })
    ),
    module_output = module_output_fn,
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
    ),
    expansion_context = dv_manager_ec
  )

  afmm <- c(
    afmm_static,
    afmm_reactive
  )

  used_datasets <- list()
  module_output <- list()

  for (idx in seq_along(module_server)) {
    fn <- module_server[[idx]]
    id <- names(module_server)[[idx]]

    ..t$add_period(id, TRUE)

    assert(is.character(id), "id must be a character")
    assert(is.function(fn), "fn must be a function")

    module_output[[id]] <- fn(afmm)
    used_datasets[[id]] <- module_meta[[id]][["dataset_info"]][["all"]]
    ..t$add_period(id, FALSE)
  }

  # Not convinced as it is set somewhere else (app_ui and filter) (gvbu)
  if (length(dataset_lists) > 0) {
    # Otherwise when no dataset_list is loaded in the app the overlay remains in screen
    shiny::observeEvent(unfiltered_dataset_list_with_filter_info(), {
      session[["sendCustomMessage"]]("dv_manager_hide_overlay", list())
    })
  } else {
    session[["sendCustomMessage"]]("dv_manager_hide_overlay", list())
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
        date_string <- sprintf("%s - %s", date_range[1], date_range[2])
      } else {
        date_string <- date_range[1]
      }
    } else {
      date_string <- "Date unavailable"
    }
    paste0("Dataset date: ", date_string)
  })

  #### Options modal

  shiny::observeEvent(input$open_options_modal, {
    shiny::showModal(create_info_modal(session = session, input = input, ns = ns))
  })

  ### Export

  local({
    selected <- list()
    ui <- list(
      shiny::h3("Export menu")
    )

    for (idx in seq_along(module_output)) {
      mo <- module_output[[idx]]
      mo_id <- names(module_output)[[idx]]
      mo_nm <- module_names[[mo_id]]

      card_items <- list(bslib::card_header(mo_nm))

      if ("to_report" %in% names(mo)) {
        ctr <- mo[["to_report"]]

        for (jdx in seq_along(ctr)) {
          el_nm <- names(ctr)[[jdx]]
          el_id <- paste0(mo_nm, "-", el_nm)

          selected[[el_id]] <- TRUE

          card_items[[length(card_items) + 1]] <-
            shiny::div(
              class = "form-check form-switch",
              shiny::tags[["label"]](
                class = "form-check-label",
                shiny::tags[["input"]](
                  class = "form-check-input",
                  type = "checkbox",
                  role = "switch",
                  checked = NA,
                  onchange = sprintf(
                    "Shiny.setInputValue('%s', {value: this.checked, id: '%s'});",
                    ns("export_menu_input"),
                    el_id
                  )
                ),
                el_nm
              )
            )
        }
      }

      ui[[length(ui) + 1]] <- do.call(bslib::card, card_items)
    }

    ui[[length(ui) + 1]] <- bslib::card(
      bslib::card_header("Output format"),
      shiny::radioButtons(
        ns("output_format"),
        label = NULL,
        choices = REPORT$OUTPUT_FORMAT
      )
    )

    shiny::observeEvent(input[["export_menu_input"]], {
      selected[[input[["export_menu_input"]][["id"]]]] <<- input[["export_menu_input"]][["value"]]
    })

    shiny::observeEvent(input[[ID$EXPORT_CODE_MENU]], {
      shiny::showModal(
        shiny::modalDialog(
          shiny::div(
            class = "d-flex flex-column vh-25",
            style = "max-height: 90vh",
            shiny::div(
              class = "overflow-auto flex-grow-1 p-3 min-h-0",
              list(
                ui
              )
            ),
            shiny::downloadButton(ns(ID$EXPORT_CODE), "Export")
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
    })

    output[[ID$EXPORT_CODE]] <- shiny::downloadHandler(
      filename = "report.zip",
      content = function(filename) {
        output_format <- input[["output_format"]]

        RATTR <- REPORT$ATTR
        REK <- REPORT$ELEMENT_KIND

        ec <- shiny::isolate(afmm[["expansion_context"]]())

        get_code_in_context <- function(x) {
          shinymeta::expandChain(
            x,
            .expansionContext = ec
          ) |>
            shinymeta::formatCode(formatter = format_with_air, width = 400L) |>
            as.character() |>
            paste(collapse = "\n")
        }

        # Replaces metareactives with other metareactives
        replace_if_htmlwidget <- function(x) {
          if (!inherits(x(), "htmlwidget")) {
            return(x)
          }

          supported_htmlwidgets <- list(
            "datatables" = function(x) {
              shinymeta::metaReactive(
                {
                  ..(x())$x$data
                },
                inline = TRUE
              )
            }
          )

          supported_type <- inherits(x(), names(supported_htmlwidgets))

          if (!identical(supported_type, 0L)) {
            replaced_x <- supported_htmlwidgets[[supported_type[[1]]]](x)
          } else {
            replaced_x <- shinymeta::metaReactive(
              {
                stop("Unsupported htmlwidget", paste(class("`", x()), "`", collapse = ", "))
              },
              inline = TRUE
            )
          }

          return(replaced_x)
        }

        process_report_element <- function(el, module_name, el_name, output_format) {
          el_header <- paste("#", module_name, el_name)
          log_inform(paste("Processing:", el_header))

          stopifnot(output_format %in% REPORT$OUTPUT_FORMAT)
          el_resolved <- try(el(), silent = TRUE)

          if (identical(output_format, REPORT$OUTPUT_FORMAT$PDF) && !inherits(el_resolved, "try-error")) {
            el <- replace_if_htmlwidget(el)
          }

          if (inherits(el_resolved, "try-error")) {
            el_processed <- list(
              header = el_header,
              el = local({
                msg <- attr(el_resolved, "condition")$message
                paste("Error creating", module_name, report_element_nm, msg)
              }),
              kind = REK$ERROR
            )
          } else if (identical(output_format, REPORT$OUTPUT_FORMAT$HTML)) {
            el_processed <- list(
              header = el_header,
              el = get_code_in_context(el()),
              kind = REK$DEFAULT
            )
          } else if (identical(output_format, REPORT$OUTPUT_FORMAT$PDF)) {
            if (is.data.frame(el_resolved)) {
              el_ <- shinymeta::metaReactive(
                {
                  ..(el())
                  gt::gt() |>
                    gt::tab_options(
                      latex.use_longtable = TRUE,
                      table.font.size = gt::px(9),
                      latex.header_repeat = TRUE
                    )
                },
                inline = TRUE
              )

              el_processed <- list(
                header = el_header,
                el = get_code_in_context(el_()),
                kind = REK$DEFAULT
              )
            } else {
              el_processed <- list(
                header = el_header,
                el = get_code_in_context(el()),
                kind = REK$DEFAULT
              )
            }
          }

          log_inform(paste("Processed:", el_header))

          return(el_processed)
        }

        data_code <- get_code_in_context(invisible(unfiltered_dataset_list_with_filter_info()))

        report_elements <- list()

        for (idx in seq_along(module_output)) {
          current_module_output <- module_output[[idx]]
          module_id <- names(module_output)[[idx]]
          module_name <- module_names[[module_id]]

          if ("to_report" %in% names(current_module_output)) {
            list_to_report <- current_module_output[["to_report"]]

            for (jdx in seq_along(list_to_report)) {
              report_element_nm <- names(list_to_report)[[jdx]]
              report_element_id <- paste0(module_name, "-", report_element_nm)
              curr_report_element <- list_to_report[[jdx]]

              if (selected[[report_element_id]]) {
                element <- process_report_element(
                  curr_report_element,
                  module_name,
                  names(list_to_report)[[jdx]],
                  output_format
                )
                report_elements <- c(report_elements, list(element))
              }
            }
          }
        }

        rmarkdown <- REPORT$TEMPLATES$HEADER[[output_format]]

        rmarkdown <- sprintf("%s\n# Data source\n```{r}\n%s\n```\n", rmarkdown, data_code)

        for (idx in seq_along(report_elements)) {
          header <- report_elements[[idx]][["header"]]
          el <- report_elements[[idx]][["el"]]
          kind <- report_elements[[idx]][["kind"]]

          element_formatters <- list()
          element_formatters[[REPORT$OUTPUT_FORMAT$PDF]] <- list()
          element_formatters[[REPORT$OUTPUT_FORMAT$PDF]][[REK$ERROR]] <- function(x) sprintf("%s", x)
          element_formatters[[REPORT$OUTPUT_FORMAT$PDF]][[REK$TABLE]] <- function(x) sprintf("```{r}\n{{%s}}\n```", x)
          element_formatters[[REPORT$OUTPUT_FORMAT$PDF]][[REK$DEFAULT]] <- function(x) sprintf("```{r}\n{{%s}}\n```", x)

          element_formatters[[REPORT$OUTPUT_FORMAT$HTML]] <- list()
          element_formatters[[REPORT$OUTPUT_FORMAT$HTML]][[REK$ERROR]] <- function(x) sprintf("%s", x)
          element_formatters[[REPORT$OUTPUT_FORMAT$HTML]][[REK$TABLE]] <- function(x) sprintf("```{r}\n{{%s}}\n```", x)
          element_formatters[[REPORT$OUTPUT_FORMAT$HTML]][[REK$DEFAULT]] <- function(x) {
            sprintf("```{r}\n{{%s}}\n```", x)
          }

          rmarkdown <- sprintf("%s\n%s\n%s\n", rmarkdown, header, element_formatters[[output_format]][[kind]](el))
        }

        rmarkdown <- sprintf("%s\n%s", rmarkdown, REPORT$TEMPLATES$FOOTER[[output_format]])

        report_dir <- tempfile(pattern = "report")
        dir.create(report_dir)
        curr_dir <- getwd()
        on.exit(
          {
            if (dir.exists(report_dir)) {
              unlink(report_dir, recursive = TRUE)
            }
            setwd(curr_dir)
          },
          add = TRUE
        )

        report_rmd <- file.path(report_dir, "report.Rmd")
        writeLines(rmarkdown, report_rmd)

        callr::r(
          function(report_rmd, report_dir, filename) {
            setwd(report_dir)
            rmarkdown::render(input = report_rmd, output_dir = report_dir)
            utils::zip(filename, list.files(report_dir))
          },
          args = list(report_rmd = report_rmd, report_dir = report_dir, filename = filename)
        )
      }
    )
  })
}

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
