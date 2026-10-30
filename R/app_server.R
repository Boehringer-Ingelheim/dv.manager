app_server <- function(input = NULL, output = NULL, session = NULL) {
  opts <- list(
    "afmm_static" = get_config("afmm_static"),
    "module_info" = get_config("module_info"),
    "filter_dataset_name" = get_config("filter_dataset_name"),
    "filter_key" = get_config("filter_key"),
    "startup_msg" = get_config("startup_msg"),
    "reload_period" = get_config("reload_period"),
    "filter_info" = get_config("filter_info"),
    "enable_subgroup" = get_config("subgroup")[["enable"]],
    "enable_odg" = get_config("odg")[["enable"]]
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
    "enable_subgroup" = get_config("subgroup")[["enable"]],
    "enable_odg" = get_config("odg")[["enable"]]
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

  afmm_static[["odg"]] <- list(
    id = ODG_ID,
    enable = opts[["enable_odg"]]
  )

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

  selected_dataset_list <- sm_mr(
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

      df <- dv.manager::add_date_range(d)
      attr(df, "dataset_list_name") <- dataset_list_name
      df
    },
    varname = ODG_ID$SELECTED_DATASET_LIST
  )

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

  unfiltered_dataset_list_ <- sm_mr2(
    {
      ..t$add_period("unfiltered_dataset_list_", TRUE)
      on.exit(..t$add_period("unfiltered_dataset_list_", FALSE))

      res_apply_subgroups <- sm_me(
        {
          dv.manager::apply_subgroups(
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
    inline = TRUE,
    varname = "unfiltered_dataset_list_"
  )

  unfiltered_dataset_list <- sm_mr2(
    {
      ..t$add_period("unfiltered_dataset_list", TRUE)
      on.exit(..t$add_period("unfiltered_dataset_list", FALSE))

      for (error in unfiltered_dataset_list_()[["error_list"]]$get_messages()) {
        shiny::showNotification(error, type = "warning")
      }
      subgroups[["set_incorrect_subgroups"]](unfiltered_dataset_list_()[["result"]][["incorrect_subgroups"]])

      sm_me(
        {
          ..(unfiltered_dataset_list_())[["result"]][["dataset_list"]]
        },
        localize = TRUE
      )
    },
    varname = ODG_ID$UNFILTERED_DATASET_LIST
  )

  unfiltered_dataset_list_with_filter_info_ <- sm_mr2(
    {
      ..t$add_period("unfiltered_dataset_list_with_filter_info_", TRUE)
      on.exit(..t$add_period("unfiltered_dataset_list_with_filter_info_", FALSE))
      # Place reqs here so all elements are synchronized before going forward
      # Consider generation counters (Check current approach)

      res <- sm_me(
        {
          r_unfiltered_dataset_list <- ..(shiny::isolate(unfiltered_dataset_list()))
          r_dataset_list_filter <- ..(dataset_list_filter()) # List that describes the filter no need of solving it in shinymeta
          filter_key_var <- ..(filter_key_var)
          filter_info <- dv.manager::combine_filter_info(dv.manager::get_filter_info(
            r_unfiltered_dataset_list,
            r_dataset_list_filter,
            filter_key_var
          ))

          list(
            unfiltered_dataset_list = r_unfiltered_dataset_list,
            filter_info = filter_info[["result"]][["filter_info"]],
            get_filtered_dataset = dv.manager::get_filtered_dataset,
            error_list = filter_info[["error_list"]],
            dataset_list_filter = r_dataset_list_filter
          )
        },
        localize = TRUE
      )

      res
    },
    inline = TRUE,
    varname = ODG_ID$UNFILTERED_INFO_DATASET_LIST_
  )

  unfiltered_dataset_list_with_filter_info <- sm_mr2(
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
          shiny::p(paste(unfiltered_dataset_list_with_filter_info_()[["error_list"]]$get_messages(), collapse = "; ")),
          shiny::p("Please select a valid filter or clear current filter to continue")
        )
        shiny::showNotification(msg, type = "error", duration = NULL)
        shiny::req(FALSE)
      }

      res <- sm_me(
        {
          ..(unfiltered_dataset_list_with_filter_info_())
        }
      )

      ..t$add_event("received unfiltered_dataset_list_with_filter_info")

      res
    },
    varname = ODG_ID$UNFILTERED_INFO_DATASET_LIST
  )

  filtered_dataset_list <- sm_mr2(
    {
      ..t$add_period("filtered_dataset_list", TRUE)
      on.exit(..t$add_period("filtered_dataset_list", FALSE))
      fd <- sm_me({
        r_unfiltered_dataset_list_with_filter_info <- ..(unfiltered_dataset_list_with_filter_info())

        dv.manager::get_filtered_dataset_list(r_unfiltered_dataset_list_with_filter_info)
      })
      ..t$add_event("received filtered_dataset_list")
      fd
    },
    varname = ODG_ID$FILTERED_DATASET_LIST
  )

  dataset_list_filter <- new_filter_server(
    ID$FILTER,
    unfiltered_dataset_list,
    subject_filter_dataset_name,
    unfiltered_dataset_list_with_filter_info
  )

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
    )
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

  if (length(dataset_lists) > 0) {
    # Not a fan of priorities, but this should run first thing whenever there is a new value in the selector
    shiny::observeEvent(
      input[["selector"]],
      session[["sendCustomMessage"]]("dv_manager_show_overlay", list(message = "Loading dataset")),
      priority = 1000
    )
    # Once everything data is ready we wait until all modules have been processed. Onflushed runs once all observers and
    # outputs are finished.
    # This also fires on every filter change, update (not show) so the overlay only appears while loading a dataset list
    shiny::observeEvent(unfiltered_dataset_list_with_filter_info(), {
      session[["sendCustomMessage"]]("dv_manager_update_overlay_if_visible", list(message = "Setting up modules"))
      session[["onFlushed"]](function() session[["sendCustomMessage"]]("dv_manager_hide_overlay", list()), once = TRUE)
    })
  } else {
    session[["sendCustomMessage"]]("dv_manager_hide_overlay", list())
  }

  # Dataset name and date

  output$dataset_name <- shiny::renderText({
    paste0("Dataset name: ", input$selector)
  })

  date_range <- sm_mr(
    {
      dr <- attr(..(unfiltered_dataset_list()), "date_range")

      if (!any(is.na(dr))) {
        dr <- format(dr, "%Y-%b-%d (%Z)")
        if (dr[1] != dr[2]) {
          date_string <- sprintf("%s - %s", dr[1], dr[2])
        } else {
          date_string <- dr[1]
        }
      } else {
        date_string <- "Date unavailable"
      }
      date_string
    },
    varname = "date_range",
    localize = TRUE
  )

  output[["dataset_date"]] <- shiny::renderText({
    paste0("Dataset date: ", date_range())
  })

  #### Options modal

  shiny::observeEvent(input$open_options_modal, {
    shiny::showModal(create_info_modal(session = session, input = input, ns = ns))
  })

  ### ODG
  if (isTRUE(opts[["enable_odg"]])) {
    local({
      filter_odg <- sm_mr2(
        {
          shiny::req(unfiltered_dataset_list_with_filter_info())

          sm_me({
            dv.manager::filter_to_odg(
              ..(unfiltered_dataset_list_with_filter_info())
            )
          })
        },
        varname = "filter_odg"
      )

      filter_txt <- sm_mr(
        {
          ..(filter_odg())[["txt"]]
        },
        varname = "filter_txt"
      )

      filter_reference_list <- sm_mr(
        {
          ..(filter_odg())[["references"]]
        },
        varname = "filter_reference_list"
      )

      # Flatten ODG elements and set defaults for missing entries
      odg_elements <- local({
        res <- list()
        for (idx in seq_along(module_output)) {
          mo <- module_output[[idx]]

          if ("to_odg" %in% names(mo)) {
            module_odg_elements <- mo[["to_odg"]]
            module_id <- names(module_output)[[idx]]
            module_name <- module_names[[module_id]]

            if (!checkmate::test_named(module_odg_elements, type = "unique")) {
              stop(sprintf("ODG elements for module %s are not named or names are not unique", module_id))
            }

            for (jdx in seq_along(module_odg_elements)) {
              current_element_list_nm <- names(module_odg_elements)[[jdx]]
              current_element <- module_odg_elements[[jdx]]

              first_module_element <- jdx == 1
              current_element[["label"]] <- current_element[["label"]] %||% current_element_list_nm
              current_element[["info"]] <- current_element[["info"]] %||% current_element[["label"]]
              current_element[["id"]] <- paste0(module_id, "-", current_element_list_nm)
              current_element[["is_first_module_element"]] <- first_module_element
              current_element[["module_name"]] <- module_name
              current_element[["module_id"]] <- module_id
              # knitr rejects some characters in a chunk label (e.g. `=`), the prefix keeps it apart from the
              # document's own chunks (e.g. `warnings-summary`)
              current_element[["chunk_label"]] <- paste0("odg-", gsub("[^A-Za-z0-9_-]", "_", current_element[["id"]]))
              # Fails on repeated ids too (module `a-b` with element `c`, module `a` with element `b-c`), which would
              # otherwise silently replace each other in `res`
              clash <- Find(function(el) identical(el[["chunk_label"]], current_element[["chunk_label"]]), res)
              if (!is.null(clash)) {
                stop(sprintf(
                  paste(
                    "ODG elements `%s` (module `%s`) and `%s` (module `%s`) share the chunk label `%s`,",
                    "rename one of them"
                  ),
                  clash[["id"]],
                  clash[["module_id"]],
                  current_element[["id"]],
                  module_id,
                  current_element[["chunk_label"]]
                ))
              }
              res[[current_element[["id"]]]] <- current_element
            }
          }
        }

        res
      })

      shiny::observeEvent(input[[ODG$ID$ODG_MENU_SELECTION]], {
        # switches are per module, so one event toggles every output of that module
        selection <- input[[ODG$ID$ODG_MENU_SELECTION]]
        module_element_ids <- names(odg_elements)[
          vapply(odg_elements, function(el) el[["module_id"]] == selection[["id"]], logical(1))
        ]
        is_output_selected_for_odg[module_element_ids] <<- selection[["value"]]
        log_inform(
          paste(
            "Selected outputs for ODG",
            paste(names(is_output_selected_for_odg), is_output_selected_for_odg, collapse = ", ")
          )
        )
      })

      is_output_selected_for_odg <- NULL # TODO replace by reactiveValue so it can be used in the testServer?
      shiny::observeEvent(input[[ODG$ID$ODG_CODE_MENU]], {
        if (is.null(attr(selected_dataset_list(), "load_fn"))) {
          dataset_list_name <- attr(selected_dataset_list(), "dataset_list_name")
          user_msg <- sprintf(
            "Current dataset list `%s` is not configured for generating output documentation.",
            dataset_list_name
          )
          dev_msg <- sprintf(
            "Generating output documentation not possible for `%s`. No `load_fn` attribute found. ",
            dataset_list_name
          )
          log_warn(dev_msg)
          shiny::showNotification(user_msg, type = "error")
          shiny::req(FALSE)
        }

        # every ODG output is listed, but only those in the tab in view start out selected
        x <- build_odg_modal_ui(odg_elements, ns, selected_tab = input[[ID$NAV_HEADER]])
        is_output_selected_for_odg <<- x[["selected"]]

        log_inform(
          paste(
            "Selected outputs for ODG by default",
            paste(names(is_output_selected_for_odg), is_output_selected_for_odg, collapse = ", ")
          )
        )

        shiny::showModal(x[["modal_dialog"]])
      })

      output[[ODG$ID$ODG_CODE]] <- shiny::downloadHandler(
        filename = "odg.zip",
        content = function(filename) {
          shiny::withProgress(message = "Generating output documentation", expr = {
            output_format <- input[[ODG$ID$OUTPUT_FORMAT]]

            ec <- shiny::isolate({
              .ec <- shinymeta::newExpansionContext()
              fn <- body(attr(selected_dataset_list(), "load_fn"))
              dln <- attr(selected_dataset_list(), "dataset_list_name")
              .ec$substituteMetaReactive(selected_dataset_list, function() {
                # This metaExpression contains part of the logic of selected dataset list, add date_range and attribute
                # It is not ideal as they may get desynchronized in the future
                sm_me({
                  df <- ..(fn)

                  # (s) Backarwads compatinility this should not be here
                  df <- dv.manager::char_vars_to_factor_vars_dataset_list(df)
                  df <- dv.manager::ungroup2df_datasets_dataset_list(df)
                  # (e) Backarwads compatinility this should not be here

                  df <- dv.manager::add_date_range(df)
                  attr(df, "dataset_list_name") <- ..(dln)
                  df
                })
              })
              .ec
            })

            get_code_in_context <- function(x) {
              shinymeta::expandChain(
                x,
                .expansionContext = ec
              ) |>
                shinymeta::formatCode(formatter = format_with_air, width = 400L) |>
                as.character() |>
                paste(collapse = "\n")
            }

            # Order of calls to get_code_in_context is relevant, once an element has been expanded in a given
            # expansion context, any additional attempt to expand it will return an empty string
            # Therefore we need to make sure that all relevant elements are included in the report and also
            # in the correct order. It is better to resolve those outside of the call as the internal order of resolving
            # due to promises may not be the same.
            #> x <- function() message("x")
            #> y <- function() message("y")
            #> z <- function(a,b){b;a}
            #> z(x(), y())

            data_code <- get_code_in_context(invisible(unfiltered_dataset_list_with_filter_info()))
            date <- build_date_section(selected_dataset_list, date_range, get_code_in_context)
            hardcoded_hash_section <- build_hardcoded_hash_section(selected_dataset_list())
            dynamic_hash_section <- build_dynamic_hash_section(selected_dataset_list, get_code_in_context)

            filter_txt_section <- local({
              cat_mr <- sm_mr(
                {
                  cat(..(filter_txt()))
                },
                inline = TRUE,
                varname = "cat_filter_txt"
              )

              build_filter_txt_section(output_format, get_code_in_context(cat_mr()))
            })

            filter_reference_list_txt_section <- local({
              if (length(filter_reference_list()) == 0) {
                cat_mr <- sm_mr(
                  {
                    cat("No references found")
                  },
                  inline = TRUE,
                  varname = "cat_filter_reference_list_txt"
                )
              } else if (output_format == ODG$OUTPUT_FORMAT$PDF) {
                # Escaped values reach LaTeX untouched: they print exactly as selected and wrap like regular text
                cat_mr <- sm_mr(
                  {
                    references <- ..(filter_reference_list())
                    entries <- character(length(references))
                    for (idx in seq_along(references)) {
                      values <- paste0(
                        "\\texttt{\"",
                        gt::escape_latex(as.character(references[[idx]])),
                        "\"}",
                        collapse = " "
                      )
                      entries[[idx]] <- sprintf("\\textbf{Reference (%d):}\\\\\n%s", idx, values)
                    }
                    cat(sprintf("\\begin{flushleft}\n%s\n\\end{flushleft}", paste(entries, collapse = "\n\n")))
                  },
                  inline = TRUE,
                  varname = "cat_filter_reference_list_txt"
                )
              } else {
                cat_mr <- sm_mr(
                  {
                    references <- ..(filter_reference_list())
                    cat(paste0(
                      paste0("Reference (", seq_along(references), "): \n"),
                      lapply(references, function(x) paste0("\"", x, "\"", collapse = "\t")),
                      "\n",
                      collapse = "\n"
                    ))
                  },
                  inline = TRUE,
                  varname = "cat_filter_reference_list_txt"
                )
              }

              build_filter_reference_section(output_format, get_code_in_context(cat_mr()))
            })

            log_inform("Preprocessing ODG elements")
            selected_odg_elements <- preprocess_odg_elements(
              odg_elements,
              is_output_selected_for_odg,
              output_format,
              get_code_in_context
            )

            log_inform("Creating rmarkdown")
            rmarkdown <- build_odg_rmd(
              selected_odg_elements = selected_odg_elements,
              output_format = output_format,
              data_code = data_code,
              sections = list(
                date = date,
                hardcoded_hash = hardcoded_hash_section,
                dynamic_hash = dynamic_hash_section,
                filter_txt = filter_txt_section,
                filter_reference = filter_reference_list_txt_section
              ),
              templates = list(
                header = ODG$TEMPLATES$HEADER[[output_format]],
                session_info = ODG$TEMPLATES$SESSION_INFO[[output_format]],
                footer = ODG$TEMPLATES$FOOTER[[output_format]]
              )
            )

            log_inform("Rendering rmarkdown")
            header <- paste(
              readLines(system.file("odg_files/header.tex", package = "dv.manager", mustWork = TRUE), warn = FALSE),
              collapse = "\n"
            )
            rendered_filename <- tryCatch(
              callr::r(
                render_odg_document,
                args = list(
                  rmarkdown = rmarkdown,
                  header = header,
                  pdf_attach_function = pdf_attach,
                  filename = filename,
                  quiet = FALSE # We want to access it in the app log
                ),
                show = TRUE
              ),
              error = function(e) {
                # e.g. no zip command or a crashed subprocess, there is no file to download
                log_warn(sprintf("Error while generating output documentation: %s", conditionMessage(e)))
                shiny::showNotification(ODG$MSG$GENERATION_FAILED, type = "error", duration = NULL)
                stop(e)
              }
            )
            if (length(attr(rendered_filename, "error_msg")) > 0) {
              log_warn(sprintf(
                "Error while generating output documentation: %s",
                attr(rendered_filename, "error_msg")
              ))
              # The zip is still downloaded, it carries error.txt
              shiny::showNotification(ODG$MSG$RENDER_FAILED, type = "error", duration = NULL)
            }
            attr(rendered_filename, "error_msg") <- NULL
          })
        }
      )
    })
  }
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
