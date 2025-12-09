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

mod_subgroup_server <- function(id, selected_dataset_list, subject_filter_dataset_name, filter_key_var) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    assign_btn_id <- "assign_button"
    clear_assign_btn_id <- "clear_assign_button"
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

    subgroup_filter <- new_filter_server("filter", selected_dataset_list, subject_filter_dataset_name, selected_dataset_list, skip_dataset_filters = TRUE) # FIXME: Pass filtered one

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

      category_conflicts <- local({
        non_other_cat_num <- r_subgroup_cat_num - 1

        cat_subjects <- vector(mode = "list", length = non_other_cat_num)
        cat_conflicts <- vector(mode = "list", length = non_other_cat_num)
        cat_conflicts[] <- list(integer(0))

        for (idx in seq_len(r_subgroup_cat_num - 1)) {
          assigned_filter <- cat_assignments()[[idx]]
          if (checkmate::test_string(assigned_filter, min.chars = 1)) {
            parsed_assigned_filter <- deserialize_filter_state_from_client(assigned_filter)[["filters"]][["subject_filter"]]
            cat_subjects[[idx]] <- create_subject_filter_info(selected_dataset_list(), parsed_assigned_filter, filter_key_var)[["subjects"]]
            for (jdx in seq_len(idx - 1)) {
              if (length(intersect(cat_subjects[[idx]], cat_subjects[[jdx]])) > 0) {
                cat_conflicts[[idx]] <- union(cat_conflicts[[idx]], jdx)
                cat_conflicts[[jdx]] <- union(cat_conflicts[[jdx]], idx)
              }
            }
          } else {
            cat_conflicts[[idx]] <- NA_integer_
          }
        }
        cat_conflicts
      })

      for (idx in seq_len(r_subgroup_cat_num - 1)) {
        label_id <- get_cat_label_id(idx)

        if (r_subgroup_cat_num > 2) {

          if (identical(category_conflicts[[idx]], NA_integer_)) {
            icon_state <- shiny::icon("circle-question", class = "text-warning", title = "Pending assignment")
          } else if (length(category_conflicts[[idx]]) == 0) {
            icon_state <- shiny::icon("circle-check", class = "text-success", title = "Correct")
          } else if (length(category_conflicts[[idx]]) > 0) {
            title <- paste("Category", idx, "shares at least one subject with categories", paste(category_conflicts[[idx]], collapse = ","))
            icon_state <- shiny::icon("circle-xmark", class = "text-danger", title = title)
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
              shiny::tags[["button"]](
                shiny::icon("user-minus"),
                class = "btn btn-sm mb-3 btn-default",
                title = "Clear assignment",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', '%d', { priority: 'event' })",
                  ns(clear_assign_btn_id),                  
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
          hidden = setdiff(names(selected_dataset_list()), subject_filter_dataset_name)
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

    shiny::observeEvent(input[[clear_assign_btn_id]], {
      idx <- as.integer(input[[clear_assign_btn_id]])
      current_assingments <- cat_assignments()
      current_assingments[[idx]] <- NULL
      cat_assignments(current_assingments)
    })

    shiny::observeEvent(input[["add_subgroup"]], {
      r_subgroup_cat_num <- as.integer(input[["subgroup_cat_num"]])
      r_subgroup_name <- input[["subgroup_name"]]
      r_subgroup_label <- if (checkmate::test_string(input[["subgroup_label"]], min.chars = 1)) input[["subgroup_label"]] else NULL

      subject_dataset <- selected_dataset_list()[[subject_filter_dataset_name]]
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
      no_subgroup_dataset_list <- selected_dataset_list()
      no_subgroup_dataset_list[[subject_filter_dataset_name]] <- no_subgroup_dataset_list[[subject_filter_dataset_name]][,!names(selected_dataset_list()[[subject_filter_dataset_name]]) %in% names(new_subgroups), drop = FALSE]
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

