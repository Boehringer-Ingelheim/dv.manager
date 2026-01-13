MAX_CATEGORIES <- 10

#' Build Subgroup Category UI
#'
#' Constructs the complete UI for all categories in a subgroup, including
#' appropriate controls and validation indicators based on whether it's a
#' binary or multi-category subgroup.
#'
#' @param cat_num Integer. Total number of categories (including "others").
#' @param cat_assignments List. Current filter assignments for each category.
#' @param selected_dataset_list Named list of data frames.
#' @param filter_key_var Character string. Name of the subject ID variable.
#' @param ns Namespace function from the module.
#' @param get_cat_label_id Function that returns the input ID for a category label
#'   given its index.
#' @param label_others_id Character string. The input ID for the "others" label.
#' @param assign_btn_id Character string. The base input ID for assign buttons.
#' @param clear_assign_btn_id Character string. The base input ID for clear buttons.
#' @param current_values Character vector of current input values for preserving text
#'   across re-renders. Should have length equal to cat_num, with values in order
#'   (categories 1 to cat_num-1, then "others").
#'
#' @return A list of Shiny UI elements, one for each category. For binary subgroups
#'   (cat_num = 2), returns simple text inputs. For multi-category subgroups
#'   (cat_num > 2), includes assignment buttons and conflict status icons.
#'
#' @details
#' The function handles three types of category UI:
#' \itemize{
#'   \item{Regular categories (1 to cat_num-1): }{Text input with optional
#'     assign/clear buttons and status icons for multi-category subgroups}
#'   \item{Others category (cat_num): }{Right-aligned text input for unassigned subjects}
#'   \item{Status icons (multi-category only): }{Indicate pending (yellow ?),
#'     valid (green check), or conflicting (red X) assignments}
#' }
#'
#' For multi-category subgroups, the function computes conflicts by checking
#' if subjects appear in multiple categories, displaying appropriate validation
#' indicators.
#'
#' @keywords internal
build_subgroup_category_ui <- function(cat_num, cat_assignments, selected_dataset_list, filter_key_var, ns, get_cat_label_id, label_others_id, assign_btn_id, clear_assign_btn_id, current_values) {

  ui <- vector(mode = "list", length = cat_num)
  is_multicat <- cat_num > 2

  # For multi-category subgroups, compute conflicts between categories
  category_conflicts <- NULL
  if (is_multicat) {
    non_other_cat_num <- cat_num - 1
    cat_subjects <- vector(mode = "list", length = non_other_cat_num)
    category_conflicts <- vector(mode = "list", length = non_other_cat_num)
    category_conflicts[] <- list(integer(0))

    for (idx in seq_len(cat_num - 1)) {
      assigned_filter <- cat_assignments[[idx]]
      if (checkmate::test_string(assigned_filter, min.chars = 1)) {
        parsed_assigned_filter <- deserialize_filter_state_from_client(assigned_filter)[["filters"]][["subject_filter"]]
        cat_subjects[[idx]] <- create_subject_filter_info(selected_dataset_list, parsed_assigned_filter, filter_key_var)[["subjects"]]

        for (jdx in seq_len(idx - 1)) {
          if (length(intersect(cat_subjects[[idx]], cat_subjects[[jdx]])) > 0) {
            category_conflicts[[idx]] <- union(category_conflicts[[idx]], jdx)
            category_conflicts[[jdx]] <- union(category_conflicts[[jdx]], idx)
          }
        }
      } else {
        category_conflicts[[idx]] <- NA_integer_
      }
    }
  }

  # Build UI for each regular category (not "others")
  for (idx in seq_len(cat_num - 1)) {
    label_id <- get_cat_label_id(idx)

    if (is_multicat) {
      conflicts <- category_conflicts[[idx]]

      if (identical(conflicts, NA_integer_)) {
        status_icon <- shiny::icon("circle-question", class = "text-warning", title = "Pending assignment")
        clear_disabled <- TRUE
      } else if (length(conflicts) == 0) {
        status_icon <- shiny::icon("circle-check", class = "text-success", title = "Correct")
        clear_disabled <- FALSE
      } else {
        icon_title <- paste("Category", idx, "shares at least one subject with categories", paste(conflicts, collapse = ","))
        status_icon <- shiny::icon("circle-xmark", class = "text-danger", title = icon_title)
        clear_disabled <- FALSE
      }

      ui[[idx]] <- shiny::div(
        shiny::div(
          style = "display: flex; align-items:baseline; gap: 2px",
          shiny::textInput(ns(label_id), label = NULL, placeholder = paste("Label for category", idx), value = current_values[idx]),
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
            ),
            disabled = if (clear_disabled) NA_character_ else NULL
          ),
          status_icon
        )
      )
    } else {
      ui[[idx]] <- shiny::div(
        shiny::div(
          style = "display: flex",
          shiny::textInput(ns(label_id), label = NULL, placeholder = paste("Label for category", idx), value = current_values[idx])
        )
      )
    }
  }

  # Build "others" category UI (always right-aligned, no buttons)
  ui[[cat_num]] <- shiny::div(
    style = "display: flex; justify-content: flex-end;",
    shiny::textInput(ns(label_others_id), label = NULL, placeholder = "Label for other subjects", value = current_values[cat_num])
  )

  ui
}

#' Subgroup Module UI
#'
#' UI function for the subgroup module. Creates the interface for defining and
#' managing subject subgroups.
#'
#' @param id Character string. The module namespace ID.
#' @param subject_filter_dataset_name Character string. Name of the dataset containing
#'   subjects to be filtered. Used to configure the filter UI.
#'
#' @return A list of Shiny UI elements including:
#'   \itemize{
#'     \item{Text inputs for subgroup name and label}
#'     \item{Selector for number of categories (2-10)}
#'     \item{Dynamic UI container for category labels and assignments}
#'     \item{Button to add the defined subgroup}
#'     \item{Display of created subgroups as badges}
#'     \item{Filter UI for defining category criteria}
#'   }
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

#' Validate Subgroup Name
#'
#' Checks if a proposed subgroup name is valid and doesn't conflict with existing
#' column names in the subject dataset.
#'
#' @param subgroup_name Character string. The proposed name for the subgroup.
#' @param subject_dataset Data frame. The dataset that will receive the subgroup variable.
#' @param subject_filter_dataset_name Character string. Name of the subject dataset
#'   for error messages.
#'
#' @return An error list object. Empty if validation passes, contains error messages
#'   if validation fails.
#'
#' @details
#' Validation rules:
#' \itemize{
#'   \item{Name must be non-empty}
#'   \item{Name must contain only alphanumeric characters, underscores, and periods}
#'   \item{Name must not conflict with existing column names in the dataset}
#' }
validate_subgroup_name <- function(subgroup_name, subject_dataset, subject_filter_dataset_name) {
  errors <- new_error_list()

  if (!checkmate::test_string(subgroup_name, min.chars = 1, pattern = "^[A-Za-z0-9_.]+$")) {
    errors$push("Subgroup name is empty or has non alphanumeric characters (`_` and `.` are allowed)")
  }

  if (subgroup_name %in% names(subject_dataset)) {
    errors$push(sprintf(
      "Subgroup name: `%s` is already a column name in the dataset `%s`",
      subgroup_name,
      subject_filter_dataset_name
    ))
  }

  return(errors)
}

#' Apply Subgroups to Dataset List
#'
#' Applies subgroup definitions to a dataset list by creating new factor variables
#' in the subject dataset. Each subgroup becomes a new column with category labels
#' as factor levels.
#'
#' @param dataset_list Named list of data frames. Must include the dataset specified
#'   by `subject_filter_dataset_name`.
#' @param subject_filter_dataset_name Character string. Name of the dataset in
#'   `dataset_list` that contains subjects.
#' @param filter_key_var Character string. Name of the variable used to identify
#'   subjects (typically a subject ID variable).
#' @param subgroups Named list of subgroup definitions. Each element should contain:
#'   \describe{
#'     \item{label}{Optional character string describing the subgroup}
#'     \item{cat_labels}{Character vector of category labels}
#'     \item{cat_filters}{Character vector of JSON-serialized filter specifications
#'       (length is one less than cat_labels, as the last category is "others")}
#'   }
#'
#' @return A list with components:
#'   \describe{
#'     \item{dataset_list}{The input dataset list with new subgroup variables added
#'       to the subject dataset}
#'     \item{errors}{A list of error conditions encountered during application}
#'   }
#'
#' @details
#' For each subgroup:
#' \itemize{
#'   \item{Creates a new factor variable in the subject dataset}
#'   \item{Assigns subjects to categories based on filter specifications}
#'   \item{Assigns remaining subjects to the "others" category}
#'   \item{Validates that no subject appears in multiple categories}
#'   \item{Validates that the subgroup name doesn't conflict with existing columns}
#'   \item{Preserves the subgroup label as an attribute}
#' }
#'
#' The function is wrapped with `maskReactiveContext()` to prevent reactive dependencies.
apply_subgroups <- (function(dataset_list, subject_filter_dataset_name, filter_key_var, subgroups) {
  subject_dataset <- dataset_list[[subject_filter_dataset_name]]
  error_list <- new_error_list()

  for (subgroup_idx in seq_along(subgroups)) {
    name <- names(subgroups)[[subgroup_idx]]
    label <- subgroups[[subgroup_idx]][["label"]]
    cat_labels <- subgroups[[subgroup_idx]][["cat_labels"]]
    cat_filters <- subgroups[[subgroup_idx]][["cat_filters"]]
    log_inform(sprintf("Adding subgroup: %s", name))

    categorized_subject_mask <- rep_len(FALSE, nrow(subject_dataset))

    if (name %in% names(subject_dataset)) {
      error_list$push(sprintf("Skipping subgroup: `%s`. It is already a column name in the dataset `%s`.", name, subject_filter_dataset_name))
    } else {
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
          subgroup_ok <- FALSE
          error_list$push(sprintf("Subgroup: `%s` has at least one subject in two categories", name))
          break
        } else {
          new_var[category_mask] <- category_label
          categorized_subject_mask <- categorized_subject_mask | category_mask
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

  return(
    list(
      dataset_list = dataset_list,
      errors = error_list
    )
  )
}) |> shiny::maskReactiveContext()

#' Subgroup Module Server
#'
#' Server function for the subgroup module that allows users to create and manage
#' subject subgroups based on filter criteria. Subgroups are created by assigning
#' subjects to categories using filter conditions. Each subject can belong to only
#' one category within a subgroup, with unassigned subjects placed in an "others" category.
#'
#' @param id Character string. The module namespace ID.
#' @param selected_dataset_list Reactive expression returning a named list of datasets.
#'   Must NOT contain any previously added subgroups. Recursively passing a dataset_list
#'   that already has subgroups added is not possible.
#' @param subject_filter_dataset_name Character string. Name of the dataset in
#'   `selected_dataset_list` that contains the subjects to be filtered and categorized.
#' @param filter_key_var Character string. Name of the variable used to join/filter
#'   subjects across datasets (typically a subject ID variable).
#'
#' @return A reactive function that when called applies the created subgroups to a
#'   dataset list. The returned function takes the same arguments as `apply_subgroups()`
#'   and returns a list with components:
#'   \describe{
#'     \item{dataset_list}{The input dataset list with subgroup variables added}
#'     \item{errors}{A list of error conditions encountered during application}
#'   }
#'
#' @details
#' The module manages the following key reactive values:
#' \itemize{
#'   \item{subgroups: }{A list of subgroup definitions, each containing category
#'     labels and filter specifications}
#'   \item{cat_assignments: }{Current filter assignments for each category while
#'     building a subgroup}
#' }
#'
#' The module supports:
#' \itemize{
#'   \item{Binary subgroups (2 categories): }{One category defined by a filter,
#'     the other contains all remaining subjects}
#'   \item{Multi-category subgroups (3-10 categories): }{Multiple categories each
#'     defined by filters, plus an "others" category for unassigned subjects.
#'     Includes validation to prevent subjects from appearing in multiple categories}
#' }
#'
#' State persistence through bookmarking is supported for the subgroups reactive value.
#'
#' @seealso \code{\link{mod_subgroup_ui}}, \code{\link{apply_subgroups}},
#'   \code{\link{create_binary_subgroup}}, \code{\link{create_multicat_subgroup}}
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   dataset_list <- reactive({
#'     list(ADSL = adsl_data, ADAE = adae_data)
#'   })
#'
#'   apply_subgroups_fn <- mod_subgroup_server(
#'     id = "subgroups",
#'     selected_dataset_list = dataset_list,
#'     subject_filter_dataset_name = "ADSL",
#'     filter_key_var = "USUBJID"
#'   )
#'
#'   # Later, apply the subgroups to a dataset list
#'   dataset_with_subgroups <- reactive({
#'     apply_fn <- apply_subgroups_fn()
#'     result <- apply_fn(
#'       dataset_list = dataset_list(),
#'       subject_filter_dataset_name = "ADSL",
#'       filter_key_var = "USUBJID"
#'     )
#'     result$dataset_list
#'   })
#' }
#' }
mod_subgroup_server <- function(id, selected_dataset_list, subject_filter_dataset_name, filter_key_var) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]

    assign_btn_id <- "assign_button"
    clear_assign_btn_id <- "clear_assign_button"
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

    subgroup_filter <- new_filter_server("filter", selected_dataset_list, subject_filter_dataset_name, filtered_subgroup_dataset_list, skip_dataset_filters = TRUE) # FIXME: Pass filtered one

    filtered_subgroup_dataset_list <- shiny::reactive({

      unfiltered_dataset_list_r <- selected_dataset_list()
      dataset_list_filter_r <- subgroup_filter()

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

    shiny::onBookmark(function(state) {
      state$values$subgroups <- I(subgroups())
    })

    shiny::onRestore(function(state) {
      subgroups(state$values$subgroups)
    })

    output[["subgroup_cat_container"]] <- shiny::renderUI({

      r_subgroup_cat_num <- as.integer(input[["subgroup_cat_num"]])
      shiny::req(checkmate::test_integer(r_subgroup_cat_num, lower = 2, len = 1, upper = MAX_CATEGORIES, any.missing = FALSE))

      current_values <- character(r_subgroup_cat_num)
      for (i in seq_len(r_subgroup_cat_num - 1)) {
        current_values[i] <- shiny::isolate(input[[get_cat_label_id(i)]]) %||% ""
      }
      current_values[r_subgroup_cat_num] <- shiny::isolate(input[[label_others_id]]) %||% ""

      build_subgroup_category_ui(
        cat_num = r_subgroup_cat_num,
        cat_assignments = cat_assignments(),
        selected_dataset_list = selected_dataset_list(),
        filter_key_var = filter_key_var,
        ns = ns,
        get_cat_label_id = get_cat_label_id,
        label_others_id = label_others_id,
        assign_btn_id = assign_btn_id,
        clear_assign_btn_id = clear_assign_btn_id,
        current_values = current_values
      )
    })

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

    notify_and_early_out <- function(expr, msg) {
      if (!expr) {
        log_warn(msg)
        shiny::showNotification(msg, type = "error", duration = Inf)
        shiny::req(FALSE)
      }
      NULL
    }

    notify_conditions_and_early_out <- function(x) {
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
      notify_and_early_out(!isTRUE(is.na(r_new_subgroup_filter[["raw"]])), "Filter is not ready")

      new_assignment <- r_new_subgroup_filter[["raw"]]
      current_assignments <- cat_assignments()
      current_assignments[[idx]] <- new_assignment
      cat_assignments(current_assignments)
    })

    shiny::observeEvent(input[[clear_assign_btn_id]], {
      idx <- as.integer(input[[clear_assign_btn_id]])
      current_assignments <- cat_assignments()
      current_assignments[[idx]] <- NULL
      cat_assignments(current_assignments)
    })

    shiny::observeEvent(input[["add_subgroup"]], {
      r_subgroup_cat_num <- as.integer(input[["subgroup_cat_num"]])
      r_subgroup_name <- input[["subgroup_name"]]
      r_subgroup_label <- if (checkmate::test_string(input[["subgroup_label"]], min.chars = 1)) input[["subgroup_label"]] else NULL
      subject_dataset <- selected_dataset_list()[[subject_filter_dataset_name]]
      current_subgroups <- subgroups()

      assert(
        length(intersect(names(current_subgroups), names(subject_dataset))) == 0,
        "Current subgroups cannot be in subject dataset by design. Are you passing the dataset with already added subgroups to the module? Check assumptions"
      )

      name_errors <- validate_subgroup_name(r_subgroup_name, subject_dataset, subject_filter_dataset_name)
      notify_conditions_and_early_out(name_errors$get_errors())

      errors <- new_error_list()
      if (r_subgroup_cat_num == 2) {
        r_subgroup_filter <- subgroup_filter()
        if (isTRUE(is.na(r_subgroup_filter[["raw"]]))) {
          errors$push("Filter is not ready")
          return(list(subgroup = NULL, errors = errors))
        }

        r_true_label <- if (checkmate::test_string(input[[get_cat_label_id(1)]], min.chars = 1)) input[[get_cat_label_id(1)]] else "TRUE"
        r_false_label <- if (checkmate::test_string(input[[label_others_id]], min.chars = 1)) input[[label_others_id]] else "FALSE"

        json_subject_filter <- r_subgroup_filter[["raw"]]
        cat_labels <- c(r_true_label, r_false_label)
        cat_filters <- c(json_subject_filter)

        if (!errors$any()) {
          new_subgroup <- I(list(
            label = r_subgroup_label,
            cat_labels = cat_labels,
            cat_filters = cat_filters
          ))
        } else {
          new_subgroup <- NULL
        }
      } else if (r_subgroup_cat_num > 2) {
        cat_labels <- vector(mode = "character", length = r_subgroup_cat_num)
        cat_filters <- vector(mode = "character", length = r_subgroup_cat_num - 1)
        r_cat_assignments <- cat_assignments()

        for (idx in seq_len(r_subgroup_cat_num - 1)) {
          cat_label <- input[[get_cat_label_id(idx)]]
          if (!checkmate::test_string(cat_label, min.chars = 1)) {
            errors$push(sprintf("No label for category %d", idx))
          }

          cat_assign <- r_cat_assignments[[idx]]
          if (!checkmate::test_string(cat_assign, min.chars = 1)) {
            errors$push(sprintf("No filter assignment for category %d", idx))
          }

          cat_labels[[idx]] <- cat_label
          cat_filters[[idx]] <- cat_assign
        }

        r_others_label <- input[[label_others_id]]
        if (!checkmate::test_string(r_others_label, min.chars = 1)) {
          errors$push("No label for last category")
        }

        cat_labels[[length(cat_labels)]] <- r_others_label

        if (!errors$any()) {
          new_subgroup <- I(list(
            label = r_subgroup_label,
            cat_labels = cat_labels,
            cat_filters = cat_filters
          ))
        } else {
          new_subgroup <- NULL
        }
      }

      notify_conditions_and_early_out(errors$get_errors())

      new_subgroups <- current_subgroups
      new_subgroups[[r_subgroup_name]] <- new_subgroup
      apply_check <- apply_subgroups(selected_dataset_list(), subject_filter_dataset_name, filter_key_var, new_subgroups)

      notify_conditions_and_early_out(apply_check[["errors"]]$get_errors())

      subgroups(new_subgroups)

      for (idx in seq_len(r_subgroup_cat_num - 1)) {
        shiny::updateTextInput(session = session, inputId = get_cat_label_id(idx), value = "")
      }
      shiny::updateTextInput(session = session, inputId = label_others_id, value = "")
      shiny::updateTextInput(session = session, inputId = "subgroup_name", value = "")
      shiny::updateTextInput(session = session, inputId = "subgroup_label", value = "")

      cat_assignments(DEFAULT_CAT_ASSIGNMENTS)
    })

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
