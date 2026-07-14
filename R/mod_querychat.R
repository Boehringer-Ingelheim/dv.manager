# This file was created with the help of Github copilot using Claude Sonnet 5 model

# Chat-based (natural language) population filter UI.
#
# Wraps a single `querychat::QueryChat` instance whose data source is a shared, in-memory
# database connection into which every dataset of the currently selected `dataset_list` is
# written as its own SQL table. `querychat` itself only auto-describes/validates the schema
# of one "primary" table (the subject/population dataset), but any SQL the LLM writes runs
# against the whole connection, so the chat can join/subquery against the other tables to
# answer cross-dataset questions (e.g. age from `adsl` and adverse events from `adae`).
# The resulting subject IDs (`filter_key_var`) present in the primary table's filtered rows
# are exposed so they can be intersected with the rest of the filtering pipeline
# (Filter tab + Subgroup), exactly like the existing subject filter.
mod_querychat_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$span(TT$QUERYCHAT_FILTER, class = "visually-hidden"),
    shiny::uiOutput(ns("chat_ui"))
  )
}

# `querychat` only includes the primary table's schema in the system prompt automatically; this
# text is passed as `data_description` so the LLM knows the other tables exist, what columns they
# have, and how to join back to the primary (subject) table using `filter_key_var`.
build_querychat_data_description <- function(dataset_list, subject_filter_dataset_name, filter_key_var) {
  other_names <- setdiff(names(dataset_list), subject_filter_dataset_name)
  if (length(other_names) == 0) {
    return(NULL)
  }

  table_blurbs <- vapply(
    other_names,
    function(nm) {
      cols <- paste(names(dataset_list[[nm]]), collapse = ", ")
      sprintf("- `%s` (columns: %s)", nm, cols)
    },
    character(1)
  )

  paste0(
    "In addition to the primary table, the following tables are also available in the same database ",
    "connection and can be referenced in subqueries/joins (all share the key column `", filter_key_var, "`):\n",
    paste(table_blurbs, collapse = "\n"),
    "\n\nIMPORTANT: Always write the final SELECT against the primary table (i.e. `SELECT * FROM ",
    subject_filter_dataset_name, " WHERE ...`), using subqueries such as `", filter_key_var,
    " IN (SELECT ", filter_key_var, " FROM <other_table> WHERE ...)` to express conditions that live in ",
    "other tables. Never select from another table directly."
  )
  # paste0(
  #   "In addition to the primary table, the following tables are also available in the same database ",
  #   "connection and can be referenced in main queries as well as subqueries/joins (all share the key column `", filter_key_var, "`):\n",
  #   paste(table_blurbs, collapse = "\n"),
  #   "\n\nIMPORTANT: The final SELECT can be against the primary table or other tables (i.e. `SELECT * FROM ",
  #   subject_filter_dataset_name, " WHERE ...`), using subqueries such as `", filter_key_var,
  #   " IN (SELECT ", filter_key_var, " FROM <other_table> WHERE ...)` to express conditions that live in ",
  #   "other tables."
  # )
}

# Writes every dataset in dataset_list into a fresh DuckDB in-memory connection so the LLM's
# generated SQL can join across all of them. Kept as its own function so it can be swapped out
# in tests (e.g. for an RSQLite connection) without needing DuckDB installed.
new_querychat_connection <- function(dataset_list) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("Please install.packages('duckdb') to use `enable_querychat = TRUE`")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Please install.packages('DBI') to use `enable_querychat = TRUE`")
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  for (name in names(dataset_list)) {
    DBI::dbWriteTable(con, name, as.data.frame(dataset_list[[name]]), overwrite = TRUE)
  }
  con
}

# Cheap signature identifying the "shape" of a dataset_list, used to detect dataset_list switches
# without hashing potentially large datasets on every reactive tick.
querychat_dataset_list_signature <- function(dataset_list) {
  paste(
    names(dataset_list),
    vapply(dataset_list, nrow, integer(1)),
    vapply(dataset_list, ncol, integer(1)),
    collapse = "|"
  )
}

#' Chat-based (natural language) population filter server
#'
#' @param id module id, use `ID$QUERYCHAT`
#' @param selected_dataset_list reactive returning the currently active dataset_list
#'   (same reactive fed to `new_filter_server()`/`mod_subgroup_server()` in `app_server.R`)
#' @param subject_filter_dataset_name string, the same `filter_dataset_name` used for the
#'   regular subject/population filter (checked already in `check_filter_dataset_name()`);
#'   registered as the PRIMARY `querychat` table.
#' @param filter_key_var string, the same `filter_key` used everywhere else (checked in
#'   `check_filter_key()`)
#' @param querychat_args a named list forwarded to `querychat::QueryChat$new()` (e.g.
#'   `client`, `greeting`, `data_description`, `extra_instructions`, `categorical_threshold`).
#'   If `querychat_args$data_description` is not supplied, one is generated via
#'   `build_querychat_data_description()`.
#'
#' @return A reactive function with the SAME calling contract as the reactive returned by
#'  `mod_subgroup_server()` / `new_filter_server()`'s consumers: calling it returns
#'  `list(result = list(subjects = <character vector or NULL>), error_list = <error_list>)`.
#'  `subjects = NULL` means "chat filter not active / no constraint" (must not restrict rows).
#' @keywords internal
mod_querychat_server <- function(
  id,
  selected_dataset_list,
  subject_filter_dataset_name,
  filter_key_var,
  querychat_args = list()
) {
  mod <- function(input, output, session) {
    if (!requireNamespace("querychat", quietly = TRUE)) {
      stop("Please install.packages('querychat') to use `enable_querychat = TRUE`")
    }

    # `querychat`'s data source is supplied once, at construction/`$server()` time, and there is
    # no supported way to swap it afterwards. Since dv.manager allows switching `dataset_list` at
    # runtime, we fully recreate the `QueryChat` instance (fresh connection, fresh chat transcript,
    # fresh sub-namespace) every time the dataset_list's identity changes, tracked via `generation`.
    generation <- shiny::reactiveVal(0L)
    qc_state <- shiny::reactiveVal(NULL) # list(qc = , qc_vals = , con = )

    dataset_list_signature <- shiny::reactive({
      querychat_dataset_list_signature(selected_dataset_list())
    })

    shiny::observeEvent(dataset_list_signature(), {
      old <- qc_state()
      if (!is.null(old)) {
        # `qc$cleanup()` also closes the underlying DBI connection (`con`) it was given.
        try(old[["qc"]]$cleanup(), silent = TRUE)
      }
      generation(generation() + 1L)
    })

    output[["chat_ui"]] <- shiny::renderUI({
      gen <- generation()
      dsl <- selected_dataset_list()
      shiny::req(length(dsl) > 0, subject_filter_dataset_name %in% names(dsl))

      con <- new_querychat_connection(dsl)

      data_description <- querychat_args[["data_description"]] %||%
        build_querychat_data_description(dsl, subject_filter_dataset_name, filter_key_var)

      qc_new_args <- utils::modifyList(
        list(
          data_source = con,
          table_name = subject_filter_dataset_name,
          id = paste0("qc_gen_", gen),
          tools = "filter",
          data_description = data_description
        ),
        querychat_args[setdiff(names(querychat_args), "data_description")]
      )

      qc <- do.call(querychat::QueryChat$new, qc_new_args)
      qc_vals <- qc$server(enable_bookmarking = TRUE)

      qc_state(list(qc = qc, qc_vals = qc_vals, con = con))
      qc$ui()
    })

    session$onSessionEnded(function() {
      current <- shiny::isolate(qc_state())
      if (!is.null(current)) {
        try(current[["qc"]]$cleanup(), silent = TRUE)
      }
    })

    res <- shiny::reactive({
      error_list <- new_error_list()
      current <- qc_state()

      if (is.null(current)) {
        return(list(result = list(subjects = NULL), error_list = error_list))
      }

      current_sql <- current[["qc_vals"]]$sql()

      if (is.null(current_sql)) {
        subjects <- NULL # No filter applied yet -> do not constrain anything
      } else {
        chat_df <- current[["qc_vals"]]$df()

        if (!filter_key_var %in% names(chat_df)) {
          error_list$push(sprintf(
            "querychat: `%s` not present in the chat result, filter not applied",
            filter_key_var
          ))
          subjects <- NULL
        } else {
          subjects <- as.character(unique(chat_df[[filter_key_var]]))
        }
      }

      list(result = list(subjects = subjects), error_list = error_list)
    })

    return(res)
  }
  shiny::moduleServer(id, mod)
}
