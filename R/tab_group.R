run_mock_app_tab_group <- function() {
  module_list <-
    run_app(
      data = list(
        "D1" = list(
          adsl = get_pharmaverse_data("adsl"),
          adae = get_pharmaverse_data("adae")
        ),
        "D2" = list(
          adsl = get_pharmaverse_data("adsl"),
          adae = get_pharmaverse_data("adae"),
          adlb = get_pharmaverse_data("adlb")
        )
      ),
      module_list = list(
        "Separate tab" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod1"),
        "Switch to nest" = mod_switch(
          "Mod 5",
          selected = c("mod5"),
          mm_dispatch("utils", "switch2mod"),
          "mod_switch1"
        ),
        "Module Tab" = tab_group(
          module_list = list(
            "Simple2" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod2"),
            "Simple3" = mod_simple(mm_dispatch("filtered_dataset", "adae"), "mod3"),
            "Send and Receive 2" = mod_com_test(
              choices = c("a", "b", "c"),
              message = "The other module has selected",
              value = mm_dispatch("module_output", "mod_rec_1"),
              mod_id = "mod_rec_2"
            ),
            "Nested modules" = tab_group(
              module_list = list(
                "Simple4" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod4"),
                "Simple5" = mod_simple(mm_dispatch("filtered_dataset", "adae"), "mod5"),
                "Send and Receive 1" = mod_com_test(
                  choices = 1:3,
                  message = "The other module has selected",
                  value = mm_dispatch("module_output", "mod_rec_2"),
                  mod_id = "mod_rec_1"
                )
              )
            )
          )
        )
      ),
      filter_data = "adsl",
      filter_key = "USUBJID"
    )
}

LAYOUT <- poc( # nolint
  ATTRIBUTE = "layout",
  TAB_GROUP = "tab_group"
)

#' Create a Tabbed Shiny Module Collection
#'
#' @description
#' `tab_group` is a function that groups dv modules in tabsets (see [shiny::tabsetPanel]).
#' This function is designed to simplify the process of organizing multiple Shiny modules into a single,
#' visually organized tabset. It allows nested modules.
#'
#' @param module_list A list of modules.
#'
#' @return The module list marked with an attribute.
#'
#' @details
#' The function does not make use of `namespace` (`NS()`) or `shiny::moduleServer` to implement traditional Shiny
#' modules, but creates a tab-like UI structure, making it an aesthetic modification.
#'
#' Outputs of grouped modules are accessible by using the module id, there is no special change required.
#'
#' When switching to grouped outputs the id of the module can be used directly. This approach is incompatible
#' with the `switch2` function and must be used with `switch2mod`.
#'
#'
#' @export
#'
tab_group <- function(module_list) {
  attr(module_list, LAYOUT$ATTRIBUTE) <- LAYOUT$TAB_GROUP # nolint
  module_list
}

is_tab_group <- function(x) {
  identical(attr(x, which = LAYOUT$ATTRIBUTE, exact = TRUE), LAYOUT$TAB_GROUP)
}

resolve_tab_group <- function(x, nm, hierarchy, tab_group_count, nested_hierarchy) {
  message(paste("Resolving tab", nm))
  new_tab_group_count <- tab_group_count + 1
  # The id of the new tabset is the same as the value of the tab in the parent tabset
  # nolint start
  # shiny::tabSetPanel(id = parent, shiny::tabPanel(value = new_parent_tab_value, shiny::tabsetPanel(id = new_child_tabset_id)))
  # nolint end
  new_tab_group_id <- paste0("__tabset_", new_tab_group_count, "__")

  assert(is.na(hierarchy[[length(hierarchy)]]))
  # First replace the parent one with the new id
  hierarchy[[length(hierarchy)]] <- new_tab_group_id
  # Then create a child tabset
  hierarchy[[new_tab_group_id]] <- NA
  tab_group_names <- character(0)
  tab_group_names[[new_tab_group_id]] <- nm

  this_nested_hierarchy <- list(
    tabset_id = new_tab_group_id,
    tabset_name = nm,
    children = list()
  )
  r <- resolve_module_list(x, hierarchy, new_tab_group_count, this_nested_hierarchy)

  return(
    list(
      ui_list = r[["ui_list"]],
      server_list = r[["server_list"]],
      module_id_list = r[["module_id_list"]],
      module_name_list = r[["module_name_list"]],
      tab_label_list = r[["tab_label_list"]],
      hierarchy_list = r[["hierarchy_list"]],
      tab_group_count = r[["tab_group_count"]],
      tab_group_names = c(tab_group_names, r[["tab_group_names"]]),
      nested_hierarchy = r[["nested_hierarchy"]]
    )
  )
}

resolve_plain <- function(x, nm, hierarchy, nested_hierarchy) {
  ui_list <- list()
  ui_list[[x[["module_id"]]]] <- list(
    ui = x[["ui"]],
    module_id = x[["module_id"]],
    module_label = nm
  )

  server_list <- list()

  server_list[[x[["module_id"]]]] <- list(
    server = x[["server"]],
    module_id = x[["module_id"]]
  )

  module_id_list <- character(0)
  module_id_list[[nm]] <- x[["module_id"]]

  module_name_list <- character(0)
  module_name_list[[x[["module_id"]]]] <- nm

  # Replace last entry by module_id
  hierarchy[[length(hierarchy)]] <- x[["module_id"]]

  hierarchy_list <- list()
  hierarchy_list[[x[["module_id"]]]] <- hierarchy

  nested_hierarchy <- x[["module_id"]]

  r <- list(
    ui_list = ui_list,
    server_list = server_list,
    module_id_list = module_id_list,
    module_name_list = module_name_list,
    hierarchy_list = hierarchy_list,
    nested_hierarchy = nested_hierarchy
  )
}

resolve_module_list <- function(
    module_list,
    hierarchy = list("__tabset_0__" = NA),
    tab_group_count = 0,
    nested_hierarchy = list(
      tabset_id = "__tabset_0__",
      tabset_name = NA,
      children = list()
    )) {
  server_list <- list()
  ui_list <- list()
  module_id_list <- character(0)
  module_name_list <- character(0)
  tab_group_names <- character(0)
  nm_module_list <- names(module_list)
  hierarchy_list <- list()

  for (idx in seq_along(module_list)) {
    module <- module_list[[idx]]
    nm <- nm_module_list[[idx]]

    if (is_tab_group(module)) {
      r <- resolve_tab_group(module, nm, hierarchy, tab_group_count, nested_hierarchy)
      tab_group_count <- r[["tab_group_count"]]
    } else {
      r <- resolve_plain(module, nm, hierarchy, nested_hierarchy)
    }

    ui_list <- c(ui_list, r[["ui_list"]])
    module_id_list <- c(module_id_list, r[["module_id_list"]])
    server_list <- c(server_list, r[["server_list"]])
    module_name_list <- c(module_name_list, r[["module_name_list"]])
    hierarchy_list <- c(hierarchy_list, r[["hierarchy_list"]])
    tab_group_names <- c(tab_group_names, r[["tab_group_names"]])
    nested_hierarchy[["children"]] <- c(nested_hierarchy[["children"]], list(r[["nested_hierarchy"]]))
  }

  res <- list(
    ui_list = ui_list,
    server_list = server_list,
    module_id_list = module_id_list,
    module_name_list = module_name_list,
    hierarchy_list = hierarchy_list,
    tab_group_count = tab_group_count,
    tab_group_names = tab_group_names,
    nested_hierarchy = nested_hierarchy
  )

  return(res)
}

process_module_list <- function(module_list) {
  module_list <- tab_group(module_list)
  resolved_module_list <- resolve_module_list(module_list)
  # We need the ns to be able to invoke all ui functions
  # TODO: Consider removing namespacing it would make all these simpler
  resolved_module_list[["ui"]] <- function(ns) {
    compose_ui(resolved_module_list[["nested_hierarchy"]], resolved_module_list[["ui_list"]], ns)
  }

  return(resolved_module_list)
}

compose_ui <- function(nh, ui_fn_list, ns) {
  if (is.list(nh)) {
    assert(setequal(c("tabset_id", "children", "tabset_name"), names(nh)), "Incorrect tabset structure")
    is_root <- is.na(nh[["tabset_name"]])
    children_ui <- lapply(nh[["children"]], \(x) compose_ui(x, ui_fn_list, ns))
    ui <- do.call(shiny::tabsetPanel, c(list(id = nh[["tabset_id"]]), children_ui, type = "pills"))

    if (!is_root) {
      res <- shiny::tabPanel(
        title = nh[["tabset_name"]],
        value = nh[["tabset_id"]],
        ui
      )
    } else {
      res <- ui
    }
  } else if (checkmate::test_string(nh)) {
    res <- shiny::tabPanel(
      title = ui_fn_list[[nh]][["module_label"]],
      value = ui_fn_list[[nh]][["module_id"]],
      ns_css(ui_fn_list[[nh]][["ui"]](ns(ui_fn_list[[nh]][["module_id"]])))
    )
  } else {
    stop("Unknown Case")
  }
  return(res)
}
