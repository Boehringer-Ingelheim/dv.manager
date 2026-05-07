run_mock_app_tab_group <- function() {
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
      "Separate tab" = mod_simple("adsl", "unfiltered_dataset_list", "mod1"),
      "Switch to nest" = mod_switch(
        "Mod 5",
        selected = c("mod5"),
        "mod_switch1"
      ),
      "Module Tab" = tab_group(
        "Simple2" = mod_simple("adsl", "unfiltered_dataset_list", "mod2"),
        "Simple3" = mod_simple("adae", "filtered_dataset_list", "mod3"),
        "Send and Receive 2" = mod_com_test(
          choices = c("a", "b", "c"),
          message = "The other module has selected",
          module_from_id = "mod_rec_1",
          mod_id = "mod_rec_2"
        ),
        "Nested modules" = tab_group(
          "Simple4" = mod_simple("adsl", "unfiltered_dataset_list", "mod4"),
          "Simple5" = mod_simple("adae", "filtered_dataset_list", "mod5"),
          "Send and Receive 1" = mod_com_test(
            choices = 1:3,
            message = "The other module has selected",
            module_from_id = "mod_rec_2",
            mod_id = "mod_rec_1"
          )
        )
      )
    ),
    filter_data = "adsl",
    filter_key = "USUBJID"
  )
}

#' Create a Tabbed Shiny Module Collection
#'
#' @description
#' `tab_group` is a function that groups dv modules in tabsets (see [shiny::tabsetPanel]).
#' This function is designed to simplify the process of organizing multiple Shiny modules into a single,
#' visually organized tabset. It allows nested modules.
#'
#' @param ... A set of davinci modules or tab groups
#'
#' @return The set of of modules wrapped in a list marked with an attribute.
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
tab_group <- function(...) {
  module_list <- list(...)
  attr(module_list, LAYOUT$ATTRIBUTE) <- LAYOUT$TAB_GROUP # nolint
  module_list
}

is_tab_group <- function(x) {
  identical(attr(x, which = LAYOUT$ATTRIBUTE, exact = TRUE), LAYOUT$TAB_GROUP)
}

resolve_module_list <- function(module_list) {
  res <- list(
    server = list(),
    ui = list(),
    meta = list(),
    module_name = character(),
    module_id = character(),
    tab_name = character(),
    hierarchy = list(
      "__tabset_0__" = list(
        name = NA_character_,
        parent = NA_character_,
        kind = "root"
      )
    )
  )

  tab_group_count <- 0

  stack <- list()
  push <- function(x) {
    stack <<- c(stack, list(x))
  }
  pop <- function() {
    x <- stack[[length(stack)]]
    stack <<- stack[-length(stack)]
    x
  }

  push(list(module_list = module_list, parent_id = names(res[["hierarchy"]])[[1]]))

  while (length(stack) > 0) {
    curr_el <- pop()
    curr_parent_id <- curr_el[["parent_id"]]
    curr_module_list <- curr_el[["module_list"]]
    for (idx in seq_along(curr_module_list)) {
      curr_child <- curr_module_list[[idx]]
      curr_name <- names(curr_module_list)[[idx]]
      if (is_tab_group(curr_child)) {
        tab_group_count <- tab_group_count + 1
        tab_group_id <- paste0("__tabset_", tab_group_count, "__")

        hierarchy_entry <- list(list(name = curr_name, parent = curr_parent_id, kind = "tab_group"))
        names(hierarchy_entry) <- tab_group_id
        res[["hierarchy"]] <- c(res[["hierarchy"]], hierarchy_entry)

        res[["hierarchy"]][[curr_parent_id]][["children"]] <- c(
          res[["hierarchy"]][[curr_parent_id]][["children"]],
          tab_group_id
        )

        tab_name_entry <- curr_name
        names(tab_name_entry) <- tab_group_id
        res[["tab_name"]] <- c(res[["tab_name"]], tab_name_entry)

        assert(length(curr_child) > 0, "Tab groups cannot be empty")

        push(list(module_list = curr_child, parent_id = tab_group_id))
      } else {
        module_id <- curr_child[["module_id"]]

        ui_entry <- list(curr_child[["ui"]])
        names(ui_entry) <- module_id
        res[["ui"]] <- c(res[["ui"]], ui_entry)

        server_entry <- list(curr_child[["server"]])
        names(server_entry) <- module_id
        res[["server"]] <- c(res[["server"]], server_entry)

        meta_entry <- list(curr_child[["meta"]])
        names(meta_entry) <- module_id
        res[["meta"]] <- c(res[["meta"]], meta_entry)

        module_id_entry <- curr_child[["module_id"]]
        names(module_id_entry) <- module_id
        res[["module_id"]] <- c(res[["module_id"]], module_id_entry)

        module_name_entry <- curr_name
        names(module_name_entry) <- module_id
        res[["module_name"]] <- c(res[["module_name"]], module_name_entry)

        hierarchy_entry <- list(list(name = curr_name, parent = curr_parent_id, kind = "module"))
        names(hierarchy_entry) <- module_id
        res[["hierarchy"]] <- c(res[["hierarchy"]], hierarchy_entry)

        res[["hierarchy"]][[curr_parent_id]][["children"]] <- c(
          res[["hierarchy"]][[curr_parent_id]][["children"]],
          module_id
        )
      }
    }
  }

  res
}

compose_ui <- function(hierarchy, ui_fn_list, ns, footer, top_buttons) {
  mod_ui_containers <- vector(mode = "list", length = length(ui_fn_list))
  mod_ids <- names(ui_fn_list)

  for (idx in seq_along(mod_ui_containers)) {
    curr_mod_id <- mod_ids[[idx]]
    mod_ui_containers[[idx]] <- shiny::div(
      value = curr_mod_id,
      ns_css(ui_fn_list[[curr_mod_id]](ns(curr_mod_id))),
      class = "dv_tab_content"
    )
  }

  tabs <- shiny::div(class = "dv_tab_container", mod_ui_containers)

  tab_menus_container <- shiny::div(class = "dv_tab_menu_container")

  default_tab <- shiny::restoreInput(ns(ID$NAV_HEADER), NA)

  top_buttons_div <- shiny::div(
    shiny::div(
      id = ns("dv_expanded_tab_container"),
      style = "display:inline",
      shiny::div(
        shiny::checkboxInput(ns("expanded_tab"), label = NULL, TRUE),
        style = "display:none"
      ),
      shiny::tags[["label"]](
        shiny::icon("compress"),
        class = "btn btn-primary compress-icon",
        "for" = ns("expanded_tab"),
        title = "Collapse Navigation Bar"
      ),
      shiny::tags[["label"]](
        shiny::icon("expand"),
        class = "btn btn-primary expand-icon",
        "for" = ns("expanded_tab"),
        title = "Expand Navigation Bar"
      )
    ),
    top_buttons,
    class = "dv_top_button_group"
  )

  header <- shiny::div(
    tab_menus_container,
    top_buttons_div,
    id = ns(ID$NAV_HEADER),
    class = "dv_manager_top_bar"
  )

  json_hierarchy <- character(length(hierarchy))
  for (idx in seq_along(hierarchy)) {
    entry <- hierarchy[[idx]]
    parent_id <- if (!is.na(entry[["parent"]])) paste0('"', entry[["parent"]], '"') else "null"
    kind <- if (!is.na(entry[["kind"]])) paste0('"', entry[["kind"]], '"') else "null"
    name <- if (!is.na(entry[["name"]])) paste0('"', entry[["name"]], '"') else "null"
    children <- if (entry[["kind"]] != "module" && length(entry[["children"]]) > 0) {
      paste0(", \"children\":[", paste0("\"", ns(entry[["children"]]), "\"", collapse = ","), "]")
    } else {
      paste0(", \"children\":[]")
    }

    json_hierarchy[[idx]] <- paste0(
      paste0("\"", names(hierarchy)[[idx]], "\":"),
      "{",
      "\"parent_id\":",
      ns(parent_id),
      ", \"kind\":",
      kind,
      ", \"name\":",
      name,
      children,
      "}"
    )
  }

  json_hierarchy <- paste0("{", paste0(json_hierarchy, collapse = ","), "}")
  default_tab <- if (!is.na(default_tab)) {
    paste0("\"", default_tab, "\"")
  } else {
    "null"
  }

  json_module_tab_selector_state <- paste0(
    "{",
    "\"default_tab\":",
    default_tab,
    ",",
    "\"hierarchy\":",
    json_hierarchy,
    "}"
  )

  escape_special_chars <- function(x) {
    y <- gsub("\\\\", "\\\\\\\\", x)
    y <- gsub('"', '\\\\"', y)
    y <- gsub("'", "\\\\'", y)
    y <- gsub("\n", "\\\\n", y)
    y <- gsub("\r", "\\\\r", y)
    y <- gsub("\t", "\\\\t", y)
    y
  }

  script <- shiny::tags[["script"]](
    sprintf(
      "dv_tab.init('%s', '%s')",
      ns(ID$NAV_HEADER),
      escape_special_chars(json_module_tab_selector_state)
    )
  )

  ui <- shiny::div(header, script, tabs, footer, class = "dv_main_panel")

  return(ui)
}
