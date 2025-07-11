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
      "Separate tab" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod1"),
      "Switch to nest" = mod_switch(
        "Mod 5",
        selected = c("mod5"),
        mm_dispatch("utils", "switch2mod"),
        "mod_switch1"
      ),
      "Module Tab" = tab_group(
        "Simple2" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod2"),
        "Simple3" = mod_simple(mm_dispatch("filtered_dataset", "adae"), "mod3"),
        "Send and Receive 2" = mod_com_test(
          choices = c("a", "b", "c"),
          message = "The other module has selected",
          value = mm_dispatch("module_output", "mod_rec_1"),
          mod_id = "mod_rec_2"
        ),
        "Nested modules" = tab_group(
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
        parent = NA_character_
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

        hierarchy_entry <- list(list(name = curr_name, parent = curr_parent_id))
        names(hierarchy_entry) <- tab_group_id
        res[["hierarchy"]] <- c(res[["hierarchy"]], hierarchy_entry)

        res[["hierarchy"]][[curr_parent_id]][["children"]] <- c(res[["hierarchy"]][[curr_parent_id]][["children"]], tab_group_id)

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

        hierarchy_entry <- list(list(name = curr_name, parent = curr_parent_id))
        names(hierarchy_entry) <- module_id
        res[["hierarchy"]] <- c(res[["hierarchy"]], hierarchy_entry)

        res[["hierarchy"]][[curr_parent_id]][["children"]] <- c(res[["hierarchy"]][[curr_parent_id]][["children"]], module_id)
      }
    }
  }

  res
}


process_module_list <- function(module_list) {
  resolved_module_list <- resolve_module_list(module_list)

  # We need the ns to be able to invoke all ui functions
  # TODO: Consider removing namespacing it would make all these simpler

  resolved_module_list[["ui"]] <- function(ns, footer) {
    compose_ui(resolved_module_list[["hierarchy"]], resolved_module_list[["ui"]], ns, footer)
  }

  return(resolved_module_list)
}

compose_ui <- function(nh, ui_fn_list, ns, footer) {

  browser()
  mod_tabs <- vector(mode = "list", length = length(ui_fn_list))
  mod_buttons <- vector(mode = "list", length = length(ui_fn_list))
  mod_nms <- names(ui_fn_list)

  for (idx in seq_along(mod_tabs)) {
    mod_id <- mod_nms[[idx]]
    mod_tabs[[idx]] <- shiny::div(value = mod_id, ns_css(ui_fn_list[[mod_id]][["ui"]](ns(ui_fn_list[[mod_id]][["module_id"]]))), class = "dv_tab_content")
    mod_buttons[[idx]] <- shiny::tags[["button"]]("data-value" = mod_id, mod_id, type = "button", class = "dv_tab_activate_button")
  }

  buttons_hierarchy <- list()

  stack <- list()
  push <- function(x) {
    stack <<- c(stack, list(x))
  }
  pop <- function() {
    x <- stack[[length(stack)]]
    stack <<- stack[-length(stack)]
    x
  }

  push(nh)

  while (length(stack) > 0) {
    curr_el <- pop()
    curr_level <- list()
    curr_el_id <- curr_el[["tabset_id"]]
    is_root <- is.na(curr_el[["tabset_name"]])
    children <- curr_el[["children"]]

    for (idx in seq_along(children)) {
      curr_child <- children[[idx]]
      is_tabset <- "tabset_id" %in% names(curr_child)
      is_leaf <- checkmate::test_string(curr_child, min.chars = 1)

      if (is_tabset) {
        curr_child_id <- curr_child[["tabset_id"]]
        curr_child_name <- curr_child[["tabset_name"]]
        curr_level[[idx]] <- shiny::tags[["button"]]("data-value" = curr_child_id, "data-type" = "hier-button", curr_child_name, type = "button", class = "dv_tab_activate_button btn btn-primary")
        push(curr_child)
      } else if (is_leaf) {
        curr_level[[idx]] <- shiny::tags[["button"]]("data-value" = ui_fn_list[[curr_child]][["module_id"]], "data-type" = "tab-button", ui_fn_list[[curr_child]][["module_label"]], type = "button", class = "dv_tab_activate_button btn btn-primary")
      } else {
        stop("Unknown element")
      }

      if (idx == 1) {
        curr_level[[idx]] <- htmltools::tagAppendAttributes(curr_level[[idx]], class = "clicked")
      }
    }

    buttons_hierarchy[[curr_el_id]] <- shiny::div("value" = curr_el_id, curr_level, class = "dv_button_level")
    if (is_root) {
      buttons_hierarchy[[curr_el_id]] <- htmltools::tagAppendAttributes(buttons_hierarchy[[curr_el_id]], class = "dv_root_button_level")
    } else {
      buttons_hierarchy[[curr_el_id]] <- htmltools::tagAppendAttributes(buttons_hierarchy[[curr_el_id]], class = "dv_child_button_level")
    }
  }

  tabs <- shiny::div(class = "dv_tab_container", mod_tabs)
  header <- shiny::div(
    buttons_hierarchy,
    class = "dv_button_container",
    id = ns(ID$NAV_HEADER)
  )
  default_tab <- shiny::restoreInput(ns(ID$NAV_HEADER), NA)
  if (!is.na(default_tab)) header <- htmltools::tagAppendAttributes(header, "default-tab" = default_tab)

  script <- shiny::tags[["script"]](
    sprintf("dv_tab.init(\"%s\")", ns(ID$NAV_HEADER))
  )

  ui <- shiny::div(header, script, tabs, footer, class = "dv_main_panel")

  return(ui)
}
