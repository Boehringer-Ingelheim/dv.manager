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

        hierarchy_entry <- list(list(name = curr_name, parent = curr_parent_id, kind = "module"))
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

  res <- resolved_module_list
  res[["ui_fn"]] <- function(ns, footer) {
    compose_ui(resolved_module_list[["hierarchy"]], resolved_module_list[["ui"]], ns, footer)
  }

  return(res)
}

compose_ui <- function(hierarchy, ui_fn_list, ns, footer) {

  mod_ui_containers <- vector(mode = "list", length = length(ui_fn_list))
  mod_ids <- names(ui_fn_list)

  for (idx in seq_along(mod_ui_containers)) {
    curr_mod_id <- mod_ids[[idx]]
    mod_ui_containers[[idx]] <- shiny::div(value = curr_mod_id, ns_css(ui_fn_list[[curr_mod_id]](ns(curr_mod_id))), class = "dv_tab_content")
  }

  buttons_hierarchy <- list()

  for (idx in seq_len(length(hierarchy))) {
    curr_el <- hierarchy[[idx]]
    curr_el_id <- names(hierarchy)[[idx]]
    is_el_root <- identical(curr_el[["kind"]], "root")
    is_el_tab_group <- identical(curr_el[["kind"]], "tab_group")
    is_el_module <- identical(curr_el[["kind"]], "module")

    if (is_el_root || is_el_tab_group) {
      curr_level <- list()
      for (jdx in seq_len(length(curr_el[["children"]]))) {
        curr_child_id <- curr_el[["children"]][[jdx]]
        curr_child <- hierarchy[[curr_child_id]]
        curr_child_name <- curr_child[["name"]]
        is_child_tab_group <- identical(curr_child[["kind"]], "tab_group")
        is_child_module <- identical(curr_child[["kind"]], "module")
        if (is_child_tab_group) {
          curr_level[[jdx]] <- shiny::tags[["button"]]("data-value" = curr_child_id, "data-type" = "hier-button", curr_child_name, type = "button", class = "dv_tab_activate_button btn btn-primary")
        } else if (is_child_module) {
          curr_level[[jdx]] <- shiny::tags[["button"]]("data-value" = ns(curr_child_id), "data-type" = "tab-button", curr_child_name, type = "button", class = "dv_tab_activate_button btn btn-primary")
        } else {
          stop(paste("Unknown kind", idx, jdx))
        }

        if (jdx == 1) {
          curr_level[[jdx]] <- htmltools::tagAppendAttributes(curr_level[[jdx]], class = "clicked")
        }

      }

      buttons_hierarchy[[curr_el_id]] <- shiny::div("value" = curr_el_id, curr_level, class = "dv_button_level")

      if (is_el_root) {
        buttons_hierarchy[[curr_el_id]] <- htmltools::tagAppendAttributes(buttons_hierarchy[[curr_el_id]], class = "dv_root_button_level")
      } else if (is_el_tab_group) {
        buttons_hierarchy[[curr_el_id]] <- htmltools::tagAppendAttributes(buttons_hierarchy[[curr_el_id]], class = "dv_child_button_level")
      } else {
        stop(paste("Unknown kind", idx, jdx))
      }

    } else if (is_el_module) {
      next
    } else {
      stop(paste("Unknown kind", idx))
    }
  }

  tabs <- shiny::div(class = "dv_tab_container", mod_ui_containers)
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
