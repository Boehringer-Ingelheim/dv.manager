#' Create a Tabbed Shiny Module Collection
#'
#' @description
#' `tab_group` is a function that groups dv modules in tabsets (see [shiny::tabsetPanel]).
#' This function is designed to simplify the process of organizing multiple Shiny modules into a single,
#' visually organized tabset. It allows nested modules.
#'
#' @param module_list A list of modules.
#' @param tab_id A string representing the `tab_id` of the new module. This must be unique among all IDs
#'
#' @return A list representing a Shiny module that contains:
#'   - `ui`: A function to generate the user interface for the tabset containing all the provided modules.
#'   - `server`: A list of server functions with their corresponding module IDs.
#'   - `module_id`: The ID for the module (provided by `tab_id` parameter).
#'
#' @details
#' The function does not make use of `namespace` (`NS()`) or `shiny::moduleServer` to implement traditional Shiny
#' modules, but creates a tab-like UI structure, making it an aesthetic modification.
#'
#' Outputs of grouped modules are accessible by using the module id, there is no special change required.
#'
#' When switching to grouped outputs the full hierarchy of grouped modules should be specified:adae
#' `selected = c("Module Tab", mod_tab = "Nested modules", nested_mod_tab = "Single nested module")`.
#'
#'
#' @export
#'
tab_group <- function(module_list, tab_id) {
  # We will mimic a module although we will not
  # No namespaces or shiny::moduleServer functions will be done
  # The change will be purely aesthetic but inner workings will remain the same
  # Length 0 so it does not namespace (see shiny::NS doc)
  module_id <- tab_id

  ui <- function(namespaced_tab_id, parent_id) {
    module_ns <- shiny::NS(parent_id)

    tabs <- unname(
      purrr::imap(
        module_list,
        function(mod, nm) {
          ui_fn <- mod[["ui"]]

          # Offer the option of getting the namespaced id or the namespace function

          if (length(formals(ui_fn)) == 2) {
            ui <- ui_fn(module_ns(mod$module_id), parent_id)
          } else {
            ui <- ui_fn(module_ns(mod$module_id))
          }

          ui_css <- ns_css(ui)

          shiny::tabPanel(title = nm, ui_css)
        }
      )
    )
    do.call(shiny::tabsetPanel, c(
      tabs,
      type = "pills",
      id = namespaced_tab_id
    ))
  }

  srv_funs <- purrr::imap(
    module_list,
    ~ list(
      server = .x[["server"]],
      module_id = .x[["module_id"]]
    )
  )

  class(srv_funs) <- "server_collection"

  mod <- list(
    ui = ui,
    server = srv_funs,
    module_id = tab_id
  )

  return(mod)
}

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
          "Mod 1",
          selected = c("Module Tab", mod_tab = "Nested modules", nested_mod_tab = "Simple5"),
          mm_dispatch("utils", "switch2"),
          "mod_switch"
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
              ),
              tab_id = "nested_mod_tab"
            )
          ),
          tab_id = "mod_tab"
        )
      ),
      filter_data = "adsl",
      filter_key = "USUBJID"
    )
}
