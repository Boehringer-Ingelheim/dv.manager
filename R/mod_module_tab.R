mod_module_tab <- function(module_list, tab_id, ...) {
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

run_mock_app_module_tab <- function() {
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
        "Module Tab" = mod_module_tab(
          module_list = list(
            "Simple2" = mod_simple(mm_dispatch("unfiltered_dataset", "adsl"), "mod2"),
            "Simple3" = mod_simple(mm_dispatch("filtered_dataset", "adae"), "mod3"),
            "Send and Receive 2" = mod_com_test(
              choices = c("a", "b", "c"),
              message = "The other module has selected",
              value = mm_dispatch("module_output", "mod_rec_1"),
              mod_id = "mod_rec_2"
            ),
            "Nested modules" = mod_module_tab(
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
