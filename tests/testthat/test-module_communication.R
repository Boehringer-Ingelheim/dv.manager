# nolint start

domain_list <- list(
  a = tibble::tibble(A = 1:3, B = 4:6),
  b = tibble::tibble(A = 1:3, B = c("a", "b", "c"))
)
datasets <- list(
  DS1 = domain_list,
  DS2 = domain_list
)

test_that(
  vdoc[["add_spec"]](
    "module_communication should allow passing reactive values between modules",
    c(specs$MODULES$MODULE_OUTPUT_LIST)
  ),
  {
    mod_return_reactive <- function(mod_id) {
      srv <- function(id) {
        shiny::moduleServer(
          id,
          function(input, output, session) {
            return(shiny::reactive("1"))
          }
        )
      }

      list(
        ui = function(id) {
          shiny::h1("")
        },
        server = function(afmm) {
          srv(id = mod_id)
        },
        module_id = mod_id
      )
    }

    testing_options <- list(
      data = datasets,
      filter_data = "a",
      filter_key = "A",
      module_info = resolve_module_list(list(
        "id 1" = mod_return_reactive(
          mod_id = "mod_1"
        ),
        "id 2" = mod_identity(
          value = "mod_1",
          from = "module_output",
          mod_id = "mod_2"
        )
      )),
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    shiny::testServer(
      app_server_test(testing_options),
      {
        # Expect warning because no date is provided supressed as it is not interesting
        session$setInputs(selector = "DS1")
        expect_equal(module_output[["mod_2"]]()(), "1")
      }
    ) |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]](
    "module_communication should allow passing non-reactive values between modules",
    c(specs$MODULES$MODULE_OUTPUT_LIST)
  ),
  {
    mod_return_static <- function(mod_id) {
      srv <- function(id) {
        shiny::moduleServer(
          id,
          function(input, output, session) {
            return("1")
          }
        )
      }

      list(
        ui = function(id) {
          shiny::h1("")
        },
        server = function(afmm) {
          srv(id = mod_id)
        },
        module_id = mod_id
      )
    }

    testing_options <- list(
      data = datasets,
      filter_data = "a",
      filter_key = "A",
      module_info = resolve_module_list(list(
        "id 1" = mod_return_static(
          mod_id = "mod_1"
        ),
        "id 2" = mod_identity(
          value = "mod_1",
          from = "module_output",
          mod_id = "mod_2"
        )
      )),
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    shiny::testServer(
      app_server_test(testing_options),
      {
        # Expect warning because no date is provided supressed as it is not interesting
        session$setInputs(selector = "DS1")
        expect_equal(module_output[["mod_2"]](), "1")
      }
    ) |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]](
    "module_communication should allow passing reactive and non-reactive values between modules",
    c(specs$MODULES$MODULE_OUTPUT_LIST)
  ),
  {
    mod_return_reactive <- function(mod_id) {
      srv <- function(id) {
        shiny::moduleServer(
          id,
          function(input, output, session) {
            return(shiny::reactive("1"))
          }
        )
      }

      list(
        ui = function(id) {
          shiny::h1("")
        },
        server = function(afmm) {
          srv(id = mod_id)
        },
        module_id = mod_id
      )
    }

    mod_return_static <- function(mod_id) {
      srv <- function(id) {
        shiny::moduleServer(
          id,
          function(input, output, session) {
            return("1")
          }
        )
      }

      list(
        ui = function(id) {
          shiny::h1("")
        },
        server = function(afmm) {
          srv(id = mod_id)
        },
        module_id = mod_id
      )
    }

    mod_return_output <- function(mod_id) {
      identity_server <- function(id, value) {
        shiny::moduleServer(
          id,
          function(input, output, session) {
            return(value)
          }
        )
      }

      list(
        ui = function(id) {
          shiny::h1("")
        },
        server = function(afmm) {
          identity_server(
            id = mod_id,
            shiny::reactive({
              afmm[["module_output"]]()[c("mod_1", "mod_3")]
            })
          )
        },
        module_id = mod_id
      )
    }

    testing_options <- list(
      data = datasets,
      filter_data = "a",
      filter_key = "A",
      module_info = resolve_module_list(list(
        "id 1" = mod_return_reactive("mod_1"),
        "id 3" = mod_return_static("mod_3"),
        "id 2" = mod_return_output("mod_2")
      )),
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    shiny::testServer(
      app_server_test(testing_options),
      {
        # Expect warning because no date is provided supressed as it is not interesting
        session$setInputs(selector = "DS1")
        expect_equal(module_output[["mod_2"]]()[["mod_3"]], "1")
        expect_equal(module_output[["mod_2"]]()[["mod_1"]](), "1")
      }
    ) |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

# nolint end
