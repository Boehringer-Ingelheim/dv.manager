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
  vdoc[["add_spec"]]("module_communication should allow passing reactive values between modules", c(specs$module_output)),
  {
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      filter_key = "A",
      module_info = resolve_module_list(list(
        "id 1" = mod_identity(
          value = mm_dispatch("filtered_dataset", c("a", "b")),
          mod_id = "mod_1"
        ),
        "id 2" = mod_identity(
          value = mm_dispatch("module_output", "mod_1"),
          mod_id = "mod_2"
        )
      )
    ))


    shiny::testServer(
      app_server_test(testing_options),
      { # Expect warning because no date is provided supressed as it is not interesting
        session$setInputs(selector = "DS1")
        expect_equal(module_output[["mod_2"]](), domain_list[c("a", "b")])
      }
    ) %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]]("module_communication should allow passing non-reactive values between modules", c(specs$module_output)),
  {
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      filter_key = "A",
      module_info = resolve_module_list(list(
        "id 1" = mod_identity(
          value = 2,
          mod_id = "mod_1"
        ),
        "id 2" = mod_identity(
          value = mm_dispatch("module_output", "mod_1"),
          mod_id = "mod_2"
        )
      )
    ))


    shiny::testServer(
      app_server_test(testing_options),
      { # Expect warning because no date is provided supressed as it is not interesting
        session$setInputs(selector = "DS1")
        expect_equal(module_output[["mod_2"]](), 2)
      }
    ) %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]]("module_communication should allow passing reactive and non-reactive values between modules{did:MM-SO-675;tid:NA;WB:WB;NOR:N;}", c(specs$module_output)),
  {
    testing_options <- list(
      data = datasets,
      filter_data = "a",
      filter_key = "A",
      module_info = resolve_module_list(list(
        "id 1" = mod_identity(
          value = mm_dispatch("filtered_dataset", c("a", "b")),
          mod_id = "mod_1"
        ),
        "id 3" = mod_identity(
          value = 3,
          mod_id = "mod_3"
        ),
        "id 2" = mod_identity(
          value = mm_dispatch("module_output", c("mod_1", "mod_3")),
          mod_id = "mod_2"
        )
      )
    ))


    shiny::testServer(
      app_server_test(testing_options),
      { # Expect warning because no date is provided supressed as it is not interesting
        session$setInputs(selector = "DS1")
        expect_equal(module_output[["mod_2"]]()[["mod_3"]], 3)
        expect_equal(module_output[["mod_2"]]()[["mod_1"]](), domain_list)
      }
    ) %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") %>%
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

# nolint end
