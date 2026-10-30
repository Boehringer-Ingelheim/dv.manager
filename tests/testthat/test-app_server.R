# nolint start
# Testing app_server ----
domain_list <- list(
  a = datasets::mtcars,
  b = datasets::mtcars,
  c = datasets::mtcars
)
test_that(
  vdoc[["add_spec"]](
    "app_server_ should set as output the selected dataset name",
    c(specs$MODULES$MODULE_ACCESS_DATASET_LIST_NAME)
  ),
  {
    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    module_info <- resolve_module_list(list())

    afmm_static <- list(
      data = datasets,
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = "a",
      module_info = module_info,
      filter_key = "mpg",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_name, "Dataset name: DS1")
    }) |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]](
    "app_server_ should work with a functions that return datasets",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE)
  ),
  {
    datasets <- list(
      DS1 = function() {
        domain_list
      },
      DS2 = function() {
        domain_list
      }
    )
    module_info <- resolve_module_list(list())

    afmm_static <- list(
      data = datasets,
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = "a",
      module_info = module_info,
      filter_key = "mpg",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_name, "Dataset name: DS1")
    }) |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]](
    "app_server_ should accept a list of modules and display them in the application",
    c(
      specs$MODULES$MODULE_CONTENT_DISPLAY,
      specs$MODULES$MODULE_DEFINITION_STRUCTURE,
      specs$MODULES$MODULE_DEFINITION_UI_NAME
    )
  ),
  {
    module_info <- resolve_module_list(list(
      "mod_1" = mod_identity(1, from = NULL, "mod_1"),
      "mod_2" = mod_identity(2, from = NULL, "mod_2")
    ))

    afmm_static <- list(
      data = list(),
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      module_info = module_info,
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    shiny::testServer(app_server_test(testing_options), {
      expect_equal(module_output[["mod_1"]], 1)
      expect_equal(module_output[["mod_2"]], 2)
    })
  }
)

test_that(
  vdoc[["add_spec"]]("app_server_ should accept an empty list of modules", c(specs$MODULES$MODULE_LIST_EMPTY_ALLOWED)),
  {
    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )
    module_info <- resolve_module_list(list())

    afmm_static <- list(
      data = datasets,
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = "a",
      module_info = module_info,
      filter_key = "mpg",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_name, "Dataset name: DS1")
    }) |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry") |>
      expect_warning(regexp = "[abc]{1} has no date. no meta attribute or no mtime entry")
  }
)

test_that(
  vdoc[["add_spec"]](
    "app_server_ should accept an empty list of datasets",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE, specs$DATASETS$DATASET_LIST_EMPTY_ALLOWED)
  ),
  {
    module_info <- resolve_module_list(list("mod_1" = mod_identity(1, mod_id = "mod_1")))
    afmm_static <- list(
      data = list(),
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = NULL,
      module_info = module_info,
      filter_key = NULL,
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    testServer(app_server_test(testing_options), {
      expect_equal(module_output[["mod_1"]], 1)
    })
  }
)

test_that(
  vdoc[["add_spec"]](
    "app_server_ should accept an empty list of datasets and an empty list of modules",
    c(
      specs$DATASETS$DATASET_ENTRY_STRUCTURE,
      specs$MODULES$MODULE_LIST_EMPTY_ALLOWED,
      specs$DATASETS$DATASET_LIST_EMPTY_ALLOWED
    )
  ),
  {
    module_info <- resolve_module_list(list())
    afmm_static <- list(
      data = list(),
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      module_info = module_info,
      filter_key = NULL,
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )
    shiny::testServer(app_server_test(testing_options), {}) |>
      expect_error(regexp = NA)
  }
)

# Testing date_output ----

component <- "date output"

test_that(
  vdoc[["add_spec"]](
    "date output should output the earliest and latest date of the selected dataset if not all are equal",
    c(specs$DATASETS$DATASET_LIST_MOD_DATE_RANGE)
  ),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-14 00:00:00"),
      c = lubridate::ymd_hms("2021-01-16 00:00:00")
    )

    domain_list <- domain_list |>
      purrr::map2(
        date_list,
        ~ {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )

    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    module_info <- resolve_module_list(list())
    afmm_static <- list(
      data = datasets,
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = "a",
      module_info = module_info,
      filter_key = "mpg",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    withr::local_locale(.new = list("LC_TIME" = "en_US.UTF-8"))

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_date, "Dataset date: 2021-Jan-13 (UTC) - 2021-Jan-16 (UTC)")
    })
  }
)

test_that(
  vdoc[["add_spec"]](
    "date output should output only one date of the selected dataset if all are equal",
    c(specs$DATASETS$DATASET_LIST_MOD_DATE_SINGLE)
  ),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-13 00:00:00"),
      c = lubridate::ymd_hms("2021-01-13 00:00:00")
    )

    domain_list <- domain_list |>
      purrr::map2(
        date_list,
        ~ {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )

    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    module_info <- resolve_module_list(list())
    afmm_static <- list(
      data = datasets,
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = "a",
      module_info = module_info,
      filter_key = "mpg",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    withr::local_locale(.new = list("LC_TIME" = "en_US.UTF-8"))

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_date, "Dataset date: 2021-Jan-13 (UTC)")
    })
  }
)

test_that(
  vdoc[["add_spec"]](
    "date output should output 'date unavailable' when at least one date is not available",
    c(specs$DATASETS$DATASET_LIST_MOD_DATE_UNAVAILABLE_UI)
  ),
  {
    date_list <- list(
      a = lubridate::ymd_hms("2021-01-13 00:00:00"),
      b = lubridate::ymd_hms("2021-01-13 00:00:00"),
      c = lubridate::ymd_hms("2021-01-13 00:00:00")
    )

    domain_list <- domain_list |>
      purrr::map2(
        date_list,
        ~ {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )

    datasets <- list(
      DS1 = domain_list,
      DS2 = domain_list
    )

    attr(datasets[["DS2"]][["a"]], "meta") <- list(mtime = NULL)

    module_info <- resolve_module_list(list())
    afmm_static <- list(
      data = datasets,
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = "a",
      module_info = module_info,
      filter_key = "mpg",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )

    withr::local_locale(.new = list("LC_TIME" = "en_US.UTF-8"))

    shiny::testServer(app_server_test(testing_options), {
      session$setInputs(selector = "DS1")
      expect_equal(output$dataset_date, "Dataset date: 2021-Jan-13 (UTC)")
      session$setInputs(selector = "DS2")
      expect_equal(output$dataset_date, "Dataset date: Date unavailable")
    }) |>
      expect_warning(regexp = "a has no date. no meta attribute or no mtime entry")
  }
)

# Testing reload_period ----

test_that(
  vdoc[["add_spec"]](
    "date output restart.txt time is altered when being touched",
    c(specs$DATASETS$DATASET_LISTS_RELOAD_DURATION)
  ),
  {
    datasets <- list(
      DS1 = function() {
        domain_list
      },
      DS2 = function() {
        domain_list
      }
    )

    module_info <- resolve_module_list(list())
    afmm_static <- list(
      data = datasets,
      module_names = module_info[["module_name"]]
    )

    testing_options <- list(
      afmm_static = afmm_static,
      filter_dataset_name = "a",
      module_info = module_info,
      filter_key = "mpg",
      reload_period = lubridate::duration(1, "seconds"),
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE
    )
    withr::with_dir(tempdir(), {
      system2(command = "touch", args = c("restart.txt"), stdout = TRUE)
      time_pre_touch <- lubridate::as_datetime(file.info("restart.txt")[["mtime"]])
      Sys.sleep(1)

      shiny::testServer(app_server_test(testing_options), {
        time_post_touch <- lubridate::as_datetime(file.info("restart.txt")[["mtime"]])
        expect_true(time_pre_touch != time_post_touch)
      })
    })
  }
)

test_that("app_server_ errors when a module has ODG elements unnamed or repeated", {
  # Not a fan as we are mimicking too many things. Maybe we can extract the flatten in a function
  odg_testing_options <- function(to_odg) {
    module_info <- resolve_module_list(list(
      "mod ODG" = list(
        ui = function(module_id) NULL,
        server = function(afmm) list(to_odg = to_odg),
        module_id = "mod_odg"
      )
    ))
    list(
      afmm_static = list(data = list(), module_names = module_info[["module_name"]]),
      module_info = module_info,
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE,
      enable_odg = TRUE
    )
  }
  msg <- "ODG elements for module mod_odg are not named or names are not unique"

  unnamed <- odg_testing_options(list(list(label = "a"), list(label = "b")))
  expect_error(shiny::testServer(app_server_test(unnamed), {}), msg)

  repeated <- odg_testing_options(list(x = list(label = "a"), x = list(label = "b")))
  expect_error(shiny::testServer(app_server_test(repeated), {}), msg)

  unique_names <- odg_testing_options(list(x = list(label = "a"), y = list(label = "b")))
  expect_no_error(shiny::testServer(app_server_test(unique_names), {}))
})

test_that("app_server_ errors when two ODG elements end up with the same chunk label", {
  odg_testing_options <- function(module_list) {
    module_info <- resolve_module_list(module_list)
    list(
      afmm_static = list(data = list(), module_names = module_info[["module_name"]]),
      module_info = module_info,
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE,
      enable_odg = TRUE
    )
  }
  odg_module <- function(module_id, element_names) {
    list(
      ui = function(module_id) NULL,
      server = function(afmm) list(to_odg = stats::setNames(lapply(element_names, function(x) list()), element_names)),
      module_id = module_id
    )
  }

  # Same id: `a-b` + `c` and `a` + `b-c` both join into `a-b-c`
  same_id <- odg_testing_options(list("Mod 1" = odg_module("a-b", "c"), "Mod 2" = odg_module("a", "b-c")))
  expect_error(
    shiny::testServer(app_server_test(same_id), {}),
    "ODG elements `a-b-c` (module `a-b`) and `a-b-c` (module `a`) share the chunk label `odg-a-b-c`",
    fixed = TRUE
  )

  # Different ids, same label once characters knitr rejects are replaced
  same_label <- odg_testing_options(list("Mod" = odg_module("m", c("n=10", "n_10"))))
  expect_error(
    shiny::testServer(app_server_test(same_label), {}),
    "ODG elements `m-n=10` (module `m`) and `m-n_10` (module `m`) share the chunk label `odg-m-n_10`",
    fixed = TRUE
  )
})

# nolint end
