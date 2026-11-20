local({
  table_module <- list(
    ui = function(module_id) NULL,
    server = function(afmm) {
      tbl <- sm_mr(..(afmm[["filtered_dataset_list"]]())[["adsl"]], varname = "tbl")
      list(to_odg = list(tbl = list(label = "Table", codegen = list(pdf = tbl))))
    },
    module_id = "mod1"
  )

  odg_options <- function(load_fn) {
    data <- list(D1 = list(adsl = data.frame(USUBJID = paste0("S", 1:4), AGE = c(20, 30, 40, 50))))
    attr(data[["D1"]], "load_fn") <- load_fn
    module_info <- resolve_module_list(list("Table" = table_module))
    list(
      afmm_static = list(data = data, module_names = module_info[["module_name"]]),
      filter_dataset_name = "adsl",
      module_info = module_info,
      filter_key = "USUBJID",
      filter_info = list(filter_default_state = NULL),
      enable_subgroup = FALSE,
      enable_odg = TRUE
    )
  }

  filter_json <- function(variable) {
    sprintf(
      '{
        "filters": {
          "datasets_filter": {"children": []},
          "subject_filter": {"children": [
            {"kind": "filter", "dataset": "adsl", "operation": "select_range", "variable": "%s",
             "min": 20, "max": 40, "include_NA": true}
          ]}
        },
        "dataset_list_name": "D1"
      }',
      variable
    )
  }

  # Records what the user would see instead of sending it to a client
  local_ui_spy <- function(env = parent.frame()) {
    spy <- new.env()
    spy$notifications <- character(0)
    spy$modal_shown <- FALSE
    testthat::local_mocked_bindings(
      showNotification = function(ui, ...) {
        spy$notifications <- c(spy$notifications, paste(format(ui), collapse = "\n"))
      },
      showModal = function(ui, ...) spy$modal_shown <- TRUE,
      .package = "shiny",
      .env = env
    )
    spy
  }

  valid_load_fn <- function() list(adsl = data.frame(USUBJID = "S1", AGE = 1))
  not_configured_msg <- "Current dataset list `D1` is not configured for generating output documentation."

  test_that("the ODG menu opens only when load_fn is a function without arguments", {
    load_fns <- list(missing = NULL, not_a_function = "not a function", with_arguments = function(x) x)
    for (case in names(load_fns)) {
      spy <- local_ui_spy()
      shiny::testServer(app_server_test(odg_options(load_fns[[case]])), {
        session$setInputs(selector = "D1")
        session$setInputs(`filter-filter_state_json_input` = filter_json("AGE"))
        session$setInputs(odg_code_menu = ODG$VAL$ODG_ALL)
      })
      expect_identical(spy$notifications, not_configured_msg, info = case)
      expect_false(spy$modal_shown, info = case)
    }

    spy <- local_ui_spy()
    shiny::testServer(app_server_test(odg_options(valid_load_fn)), {
      session$setInputs(selector = "D1")
      session$setInputs(`filter-filter_state_json_input` = filter_json("AGE"))
      session$setInputs(odg_code_menu = ODG$VAL$ODG_ALL)
    })
    expect_identical(spy$notifications, character(0))
    expect_true(spy$modal_shown)
  })

  test_that("an ODG download that fails before rendering notifies the user", {
    # The menu refuses an invalid load_fn, the download handler must not rely on it
    spy <- local_ui_spy()
    shiny::testServer(app_server_test(odg_options("not a function")), {
      session$setInputs(selector = "D1")
      session$setInputs(`filter-filter_state_json_input` = filter_json("AGE"))
      session$setInputs(output_format = ODG$OUTPUT_FORMAT$PDF, odg_file_name = "report")
      expect_error(output[[ODG$ID$ODG_CODE]])
    })
    expect_identical(spy$notifications, ODG$MSG$GENERATION_FAILED)

    # A filter that cannot be applied ends the code expansion with a silent error
    spy <- local_ui_spy()
    shiny::testServer(app_server_test(odg_options(valid_load_fn)), {
      session$setInputs(selector = "D1")
      session$setInputs(`filter-filter_state_json_input` = filter_json("MISSING"))
      session$setInputs(output_format = ODG$OUTPUT_FORMAT$PDF, odg_file_name = "report")
      expect_error(output[[ODG$ID$ODG_CODE]])
    })
    expect_identical(spy$notifications[[length(spy$notifications)]], ODG$MSG$GENERATION_FAILED)
  })
})
