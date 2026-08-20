skip_if_not_running_shiny_tests()

dv.manager:::..activate_export()
on.exit(
  dv.manager:::..deactivate_export(),
  add = TRUE
)

# Only test export is produced the rest is tested in the rest of test functions
# `dv.export_enabled` gates code defined at *package load time* (see R/aaaaa_export.R), so it has to
# be set before the subprocess app.R loads the package - `start_app_driver(options = ...)` does that.
local({
  data <- list(
    D1 = structure(
      list(adsl = data.frame(USUBJID = paste0("SBJ-", 1:5), AGE = c(20, 30, 40, 50, 60))),
      load_fn = function() {
        list(adsl = data.frame(USUBJID = paste0("SBJ-", 1:5), AGE = c(20, 30, 40, 50, 60)))
      }
    )
  )

  filter_default_state <- '{
    "filters": {
      "datasets_filter": {"children": []},
      "subject_filter": {
        "children": [
          {
            "kind": "filter",
            "dataset": "adsl",
            "operation": "select_subset",
            "variable": "USUBJID",
            "values": ["SBJ-1", "SBJ-2", "SBJ-3"],
            "include_NA": false
          }
        ]
      }
    },
    "dataset_list_name": "D1"
  }'

  test_that("a module's exported value is downloaded end to end as a rendered export.zip", {
    app <- start_app_driver(
      rlang::quo({
        dv.manager:::..activate_export()
        on.exit(
          dv.manager:::..deactivate_export(),
          add = TRUE
        )

        dv.manager:::run_app(
          data = !!data,
          module_list = list(
            Export = dv.manager:::mod_export_dataset_name("mod1")
          ),
          filter_dataset_name = "adsl",
          filter_key = "USUBJID",
          filter_default_state = !!filter_default_state
        )
      })
    )

    app$run_js(sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      EXPORT$ID$EXPORT_CODE_MENU,
      EXPORT$VAL$EXPORT_ALL
    ))
    app$wait_for_idle()

    downloaded_file <- app$get_download(EXPORT$ID$EXPORT_CODE)
    unzip_dir <- tempfile()
    utils::unzip(downloaded_file, exdir = unzip_dir)
    unzipped <- list.files(unzip_dir)

    if ("error.txt" %in% unzipped) {
      cat("\n---- error.txt ----\n")
      cat(readLines(file.path(unzip_dir, "error.txt")), sep = "\n")
    }

    expect_false("error.txt" %in% unzipped)
    expect_true("export.html" %in% unzipped)

    html <- paste(readLines(file.path(unzip_dir, "export.html"), warn = FALSE), collapse = "\n")
    expect_match(html, "D1")
  })
})
