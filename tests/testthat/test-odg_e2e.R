skip_if_not_running_shiny_tests()

dv.manager:::..activate_odg()
on.exit(
  dv.manager:::..deactivate_odg(),
  add = TRUE
)

# Only test the output document is produced, the rest is tested in the rest of test functions
# `..activate_odg()` installs the shinymeta aliases used by code defined at *package load time*
# (see R/aaaaa_odg.R), so it has to run inside the subprocess app too, not only in this process.
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

  test_that("a module's exported value is downloaded end to end as a rendered odg.zip", {
    app <- start_app_driver(
      rlang::quo({
        dv.manager:::..activate_odg()
        on.exit(
          dv.manager:::..deactivate_odg(),
          add = TRUE
        )

        dv.manager:::run_app(
          data = !!data,
          module_list = list(
            ODG = dv.manager:::mod_odg_dataset_name("mod1")
          ),
          filter_dataset_name = "adsl",
          filter_key = "USUBJID",
          filter_default_state = !!filter_default_state
        )
      })
    )

    app$run_js(sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      ODG$ID$ODG_CODE_MENU,
      ODG$VAL$ODG_ALL
    ))
    app$wait_for_idle()

    downloaded_file <- app$get_download(ODG$ID$ODG_CODE)
    unzip_dir <- tempfile()
    utils::unzip(downloaded_file, exdir = unzip_dir)
    unzipped <- list.files(unzip_dir)

    if ("error.txt" %in% unzipped) {
      cat("\n---- error.txt ----\n")
      cat(readLines(file.path(unzip_dir, "error.txt")), sep = "\n")
    }

    expect_false("error.txt" %in% unzipped)
    expect_true("odg.html" %in% unzipped)

    html <- paste(readLines(file.path(unzip_dir, "odg.html"), warn = FALSE), collapse = "\n")
    expect_match(html, "D1")
  })
})
