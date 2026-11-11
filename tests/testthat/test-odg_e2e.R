skip_if_not_running_shiny_tests()

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

  test_that("a module's exported value is downloaded end to end as a rendered PDF, named as typed", {
    app <- start_app_driver(
      rlang::quo({
        dv.manager:::run_app(
          data = !!data,
          module_list = list(
            ODG = dv.manager:::mod_odg_dataset_name("mod1")
          ),
          filter_dataset_name = "adsl",
          filter_key = "USUBJID",
          filter_default_state = !!filter_default_state,
          enable_odg = TRUE
        )
      })
    )

    app$run_js(sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      ODG$ID$ODG_CODE_MENU,
      ODG$VAL$ODG_ALL
    ))
    app$wait_for_idle()

    # unrequired right now but it will be in the future when more formats are available
    app$set_inputs(!!ODG$ID$OUTPUT_FORMAT := ODG$OUTPUT_FORMAT$PDF, wait_ = FALSE)
    app$set_inputs(!!ODG$ID$FILE_NAME := "My report", wait_ = FALSE)
    app$wait_for_idle()

    downloaded_file <- app$get_download(ODG$ID$ODG_CODE)
    expect_identical(basename(downloaded_file), "My_report.zip")
    unzip_dir <- tempfile()
    utils::unzip(downloaded_file, exdir = unzip_dir)
    unzipped <- list.files(unzip_dir)

    error_file <- ODG$FILE_NAME$RESERVED$ERROR
    if (error_file %in% unzipped) {
      cat(sprintf("\n---- %s ----\n", error_file))
      cat(readLines(file.path(unzip_dir, error_file)), sep = "\n")
    }

    expect_false(error_file %in% unzipped)
    expect_true("My_report.pdf" %in% unzipped)

    # The PDF carries its source and the session info; the session info file itself is not shipped
    session_info_file <- ODG$FILE_NAME$RESERVED$SESSION_INFO
    attachments <- system2("qpdf", c("--list-attachments", shQuote(file.path(unzip_dir, "My_report.pdf"))), stdout = TRUE)
    expect_true(any(startsWith(attachments, "My_report.Rmd ")), info = paste(attachments, collapse = "\n"))
    expect_true(any(startsWith(attachments, paste0(session_info_file, " "))), info = paste(attachments, collapse = "\n"))
    expect_false(session_info_file %in% unzipped)

    # pdf is binary test against Rmd
    expect_true("My_report.Rmd" %in% unzipped)
    rmd <- paste(readLines(file.path(unzip_dir, "My_report.Rmd"), warn = FALSE), collapse = "\n")
    expect_match(rmd, "D1")

    # The document reloads the data itself, it must hash the same as the data in the app
    tex <- paste(readLines(file.path(unzip_dir, "My_report.tex"), warn = FALSE), collapse = "\n")
    expect_match(tex, "Dynamic Data hash", fixed = TRUE)
    expect_no_match(tex, "Hash mismatch", fixed = TRUE)
  })
})
