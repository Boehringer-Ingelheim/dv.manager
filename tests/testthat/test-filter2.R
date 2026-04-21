# Test moved to a separate file as apparently sometimes the apps are not correctly destroyed and there is across
# test influence

skip_if_not_running_shiny_tests()

date_var <- as.Date("2024-01-01") + c(0L:4L, NA)

dataset_lists <- list(
  dl1 = list(
    ds1 = data.frame(
      row.names = 1:6,
      range_var = c(1.0:5.0, NA),
      date_var = date_var,
      posix_var = as.POSIXct(date_var),
      subset_var = factor(c(letters[1:5], NA)),
      logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
      sbj_var = paste0("SBJ-", 1:6)
    ),
    ds2 = data.frame(
      row.names = 1:6,
      range_var = c(1.0:5.0, NA),
      date_var = date_var,
      posix_var = as.POSIXct(date_var),
      subset_var = factor(c(letters[1:5], NA)),
      logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
      sbj_var = paste0("SBJ-", 1:6)
    )
  ),
  dl2 = list(
    ds1 = data.frame(
      row.names = 1:6,
      range_var = c(1.0:5.0, NA),
      date_var = date_var,
      posix_var = as.POSIXct(date_var),
      subset_var = factor(c(letters[1:5], NA)),
      logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
      sbj_var = paste0("SBJ-", 1:6)
    ),
    ds2 = data.frame(
      row.names = 1:6,
      range_var = c(1.0:5.0, NA),
      date_var = date_var,
      posix_var = as.POSIXct(date_var),
      subset_var = factor(c(letters[1:5], NA)),
      logical_var = c(TRUE, TRUE, TRUE, FALSE, FALSE, NA),
      sbj_var = paste0("SBJ-", 1:6)
    )
  )
)

absolute_state_file <- file.path(getwd(), "./test_data/filter_state.txt")

test_that(
  "Bookmark can be restored with no state" |>
    vdoc[["add_spec"]](c(
      specs$FILTERING$FILTER_BOOKMARKABLE
    )),
  {
    root_app <- start_app_driver(rlang::quo({
      dv.manager:::run_app(
        data = dataset_lists,
        module_list = list(
          Simple3 = dv.manager:::mod_simple(
            dataset = "ds1",
            module_id = "mod",
            from = "filtered_dataset_list"
          )
        ),
        filter_data = "ds1",
        filter_key = "sbj_var",
        enableBookmarking = "url"
      )
    }))

    url <- "?_inputs_&filter-IGNORE_INPUT=null&__tabset_0__=%22mod%22&open_options_modal=0&selector=%22dl1%22&click=true&filter-checkbox=false&filter-log=null&filter-filter_state_json_input=%22%7B%5C%22filters%5C%22%3A%7B%5C%22datasets_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%5D%7D%2C%5C%22subject_filter%5C%22%3A%7B%5C%22children%5C%22%3A%5B%7B%5C%22kind%5C%22%3A%5C%22filter%5C%22%2C%5C%22dataset%5C%22%3A%5C%22ds1%5C%22%2C%5C%22operation%5C%22%3A%5C%22select_subset%5C%22%2C%5C%22variable%5C%22%3A%5C%22sbj_var%5C%22%2C%5C%22values%5C%22%3A%5B%5C%22SBJ-1%5C%22%5D%2C%5C%22include_NA%5C%22%3Afalse%7D%5D%7D%7D%2C%5C%22dataset_list_name%5C%22%3A%5C%22dl1%5C%22%7D%22"

    full_url <- paste0(root_app$get_url(), url)
    app <- shinytest2::AppDriver$new(full_url)

    expect_identical(app$get_value(output = "mod-text"), "1")
  }
)
