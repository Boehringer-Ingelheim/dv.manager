# Values interpolated into the inline bootstrap scripts may still inject code when quotes are scaped.
# the HTML parser runs before the JS one and ends the script at the first `</script>`
# "<" must be scaped too.
# Injection is tested in both restored header tab and filter state.
# In both cases the inject script must not run
#
# 1. Start an app with a filter and one module
# 2. Open it from a bookmark URL restoring the tab with the payload
# 3. Open it from a bookmark URL restoring a filter state with the payload

xss_payload <- "</script><script>window.dv_xss = true</script>"

# Real bookmarked states come from JS `JSON.stringify`, which does not escape `/` as jsonlite does (`<\/script>`)
to_json_like_js <- function(x) gsub("\\/", "/", jsonlite::toJSON(x, auto_unbox = TRUE), fixed = TRUE)

test_that("crafted bookmark URLs cannot inject scripts with the default bookmarking store", {
  skip_if_not_running_shiny_tests()

  root_app <- start_app_driver(rlang::quo({
    dv.manager:::run_app(
      data = list(dl1 = list(ds1 = data.frame(sbj_var = c("SBJ-1", "SBJ-2")))),
      module_list = list(
        Simple = dv.manager:::mod_simple(dataset = "ds1", module_id = "mod", from = "filtered_dataset_list")
      ),
      filter_dataset_name = "ds1",
      filter_key = "sbj_var"
    )
  }))

  # Same app, with `input_id` restored to `value` as a crafted `?_inputs_` bookmark URL would do
  open_with_input <- function(input_id, value) {
    value <- utils::URLencode(jsonlite::toJSON(value, auto_unbox = TRUE), reserved = TRUE)
    shinytest2::AppDriver$new(paste0(root_app$get_url(), "?_inputs_&", input_id, "=", value))
  }

  app <- open_with_input("nav_header", xss_payload)
  expect_identical(app$get_js("typeof window.dv_xss"), "undefined")
  app$stop()

  # Valid filter state carrying the payload as a value: it must reach the filter intact and still be applied
  filter_state <- to_json_like_js(list(
    filters = list(
      datasets_filter = list(children = list()),
      subject_filter = list(
        children = list(list(
          kind = "filter",
          dataset = "ds1",
          operation = "select_subset",
          variable = "sbj_var",
          values = c("SBJ-1", xss_payload),
          include_NA = FALSE
        ))
      )
    ),
    dataset_list_name = "dl1"
  ))

  app <- open_with_input("filter-filter_state_json_input", filter_state)
  expect_identical(app$get_js("typeof window.dv_xss"), "undefined")
  expect_identical(app$get_value(output = "mod-text"), "1")
  app$stop()
})

test_that("escape_js_string encodes the XSS payload and decodes JSON with legal `<` back unchanged", {
  json <- paste0('{"values": ["<5", "a < b", "', xss_payload, '"]}')
  encoded <- escape_js_string(json)
  expect_false(grepl("<", encoded, fixed = TRUE))
  expect_identical(jsonlite::fromJSON(paste0('"', encoded, '"')), json)
})
