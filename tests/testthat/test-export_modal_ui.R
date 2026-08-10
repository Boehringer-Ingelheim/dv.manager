local({
  skip_if_no_export_code()

  test_ns <- function(x) paste0("test-", x)

  make_element <- function(id, module_id, module_name, is_first) {
    list(
      id = id,
      module_id = module_id,
      module_name = module_name,
      label = paste("Label", id),
      info = paste("Info", id),
      is_first_module_element = is_first
    )
  }

  fixture_elements <- list(
    "m1-a" = make_element("m1-a", "m1", "Module 1", TRUE),
    "m1-b" = make_element("m1-b", "m1", "Module 1", FALSE),
    "m2-a" = make_element("m2-a", "m2", "Module 2", TRUE)
  )

  test_that("build_export_modal_ui shows every element and selects all of them by default", {
    res <- build_export_modal_ui(fixture_elements, test_ns)

    expect_identical(res[["selected"]], c("m1-a" = TRUE, "m1-b" = TRUE, "m2-a" = TRUE))
    expect_snapshot(cat(as.character(res[["modal_dialog"]])))
  })

  test_that("build_export_modal_ui filters to a single module when show_tab is set", {
    res <- build_export_modal_ui(fixture_elements, test_ns, show_tab = "m1")

    expect_identical(res[["selected"]], c("m1-a" = TRUE, "m1-b" = TRUE, "m2-a" = FALSE))
    html <- as.character(res[["modal_dialog"]])
    expect_true(grepl("Module 1", html, fixed = TRUE))
    expect_false(grepl("Module 2", html, fixed = TRUE))
  })

  test_that("build_export_modal_ui shows a fallback message and no download button when nothing is selected", {
    res <- build_export_modal_ui(list(), test_ns)

    expect_identical(res[["selected"]], logical(0))
    html <- as.character(res[["modal_dialog"]])
    expect_true(grepl("No elements available for export", html, fixed = TRUE))
    expect_false(grepl("output_format", html, fixed = TRUE))
    expect_false(grepl("shiny-download-link", html, fixed = TRUE))
  })

  test_that("build_export_modal_ui falls back to the same message when show_tab matches nothing", {
    res <- build_export_modal_ui(fixture_elements, test_ns, show_tab = "does-not-exist")

    expect_identical(res[["selected"]], c("m1-a" = FALSE, "m1-b" = FALSE, "m2-a" = FALSE))
    html <- as.character(res[["modal_dialog"]])
    expect_true(grepl("No elements available for export", html, fixed = TRUE))
    expect_false(grepl("output_format", html, fixed = TRUE))
  })
})
