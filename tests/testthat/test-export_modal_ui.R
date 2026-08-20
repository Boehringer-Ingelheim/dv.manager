dv.manager:::..activate_export()
on.exit(
  dv.manager:::..deactivate_export(),
  add = TRUE
)

local({
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

  test_that("build_export_modal_ui shows one switch per module and selects all of them by default", {
    res <- build_export_modal_ui(fixture_elements, test_ns)

    expect_identical(res[["selected"]], c("m1-a" = TRUE, "m1-b" = TRUE, "m2-a" = TRUE))
    html <- as.character(res[["modal_dialog"]])
    expect_length(gregexpr("form-check form-switch", html, fixed = TRUE)[[1]], 2)
    expect_false(grepl("Label m1-a", html, fixed = TRUE)) # per-output entries are not listed
    expect_snapshot(cat(html))
  })

  test_that("build_export_modal_ui shows every module but preselects only the outputs of selected_tab", {
    res <- build_export_modal_ui(fixture_elements, test_ns, selected_tab = "m1")

    expect_identical(res[["selected"]], c("m1-a" = TRUE, "m1-b" = TRUE, "m2-a" = FALSE))
    html <- as.character(res[["modal_dialog"]])
    expect_true(grepl("Module 1", html, fixed = TRUE))
    expect_true(grepl("Module 2", html, fixed = TRUE))
    is_switch_checked <- function(module_id) grepl(paste0("checked onchange=[^>]*", module_id), html)
    expect_true(is_switch_checked("m1"))
    expect_false(is_switch_checked("m2"))
  })

  test_that("build_export_modal_ui shows a fallback message and no download button when nothing is exportable", {
    res <- build_export_modal_ui(list(), test_ns)

    expect_identical(res[["selected"]], logical(0))
    html <- as.character(res[["modal_dialog"]])
    expect_true(grepl("No outputs available for export", html, fixed = TRUE))
    expect_false(grepl("output_format", html, fixed = TRUE))
    expect_false(grepl("shiny-download-link", html, fixed = TRUE))
  })

  test_that("build_export_modal_ui preselects nothing when selected_tab matches no module", {
    res <- build_export_modal_ui(fixture_elements, test_ns, selected_tab = "does-not-exist")

    expect_identical(res[["selected"]], c("m1-a" = FALSE, "m1-b" = FALSE, "m2-a" = FALSE))
    html <- as.character(res[["modal_dialog"]])
    expect_true(grepl("Module 1", html, fixed = TRUE))
    expect_true(grepl("Module 2", html, fixed = TRUE))
    expect_true(grepl("output_format", html, fixed = TRUE))
  })
})
