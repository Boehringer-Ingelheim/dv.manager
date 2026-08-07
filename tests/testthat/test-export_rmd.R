local({
  make_element <- function(
    id = "mod1-el1",
    kind = "default",
    label = "Element 1",
    module_name = "Module 1",
    is_first_module_element = TRUE,
    code = "1 + 1",
    char_width = NULL
  ) {
    list(
      id = id,
      kind = kind,
      code = code,
      char_width = char_width,
      label = label,
      module_name = module_name,
      is_first_module_element = is_first_module_element
    )
  }

  test_that("export_element_formatters formats PDF default elements, with a section header only for the first element", {
    formatters <- export_element_formatters()

    first <- formatters[["pdf"]][["default"]](make_element(is_first_module_element = TRUE))
    later <- formatters[["pdf"]][["default"]](make_element(is_first_module_element = FALSE))

    expect_snapshot(cat(first))
    expect_snapshot(cat(later))
  })

  test_that("export_element_formatters wraps PDF tables in a wide page sized to char_width", {
    formatters <- export_element_formatters()

    out <- formatters[["pdf"]][["table"]](make_element(kind = "table", char_width = 42))
    expect_snapshot(cat(out))
  })

  test_that("export_element_formatters flags PDF errors with alertwarning", {
    formatters <- export_element_formatters()

    out <- formatters[["pdf"]][["error"]](make_element(kind = "error", code = "boom"))
    expect_snapshot(cat(out))
  })

  test_that("export_element_formatters formats HTML elements, and table delegates to default", {
    formatters <- export_element_formatters()
    el <- make_element()

    default_out <- formatters[["html"]][["default"]](el)
    table_out <- formatters[["html"]][["table"]](el)

    expect_snapshot(cat(default_out))
    expect_snapshot(cat(table_out))
  })

  test_that("export_element_formatters flags HTML errors with an alert div", {
    formatters <- export_element_formatters()

    out <- formatters[["html"]][["error"]](make_element(kind = "error", code = "boom"))
    expect_snapshot(cat(out))
  })

  fixture_sections <- list(
    date = "FIXTURE_DATE_SECTION",
    hardcoded_hash = "FIXTURE_HARDCODED_HASH_SECTION",
    dynamic_hash = "FIXTURE_DYNAMIC_HASH_SECTION",
    filter_txt = "FIXTURE_FILTER_TXT_SECTION",
    filter_reference = "FIXTURE_FILTER_REFERENCE_SECTION"
  )

  fixture_templates <- list(
    header = "FIXTURE_HEADER_TEMPLATE",
    session_info = "FIXTURE_SESSION_INFO_TEMPLATE",
    footer = "FIXTURE_FOOTER_TEMPLATE"
  )

  test_that("build_export_rmd assembles an HTML document from all pieces, in order", {
    elements <- list(
      make_element(id = "mod1-el1", kind = "default", label = "Element 1", is_first_module_element = TRUE),
      make_element(
        id = "mod1-el2",
        kind = "error",
        label = "Element 2",
        is_first_module_element = FALSE,
        code = "boom"
      )
    )

    rmd <- build_export_rmd(
      elements_to_export = elements,
      output_format = "html",
      data_code = "df <- load_data()",
      sections = fixture_sections,
      templates = fixture_templates
    )

    expect_snapshot(rmd |> cat())
  })

  test_that("build_export_rmd wraps a PDF table element in a wide page", {
    elements <- list(
      make_element(id = "mod1-el1", kind = "table", label = "Table 1", is_first_module_element = TRUE, char_width = 10)
    )

    rmd <- build_export_rmd(
      elements_to_export = elements,
      output_format = "pdf",
      data_code = "df <- load_data()",
      sections = fixture_sections,
      templates = fixture_templates
    )

    expect_snapshot(rmd |> cat())
  })
})
