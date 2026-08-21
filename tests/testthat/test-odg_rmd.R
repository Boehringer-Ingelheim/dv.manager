dv.manager:::..activate_odg()
on.exit(
  dv.manager:::..deactivate_odg(),
  add = TRUE
)

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

  test_that("odg_element_formatters formats PDF default elements, with a section header only for the first element", {
    formatters <- ODG_ELEMENT_FORMATTERS

    first <- formatters[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$DEFAULT]](
      make_element(is_first_module_element = TRUE)
    )
    later <- formatters[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$DEFAULT]](
      make_element(is_first_module_element = FALSE)
    )

    expect_snapshot(cat(first))
    expect_snapshot(cat(later))
  })

  test_that("odg_element_formatters wraps PDF tables in a wide page sized to char_width", {
    formatters <- ODG_ELEMENT_FORMATTERS

    out <- formatters[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$TABLE]](
      make_element(kind = ODG$ELEMENT_KIND$TABLE, char_width = 42)
    )
    expect_snapshot(cat(out))
  })

  test_that("odg_element_formatters flags PDF errors with alertwarning", {
    formatters <- ODG_ELEMENT_FORMATTERS

    out <- formatters[[ODG$OUTPUT_FORMAT$PDF]][[ODG$ELEMENT_KIND$ERROR]](
      make_element(kind = ODG$ELEMENT_KIND$ERROR, code = "boom")
    )
    expect_snapshot(cat(out))
  })

  test_that("odg_element_formatters formats HTML elements, and table delegates to default", {
    formatters <- ODG_ELEMENT_FORMATTERS
    el <- make_element()

    default_out <- formatters[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$DEFAULT]](el)
    table_out <- formatters[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$TABLE]](el)

    expect_snapshot(cat(default_out))
    expect_snapshot(cat(table_out))
  })

  test_that("odg_element_formatters flags HTML errors with an alert div", {
    formatters <- ODG_ELEMENT_FORMATTERS

    out <- formatters[[ODG$OUTPUT_FORMAT$HTML]][[ODG$ELEMENT_KIND$ERROR]](
      make_element(kind = ODG$ELEMENT_KIND$ERROR, code = "boom")
    )
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

  test_that("build_odg_rmd assembles an HTML document from all pieces, in order", {
    elements <- list(
      make_element(
        id = "mod1-el1",
        kind = ODG$ELEMENT_KIND$DEFAULT,
        label = "Element 1",
        is_first_module_element = TRUE
      ),
      make_element(
        id = "mod1-el2",
        kind = ODG$ELEMENT_KIND$ERROR,
        label = "Element 2",
        is_first_module_element = FALSE,
        code = "boom"
      )
    )

    rmd <- build_odg_rmd(
      selected_odg_elements = elements,
      output_format = ODG$OUTPUT_FORMAT$HTML,
      data_code = "df <- load_data()",
      sections = fixture_sections,
      templates = fixture_templates
    )

    expect_snapshot(rmd |> cat())
  })

  test_that("build_odg_rmd wraps a PDF table element in a wide page", {
    elements <- list(
      make_element(
        id = "mod1-el1",
        kind = ODG$ELEMENT_KIND$TABLE,
        label = "Table 1",
        is_first_module_element = TRUE,
        char_width = 10
      )
    )

    rmd <- build_odg_rmd(
      selected_odg_elements = elements,
      output_format = ODG$OUTPUT_FORMAT$PDF,
      data_code = "df <- load_data()",
      sections = fixture_sections,
      templates = fixture_templates
    )

    expect_snapshot(rmd |> cat())
  })
})
