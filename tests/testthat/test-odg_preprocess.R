dv.manager:::..activate_odg()
on.exit(
  dv.manager:::..deactivate_odg(),
  add = TRUE
)

local({
  # get_code_in_context() is injected. The real one hands its argument to
  # shinymeta::expandChain(), which forces that promise in meta mode; this stub
  # deliberately never forces it, so tests stay independent of code generation.
  fake_get_code_in_context <- function(x) "FIXTURE_CODE"

  make_element <- function(metareactive) {
    list(
      id = "mod1-el1",
      label = "Element 1",
      module_name = "Module 1",
      is_first_module_element = TRUE,
      metareactive = metareactive
    )
  }

  # preprocess_odg_elements() is the only entry point now; this drives it
  # with a single selected element to exercise the per-element logic in isolation.
  preprocess_one <- function(el, output_format, get_code_in_context) {
    preprocess_odg_elements(
      list(el = el),
      c(el = TRUE),
      output_format,
      get_code_in_context
    )[[1]]
  }

  test_that("preprocess_odg_elements reports an error for a format the element does not provide", {
    el <- make_element(list(html = function() "only html"))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$PDF, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$ERROR)
    expect_match(res[["code"]], "Not avaliable in pdf format")
  })

  test_that("preprocess_odg_elements reports an error when the metareactive fails to resolve", {
    el <- make_element(list(html = function() stop("boom")))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$ERROR)
    expect_match(res[["code"]], "boom")
  })

  test_that("preprocess_odg_elements drops the metareactive and keeps the other element fields", {
    el <- make_element(list(html = function() "a plot"))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_false("metareactive" %in% names(res))
    expect_identical(res[["id"]], "mod1-el1")
    expect_identical(res[["label"]], "Element 1")
    expect_identical(res[["module_name"]], "Module 1")
    expect_true(res[["is_first_module_element"]])
  })

  test_that("preprocess_odg_elements treats a non-table element as default, with no char_width", {
    el <- make_element(list(html = function() "a plot"))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$DEFAULT)
    expect_identical(res[["code"]], "FIXTURE_CODE")
    expect_null(res[["char_width"]])
  })

  test_that("preprocess_odg_elements treats a data.frame as default (not a table) in html", {
    el <- make_element(list(html = function() data.frame(a = 1:2)))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$DEFAULT)
    expect_null(res[["char_width"]])
  })

  test_that("preprocess_odg_elements converts a pdf data.frame into a table with an estimated width", {
    skip_if_not_installed("gt")

    el <- make_element(list(pdf = shiny::isolate(shinymeta::metaReactive(data.frame(a = 1:2), varname = "src"))))

    res <- shiny::isolate(preprocess_one(el, ODG$OUTPUT_FORMAT$PDF, fake_get_code_in_context))

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$TABLE)
    expect_true(res[["char_width"]] > 0)
  })

  test_that("preprocess_odg_elements converts a pdf gt_tbl into a table with an estimated width", {
    skip_if_not_installed("gt")

    el <- make_element(
      list(pdf = shiny::isolate(shinymeta::metaReactive(gt::gt(data.frame(a = 1:2)), varname = "src")))
    )

    res <- shiny::isolate(preprocess_one(el, ODG$OUTPUT_FORMAT$PDF, fake_get_code_in_context))

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$TABLE)
    expect_true(res[["char_width"]] > 0)
  })

  test_that("preprocess_odg_elements rejects an unknown output_format", {
    el <- make_element(list(html = function() "a plot"))

    expect_error(preprocess_one(el, "bogus", fake_get_code_in_context))
  })

  test_that("preprocess_odg_elements processes only the selected elements, in order", {
    odg_elements <- list(
      a = list(id = "a", metareactive = list(html = function() "A")),
      b = list(id = "b", metareactive = list(html = function() "B")),
      c = list(id = "c", metareactive = list(html = function() "C"))
    )
    is_selected <- c(a = TRUE, b = FALSE, c = TRUE)

    res <- preprocess_odg_elements(
      odg_elements,
      is_selected,
      ODG$OUTPUT_FORMAT$HTML,
      fake_get_code_in_context
    )

    expect_length(res, 2)
    expect_identical(vapply(res, `[[`, character(1), "id"), c("a", "c"))
  })

  test_that("preprocess_odg_elements returns an empty list when nothing is selected", {
    odg_elements <- list(a = list(id = "a", metareactive = list(html = function() "A")))

    res <- preprocess_odg_elements(
      odg_elements,
      c(a = FALSE),
      ODG$OUTPUT_FORMAT$HTML,
      fake_get_code_in_context
    )

    expect_identical(res, list())
  })
})
