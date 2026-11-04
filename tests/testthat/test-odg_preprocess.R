local({
  # get_code_in_context() is injected. The real one hands its argument to
  # shinymeta::expandChain(), which forces that promise in meta mode; this stub
  # deliberately never forces it, so tests stay independent of code generation.
  fake_get_code_in_context <- function(x) "FIXTURE_CODE"

  # preprocess_odg_elements() rejects anything that is not a metareactive, so the
  # stubs below carry the class shinymeta::metaReactive() would have given them.
  fake_metareactive <- function(f) structure(f, class = c("shinymeta_reactive", "reactive", "function"))

  make_element <- function(codegen) {
    list(
      id = "mod1-el1",
      label = "Element 1",
      module_name = "Module 1",
      is_first_module_element = TRUE,
      codegen = codegen
    )
  }

  # preprocess_odg_elements() is the only entry point now; this drives it
  # with a single element to exercise the per-element logic in isolation.
  preprocess_one <- function(el, output_format, get_code_in_context) {
    preprocess_odg_elements(
      list(el = el),
      output_format,
      get_code_in_context
    )[[1]]
  }

  test_that("preprocess_odg_elements reports an error for a format the element does not provide", {
    el <- make_element(list(html = fake_metareactive(function() "only html")))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$PDF, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$ERROR)
    expect_match(res[["code"]], "Not available in pdf format")
  })

  test_that("preprocess_odg_elements reports an error when the codegen field is neither a metareactive nor a chunk", {
    # a module that was never prepared for ODG hands over a plain reactive/function
    el <- make_element(list(html = function() "a plot"))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$ERROR)
    expect_match(res[["code"]], "is neither a metareactive nor a chunk")
  })

  test_that("preprocess_odg_elements reports an error when the metareactive fails to resolve", {
    el <- make_element(list(html = fake_metareactive(function() stop("boom"))))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$ERROR)
    expect_match(res[["code"]], "boom")
  })

  test_that("preprocess_odg_elements drops the codegen field and keeps the other element fields", {
    el <- make_element(list(html = fake_metareactive(function() "a plot")))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_false("codegen" %in% names(res))
    expect_identical(res[["id"]], "mod1-el1")
    expect_identical(res[["label"]], "Element 1")
    expect_identical(res[["module_name"]], "Module 1")
    expect_true(res[["is_first_module_element"]])
  })

  test_that("preprocess_odg_elements treats a non-table element as default", {
    el <- make_element(list(html = fake_metareactive(function() "a plot")))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$DEFAULT)
    expect_identical(res[["code"]], "FIXTURE_CODE")
  })

  test_that("preprocess_odg_elements treats a data.frame as default (not a table) in html", {
    el <- make_element(list(html = fake_metareactive(function() data.frame(a = 1:2))))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$DEFAULT)
  })

  test_that("preprocess_odg_elements converts a pdf data.frame into a table", {
    skip_if_not_installed("gt")

    el <- make_element(list(pdf = shiny::isolate(shinymeta::metaReactive(data.frame(a = 1:2), varname = "src"))))

    res <- shiny::isolate(preprocess_one(el, ODG$OUTPUT_FORMAT$PDF, fake_get_code_in_context))

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$TABLE)
  })

  test_that("preprocess_odg_elements converts a pdf gt_tbl into a table", {
    skip_if_not_installed("gt")

    el <- make_element(
      list(pdf = shiny::isolate(shinymeta::metaReactive(gt::gt(data.frame(a = 1:2)), varname = "src")))
    )

    res <- shiny::isolate(preprocess_one(el, ODG$OUTPUT_FORMAT$PDF, fake_get_code_in_context))

    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$TABLE)
  })

  fake_chunk <- function(f) structure(f, class = "chunk")

  test_that("preprocess_odg_elements hands the formatted header to a chunk and keeps its markup", {
    el <- make_element(list(html = fake_chunk(function(header) paste0(header, "chunk body"))))

    res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

    header <- ODG_ELEMENT_FORMATTERS[[ODG$OUTPUT_FORMAT$HTML]][["header"]](el)
    expect_identical(res[["kind"]], ODG$ELEMENT_KIND$CHUNK)
    expect_identical(res[["code"]], paste0(header, "chunk body"))
  })

  test_that("preprocess_odg_elements reports an error when a chunk does not return a single string", {
    bad_returns <- list(NULL, character(0), c("a", "b"), NA_character_, 1)
    for (bad_return in bad_returns) {
      el <- make_element(list(html = fake_chunk(function(header) bad_return)))

      res <- preprocess_one(el, ODG$OUTPUT_FORMAT$HTML, fake_get_code_in_context)

      expect_identical(res[["kind"]], ODG$ELEMENT_KIND$ERROR)
      expect_match(res[["code"]], "`chunk` must return a single string")
    }
  })

  test_that("preprocess_odg_elements rejects an unknown output_format", {
    el <- make_element(list(html = fake_metareactive(function() "a plot")))

    expect_error(preprocess_one(el, "bogus", fake_get_code_in_context))
  })

  test_that("preprocess_odg_elements processes every element it is given, in order", {
    odg_elements <- list(
      c = list(id = "c", codegen = list(html = fake_metareactive(function() "C"))),
      a = list(id = "a", codegen = list(html = fake_metareactive(function() "A"))),
      b = list(id = "b", codegen = list(html = fake_metareactive(function() "B")))
    )

    res <- preprocess_odg_elements(
      odg_elements,
      ODG$OUTPUT_FORMAT$HTML,
      fake_get_code_in_context
    )

    expect_length(res, 3)
    expect_identical(vapply(res, `[[`, character(1), "id"), c("c", "a", "b"))
  })

  test_that("preprocess_odg_elements returns an empty list when given no elements", {
    res <- preprocess_odg_elements(
      list(),
      ODG$OUTPUT_FORMAT$HTML,
      fake_get_code_in_context
    )

    expect_identical(res, list())
  })
})
