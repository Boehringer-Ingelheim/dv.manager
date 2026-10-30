local({
  test_that("build_hardcoded_hash_section lists a content hash per dataset", {
    dataset_list <- list(a = data.frame(x = 1:2), b = data.frame(y = "z"))
    expect_snapshot(cat(build_hardcoded_hash_section(dataset_list)))
  })

  test_that("build_hardcoded_hash_section handles an empty dataset list", {
    expect_snapshot(cat(build_hardcoded_hash_section(list())))
  })

  # use shinymeta machinery, not a stub
  # These two functions rely on is the *sharing* of one expansion context across calls, which is exactly what
  # collapses a metareactive reference down to a bare variable name instead of
  # re-emitting its assignment. `prime()` mirrors the call site: `data_code` is always generated
  # before these sections, which is what emits the assignment in the first
  # place.
  new_shared_get_code_in_context <- function() {
    ec <- shinymeta::newExpansionContext()
    function(x) shinymeta::expandChain(x, .expansionContext = ec) |> as.character() |> paste(collapse = "\n")
  }

  prime <- function(get_code_in_context, mr) {
    invisible(get_code_in_context(mr()))
  }

  test_that("build_dynamic_hash_section emits an asis loop referencing the shared dataset-list variable", {
    shiny::isolate({
      gcic <- new_shared_get_code_in_context()
      sdl <- shinymeta::metaReactive(
        list(a = data.frame(x = 1:2), b = data.frame(y = "z")),
        varname = "selected_dataset_list"
      )
      prime(gcic, sdl)

      expect_snapshot(cat(build_dynamic_hash_section(sdl, gcic)))
    })
  })

  test_that("build_dynamic_hash_section handles zero datasets", {
    shiny::isolate({
      gcic <- new_shared_get_code_in_context()
      sdl <- shinymeta::metaReactive(list(), varname = "selected_dataset_list")
      prime(gcic, sdl)

      expect_snapshot(cat(build_dynamic_hash_section(sdl, gcic)))
    })
  })

  test_that("build_date_section embeds the date range and a per-dataset modification-time loop", {
    shiny::isolate({
      gcic <- new_shared_get_code_in_context()
      sdl <- shinymeta::metaReactive(
        list(
          a = structure(data.frame(x = 1), meta = list(mtime = as.POSIXct("2026-01-01", tz = "UTC"))),
          b = structure(data.frame(x = 2), meta = list(mtime = as.POSIXct("2026-01-02", tz = "UTC")))
        ),
        varname = "selected_dataset_list"
      )
      date_range_mr <- shinymeta::metaReactive(
        as.POSIXct(c("2026-01-01", "2026-01-02"), tz = "UTC"),
        varname = "date_range"
      )
      prime(gcic, sdl)

      expect_snapshot(cat(build_date_section(sdl, date_range_mr, gcic)))
    })
  })

  test_that("build_date_section shows the modification time as not available when meta is missing", {
    shiny::isolate({
      gcic <- new_shared_get_code_in_context()
      sdl <- shinymeta::metaReactive(list(a = data.frame(x = 1)), varname = "selected_dataset_list")
      date_range_mr <- shinymeta::metaReactive(as.POSIXct(character(0)), varname = "date_range")
      prime(gcic, sdl)

      section <- build_date_section(sdl, date_range_mr, gcic)
      expect_snapshot(cat(section))

      # Run the generated chunk as the rendered document would
      chunk <- regmatches(
        section,
        regexpr("(?s)```\\{r data_modification_dates[^\n]*\n\\K.*?(?=\n```)", section, perl = TRUE)
      )
      printed <- utils::capture.output(
        eval(parse(text = chunk), list(selected_dataset_list = list(a = data.frame(x = 1))))
      )
      expect_identical(
        trimws(paste(printed, collapse = "")),
        "**name**: `a` **modification time**: Time not available"
      )
    })
  })

  test_that("build_date_section handles zero datasets", {
    shiny::isolate({
      gcic <- new_shared_get_code_in_context()
      sdl <- shinymeta::metaReactive(list(), varname = "selected_dataset_list")
      date_range_mr <- shinymeta::metaReactive(as.POSIXct(character(0)), varname = "date_range")
      prime(gcic, sdl)

      expect_snapshot(cat(build_date_section(sdl, date_range_mr, gcic)))
    })
  })

  test_that("build_filter_txt_section wraps the filter code in format-appropriate verbatim tags", {
    expect_snapshot(cat(build_filter_txt_section(ODG$OUTPUT_FORMAT$HTML, "FIXTURE_FILTER_TXT_CODE")))
    expect_snapshot(cat(build_filter_txt_section(ODG$OUTPUT_FORMAT$PDF, "FIXTURE_FILTER_TXT_CODE")))
  })

  test_that("build_filter_txt_section rejects an unknown output_format instead of erroring deep inside sprintf", {
    expect_error(build_filter_txt_section("bogus", "FIXTURE_FILTER_TXT_CODE"))
  })

  test_that("build_filter_reference_section wraps the filter reference code so pandoc does not read it as markdown", {
    expect_snapshot(cat(build_filter_reference_section(ODG$OUTPUT_FORMAT$HTML, "FIXTURE_FILTER_REFERENCE_CODE")))
    expect_snapshot(cat(build_filter_reference_section(ODG$OUTPUT_FORMAT$PDF, "FIXTURE_FILTER_REFERENCE_CODE")))
  })

  test_that("build_filter_reference_section rejects an unknown output_format", {
    expect_error(build_filter_reference_section("bogus", "FIXTURE_FILTER_REFERENCE_CODE"))
  })
})
