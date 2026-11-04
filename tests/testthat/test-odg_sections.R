local({
  test_that("build_hardcoded_hash_section lists a content hash per dataset", {
    dataset_list <- list(a = data.frame(x = 1:2), b = data.frame(y = "z"))
    expect_snapshot(cat(build_hardcoded_hash_section(hash_dataset_list(dataset_list))))
  })

  test_that("build_hardcoded_hash_section handles an empty dataset list", {
    expect_snapshot(cat(build_hardcoded_hash_section(hash_dataset_list(list()))))
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

  # The generated code only refers to `selected_dataset_list` by name, so its value here never reaches the section
  build_dynamic_hash_section_for <- function(in_app_list, output_format) {
    gcic <- new_shared_get_code_in_context()
    sdl <- shinymeta::metaReactive(list(), varname = "selected_dataset_list")
    prime(gcic, sdl)
    build_dynamic_hash_section(sdl, hash_dataset_list(in_app_list), output_format, gcic)
  }

  # Run the generated chunk as the rendered document would
  run_dynamic_hash_chunk <- function(section, dataset_list) {
    chunk <- regmatches(
      section,
      regexpr("(?s)```\\{r dynamic_data_hash[^\n]*\n\\K.*?(?=\n```)", section, perl = TRUE)
    )
    printed <- utils::capture.output(eval(parse(text = chunk), list(selected_dataset_list = dataset_list)))
    paste(printed, collapse = "\n")
  }

  test_that("build_dynamic_hash_section emits an asis loop checking the dataset-list variable against the in-app hashes", {
    shiny::isolate({
      in_app_list <- list(a = data.frame(x = 1:2), b = data.frame(y = "z"))
      expect_snapshot(cat(build_dynamic_hash_section_for(in_app_list, ODG$OUTPUT_FORMAT$HTML)))
      expect_snapshot(cat(build_dynamic_hash_section_for(in_app_list, ODG$OUTPUT_FORMAT$PDF)))
    })
  })

  test_that("build_dynamic_hash_section handles zero datasets", {
    shiny::isolate({
      section <- build_dynamic_hash_section_for(list(), ODG$OUTPUT_FORMAT$HTML)
      expect_snapshot(cat(section))
      expect_identical(run_dynamic_hash_chunk(section, list()), "")
    })
  })

  test_that("build_dynamic_hash_section rejects an unknown output_format", {
    shiny::isolate({
      expect_error(build_dynamic_hash_section_for(list(), "bogus"), "output_format")
    })
  })

  test_that("the dynamic hash chunk highlights nothing when the data matches the in-app data", {
    shiny::isolate({
      dataset_list <- list(a = data.frame(x = 1:2), b = data.frame(y = "z"))
      for (output_format in c(ODG$OUTPUT_FORMAT$HTML, ODG$OUTPUT_FORMAT$PDF)) {
        printed <- run_dynamic_hash_chunk(build_dynamic_hash_section_for(dataset_list, output_format), dataset_list)
        expect_no_match(printed, "Hash mismatch", info = output_format)
        expect_no_match(printed, "alert", info = output_format)
      }
    })
  })

  test_that("the dynamic hash chunk highlights changed datasets and those only one side has", {
    shiny::isolate({
      # `a` changed, `b` unchanged, `c` only in the app, `d` only in the document
      in_app_list <- list(a = data.frame(x = 1:3), b = data.frame(y = "z"), c = data.frame(w = 1))
      dataset_list <- list(a = data.frame(x = 1:2), b = data.frame(y = "z"), d = data.frame(v = 1))

      html <- run_dynamic_hash_chunk(build_dynamic_hash_section_for(in_app_list, ODG$OUTPUT_FORMAT$HTML), dataset_list)
      pdf <- run_dynamic_hash_chunk(build_dynamic_hash_section_for(in_app_list, ODG$OUTPUT_FORMAT$PDF), dataset_list)
      expect_snapshot(cat(html))
      expect_snapshot(cat(pdf))
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

  # The generated code only refers to `filter_odg` by name, so its value here never reaches the section.
  # Primed so snapshots show only the section's own code.
  build_filter_section <- function(build_section, output_format) {
    gcic <- new_shared_get_code_in_context()
    filter_odg <- shinymeta::metaReactive(list(), varname = "filter_odg")
    prime(gcic, filter_odg)
    build_section(filter_odg, output_format, gcic)
  }

  test_that("build_filter_txt_section wraps the filter code in format-appropriate verbatim tags", {
    shiny::isolate({
      expect_snapshot(cat(build_filter_section(build_filter_txt_section, ODG$OUTPUT_FORMAT$HTML)))
      expect_snapshot(cat(build_filter_section(build_filter_txt_section, ODG$OUTPUT_FORMAT$PDF)))
    })
  })

  test_that("build_filter_txt_section rejects an unknown output_format instead of erroring deep inside sprintf", {
    shiny::isolate({
      expect_error(build_filter_section(build_filter_txt_section, "bogus"), "output_format")
    })
  })

  test_that("build_filter_reference_section wraps the filter reference code so pandoc does not read it as markdown", {
    shiny::isolate({
      expect_snapshot(cat(build_filter_section(build_filter_reference_section, ODG$OUTPUT_FORMAT$HTML)))
      expect_snapshot(cat(build_filter_section(build_filter_reference_section, ODG$OUTPUT_FORMAT$PDF)))
    })
  })

  test_that("build_filter_reference_section prints a fallback when there are no references", {
    shiny::isolate({
      for (output_format in c(ODG$OUTPUT_FORMAT$HTML, ODG$OUTPUT_FORMAT$PDF)) {
        section <- build_filter_section(build_filter_reference_section, output_format)

        # Run the generated chunk as the rendered document would
        chunk <- regmatches(
          section,
          regexpr("(?s)```\\{r filter_odg_reference_list[^\n]*\n\\K.*?(?=\n```)", section, perl = TRUE)
        )
        printed <- utils::capture.output(
          eval(parse(text = chunk), list(filter_odg = list(txt = "", references = list())))
        )
        expect_identical(printed, "No references found", info = output_format)
      }
    })
  })

  test_that("build_filter_reference_section rejects an unknown output_format", {
    shiny::isolate({
      expect_error(build_filter_section(build_filter_reference_section, "bogus"), "output_format")
    })
  })
})
