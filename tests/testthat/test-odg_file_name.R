local({
  test_that("sanitize_odg_file_name keeps a portable file name, without extension or leading dash", {
    expect_identical(sanitize_odg_file_name("my_report-v2"), "my_report-v2")
    expect_identical(sanitize_odg_file_name("  Study 123: draft (v1)  "), "Study_123_draft_v1_")
    expect_identical(sanitize_odg_file_name("report.PDF"), "report")
    expect_identical(sanitize_odg_file_name("report.v1.zip"), "report_v1")
    expect_identical(sanitize_odg_file_name("../../etc/passwd"), "_etc_passwd")
    expect_identical(sanitize_odg_file_name("-rf"), "_rf")
    expect_identical(sanitize_odg_file_name("Müller"), "M_ller")
    expect_identical(nchar(sanitize_odg_file_name(strrep("a", 500))), ODG$FILE_NAME$MAX_CHARS)
  })

  test_that("sanitize_odg_file_name falls back to the default name when nothing usable is typed", {
    for (x in list(NULL, NA_character_, "", "   ", ".pdf", c("a", "b"))) {
      expect_identical(sanitize_odg_file_name(x), ODG$FILE_NAME$DEFAULT)
    }
  })

  test_that("sanitize_odg_file_name renames the names that would overwrite a file next to the document", {
    expect_identical(sanitize_odg_file_name("header"), "header_")
    expect_identical(sanitize_odg_file_name("Header"), "Header_")
  })
})
