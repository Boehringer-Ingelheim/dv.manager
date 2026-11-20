test_that("ODG chunk labels are in the reserved namespace and written alike", {
  labels <- unlist(unclass(ODG$CHUNK_LABEL))
  expect_true(all(startsWith(labels, paste0(ODG$RESERVED_NAMESPACE, "-"))))
  # Words joined by `_`, the namespace separated by the only `-`
  expect_true(all(grepl("^[a-z]+-[a-z_]+$", labels)), info = paste(labels, collapse = ", "))
  expect_false(anyDuplicated(labels) > 0)
})

test_that("ODG templates have their chunk labels filled in", {
  pdf <- ODG$OUTPUT_FORMAT$PDF
  chunk_start <- function(label) paste0("```{r ", label, ",")
  expect_match(ODG$TEMPLATES$HEADER[[pdf]], chunk_start(ODG$CHUNK_LABEL$SETUP), fixed = TRUE)
  expect_match(ODG$TEMPLATES$HEADER[[pdf]], chunk_start(ODG$CHUNK_LABEL$CONDITION_CATCHER), fixed = TRUE)
  expect_match(ODG$TEMPLATES$SESSION_INFO[[pdf]], chunk_start(ODG$CHUNK_LABEL$SESSION_INFO), fixed = TRUE)
  expect_match(ODG$TEMPLATES$FOOTER[[pdf]], chunk_start(ODG$CHUNK_LABEL$WARNINGS_SUMMARY), fixed = TRUE)
  expect_false(any(grepl("{{", unlist(ODG$TEMPLATES), fixed = TRUE)))
})
