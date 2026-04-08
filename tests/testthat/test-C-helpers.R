test_that("count_factor_C counts each level correctly", {
  x <- factor(c("a", "b", "a", "c", "b", "b"))
  res <- count_factor_C(x)
  expect_equal(res[2:4], c(2L, 3L, 1L)) # index 1 is NA slot
})

test_that("count_factor_C NA values land in index 1 (slot 0)", {
  x <- factor(c("a", NA, "a", NA))
  res <- count_factor_C(x)
  expect_equal(res[1], 2L)
})

test_that("count_factor_C empty factor returns all-zero counts", {
  x <- factor(character(0), levels = c("a", "b"))
  res <- count_factor_C(x)
  expect_true(all(res == 0L))
})

test_that("count_factor_C single-element factor is handled", {
  x <- factor("a", levels = c("a", "b"))
  res <- count_factor_C(x)
  expect_equal(res[2], 1L)
  expect_equal(res[3], 0L)
})


test_that("max_min_count_na_C returns correct max, min, and na_count", {
  res <- max_min_count_na_C(c(1.0, 3.0, 2.0))
  expect_equal(res[[1]], 3.0)
  expect_equal(res[[2]], 1.0)
  expect_equal(res[[3]], 0L)
})

test_that("max_min_count_na_C NaN and NA are counted as NA", {
  res <- max_min_count_na_C(c(1.0, NA, NaN, 2.0))
  expect_equal(res[[3]], 2L)
})

test_that("max_min_count_na_C all-NA input returns Inf sentinels", {
  res <- max_min_count_na_C(c(NA_real_, NA_real_))
  expect_equal(res[[1]], -Inf)
  expect_equal(res[[2]], Inf)
  expect_equal(res[[3]], 2L)
})

test_that("max_min_count_na_C single finite element works", {
  res <- max_min_count_na_C(42.0)
  expect_equal(res[[1]], 42.0)
  expect_equal(res[[2]], 42.0)
  expect_equal(res[[3]], 0L)
})


test_that("has_finite_C returns TRUE when at least one finite value exists", {
  expect_true(has_finite_C(c(NA_real_, 1.0, Inf)))
})

test_that("has_finite_C returns FALSE for all non-finite input", {
  expect_false(has_finite_C(c(NA_real_, NaN, Inf, -Inf)))
})

test_that("has_finite_C returns FALSE for empty vector", {
  expect_false(has_finite_C(numeric(0)))
})

test_that("has_finite_C Inf and -Inf are not considered finite", {
  expect_false(has_finite_C(c(Inf, -Inf)))
})
