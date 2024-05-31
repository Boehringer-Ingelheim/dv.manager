component <- "has_all_items_named"

test_that(
  paste(
    component,
    "should return true when the list is empty

    "
  ),
  {
    expect_true(has_all_items_named(list()))
  }
)

test_that(
  paste(
    component,
    "should return true when the list is not empty and named

    "
  ),
  {
    expect_true(has_all_items_named(list(a = 1)))
    expect_true(has_all_items_named(list(a = 1, b = 2)))
  }
)

test_that(
  paste(
    component,
    "should return false when at least one elements is not named

    "
  ),
  {
    expect_false(has_all_items_named(list(1)))
    expect_false(has_all_items_named(list(1, 2)))
    expect_false(has_all_items_named(list(a = 1, 2)))
  }
)

test_that(
  vdoc[["add_spec"]]("my test description", specs$a_spec),
  {
    expect_true(TRUE)
  }
)
