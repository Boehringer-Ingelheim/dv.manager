# Testing dispatchers ----

# Testing mm_dispatch  ----

component <- "mm_dispatch"

test_that(
  paste(
    component,
    "should return a list with two entries from and selection with class \"mm_dispatcher\"

    "
  ),
  {
    mm_dispatch("A", "B") %>%
      expect_equal(
        structure(
          list(from = "A", selection = "B"),
          class = "mm_dispatcher"
        )
      )
  }
)

# Testing mm_resolve_dispatcher  ----

component <- "mm_resolve_dispatcher"

## Objects across tests

mock_afmm <- list(
  "react" = shiny::reactive(list(A = 1, B = 2)),
  "metareact" = shinymeta::metaReactive({
    list(A = 1, B = 2)
  }),
  "non_react" = list(A = 1, B = 2)
)


# Testing mm_dispatch (filtered) ----

component <- "mm_resolve_dispatcher"

test_that(
  paste(
    component,
    "should return the same object if the object is not of type mm_dispatcher

    "
  ),
  {
    mm_resolve_dispatcher("A", list()) %>%
      expect_equal("A")
  }
)

# Reactive
test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], reactive-single case

    "
  ),
  {
    # Flatten
    mm_resolve_dispatcher(mm_dispatch("react", "A"), mock_afmm, flatten = TRUE) %>%
      checkmate::expect_class("reactive") %>%
      (function(x) {
        shiny::isolate(x())
      }) %>%
      expect_equal(1)

    mm_resolve_dispatcher(mm_dispatch("react", "A"), mock_afmm, flatten = FALSE) %>%
      checkmate::expect_class("reactive") %>%
      (function(x) {
        shiny::isolate(x())
      }) %>%
      expect_equal(list(A = 1))
  }
)

test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], reactive-multi case

    "
  ),
  {
    val <- mm_resolve_dispatcher(mm_dispatch("react", c("A", "B")), mock_afmm, flatten = FALSE)
    expect_true(shiny::is.reactive(val))
    expect_equal(shiny::isolate(val()), list(A = 1, B = 2))
  }
)

test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], reactive-all case

    "
  ),
  {
    val <- mm_resolve_dispatcher(mm_dispatch("react"), mock_afmm, flatten = FALSE)
    expect_true(shiny::is.reactive(val))
    expect_equal(shiny::isolate(val()), list(A = 1, B = 2))
  }
)

# Metareactive
test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], metareactive-single case

    "
  ),
  {
    mm_resolve_dispatcher(mm_dispatch("metareact", "A"), mock_afmm, flatten = TRUE) %>%
      checkmate::expect_class("shinymeta_reactive") %>%
      (function(x) {
        shiny::isolate(x())
      }) %>%
      expect_equal(1)

    mm_resolve_dispatcher(mm_dispatch("metareact", "A"), mock_afmm, flatten = FALSE) %>%
      checkmate::expect_class("shinymeta_reactive") %>%
      (function(x) {
        shiny::isolate(x())
      }) %>%
      expect_equal(list(A = 1))
  }
)

test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], metareactive-multi case

    "
  ),
  {
    # Check flatten has no effect
    val <- mm_resolve_dispatcher(mm_dispatch("metareact", c("A", "B")), mock_afmm, flatten = FALSE)
    expect_true(is.metareactive(val))
    expect_equal(shiny::isolate(val()), list(A = 1, B = 2))
  }
)

test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], reactive-all case

    "
  ),
  {
    val <- mm_resolve_dispatcher(mm_dispatch("metareact"), mock_afmm, flatten = FALSE) # Check flatten has no effect)
    expect_true(is.metareactive(val))
    expect_equal(shiny::isolate(val()), list(A = 1, B = 2))
  }
)

# Non-reactive
test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], metareactive-single case

    "
  ),
  {
    mm_resolve_dispatcher(mm_dispatch("non_react", "A"), mock_afmm, flatten = TRUE) %>%
      expect_equal(1)

    mm_resolve_dispatcher(mm_dispatch("non_react", "A"), mock_afmm, flatten = FALSE) %>%
      expect_equal(list(A = 1))
  }
)

test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], reactive-multi case

    "
  ),
  {
    val <- mm_resolve_dispatcher(mm_dispatch("non_react", c("A", "B")), mock_afmm, flatten = FALSE)
    expect_true(is.list(val))
    expect_equal(val, list(A = 1, B = 2))
  }
)

test_that(
  paste(
    component,
    "should return the 'selection' entry from mock_afmm[[from]], reactive-all case

    "
  ),
  {
    val <- mm_resolve_dispatcher(mm_dispatch("non_react"), mock_afmm, flatten = FALSE)
    expect_true(is.list(val))
    expect_equal(val, list(A = 1, B = 2))
  }
)

test_that(
  paste(
    component,
    "should throw an error when selection length is greater than one and flatten is TRUE

    "
  ),
  {
    mm_resolve_dispatcher(mm_dispatch("react", c("A", "B")), mock_afmm, flatten = TRUE) %>%
      expect_error("^Assertion on 'sel_length > 1 && flatten' failed: Must be FALSE\\.$")
  }
)
