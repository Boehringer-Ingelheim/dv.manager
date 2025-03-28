local({
  d1 <- data.frame(
    num = 1:4,
    char_var1 = c("a", "b", "c", "d"),
    char_var2 = c("e", "f", "g", "h"),
    fac = factor(c("fa", "fb", "fc", "fd"))
  )

  d2 <- data.frame(
    num = 1:4,
    char_var1 = c("e", "f", "g", "h"),
    char_var2 = c("i", "j", "k", "l"),
    fac = factor(c("fa", "fb", "fc", "fd"))
  )

  attr(d1[["char_var1"]], "label") <- "char_var1_label"
  attr(d1[["char_var2"]], "label") <- "char_var2_label"
  attr(d2[["char_var1"]], "label") <- "char_var1_label"
  attr(d2[["char_var2"]], "label") <- "char_var2_label"

  e1 <- d1
  e1[["char_var1"]] <- factor(e1[["char_var1"]])
  attr(e1[["char_var1"]], "label") <- "char_var1_label"
  e1[["char_var2"]] <- factor(e1[["char_var2"]])
  attr(e1[["char_var2"]], "label") <- "char_var2_label"

  e2 <- d2
  e2[["char_var1"]] <- factor(e2[["char_var1"]])
  attr(e2[["char_var1"]], "label") <- "char_var1_label"
  e2[["char_var2"]] <- factor(e2[["char_var2"]])
  attr(e2[["char_var2"]], "label") <- "char_var2_label"

  dl <- list(ds1 = d1, ds2 = d2)
  edl <- list(ds1 = e1, ds2 = e2)

  dataset_lists <- list(
    dl1 = dl,
    dl2 = function() dl
  )

  expected_dataset_lists <- list(
    dl1 = edl,
    dl2 = function() edl
  )

  test_that("char_vars_to_factor_vars_dataset transform character variable into factors variables", {
    r <- char_vars_to_factor_vars_dataset(d1)
    expect_identical(r, e1)
  })

  test_that("char_vars_to_factor_vars respect lables", {    
    r <- char_vars_to_factor_vars_dataset(dl)    
    expect_identical(attr(e1[["char_var1"]], "label"), "char_var1_label")
  })

  test_that("decorate_char_vars_to_factor_vars_dataset_list decorates a functions that returns a list of data.frames and transforms character variables into factor variables", {
    f <- function() dl
    dec_f <- decorate_char_vars_to_factor_vars_dataset_list(f)
    e <- edl
    expect_identical(dec_f(), e)
  })

  test_that("char_vars_to_factor_vars_dataset_lists applies decorators and transformations to dataset_lists", {
    r <- char_vars_to_factor_vars_dataset_lists(dataset_lists)
    expect_identical(r[["dl1"]][["ds1"]], expected_dataset_lists[["dl1"]][["ds1"]])
    expect_identical(r[["dl1"]][["ds2"]], expected_dataset_lists[["dl1"]][["ds2"]])
    expect_identical(r[["dl2"]]()[["ds1"]], expected_dataset_lists[["dl2"]]()[["ds1"]])
    expect_identical(r[["dl2"]]()[["ds2"]], expected_dataset_lists[["dl2"]]()[["ds2"]])   
  })

  test_that("character variables are transformed into factors during run_app calls", {
    r <- suppressMessages(suppressWarnings(run_app(dataset_lists, module_list = list(), filter_data = "ds1", filter_key = "char_var1", .launch = FALSE)))

    expect_identical(r[["config"]][["data"]][["dl1"]][["ds1"]], expected_dataset_lists[["dl1"]][["ds1"]])
    expect_identical(r[["config"]][["data"]][["dl1"]][["ds2"]], expected_dataset_lists[["dl1"]][["ds2"]])
    expect_identical(r[["config"]][["data"]][["dl2"]]()[["ds1"]], expected_dataset_lists[["dl2"]]()[["ds1"]])
    expect_identical(r[["config"]][["data"]][["dl2"]]()[["ds2"]], expected_dataset_lists[["dl2"]]()[["ds2"]])
  })
})
