local({

  dv_mosl <- as_dv_manager_module_output_safe_list(list(a = 1, b = 2))

  test_that("returns elements when indexing with integers", {
    expect_identical(dv_mosl[[1]], 1)    
    expect_identical(dv_mosl[c(1,2)], list(a=1, b = 2))    
  })

  test_that("fails when accesing a missing element using [[]]", {
    expect_error(
      dv_mosl[["c"]],
      regexp = "Element 'c' not found in module output",
      class = "shiny.silent.error"      
    )
  })

  test_that("fails when accesing a missing element using $", {
    expect_error(
      dv_mosl$c,
      regexp = "Element 'c' not found in module output",
      class = "shiny.silent.error"      
    )
  })

  test_that("fails when accesing missing elements using []", {
    expect_error(
      dv_mosl[c("c", "d")],
      regexp = "Elements 'c', 'd' not found in module_output",
      class = "shiny.silent.error"      
    )

    expect_error(
      dv_mosl[c("a", "d")],
      regexp = "Elements 'd' not found in module_output",
      class = "shiny.silent.error"      
    )
  })

  test_that("returns element when accesing a present element using [[]]", {
    expect_identical(dv_mosl[['a']], 1)  
  })

  test_that("returns element when accesing a present element using $", {
    expect_identical(dv_mosl$a, 1)  
  })

  test_that("returns element when accesing present elements using []", {
    expect_identical(dv_mosl[c('a','b')], list(a=1, b = 2))
  })
})