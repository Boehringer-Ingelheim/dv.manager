
test_that("flatten_srv_module_list handles a simple list", {
  input_list <- list(
    list(server = "server1", module_id = "module1"),
    list(server = "server2", module_id = "module2")
  )
  expected_output <- list(
    list(server = "server1", module_id = "module1"),
    list(server = "server2", module_id = "module2")
  )
  expect_equal(flatten_srv_module_list(input_list), expected_output)
})

test_that("flatten_srv_module_list handles nested server collections", {
    srv_collection <- list(
      list(server = "server3", module_id = "module3"),
      list(server = "server4", module_id = "module4")
    )
    class(srv_collection) <- "server_collection"

    nested_srv_collection <- list(
      list(server = "server2", module_id = "module2"),
      list(module_id = "sub_level", server = srv_collection)
    )
    class(nested_srv_collection) <- "server_collection"

  input_list <- list(
    list(server = "server1", module_id = "module1"),
    list(module_id = "top_level", server = nested_srv_collection)    
    )
  
  expected_output <- list(
    list(server = "server1", module_id = "module1"),
    list(server = "server2", module_id = "module2"),
    list(server = "server3", module_id = "module3"),
    list(server = "server4", module_id = "module4")
  )
  expect_equal(flatten_srv_module_list(input_list), expected_output)
})

test_that("flatten_srv_module_list handles empty list", {
  input_list <- list()
  expected_output <- list()
  expect_equal(flatten_srv_module_list(input_list), expected_output)
})
