expect_identical_serialization_deserialization <- function(dataset_lists) {  
  fd_lite <- get_filter_data_binary_serialize(dataset_lists)
  Rser_lite <- binary_serialize_filter_data(fd_lite)
  Runser_liteR <- binary_deserialize_filter_data(Rser_lite)

  Cser_lite <- binary_serialize_filter_data_C(fd_lite)
  Cunser_liteC <- binary_deserialize_filter_data_C(Cser_lite)

  # Cross deserialization
  Runser_liteC <- binary_deserialize_filter_data_C(Rser_lite)
  Cunser_liteR <- binary_deserialize_filter_data(Cser_lite)

  # Serialization are equal
  expect_identical(Rser_lite, Cser_lite)

  # Unserialization are equal
  expect_identical(Runser_liteR, Runser_liteC)
  expect_identical(Runser_liteC, Cunser_liteR)
  expect_identical(Cunser_liteR, Cunser_liteC)  

  # Given that all are identical any should work
  expect_identical(Cunser_liteC, fd_lite)  
}

expect_identical_serialization_deserialization_quietly <- function(...){
  capture.output(expect_identical_serialization_deserialization(...))
}

basic_dataset_lists_mixed_NA_INF <- local({
  date_var <- as.Date("2024-01-01") + c(0L:4L, NA, Inf, -Inf)

  dataset_list_1 <- list(
    dataset_1 = data.frame(
      row.names = 1:8    
      , range_var = c(1.0:5.0, NA, Inf, -Inf)
      , date_var = date_var
      , posix_var = as.POSIXct(date_var)
      , subset_var = factor(c(letters[1:5], NA, Inf, -Inf))
      , logical_var = c(FALSE, TRUE, TRUE, FALSE, FALSE, NA, Inf, -Inf)
      , sbj_var = paste0("SBJ-", c(1:6, Inf, -Inf))
      , ALL_NA_real = NA_real_
      , ALL_NA_integer = NA_integer_
      , ALL_NA_factor = as.factor(NA_character_)
      , ALL_NA_date = as.Date(NA)
      , ALL_NA_POSIXct = as.POSIXct(NA)
      # , ALL_integer_inf = as.integer(Inf) # Inf not representable as Integer
      , ALL_real_inf = as.double(Inf)
      , ALL_date_inf = as.Date(Inf)
      , ALL_posixct_minf = as.POSIXct(-Inf)
      # , ALL_integer_minf = as.integer(-Inf) # Inf not representable as Integer
      , ALL_real_minf = as.double(-Inf)
      , ALL_date_minf = as.Date(-Inf)
      , ALL_posixct_minf = as.POSIXct(-Inf)
    ),
    dataset_2 = data.frame(
      row.names = 1:6,
      sbj_var = paste0("SBJ-", 1:6)
    )
  )
  
  dataset_list_2 <- dataset_list_1
  dataset_list_2$dataset_1$sbj_var <- paste0("SBJ-", c(1:6, Inf, -Inf) * 2)
  dataset_list_2$dataset_2$sbj_var <- paste0("SBJ-", c(1:6) * 2)

  dataset_lists <- list(dataset_list_1 = dataset_list_1, dataset_list_2 = dataset_list_2)
})

basic_dataset_lists_mixed_NA_no_mixed_INF <- local({
  date_var <- as.Date("2024-01-01") + c(0L:4L, NA)

  dataset_list_1 <- list(
    dataset_1 = data.frame(
      row.names = 1:6    
      , range_var = c(1.0:5.0, NA)
      , date_var = date_var
      , posix_var = as.POSIXct(date_var)
      , subset_var = factor(c(letters[1:5], NA))
      , logical_var = c(FALSE, TRUE, TRUE, FALSE, FALSE, NA)
      , sbj_var = paste0("SBJ-", c(1:6))
      , ALL_NA_real = NA_real_
      , ALL_NA_integer = NA_integer_
      , ALL_NA_factor = as.factor(NA_character_)
      , ALL_NA_date = as.Date(NA)
      , ALL_NA_POSIXct = as.POSIXct(NA)
      # , ALL_integer_inf = as.integer(Inf) # Inf not representable as Integer
      , ALL_real_inf = as.double(Inf)
      , ALL_date_inf = as.Date(Inf)
      , ALL_posixct_minf = as.POSIXct(-Inf)
      # , ALL_integer_minf = as.integer(-Inf) # Inf not representable as Integer
      , ALL_real_minf = as.double(-Inf)
      , ALL_date_minf = as.Date(-Inf)
      , ALL_posixct_minf = as.POSIXct(-Inf)
    ),
    dataset_2 = data.frame(
      row.names = 1:6,
      sbj_var = paste0("SBJ-", 1:6)
    )
  )
  
  dataset_list_2 <- dataset_list_1
  dataset_list_2$dataset_1$sbj_var <- paste0("SBJ-", c(1:6) * 2)
  dataset_list_2$dataset_2$sbj_var <- paste0("SBJ-", c(1:6) * 2)

  dataset_lists <- list(dataset_list_1 = dataset_list_1, dataset_list_2 = dataset_list_2)
})

test_that("Supports empty dataset_lists serialization deserialization", {
  dataset_lists <- list()
  expect_identical_serialization_deserialization_quietly(dataset_lists)
})

test_that("Supports one dataset_list with no datasets serialization deserialization", {
  dataset_lists <- list(
    dsl1 = list(      
    )
  )
  expect_identical_serialization_deserialization_quietly(dataset_lists)
})

test_that("Supports one dataset_list with 1 datasets and 0 rows serialization deserialization", {

  dataset_lists <- local({
    dataset_lists <- basic_dataset_lists_mixed_NA_no_mixed_INF

    for(dataset_list_idx in seq_along(dataset_lists)){
      dataset_list <- dataset_lists[[dataset_list_idx]]
      for(dataset_idx in seq_along(dataset_list)){
        dataset <- dataset_list[[dataset_idx]]
        dataset <- dataset[integer(0), , drop = FALSE]
        dataset_list[[dataset_idx]] <- dataset
      }
      dataset_lists[[dataset_list_idx]] <- dataset_list
    }

    dataset_lists
  })
  
  expect_identical_serialization_deserialization_quietly(dataset_lists)
})

test_that("Supports a regular dataset_lists", {
  expect_identical_serialization_deserialization_quietly(basic_dataset_lists_mixed_NA_no_mixed_INF)
})

test_that("Supports a regular dataset_lists with Infs", {
  # Separated because Infs will always be the the max and min values
  expect_identical_serialization_deserialization_quietly(basic_dataset_lists_mixed_NA_INF)
})

generate_dataset_lists <- function() {

  var_kind <- list("integer", "double", "date", "posixct", "categorical", "logical")

  random_strings <- function(n) {
    vapply(
      seq_len(n),
      function(i) {
        len <- sample.int(MAX_STR_LENGTH, 1) # random length
        paste0(sample(c(letters, LETTERS, 0:9), len, TRUE), collapse = "")
      },
      FUN.VALUE = ""
    )
  }

  MAX_DATASET_LISTS_LEN <- 5
  MAX_DATASET_LIST_LEN <- 100
  MAX_DATASET_NVARS <- 100
  MAX_DATASET_NROW <- 100
  MAX_NUM_VALUE <- 1e300
  MIN_NUM_VALUE <- -1e300
  MAX_STR_LENGTH = 2^10


  dataset_lists <- list()
  dataset_lists_len <- sample(MAX_DATASET_LISTS_LEN, size = 1)

  for (dataset_list_idx in seq_len(dataset_lists_len)) {
    dataset_list <- list()
    dataset_list_len <- sample(MAX_DATASET_LIST_LEN, size = 1)
    for (dataset_idx in seq_len(dataset_list_len)) {      
      dataset_nvar <- sample(MAX_DATASET_NVARS, size = 1)
      dataset_nrow <- sample(MAX_DATASET_NROW, size = 1)
      dataset <- data.frame(row.names = 1:dataset_nrow)
      for (var_idx in seq_len(dataset_nvar)){
        dataset[[var_idx]] <- local({
          kind <- sample(var_kind, size = 1)
          if(kind == "integer") {
            var <- round(runif(dataset_nrow, MIN_NUM_VALUE, MAX_NUM_VALUE))
            na_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            minus_inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            var[na_idx] <- NA_integer_
            var[inf_idx] <- Inf
            var[minus_inf_idx] <- -Inf
          } else if (kind == "double") {
            var <- runif(dataset_nrow, -MIN_NUM_VALUE, MAX_NUM_VALUE)
            na_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            minus_inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            var[na_idx] <- NA_real_
            var[inf_idx] <- Inf
            var[minus_inf_idx] <- -Inf
          } else if (kind == "date") {
            var <- round(runif(dataset_nrow, MIN_NUM_VALUE, MAX_NUM_VALUE))
            na_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            minus_inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            var[na_idx] <- NA_integer_
            var[inf_idx] <- Inf
            var[minus_inf_idx] <- -Inf
            var <- as.Date(var)            
          } else if (kind == "posixct") {
            var <- round(runif(dataset_nrow, MIN_NUM_VALUE, MAX_NUM_VALUE))
            na_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            minus_inf_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))
            var[na_idx] <- NA_real_
            var[inf_idx] <- Inf
            var[minus_inf_idx] <- -Inf
            var <- as.POSIXct(var)            
          } else if (kind == "categorical") {                        
            var <- random_strings(dataset_nrow)
            na_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))                        
            var[na_idx] <- NA_character_
            var <- as.factor(var)            
          } else if (kind == "logical") {                        
            var <- sample(c(TRUE, FALSE), size = dataset_nrow, replace = TRUE)
            na_idx <- sample(dataset_nrow, size = sample (dataset_nrow, size = 1))                        
            var[na_idx] <- as.logical(NA)
            var <- as.factor(var)            
          }
        })
      }      
      names(dataset) <- random_strings(dataset_nvar)
      dataset_list[[dataset_idx]] <- dataset
    }
    names(dataset_list) <- random_strings(dataset_list_len)
    dataset_lists[[dataset_list_idx]] <- dataset_list
  }
  names(dataset_lists) <- random_strings(dataset_lists_len)

  dataset_lists
}

