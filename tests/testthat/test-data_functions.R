test_that(
  vdoc[["add_spec"]](
    "Dataset functions are only called once during app startup and the result of the last call is cached",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE, specs$DATASETS$DATASET_LIST_CACHING)
  ), {
    df <- structure(data.frame(ID = "0", VAL = 0L), meta = list(mtime = Sys.time()))
    
    DS1_call_count <- 0L
    DS2_call_count <- 0L

    captured_afmm <- NULL
    
    mod_simple_prime <- dv.manager:::mod_simple("adsl", "filtered_dataset_list", "mod1")
    mod_simple_prime[["meta"]] <- list(
      check_mod_fn = function(afmm, dataset_list, dataset_list_name) {
        captured_afmm <<- afmm
        return(character(0))
      }
    )
    
    run_app(
      data = list(
        "DS1" = function(){
          DS1_call_count <<- DS1_call_count + 1L
          return(list(DF = df))
        },
        "DS2" = function(){
          DS2_call_count <<- DS2_call_count + 1L
          return(list(DF = df))
        }
      ),
      module_list = list("Simple" = mod_simple_prime),
      filter_dataset_name = "DF",
      filter_key = "ID",
      .launch = FALSE
    )
    expect_equal(DS1_call_count, 1L)
    expect_equal(DS2_call_count, 1L)

    # Cache testing (we use `afmm` to mimic reactive-time data accesses)
    captured_afmm[["data"]][["DS1"]]()
    expect_equal(DS1_call_count, 1L) # DS1 was cached, so there is no extra call associated
    
    captured_afmm[["data"]][["DS2"]]()
    expect_equal(DS2_call_count, 2L) # DS2 was not cached
    captured_afmm[["data"]][["DS2"]]()
    expect_equal(DS2_call_count, 2L) # DS2 was cached
    
    captured_afmm[["data"]][["DS1"]]()
    expect_equal(DS1_call_count, 2L) # DS1 was not cached
  }
)
