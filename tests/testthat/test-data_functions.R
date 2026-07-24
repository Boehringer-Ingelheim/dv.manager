test_that(
  vdoc[["add_spec"]](
    "Dataset functions should only be called once during application startup",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE)
  ), {
    df <- structure(data.frame(ID = "0", VAL = 0L), meta = list(mtime = Sys.time()))
    
    DS1_call_count <- 0L
    DS2_call_count <- 0L
    
    mod_simple_prime <- dv.manager:::mod_simple("adsl", "filtered_dataset_list", "mod1")
    mod_simple_prime[["meta"]] <- list(
      check_mod_fn = function(afmm, dataset_list, dataset_list_name) {
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
  }
)
