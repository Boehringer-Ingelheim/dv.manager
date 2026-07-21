test_that(
  vdoc[["add_spec"]](
    "Dataset functions should only be called once during application startup",
    c(specs$DATASETS$DATASET_ENTRY_STRUCTURE)
  ), {
    df <- structure(data.frame(ID = "0", VAL = 0L), meta = list(mtime = Sys.time()))
    
    DS1_call_count <- 0L
    DS2_call_count <- 0L
    
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
      module_list = list("Simple" = dv.manager:::mod_simple("adsl", "filtered_dataset_list", "mod1")),
      filter_dataset_name = "DF",
      filter_key = "ID",
      .launch = FALSE
    )
    expect_equal(DS1_call_count, 1L)
    expect_equal(DS2_call_count, 1L)
  }
)