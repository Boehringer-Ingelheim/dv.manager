# nolint start

TT <- local({
  SUBJECT_LEVEL_FILTER <-
    "Apply a filter to the dataset and use the resulting subject IDs (default) to consistently filter the rest of datasets."

  DATASET_FILTER <-
    "Apply a filter to an specific dataset. Does not impact the rest of datasets."

  DATA_FILTER <-
    "Data Filter"

  poc(
    SUBJECT_LEVEL_FILTER = SUBJECT_LEVEL_FILTER,
    DATASET_FILTER = DATASET_FILTER,
    DATA_FILTER = DATASET_FILTER
  )
})

# nolint end
