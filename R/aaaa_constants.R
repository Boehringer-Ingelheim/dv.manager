# nolint start

TT <- local({
  SUBJECT_LEVEL_FILTER <-
    "Apply a filter to the dataset and use the resulting subject IDs (default) to consistently filter the rest of datasets."

  DATASET_FILTER <-
    "Apply a filter to a specific dataset. Does not impact the rest of datasets. Only datasets that are used by the currently selected module are shown in this dataset."

  poc(
    SUBJECT_LEVEL_FILTER = SUBJECT_LEVEL_FILTER,
    DATASET_FILTER = DATASET_FILTER
  )
})

# nolint end


LAYOUT <- poc( # nolint
  ATTRIBUTE = "layout",
  TAB_GROUP = "tab_group"
)

ID <- poc(
  NAV_HEADER = "nav_header"
)
