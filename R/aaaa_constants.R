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
  NAV_HEADER = "nav_header",
  FILTER = "filter",
  FILTER_JSON_INPUT = "filter_json_input",
  FILTER_LOG_INPUT = "filter_log_input",
  FILTER_CONTAINER = "filter_container",
  SELECT = "select",
  SIMPLE = poc(
    CONTAINER = "simple_container"
  ),
  DATASETS = poc(
    CONTAINER = "datasets_container"
  ),
  BLOCKLY = poc(
    CONTAINER = "blockly_container",
    GEN_CODE = "gen_code_button",
    EXPORT_CODE = "export_code_button",
    INNER_CONTAINER = "blockly_inner_filter_container"
  )
)

FILTER <- poc(
  TYPE = poc(
    SIMPLE = "simple",
    DATASETS = "datasets",
    BLOCKLY = "blockly"
  )
)
