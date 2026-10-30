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

LAYOUT <- poc(
  # nolint
  ATTRIBUTE = "layout",
  TAB_GROUP = "tab_group"
)

ID <- poc(
  NAV_HEADER = "nav_header",
  FILTER = "filter",
  SUBGROUP = "subgroup",
  FILTER_STATE_JSON_INPUT = "filter_state_json_input",
  FILTER_LOG_INPUT = "filter_log_input",
  SAVED_FILTER_STATE_JSON_MSG_INPUT = "saved_filter_state_json_msg_input",
  FILTER_MODE_INPUT = "filter_mode",
  EXPORT_FILTER_CODE_INPUT = "export_filter_code_button_input",
  BLOCKLY = poc(
    CONTAINER = "blockly_container",
    GEN_CODE = "gen_code_button",
    INNER_CONTAINER = "blockly_inner_filter_container"
  )
)


ODG_ID <- poc(
  SELECTED_DATASET_LIST = "selected_dataset_list",
  UNFILTERED_DATASET_LIST = "unfiltered_dataset_list",
  FILTERED_DATASET_LIST = "filtered_dataset_list",
  UNFILTERED_INFO_DATASET_LIST_ = "unfiltered_dataset_list_with_filter_info_",
  UNFILTERED_INFO_DATASET_LIST = "unfiltered_dataset_list_with_filter_info"
)
