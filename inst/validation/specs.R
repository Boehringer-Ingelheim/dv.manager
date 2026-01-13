# Use a list to declare the specs
#nolint start

specs <- list(

  # --- Interface ---
  INTERFACE = list(
    "INTERFACE_RUN_APP" = "Application is launched via run_app(), which returns an app object.",    
    "INTERFACE_TITLE" = "If a title is provided, it is displayed in the browser tab/window. If no title is provided, the default app title is 'Untitled'.",    
    "INTERFACE_STARTUP_MESSAGE" = "Startup message can be a shiny::modalDialog without error. NULL for no message.",
    "INTERFACE_CSS_NAMESPACE" = "By default, CSS rules from modules are namespaced.CSS namespacing can be disabled with options('dv.manager.disable_css_namespacing').",
    "INTERFACE_SIDEBAR" = "A sidebar is present in the application",
    "INTERFACE_DATASET_LIST_NAME" = "Dataset list name is present in the interface",
    "INTERFACE_MOD_DATE_LIST" = "Modification date of dataset list is present in the interface"
  ),

  # --- Modules ---
  MODULES = list(
    "MODULE_DEFINITION_STRUCTURE" = "Each module entry in module_list must include ['ui'], ['server'], and ['module_id'] (ids must be unique and not ''; error thrown if not).",
    "MODULE_DEFINITION_UI_NAME" = "The entry name in module_list is displayed as the module name in the UI.",
    "MODULE_LIST_EMPTY_ALLOWED" = "Module list can be empty; warning thrown if empty.",
    "MODULE_TABS_TOPNAV" = "Modules appear as tabs in the top navigation bar in the specified order.",
    "MODULE_CONTENT_DISPLAY" = "When a module is selected, its content is displayed.",
    "MODULE_GROUPING" = "Modules can be grouped under a single tab.",
    "MODULE_NESTING" = "Modules can be nested inside other modules.",
    "MODULE_ACCESS_SELF_NAME" = "A module can access its own name.",
    "MODULE_ACCESS_OTHER_NAMES" = "A module can access the names of other modules.",
    "MODULE_ACCESS_DATASET_LIST_NAME" = "A module can access the name of the active dataset list.",
    "MODULE_ACCESS_UNFILTERED_DATASET_LIST" = "A module can access the unfiltered dataset.",
    "MODULE_ACCESS_FILTERED_DATASET_LIST" = "A module can access the filtered dataset.",
    "MODULE_ACCESS_MOD_DATES" = "A module can access dataset modification dates.",
    "MODULE_ACCESS_OTHER_OUTPUTS" = "A module can access the outputs of other modules.",
    "MODULE_SWITCHING_PROGRAMMATIC" = "Programmatic switching between module tabs is supported.",
    "MODULE_BOOKMARKABLE" = "Modules can expose their internal state to be included in bookmarks.",
    "MODULE_OUTPUT_LIST" = "All module outputs are collected in a named list keyed by module_id.",
    "UI_TAB_SELECTOR" = "A tab selector is provided with one entry per module."
  ),

  # --- Datasets ---
  DATASETS = list(
    "DATASET_LIST_NAMED" = "Datasets must be defined as a named list (must not be NULL; error thrown if not).",
    "DATASET_LIST_NOT_NULL" = "Dataset list must not be NULL.",
    "DATASET_LIST_EMPTY_ALLOWED" = "Dataset list can be empty but not NULL.",
    "DATASET_ENTRY_STRUCTURE" = "Each dataset entry can be:
        - a list of data tables, or
        - a function returning a list of data tables.  
      All datasets must contain the same data tables.",
    "DATASET_LIST_SELECTOR_VISIBLE" = "Dataset list selector is displayed if more than one dataset is available.",    
    "DATASET_LIST_SWITCHING_ALLOWED" = "Users can switch between datasets during execution.",
    "DATASET_LIST_SINGLE_ACTIVE" = "Only one dataset can be active at a time.",
    "DATASET_LIST_LABELS_DISPLAY" = "Dataset labels are displayed in the UI.",
    "DATASET_LIST_MOD_DATE_SINGLE" = "If all tables have the same mtime, a single date is displayed (mtime must be POSIXct).",
    "DATASET_LIST_MOD_DATE_RANGE" = "If tables have different mtimes, earliest and latest are displayed as a range (mtime must be POSIXct).",
    "DATASET_LIST_MOD_DATE_UNAVAILABLE_UI" = "If no mtime is available, 'Date unavailable' is displayed in the UI; a warning is logged if meta$mtime is NULL.",
    "DATASET_LIST_EMPTY_SUPPORTED" = "Datasets with zero rows are supported.",
    "DATASET_LISTS_RELOAD_DURATION" = "Datasets can be reloaded after a duration specified by data_reload (must be lubridate duration or positive numeric).",
    "DATASET_LIST_SELECTION_BOOKMARKABLE" = "The identity of the active dataset is included in bookmarks."
  ),

  # --- Filtering ---
  FILTERING = list(
    "FILTER_MENU_SIDEBAR" = "Filtering menus are presented in the sidebar",
    "FILTER_ACTIVE_DATASET_LIST" = "Active dataset_list can be filtered",
    "FILTER_GLOBAL_TABLE" = "The table specified by filter_data is used for global filtering (must exist in all datasets).",
    "FILTER_GLOBAL_KEY" = "The field specified by filter_key is used across tables for global filtering (must exist in all tables).",
    "FILTER_INDEPENDENT_TABLES" = "Tables other than filter_data can have independent filters affecting only themselves.",
    "FILTER_SUPPORTED_TYPES" = "Filtering supports numeric, date, logical, and factor variables.",
    "FILTER_ADD_REMOVE" = "Users can add or remove filters.",
    "FILTER_INCLUDE_EXCLUDE_NA" = "Filters allow inclusion or exclusion of NA values.",
    "FILTER_INITIAL_STATE" = "Filters can be initialized with a default state.",
    "FILTER_SAVE_RESTORE" = "Filters can be saved and restored.",
    "FILTER_CONTEXTUAL" = "Filters display only information relevant to the currently active module.",
    "FILTER_BOOKMARKABLE" = "Filter state is included in bookmarks.",
    "FILTER_LEVEL_DROP" = "Filtered out levels in factors will be dropped"
  ),

  # --- Bookmarking ---
  BOOKMARKING = list(
    "BOOKMARK_BUTTON" = "A bookmarking button is available in the UI.",
    "BOOKMARK_DATASET_LIST" = "Bookmarks save the identity of the active dataset.",
    "BOOKMARK_FILTERS" = "Bookmarks save applied filters.",
    "BOOKMARK_ACTIVE_MODULE" = "Bookmarks save the currently active module.",
    "BOOKMARK_MODULE_STATES" = "Bookmarks save the internal states of bookmarkable modules. Internal bookmarking is a module responsibility."
  ),

  # --- Authentication ---
  AUTHENTICATION = list(
    "SSO_ENABLED" = "Azure SSO authentication can be enabled.",
    "SSO_DISABLED" = "Azure SSO authentication can be disabled.",
    "AZURE_OPTIONS_REQUIRED" = "azure_options must include redirect, resource, tenant, app, version, password (error thrown if missing).",
    "AZURE_OPTIONS_NULL" = "azure_options can be NULL."
  ),

  # --- Preprocessing ---
  PREPROCESSING = list(
    "PREPROCESS_CHAR_TO_FACTOR" = "Character columns are automatically converted to factors.",
    "PREPROCESS_UNGROUP" = "Datasets are automatically ungrouped.",
    "PREPROCESS_TIBBLE_TO_DF" = "Tibbles are automatically converted to data.frames."
  ),

  # --- Subgroups ---
  SUBGROUPS = list(
    "SUBGROUP_CREATION" = "Subgroups can be created during app usage.",
    "SUBGROUP_BOOKMARKABLE" = "Subgroup definitions are included in bookmarks."
  )
)

#nolint end