# Use a list to declare the specs
#nolint start

specs_list <- list

fs_spec <- specs_list(
  "display_modules" = "dv.manager receives a list of modules and displays it in a application.",
  "sidebar_menu_display" = "A sidebar menu will displayed (GUI).",
  "top_navigation_bar_module_list" = "The list of the included modules will appear in a top navigation bar (GUI) in the same order as specified.",
  "module_content_display" = "The content of each module will be presented when selected.",
  "custom_title_display" = "dv.manager will show a custom title that appear in the browser's title bar or in the page's tab",
  "custom_startup_message" = "dv.manager will allow showing a custom startup message",
  "dataset_list_availability" = "dv.manager receives a list of datasets and make it available for the modules in the application.",
  "dataset_label_display" = "dv.manager allows datasets to be labeled and displays lables in the application.",
  "dataset_switching" = "Datasets can be switched during the application execution.",
  "dataset_selection_sidebar_menu" = "The sidebar menu will allow for selecting among the different datasets passed as parameters to modulemanager (GUI).",
  "single_dataset_display" = "Only one dataset is displayed in the application at a given time, from now on active dataset.",
  "modification_dates_display" = "The earliest and latest modification dates of all the data tables are displayed for the active dataset",
  "date_unavailability_message" = "If no date is available for any of the data tables in the loaded dataset the system displays a 'Date unavailable' message",
  "data_reloading" = "dv.manager allows the reloading of the data after a specific amount of time.",
  "filtering_menu_display" = "The sidebar menu will display a filtering menu/s using the datafilter module (GUI).",
  "active_dataset_filtering" = "The active dataset can be filtered.",
  "global_filtering" = "A global filter will be available in dv.manager. One table of the dataset will be used to filter all other tables in the dataset and itself by using a common field among them.",
  "single_filtering" = "All data tables, with exception of the one used in the global filter, will have its own independent filter that only affects itself. These filters will not influence other datasets.",  
  "bookmarking_features" = "Bookmarking will include:
- the identity of the loaded dataset
- the set of filters applied to the loaded dataset
- the inner state of all modules that support bookmarking included in the app
- which module is active",
  "bookmarking_button_display" = "dv.manager will display a bookmarking button",
  "unfiltered_dataset_access" = "Modules will have access to the unfiltered dataset.",
  "filtered_dataset_access" = "Modules will have access to the filtered dataset",
  "other_module_output_access" = "Modules will have access to the output of other modules",
  "selected_dataset_name_access" = "Modules will have access to the name of the selected dataset",
  "module_name_access" = "Modules will have access to its name and the name of the other modules",
  "modification_dates_access" = "Modules will have access to the earliest and latest modification dates of all the data tables.",
  "module_tab_switching" = "dv.manager allows programatically switching from one module tab to another",  
  "SSO_authentication_option" = "Modulemanager provides the option to enable the authentication of App Users with SSO to access the app."
)

sds_spec <- specs_list(
  "primary_interface_run_app" = "Primary interface:
The primary interface for modulemanager is based on its 'run_app()'' function that returns an app object.",
  "module_list_structure" = "- module_list: A named list.
    -  Each of its entries will contain a list with three entries named:
        - ['ui']: contains a function with a single parameter that will be the module_id
        - ['server']: a function with one argument, that will internally call to the corresponding server function.
        - ['module_id']: a string that will serve as the module_id.
    - The name of of each entry will act as the name of the module displayed in the UI.",
  "data_list_structure" = "- data: a named list of datasets to be used in the R/Shiny application.
    -   Each entry can be:
        -  A list of data tables.
        -  A list of functions that return a list of data tables.
    - All datasets must contain the same data tables.
    - The name of of each entry will act as the name of the dataset displayed in the UI.
    - The list can be empty
    - The list cannot be NULL",
  "app_title" = "- title: the title of the app that will be displayed in the window name/tab in the browser. Default: 'Untitled'",
  "filter_data" = "- filter_data: a string indicating which of all the data tables available in each dataset will be used for filtering",
  "filter_key" = "- filter_key: a string specifying a common field across all datasets that will be used to apply the filtering to all data tables. Default = 'USUBJID'",
  "startup_message" = "- startup_message:  a message to be displayed at the start of the application. It can be either NULL or a modal message defined with shiny::modalDialog",
  "azure_options" = "-azure_options: a list with the necessary information for an Azure SSO login. Required entries are redirect, resource, tenant, app, version, password. As defined in the package AzureAuth functions get_azure_token and build_authorization_uri. Or NULL for no login.",
  "data_reload" = "- data_reload: Either a lubridate object to specify a duration
or a positive numeric value which is then interpreted as a lubridate duration object in days. By default NULL",
  "filter_key_check" = "- filter_key must be a field in all data tables in all datasets. Otherwise the application throws an error. If data is empty checking is skipped.",
  "filter_data_check" = "- filter_data must be a data table in all datasets. Otherwise the application throws an error. If data is empty checking is skipped.",
  "data_table_meta_check" = "- All data tables in all datasets must have an attribute meta which contains a list with an entry mtime indicating the last modification time. This mtime must contain a POSIXct object. Otherwise the application:
    -  shows a warning in the application log if is NULL
    - throws an error if it not POSIXct or NULL
",
  "data_structure_check" = "- data is a list of lists of dataframes, or a list of functions. Otherwise throw an informative error.
- data is not NULL. Otherwise it throws an informative error.",
  "module_list_check" = "
- module_list is not empty. Otherwise it throws an informative warning.
- ids of the modules in module_list are not repeated. Otherwise it throws an informative error.",
  "startup_message_check" = "- startup_message is null or a shiny::modalDialog. Otherwise it throws an informative error.",
  "azure_options_check" = "-azure_options: must be a list with all the required fields or NULL. Otherwise an error is thrown.",
  "filtering_menus" = "- A filtering menu that is an/several instances of datafilter",
  "dataset_selector" = "- A dataset selector that:
    - contains one entry per entry in the data parameters list
    - when changed will load the selected dataset in the application
    - When one or none dataset are loaded this selector will not be displayed",
  "tab_selector" = "- a tab selector with one entry per entry in the module_list parameter",
  "bookmark_button" = "- A bookmark button that starts the bookmarking process",
  "modification_date_display" = "- The date of modification of the selected dataset as specified by the mtime entry in the meta attribute of the data tables:
    - The format for the dates is 'Year-Month-Day (UTC)' similar to '2022-Jan-14 (UTC)'
    - If all data tables have the same modification time only one data is presented
    - If not all data table have the same modification time, earliest and latest datas are presented, separated by a hyphen
    - If no date is available for any of the data tables in the loaded dataset the system    displays a 'Date unavailable' message
    - If no date is available for any of the data tables in any of the datasets, active or not,  an informative log will be provided for the correction of the offending data tables",
  "selected_dataset_name" = "- The name of the selected dataset",
  "css_namespacing" = "Module manager by default adds a namespace to the css rules that can be included by the modules. This namespacing can be deactivated by using the option `options('dv.manager.disable_css_namespacing')`",
  "module_output" = "- module_output is a named list. Each entry of this list contains the output of a module and the name of each entry is the module_id of each module. The nature of the returned values is specified by each module",
  "dispatchers" = "- dispatchers: A dispatcher function that simplifies the acces to datasets from the module invocation in the module list.",
  "SSO_login_option" = "module manager offers the option of providing an SSO login. For this the app uses the AzureAuth package.",
  "AzureAuth_integration" = "module manager just passes the information to AzureAuth therefore no development testing is done at this level, as it requires an Azure AD in place and it is not available at build time in Jenkins.",
  "data_reloading" = "Module Manager allows reloading the data after a given amount of time. The data_reload parameter will be specified by the App Creator."
)

tab_group <- list(
  "group_modules" = "module allows grouping a set of modules under a single tab in module manager",
  "allows_nesting" = "module allows nesting",
  "output_accesible" = "module output can be accesed by other modules in module.manager",
  "allows_switching" = "module can switch to/from other modules"
)

specs <- c(
  fs_spec,
  sds_spec
)

#nolint end