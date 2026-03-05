# Run the Shiny Application

This function serves as an entry point for the applications created by
the user.

## Usage

``` r
run_app(
  data = NULL,
  module_list = list(),
  title = "Untitled",
  filter_data = NULL,
  filter_key = if (!is.null(data)) {
     "USUBJID"
 } else {
     NULL
 },
  startup_msg = NULL,
  reload_period = NULL,
  enableBookmarking = "server",
  filter_type = "simple",
  enable_dataset_filter = NULL,
  enable_subgroup = FALSE,
  filter_default_state = NULL,
  .launch = TRUE
)
```

## Arguments

- data:

  the list of datasets to be used in the Shiny application. This data
  parameter can be defined as:

  - a named list of lists in which each of the lists will be a list of
    data.frames. Each of the lists of data.frames contains a domain, a
    structure similar to that returned in a `dv.loader::load_data` call.

  - a named list of functions in which each of the functions will return
    a list of data.frames.

  All `character` variables will be automatically mapped into `factors`.

- module_list:

  a list of the modules to be included in the Shiny application

- title:

  title to be displayed in the browser tab

- filter_data:

  a string indicating which of the loaded datasets is used for
  filtering.

- filter_key:

  a string specifying a common field across all datasets that will be
  used to expand the filtering. Default = "USUBJID" or NULL if no data =
  NULL

- startup_msg:

  a message to be displayed at the start of the application. It can be
  either NULL or a modal message defined with shiny::modalDialog.

- reload_period:

  Either a lubridate object to specify a duration or a positive numeric
  value which is then interpreted as a lubridate duration object in
  days. By default NULL

- enableBookmarking:

  Can be one of `"url"`, `"server"`, or `"disable"`. The default value,
  `NULL`, will respect the setting from any previous calls to
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html).
  See
  [`enableBookmarking()`](https://rdrr.io/pkg/shiny/man/enableBookmarking.html)
  for more information on bookmarking your app.

- filter_type:

  **DEPRECATED** Indicates which filter type, `simple`, `datasets`,
  `development`, will be used in the application.

- enable_dataset_filter:

  **DEPRECATED** A boolean flag indicating if dataset filters are
  enabled. The default value is FALSE.

- enable_subgroup:

  A boolean flag indicating if subgroup controls are enabled. The
  default value is FALSE.

- filter_default_state:

  A JSON string or file (usually exported from the app) that describes
  the default state of the filter (Only available for `development`
  filters).

- .launch:

  by default it should always be TRUE. It should only be false for
  debugging and testing. When TRUE it will return the app. When FALSE it
  will return the options with which the app will be launched.
