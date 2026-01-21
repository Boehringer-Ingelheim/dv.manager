# dv.manager 2.2.0-9007
- Improves subgroup creation and includes tests

# dv.manager 2.2.0-9006
- Prevents running application with Shiny version 1.11.0 which container several bugs

# dv.manager 2.2.0-9005
- Filter: Includes fast routine for binary serialization for filter_data
- Subgroup: Includes initial POC for subgroup creation in app runtime


# dv.manager 2.2.0-9004
- Filter: Fixes out of range blockly color

# dv.manager 2.2.0-9003
- Filter: Forcefully casts variables with type numeric to numeric

# dv.manager 2.2.0-9002
- Factor levels are dropped when filtered

# dv.manager 2.2.0-9001
- Development filter:
    - Revamps the UI for the filter in development
    - Includes filter clearing, saving and exporting
    - Includes an alternative filter that mimicks the previous filter with extra features
    - Updates documentation
- Defuncts dispatchers
- Upgrades the bootstrap version to 5.0.0

# dv.manager 2.2.0

- Blockly filter:
    - Expands operations in blockly filter under development
    - Improve the documentation for the blockly filter in development
    - Removes the `run_app_dev_filter` function and includes a new arugment `filter_type` in `run_app` to set which filter should be used
    in each of the applications. `enable_dataset_filter` is deprecated and replaced by `filter_type = 'datasets'` argument in
    the `run_app` call.
    
- Refactors UI
    - Top menus are no longer shiny tabsets and all is managed directly by css and javascript
    - Module UIs are no longer nested as required by shiny tabsets and all are child of the same container divisor.
    - All module containers have an absolute height.
    - Filter menus height is fixed, to avoid sizing errors depending on the sidebar vertical localtion of the filter menus.

# dv.manager 2.1.11

- Fails when modules try to access a non-existent module output

# dv.manager 2.1.10

- Includes a new blockly filter in development mode
- Updates old documentation
- De-exports several util functions
- `dv.manager` automatically maps `character` variables into `factors`
- Fixes tab group menu rendering

# dv.manager 2.1.5

- dv.manager dataset filters are now deactivated by default and can be activated by setting `enable_dataset_filter` parameter in `run_app`.
- dv.manager filter hide/shows filters depending on the selected module.
- Empty datasets can be included in the application again.
- Fixed a bug that removed labels from column datasets when data was filtered.
- Module names can no longer be an empty string `''`.

# dv.manager 2.1.4

- Removes leftover title automatic capitalization.

- Extends Arguments From Module Manager to include include a `data` slot that acts as a pass-through of the `run_app` argument by the same name.

# dv.manager 2.1.3

- dv.manager includes the option to filter using several datasets
- dv.manager includes `tab_group` that allows grouping of modules in TabSets.

# dv.manager 2.1.2

- Tab titles are no longer capitalized

- Removes `golem` dependency

# dv.manager 2.1.1

- General code housekeeping

- Move all tests from `shinytest` to `shinytest2`

# dv.manager 2.1.0

- `dispatchers` now include a flatten argument. When selection length is 1 it flattens the list if TRUE. If selection length is greater than 1 flatten must be FALSE.
- Passing expressions and using session$userData$manager_utils$switch_tab are now defunct
- It no longer hardcodes the `enableBookmarking` as `"url"`. The argument passed to the `enableBookmarking` parameter is forwarded to shiny runApp. Therefore, bookmarking is delegated to `Shiny` and no special implementacion happens inside dv.manager.
    - When `"server"` bookmarking is selected and the app is running in shiny-server we refer the user to: https://docs.rstudio.com/shiny-server/#bookmark_state_dir forbookmark storage configuration.
    - For local or other approaches: https://community.rstudio.com/t/changing-bookmark-state-directory-when-saving-to-disk/37877 (Undocumented)
- It now includes a simple logging system described in: [here](https://boehringer-ingelheim.github.io/dv.manager/articles/logging.html)
- The arguments from module manager will now include an entry `dataset_metadata` entry that will contain the name and date of the selected dataset.
    - `dataset_name` entry will be deprecated in following versions.
- Allows periodical reloading of the datasets via the `reload_period` parameter.

# dv.manager 2.0.0

## Major changes
Instantiating modules is no longer done by means of an rlang expression but with a function with a single argument.
Documentation about this can be found [here](https://boehringer-ingelheim.github.io/dv.manager/articles/developing_module.html) and
[here](https://boehringer-ingelheim.github.io/dv.manager/articles/arguments_from_module_manager.html) This argument contains a list with:

-   `unfiltered_dataset`: a metaReactive list containing the tables inside the selected dataset before filtering them.

-   `filtered_dataset`: a metaReactive list containing the tables inside the selected dataset after filtering them.

-   `url_parameters`: a reactive list of the parameters passed in the url.

-   `dataset_name`: a reactive string containing the name of the selected dataset.

-   `module_output`: a function that when called returns a list of all the values returned by the different modules indexed by the module_id.

-   `module_names`: a non-reactive named list containing as values the names of the module entries as displayed on the tab and as names the ids of the module entries as used by Shiny.

-   `utils`: a list of convenience functions:
    - `switch2`: a function that allows switching between tabs programatically

- A set of dispatcher functions that facilitate the access are included. Documentation can be found
[here](https://boehringer-ingelheim.github.io/dv.manager/articles/arguments_from_module_manager.html#dispatchers-1).

dv.manager isolates custom CSS styling for loaded modules. This ensures CSS styles are applied where they are intended.

dv.manager allows including an startup message

dv.manager now includes the possibility of SSO using Azure Active Directory

## Minor Changes

-   Data selector disappears when no, or one, dataset is passed into the app.

-   The format of the date presented in the application is now "2021-Jan-01 (UTC)".

-   The size of the package has been reduced to avoid check notes.

# dv.manager 1.0.0

-   modulemanager packages is now **dv.manager**
-   This is is the first productive release of module manager
-   Easily generates a Shiny app from simple code
-   Offers a UI container for Shiny modules developed to be compatible with it
-   Acts as a data dispatcher that transparently allows dataset switching and filtering
-   Allows **bookmarking** of the state of the generated Shiny app (included modules must be compatible with this feature)
