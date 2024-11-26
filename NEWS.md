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
