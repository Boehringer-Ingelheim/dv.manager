# A testing module that exports the whole afmm object

This module is used for testing purposes to access the full afmm object
via shinytest2's get_value(export = "module_id-afmm").

## Usage

``` r
mod_afmm_export(mod_id)
```

## Arguments

- mod_id:

  shiny module ID

## Value

A module list with ui, server, and module_id
