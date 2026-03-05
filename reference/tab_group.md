# Create a Tabbed Shiny Module Collection

`tab_group` is a function that groups dv modules in tabsets (see
[shiny::tabsetPanel](https://rdrr.io/pkg/shiny/man/tabsetPanel.html)).
This function is designed to simplify the process of organizing multiple
Shiny modules into a single, visually organized tabset. It allows nested
modules.

## Usage

``` r
tab_group(...)
```

## Arguments

- ...:

  A set of davinci modules or tab groups

## Value

The set of of modules wrapped in a list marked with an attribute.

## Details

The function does not make use of `namespace` (`NS()`) or
[`shiny::moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
to implement traditional Shiny modules, but creates a tab-like UI
structure, making it an aesthetic modification.

Outputs of grouped modules are accessible by using the module id, there
is no special change required.

When switching to grouped outputs the id of the module can be used
directly. This approach is incompatible with the `switch2` function and
must be used with `switch2mod`.
