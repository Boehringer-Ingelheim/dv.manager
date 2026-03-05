# A module that allows switching to another module

This simple module is used for demonstration purposes in documentation

## Usage

``` r
switch_UI(id, name)

switch_server(id, selected, switch_func)

mod_switch(name, selected, module_id)
```

## Arguments

- id:

  the shiny module id+

- name:

  A title that will be shown inside the module

- selected:

  The name of the tab that we want to switch to

- switch_func:

  a function that when passed the name of a tab switches the focus to it

- module_id:

  shiny module ID

## Functions

- `switch_UI()`: Module UI

- `switch_server()`: Module server
