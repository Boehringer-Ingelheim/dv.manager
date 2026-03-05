# Functional dispatcher helper functions

Dispatch is a helper function that allows accessing data and utility
functions inside module manager in a dynamic way.

## Usage

``` r
mm_dispatch(from, selection = character(0))
```

## Arguments

- from:

  a string indicating what structure do we want to access inside module
  manager

- selection:

  an optional subsetting within the selected data structure

## Details

The parameter `from` can take the following values:

- unfiltered_dataset

- filtered_dataset

- url_parameters

- module_output

- utils
