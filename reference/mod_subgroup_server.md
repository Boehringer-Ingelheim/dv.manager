# Subgroup Module Server

Server function for the subgroup module that allows users to create and
manage subject subgroups based on filter criteria. Subgroups are created
by assigning subjects to categories using filter conditions. Each
subject can belong to only one category within a subgroup, with
unassigned subjects placed in an "others" category.

## Usage

``` r
mod_subgroup_server(
  id,
  selected_dataset_list,
  subject_filter_dataset_name,
  filter_key_var
)
```

## Arguments

- id:

  Character string. The module namespace ID.

- selected_dataset_list:

  Reactive expression returning a named list of datasets. Must NOT
  contain any previously added subgroups. Recursively passing a
  dataset_list that already has subgroups added is not possible.

- subject_filter_dataset_name:

  Character string. Name of the dataset in `selected_dataset_list` that
  contains the subjects to be filtered and categorized.

- filter_key_var:

  Character string. Name of the variable used to join/filter subjects
  across datasets (typically a subject ID variable).

## Value

A reactive function that when called applies the created subgroups to a
dataset list. The returned function takes the same arguments as
`apply_subgroups()` and returns a list with components:

- `dataset_list` The input dataset list with subgroup variables added

- `errors` A list of error conditions encountered during application

## Details

The module manages the following key reactive values:

- `subgroups` A list of subgroup definitions, each containing category
  labels and filter specifications

- `cat_assignments` Current filter assignments for each category while
  building a subgroup

The module supports:

- Binary subgroups: One category defined by a filter, the other contains
  all remaining subjects

- Multi-category subgroups (3-10 categories): Multiple categories each
  defined by filters, plus an "others" category for unassigned subjects.
  Includes validation to prevent subjects from appearing in multiple
  categories

State persistence through bookmarking is supported for the subgroups
reactive value.
