# Set several labels in a dataframe

Set several labels in a dataframe

## Usage

``` r
possibly_set_lbls(df, lbls)
```

## Arguments

- df:

  a dataframe.

- lbls:

  a named list. Each entry will have as name the name of a given column
  in df and as value the expected label of the given column. If df has
  no column with the name of the entry, the entry is ignored.

## Value

A dataframe with the set labels
