# Robust getter for dataframe labels

It returns the label of the column of a dataframe if it is not NULL.
Otherwise it returns the name of the column.

## Usage

``` r
get_lbl_robust(df, var)
```

## Arguments

- df:

  a dataframe.

- var:

  a column of the dataframe

## Value

The label attribute or the name of the column if the label attribute is
NULL.
