# Replace NULL values in a list with the name of the entry

If one of the values is NULL it will be replaced with the name of the
NULL entry.

## Usage

``` r
rpl_nulls_name(l)
```

## Arguments

- l:

  a named list.

## Value

A named list.

## Details

The input of this functions is usually the output of a `get_lbls` call.
