# Return a range of dates

Calculates a range of dates from a list of POSIXct objects. If any of
the entries is NULL it will show a warning indicating which of the
entries was NULL. If any of the entries is not a date object it will
throw an error.

## Usage

``` r
get_date_range(x)
```

## Arguments

- x:

  A list of date objects

## Value

A character vector with two entries, the first and last date in the
input vector.
