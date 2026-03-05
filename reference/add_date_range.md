# Add a date_range attribute to a dataset

This function will add an attribute `date_range` to a dataset. The
source for this dates is the `mtime` (POSIXct) entry from the `meta`
attribute as provided by `dv.loader`. As different domains are contained
as a list in a single dataset, this attribute will contain the minimum
and the maximum modification date from all the domains in the dataset.

See
[`vignette("customizing_application")`](../articles/customizing_application.md)

## Usage

``` r
add_date_range(dataset)
```

## Arguments

- dataset:

  a dataset loaded with dv.loader

## Details

In case any of the entries in dataset does not have the expected
attribute, it will show a warning.
