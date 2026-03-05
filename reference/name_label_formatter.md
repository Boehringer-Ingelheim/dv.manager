# Renames a list to include its value in the name

Renames a list to include its value in the name

## Usage

``` r
name_label_formatter(l)
```

## Arguments

- l:

  a named list

## Details

If value and name are equal the entry name is not modified. These lists
usually come from swap_names(rpl_nulls_name(get_lbl(dataset)))
