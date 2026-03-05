# Extract constant from pack

Extract constant from pack

## Usage

``` r
# S3 method for class 'pack_of_constants'
pack$name
```

## Arguments

- pack:

  pack_of_constants

- name:

  target constant

  This function differs from the base list extraction method in that it
  avoids partial matching of keys and throws an error if the looked-for
  constant is not contained within the pack.
