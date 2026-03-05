# Dispatcher resolver

Dispatcher resolver

## Usage

``` r
mm_resolve_dispatcher(dispatcher, afmm, flatten = FALSE)
```

## Arguments

- dispatcher:

  A list of class "mm_dispatcher" to be resolved. If the object is any
  of other class it is returned unmodified.

- afmm:

  a list of arguments from module manager

- flatten:

  for dispatcher where selection length is 1 and disppatcher is of class
  "mm_dispatcher" return the first element instead of a one element
  list. By default, it will try to flatten them. If selection is greater
  than 1 flatten must be FALSE, otherwise an error will be thrown.
