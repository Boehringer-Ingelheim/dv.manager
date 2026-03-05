# Namespaces the CSS dependency of a `{shiny}` tag

By default the scope of all CSS styles is `global`. This means a certain
CSS style is applied across the HTML page. This presents some problems
when one module styles override another module styles.

## Usage

``` r
ns_css(
  module = NULL,
  disable_css_namespacing = isTRUE(getOption("dv.manager.disable_css_namespacing"))
)
```

## Arguments

- module:

  `{shiny}` module

- disable_css_namespacing:

  disables css namespacing and returns the input module unmodified

## Details

Namespacing CSS addresses this issue by creating special tags that
isolate the effect of CSS to only that namespace.

In some particular cases, css namespacing can create interactions
between modules. A disabling flag is included for those rare cases. This
flag can passed explicitly or it will take by default the value of
`options("dv.manager.disable_css_namespacing")`.

A detail that every module developer that uses custom css should
consider is that when defining the custom dependency via
`{htmltools::htmlDependency}` the name of the dependency should start
with `custom-`. For more examples pleasee see the vigniette.
