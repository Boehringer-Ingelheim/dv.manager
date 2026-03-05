# Adding custom CSS styles to a module

## Adding custom CSS styles to a module

There are multiple ways of adding custom CSS styles to a Shiny module:

1.  Inline styles

&nbsp;

1.  insert the style at the tag level, through the style attribute

``` r

p(style = "color:red;", "Red text")
```

2.  insert the style at the `head` level, through the style tag

``` r

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-input-container {
        color: #474747;
      }"))
  )
)
```

2.  As an external css file

&nbsp;

1.  Insert the style in the `head` of the HTML page

``` r

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_file.css")
  )
)
```

2.  Insert the style via `includeCSS()` function call

``` r

ui <- fluidPage(
  includeCSS("www/custom_file.css")
)
```

These are **general** options and can be used in all Shiny applications.
See [Using custom CSS in your
app](https://shiny.rstudio.com/articles/css.html) and [Style your apps
with CSS](https://shiny.posit.co/r/articles/build/css/) for more
examples and in-depth explanations.

However, for the purposes of working with **dv.manager** we recommend
using a 3rd alternative in order to avoid `CSS leaking` (your specific
CSS style being applied to other parts/modules of a larger Shiny
application unintentionally).

### Add custom CSS reference as an HTML dependency

1.  Define your CSS styles in a separate file and place it inside the
    `www` folder of your package

2.  Inside the UI of your Shiny module, define an HTML dependency
    pointing to that file

``` r

ui <- function(id) {
  dependency <- htmltools::htmlDependency(
    name = "custom-package_name_dep", # use a name that includes your package name,
    # so it will be easier to debug later
    version = 1, # for our purposes any value will suffice
    src = "/path/to/custom_css_file/",
    stylesheet = "custom.css",
    package = "package_name" # needed when the src parameter is relative
  )
  dependency
}
```

For more options, please check
[`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html)
documentation page.

3.  Attach the dependency variable to the UI structure. For example:

``` r

ui <- function(id) {
  dependency <- htmltools::htmlDependency(
    # ...
  )

  shiny::tagList(
    # ...,
    dependency
  )
}
```

For other options of attaching an HTML dependency to a UI element,
please consult
[`htmltools::attachDependencies`](https://rstudio.github.io/htmltools/reference/htmlDependencies.html)
documentation.

### CSS namespacing in module manager

Advantages:

- Custom CSS module dependencies impact only that module.
- The `dv.manager` can still change the styles of all other modules if
  desired (for example with a custom Shiny theme)
- However the modules cannot impact the `dv.manager` CSS styles even
  with the use of `!important` keyword

Limitation:

- This approach only prevents **unintentional** CSS style overflow. It
  cannot work against JavaScript libraries (like the ones contained in
  styling packages: `shinydashboard`, `flexdashboard` and similar) which
  **actively** and **purposefully** change and overwrite class names and
  CSS styles at runtime.
- Dependency name must begin with the prefix **“custom-”** otherwise it
  will not be namespaced.
