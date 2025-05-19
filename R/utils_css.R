#' Namespaces the CSS dependency of a `{shiny}` tag
#'
#' By default the scope of all CSS styles is `global`. This means a certain CSS style
#' is applied across the HTML page. This presents some problems when one module styles
#' override another module styles.
#'
#' Namespacing CSS addresses this issue by creating special tags that isolate the effect
#' of CSS to only that namespace.
#'
#' @details
#' In some particular cases, css namespacing can create interactions between modules. A disabling flag
#' is included for those rare cases. This flag can passed explicitly or it will take by default the value
#' of `options("dv.manager.disable_css_namespacing")`.
#'
#' A detail that every module developer that uses custom css should consider is that when defining
#' the custom dependency via `{htmltools::htmlDependency}` the name of the dependency should start with
#' `custom-`. For more examples pleasee see the vigniette.
#'
#' @param module `{shiny}` module
#' @param disable_css_namespacing disables css namespacing and returns the input module unmodified
#' @keywords internal
ns_css <- function(module = NULL,
                   disable_css_namespacing = isTRUE(getOption("dv.manager.disable_css_namespacing"))) {
  if (disable_css_namespacing) {
    warning("Skipping css namespacing")
    return(module)
  }

  deps <-
    htmltools::resolveDependencies(htmltools::findDependencies(module), resolvePackageDir = TRUE)

  deps <- lapply(deps, function(x) {
    if (startsWith(x$name, "custom-")) {
      x
    }
  })

  deps <- Filter(Negate(is.null), deps)

  if (length(deps) > 0) {
    paths <-
      unlist(purrr::map(htmltools::findDependencies(deps), function(dep) {
        if (!is.null(dep$stylesheet)) {
          if (is.null(dep$package)) {
            if (is.null(dep$src$file)) {
              base_url <- strsplit(dep$src$href, "/")[[1]][1]
              if (is.null(dep$package) && base_url == "shared") {
                # skip shared shiny dependencies
                NULL
              }
            } else {
              paste0(
                ifelse(is.null(dep$src$file), dep$src$href, dep$src$file),
                "/",
                dep$stylesheet
              )
            }
          } else {
            system.file(dep$src$file, dep$stylesheet, package = dep$package)
          }
        }
      }))

    list(
      purrr::map(deps, function(dep) {
        if (!is.null(dep$stylesheet)) {
          if (is.null(dep$script)) {
            htmltools::suppressDependencies(dep$name)
          } else {
            dep$stylesheet <- NULL
            dep
          }
        }
      }),
      shiny::div(
        shiny::tags$style(
          `scoped` = "",
          purrr::map(paths, function(path) {
            gsub("!important", "", shiny::includeText(path))
          })
        ),
        list(module)
      )
    )
  } else {
    module
  }
}

# Helper function for theme management
get_app_theme <- function(custom = FALSE, version = 3) {
  if (!isTRUE(getOption("dv_manager_ignore_css"))) {
    if (isFALSE(custom)) {
      theme <- bslib::bs_theme(version = version) %>%
        bslib::bs_add_variables("brand-primary" = "#08312A") %>%
        bslib::bs_add_rules(sass::sass_file(app_sys("www/css/custom.scss")))
    } else {
      theme <- bslib::bs_theme(version = version) %>%
        sass::sass_bundle(sass::sass_layer(
          defaults = list(sass::sass_file(app_sys("www/themes/_variables.scss"))),
          rules = list(
            sass::sass_file(app_sys("www/themes/_dark.scss")),
            sass::sass_file(app_sys("www/css/custom.scss"))
          )
        )) %>%
        structure(class = class(bslib::bs_theme(version = version)))
    }
  } else {
    theme <- NULL
  }
  theme
}
