#' Setting up the validation

if (!exists("package_name")) stop("package name must be in the environment when this script is sourced")

#' How to link tests and specs

if (FALSE) {
  test_that(
    vdoc[["add_spec"]]("my test description", specs$a_spec),
    {
      expect_true(TRUE)
    }
  )
}
#' The specs variable on the call references the one declared in specs.R

#' 3. For those tests covering more than one spec.
#' NOTE: It must be c() and not list()
#'

if (FALSE) {
  test_that(
    vdoc[["add_spec"]]("my test_description", c(specs$my$hier$spec, vdoc_specs$my$hier$other_spec)),
    {
      expect_true(TRUE)
    }
  )
}

#' Considerations:
#' - parse_spec uses deparse(substitute()). These spec_ids are later used to check if all requirements
#'  are covered or not, therefore those calls cannot by substituted for:

if (FALSE) {
  my_spec <- specs$my$hier$spec
  test_that(vdoc[["add_spec"]]("my test_description", my_spec), {
    ...
  })
  
  test_that(vdoc[["add_spec"]]("my test_description", specs[["my"]][["hier"]][["spec"]]), {
    ...
  })
}

# In this case the substitute captures my_spec and cannot be used later.
# If you want to do this you must use the spec_id parameter where you pass a
# character vector with the ids.
# Notice that the ids in character form do no longer have the specs particle
# at the beginning, only the pathing of the spec is needed.

if (FALSE) {
  my_spec <- specs$my$hier$spec
  test_that(vdoc$parse_spec(my_spec, "my test_description", spec_id = c("my$hier$spec")), {
    ...
  })
}

# Validation code

local({
  specs <- source(
    system.file("validation", "specs.R", package = package_name, mustWork = TRUE),
    local = TRUE
  )[["value"]]
  recursive_ids <- function(x, parent = character(0)) {
    if (!is.list(x)) {
      return(parent)
    }
    unlist(mapply(recursive_ids,
      x,
      paste(parent, names(x),
        sep = if (identical(parent, character(0))) "" else "$"
      ),
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    ))
  }

  recursive_ids <- function(x, parent = character(0)) {
    if (!is.list(x)) {
      return(parent)
    }
    unlist(mapply(recursive_ids, x,
      paste(parent, names(x),
        sep = if (identical(parent, character(0))) "" else "$"
      ),
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    ))
  }


  spec_id_list <- recursive_ids(specs)

  list(
    specs = specs,
    spec_id_list = spec_id_list,
    add_spec = function(desc, spec, spec_id) {
      if (missing(spec_id)) {
        if (!is.character(spec) || length(spec) == 0) stop("spec must be a non-empty character vector")
        s_spec <- substitute(spec)
        if (s_spec[[1]] == "c") {
          spec_id <- sapply(s_spec[2:length(s_spec)], identity)
        } else {
          spec_id <- list(s_spec) # Otherwise the posterior vapply iterates over the expression
        }

        spec_id_chr <- vapply(spec_id, function(x) {
          sub("^[^$]*\\$", "", deparse(x))
        }, FUN.VALUE = character(1))

        if (!all(spec_id_chr %in% spec_id_list)) {
          stop("At least one spec is not declared in the spec list")
        } # This should be covered by pack of constants but just in case
      } else {
        spec_id_chr <- spec_id
      }
      structure(desc, spec_id = spec_id_chr, spec = spec)
    },
    get_spec = function(result) {
      lapply(
        result,
        function(x) {
          first_result <- try(
            x[[1]][["test"]],
            silent = TRUE
          )
          if (inherits(first_result, "try-error")) {
            list(spec_id = NULL, desc = NULL)
          } else {
            list(
              spec_id = attr(first_result, "spec_id", exact = TRUE),
              spec = attr(first_result, "spec", exact = TRUE)
            )
          }
        }
      )
    }
  )
})
