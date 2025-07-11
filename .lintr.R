linters <- lintr::default_linters             # -[ diff with dv.templates 3ca8d7a10cfc7ad2307644dcac603e1f1f0feb72]-
linters <- lintr::modify_defaults(
  linters
  , line_length_linter = NULL                 # we see how long lines are when we write them
  , indentation_linter = NULL
  # , trailing_whitespace_linter = NULL
  , cyclocomp_linter = NULL                   # prevents trivial amount of nesting and long but straightforward functions
  , object_name_linter = NULL                 # we have reasons to capitalize. nobody in our team CamelCase. shiny does
  , object_length_linter = NULL               # we don't type long var names just because
  , pipe_continuation_linter = NULL           # wickham being overly prescriptive
  , trailing_blank_lines_linter = NULL        # natural extension of trailing_whitespace_linter, present on the template
)

if(identical(Sys.getenv('CI'), "true")){
  linters <- lintr::modify_defaults(
    linters
    , object_usage_linter = NULL              # R lacks var declarations; it's easy to assign to the wrong variable by mistake
  )                                           # We only disable this lint rule on github because it fails there because
}                                             # of a long-standing lintr bug

exclusions <- list("tests")