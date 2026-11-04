# validation (S)
vdoc <- local({
  #                      ##########
  # package_name is used # INSIDE # the sourced file below
  #                      ##########
  package_name <- "dv.manager"

  source(
    system.file("validation", "utils-validation.R", package = package_name, mustWork = TRUE),
    local = TRUE
  )[["value"]]
})
specs <- vdoc[["specs"]]
#  validation (F)

if (
  !isTRUE(as.logical(Sys.getenv("CI"))) &&
    !isTRUE(as.logical(Sys.getenv("LOCAL_SHINY_TESTS")))
) {
  warning("Attempting to run local tests without 'LOCAL_SHINY_TESTS' option")
}

# ODG PDF tests skip without xelatex. The CI image should already ship it; this is a fallback for when it does not.
if (
  isTRUE(as.logical(Sys.getenv("CI"))) &&
    isTRUE(as.logical(Sys.getenv("GITHUB_ACTIONS"))) &&
    !nzchar(Sys.which("xelatex"))
) {
  message("xelatex not found on CI, installing TinyTeX")
  tryCatch(
    {
      # force: a partial TeX Live (e.g. pdftex without xetex) on the image would otherwise abort the install
      tinytex_dir <- tinytex::install_tinytex(force = TRUE)
      # The symlinks install_tinytex() creates (~/bin or ~/.local/bin) may not be on PATH, use its bin dir directly.
      # Child processes (callr ODG render, shinytest2 apps) inherit this PATH.
      tinytex_bin <- list.dirs(file.path(tinytex_dir, "bin"), recursive = FALSE)
      Sys.setenv(PATH = paste(c(tinytex_bin, Sys.getenv("PATH")), collapse = .Platform$path.sep))
      message("Prepended TinyTeX bin dir to PATH: ", paste(tinytex_bin, collapse = ", "))
      if (!nzchar(Sys.which("xelatex"))) stop("xelatex not on PATH after installing TinyTeX")
      message("xelatex installed: ", Sys.which("xelatex"))
    },
    error = function(e) warning("Could not install xelatex, ODG PDF tests will be skipped: ", conditionMessage(e))
  )
}

run_shiny_tests <- !isTRUE(as.logical(Sys.getenv("SKIP_SHINY_TESTS")))

skip_if_not_running_shiny_tests <- function() testthat::skip_if_not(run_shiny_tests, message = "Skip tests") # nolint

# `expr` must be a quosure or a regular call, in both cases they must be self-contained as they will be deparsed
# and run in another process
start_app_driver <- function(expr, defer = TRUE) {
  root_app <- if (run_shiny_tests) {
    app_dir <- if (testthat::is_testing()) {
      "app/app.R"
    } else {
      "tests/testthat/app/app.R"
    }

    call <- if (rlang::is_quosure(expr) || rlang::is_expression(expr)) expr else substitute(expr)

    # tryCatch to avoid snapshots being deleted when the app cannot be started
    tryCatch(
      {
        temp <- tempfile()
        saveRDS(expr, temp)

        app <- shinytest2::AppDriver$new(
          app_dir = app_dir,
          seed = 1,
          options = list(
            "__quo_file" = temp,
            "__use_load_all" = isTRUE(as.logical(Sys.getenv("LOCAL_SHINY_TESTS")))
          )
        )
        app$wait_for_idle()
        if (defer) {
          withr::defer_parent(app$stop())
        }
        app
      },
      condition = function(e) {
        if (exists("app") && "stop" %in% names(app)) {
          app$stop()
        }
        print(e)
        NULL
      }
    )
  } else {
    NULL
  }
  root_app
}

..switch_to_module <- function(tab_id, app) {
  app$run_js(sprintf("dv_tab.set_tab_by_tab_id('%s', '%s')", tab_id, ID$NAV_HEADER))
}
