use_load_all <- getOption("__use_load_all")
message("loading all pre")
if (use_load_all) {
  message("loading all")
  pkg_path <- "."
  prev_path <- ""
  while (!length(list.files(pkg_path, pattern = "^DESCRIPTION$")) == 1) {
    if (normalizePath(pkg_path) == prev_path) rlang::abort("root folder reached and no DESCRIPTION file found")
    prev_path <- normalizePath(pkg_path)
    pkg_path <- file.path("..", pkg_path)
  }
  devtools::load_all(pkg_path, quiet = TRUE)
}

fn_expr <- getOption("__test_fn_expr")
eval(parse(text = fn_expr))
