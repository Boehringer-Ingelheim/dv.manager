get_pharmaverse_data <- function(dataset) {
  if (!rlang::is_installed("pharmaverseadam")) stop("Please, install.package('pharmaverseadam')")

  chr2fct <- function(x) {
    x[] <- purrr::map(x, ~ if (is.character(.x)) factor(.x) else .x)
    x
  }

  cd2fct <- function(x) {
    x[] <- purrr::imap(x, ~ if (endsWith(.y, "CD")) factor(.x) else .x)
    x
  }

  if (dataset == "adsl") {
    res <- pharmaverseadam::adsl |>
      chr2fct() |>
      cd2fct()
    return(res)
  }
  if (dataset == "adae") {
    res <- pharmaverseadam::adae |>
      chr2fct() |>
      cd2fct()
    return(res)
  }
  if (dataset == "adlb") {
    res <- pharmaverseadam::adlb |>
      chr2fct() |>
      cd2fct()
    return(res)
  }
  stop("Unknown dataset")
}
