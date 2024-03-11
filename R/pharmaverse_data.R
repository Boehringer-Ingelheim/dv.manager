get_pharmaverse_data <- function(dataset) {
  if (!rlang::is_installed("pharmaverseadam")) stop("Please, install.package('pharmaverseadam')")
  if (dataset == "adsl") {
    return(pharmaverseadam::adsl)
  }
  if (dataset == "adae") {
    return(pharmaverseadam::adae)
  }
  stop("Unknown dataset")
}
