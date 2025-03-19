get_pharmaverse_data <- function(dataset) {
  if (!rlang::is_installed("pharmaverseadam")) stop("Please, install.package('pharmaverseadam')")

  res <- list(adsl = pharmaverseadam::adsl, adae = pharmaverseadam::adae, adlb = pharmaverseadam::adlb)[[dataset]]
  if (is.null(res)) stop("Unknown dataset")

  for (col in names(res)) {

    if (endsWith(col, "DTC")) {
      label <- attr(res[[col]], "label")
      res[[col]] <- as.Date(res[[col]])
      attr(res[[col]], "label") <- label
    } else if (is.character(res[[col]]) || endsWith(col, "CD")) {
      label <- attr(res[[col]], "label")
      res[[col]] <- factor(res[[col]])
      attr(res[[col]], "label") <- label
    }
    
  }

  return(res)
}
