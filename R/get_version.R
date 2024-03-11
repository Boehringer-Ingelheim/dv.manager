#' Convenience function to return information relevant about the application
#'
#' It returns the Version, Branch and Commit data of the application to be shown inside the application ui.
#'
#' @noRd

get_version <- function() {
  if (golem::app_prod()) {
    version <- utils::packageDescription("dv.manager", fields = "Version") # nolint
    branch <- utils::packageDescription("dv.manager", fields = "Branch") # nolint
    date <- utils::packageDescription("dv.manager", fields = "Date")
  } else {
    version <- get_desc_field("Version")
    branch <- get_desc_field("Branch")
    date <- get_desc_field("Date")
  }
  return(list(version = glue::glue("{branch}/{version}"), date = date))
}

get_desc_field <- function(field, path = ".") {
  con <- file("DESCRIPTION")
  on.exit(close(con))
  desc_content <- readLines(con)
  idx <- which(startsWith(desc_content, paste0(field, ":")))
  value <- unlist(strsplit(desc_content[idx],
    split = ": ", fixed = TRUE
  ))[2]
  return(value)
}
