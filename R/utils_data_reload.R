# Data reloading functionality

check_data_reload <- function(reload_period) {
  if (!is.null(reload_period)) {
    last_restart_modification_time <- lubridate::as_datetime(file.info("restart.txt")[["mtime"]])
    if (last_restart_modification_time + reload_period < lubridate::as_datetime(Sys.time())) {
      system2(command = "touch", args = c("restart.txt"), stdout = TRUE)
      log_inform(
        paste0(
          "touched the restart.txt with a last modified time ",
          last_restart_modification_time,
          " and a specified interval of {reload_period}"
        )
      )
    }
  }
}

get_reload_period <- function(reload_period) {
  reload_period_time <- NULL
  if (lubridate::is.duration(reload_period)) {
    reload_period_time <- reload_period
  } else if (is.numeric(reload_period)) {
    reload_period_time <- lubridate::duration(reload_period, "days")
  }
  return(reload_period_time)
}
