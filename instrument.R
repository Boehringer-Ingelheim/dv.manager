##### TIMING

..t <- local({
  MAX_IDX <- 1e6
  DUMMY_TOKEN <- "DUMMY_TOKEN"

  # Copy and assign to ..t inside the package in file timing.R
  default_t <- local({
    list(
      init = function(...) {},
      dispose = function(...) {},
      get_cont = function(...) {},
      add_period = function(...) {},
      add_event = function(...) {},
      time_env_to_df = function(...) {}
    )
  })

  get_token <- function() {
    x <- shiny::getDefaultReactiveDomain()
    if (!is.null(x)) {
      return(x[["token"]])
    } else {
      return(DUMMY_TOKEN)
    }
  }

  time <- .POSIXct(numeric(MAX_IDX))
  label <- vector(mode = "character", length = MAX_IDX)
  session <- vector(mode = "character", length = MAX_IDX)
  start <- rep(FALSE, MAX_IDX)
  idx <- 0L

  add_period = function(label, start) {
    time_ <- Sys.time()
    idx <<- idx + 1
    time[[idx]] <<- time_
    label[[idx]] <<- label
    start[[idx]] <<- start
    session[idx] <<- get_token()
  }

  add_event = function(label) {
    time_ <- Sys.time()
    if (idx >= MAX_IDX) {
      message("Timings container is full. No more times will be recorded")
      return()
    } else {
      idx <<- idx + 1
      time[[idx]] <<- time_
      label[[idx]] <<- label
      start[[idx]] <<- TRUE
      session[idx] <<- get_token()
      idx <<- idx + 1
      time[[idx]] <<- time_
      label[[idx]] <<- label
      start[[idx]] <<- FALSE
      session[idx] <<- get_token()
    }
  }

  get_member = function(x) {
    members <- get_members()
    stopifnot(members %in% names(members))
    return(members(x))
  }

  get_members = function() {
    list(
      time = time,
      label = label,
      idx = idx,
      start = start,
      session = session
    )
  }

  time_list_to_df <- function(time_list) {
    overflow <- time_list[["idx"]] >= 1e6

    stopifnot(!overflow)

    max_idx <- ceiling(time_list[["idx"]]) / 2
    incomplete_tags <- sum(time_list[["period_start"]][time_list[["idx"]]]) !=
      sum(!time_list[["start"]][time_list[["idx"]]])

    start_vec <- time_list[["start"]][seq_len(time_list[["idx"]])]
    stack_depth <- integer(max_idx)
    st_stack <- integer(max_idx)
    st_idx <- integer(max_idx)
    et_idx <- integer(max_idx)

    st_et_ptr <- 0
    st_stack_ptr <- 0

    for (vec_idx in seq_along(start_vec)) {
      cs <- start_vec[vec_idx]
      if (cs) {
        # push in the stack
        st_stack_ptr <- st_stack_ptr + 1
        st_stack[[st_stack_ptr]] <- vec_idx
      } else {
        st_et_ptr <- st_et_ptr + 1
        st_idx[[st_et_ptr]] <- st_stack[[st_stack_ptr]]
        stack_depth[[st_et_ptr]] <- st_stack_ptr
        st_stack_ptr <- st_stack_ptr - 1
        et_idx[[st_et_ptr]] <- vec_idx
      }
    }

    df <- data.frame(
      label_st = time_list[["label"]][st_idx],
      label_et = time_list[["label"]][et_idx],
      session = time_list[["session"]][st_idx],
      st = as.numeric(time_list[["time"]][st_idx]),
      et = as.numeric(time_list[["time"]][et_idx]),
      depth = factor(stack_depth),
      imputed = FALSE
    )

    df <- df[order(df[["st"]]), , drop = FALSE]
    df[["duration"]] <- df[["et"]] - df[["st"]]

    # yyjsonr::write_json_file(df, "data.json")

    return(df)
  }

  instrument <- function(pkgs) {
    for (pkg in pkgs) {
      utils::assignInNamespace("..t", ..t, pkgs)
    }
  }

  deinstrument <- function(pkgs) {
    for (pkg in pkgs) {
      utils::assignInNamespace("..t", default_t, pkgs)
    }
  }

  list(
    add_period = add_period,
    add_event = add_event,
    instrument = instrument,
    deinstrument = deinstrument,
    get_members = get_members,
    time_list_to_df = time_list_to_df,
    DUMMY_TOKEN = DUMMY_TOKEN
  )
})

if (FALSE) {
  ..t$add_period("c", TRUE, "C")
  ..t$add_period("c", FALSE, "C")
}
