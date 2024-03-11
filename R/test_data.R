test_data <- function() {
  set.seed(1)

  n_participants <- 20
  n_categories <- 4
  n_param_per_cat <- 5

  bm <- expand.grid(
    SUBJID = 1:n_participants,
    VISITN = 1:3,
    PARCATN = 1:n_categories,
    PARAMN = 1:n_param_per_cat
  )

  bm[["VISIT"]] <- paste0("VISIT", bm[["VISITN"]])
  bm[["VISIT2"]] <- bm[["VISITN"]] * bm[["VISITN"]]
  bm[["PARCAT"]] <- paste0("PARCAT", bm[["PARCATN"]])
  bm[["PARAM"]] <- paste0("PARAM", bm[["PARCATN"]], bm[["PARAMN"]])
  bm[["VALUE1"]] <- seq_len(nrow(bm))
  bm[["VALUE2"]] <- 10 + seq_len(nrow(bm))
  bm[["VALUE3"]] <- 100 + seq_len(nrow(bm))

  sl <- data.frame(
    SUBJID = 1:n_participants
  )
  sl[["CAT1"]] <- sample(c("Y", "N"), size = nrow(sl), replace = TRUE)
  sl[["CAT2"]] <- sample(c("A", "B", "C", "D"), size = nrow(sl), replace = TRUE)
  sl[["CAT3"]] <- sample(c("E", "F", "G", "H"), size = nrow(sl), replace = TRUE)
  sl[["CONT1"]] <- sample(1:100, size = nrow(sl), replace = TRUE)
  sl[["CONT2"]] <- sample(100:200, size = nrow(sl), replace = TRUE)
  sl[["CONT3"]] <- sample(200:300, size = nrow(sl), replace = TRUE)

  bm <- bm |>
    dplyr::mutate(
      SUBJID = factor(.data[["SUBJID"]]), # nolint
      PARCAT = factor(.data[["PARCAT"]]), # nolint
      PARAM = factor(.data[["PARAM"]]), # nolint
      VISIT = factor(.data[["VISIT"]]), # nolint
      VISIT2 = factor(.data[["VISIT2"]]) # nolint
    )

  # Duplicate for the rest of test cases, default with no default, default, bookmark, setup
  sl <- dplyr::mutate(
    sl,
    SUBJID = factor(.data[["SUBJID"]]) # nolint
  )

  for (col in names(bm)) {
    attr(bm[[col]], "label") <- paste("Label of", col)
  }

  for (col in names(sl)) {
    attr(sl[[col]], "label") <- paste("Label of", col)
  }

  list(bm = bm, sl = sl)
}
