# library(ggplot2)
# library(dplyr)

# # Reshape into one row per function

# overflow <- y[["period_idx"]] > 1e6
# max_idx <- ceiling(y[["period_idx"]] - 1) / 2
# incomplete_tags <- sum(y[["period_start"]][y[["period_idx"]] - 1]) != sum(!y[["period_start"]][y[["period_idx"]] - 1])

# df <- data.frame(
#   label = y[["period_label"]][y[["period_start"]][seq_len(max_idx)]],
#   start = y[["period_time"]][y[["period_start"]][seq_len(max_idx)]],
#   end = numeric(max_idx),
#   imputed = FALSE
# )

# start_vec <- y[["period_start"]][seq_len(y[["period_idx"]] - 1)]
# stack_depth <- integer(max_idx)
# st_stack <- integer(max_idx)
# st_idx <- integer(max_idx)
# et_idx <- integer(max_idx)

# st_et_ptr <- 0
# st_stack_ptr <- 0

# for (vec_idx in seq_along(start_vec)) {
#   cs <- start_vec[vec_idx]
#   if (cs) {
#     # push in the stack
#     st_stack_ptr <- st_stack_ptr + 1
#     st_stack[[st_stack_ptr]] <- vec_idx
#   } else {
#     st_et_ptr <- st_et_ptr + 1
#     st_idx[[st_et_ptr]] <- st_stack[[st_stack_ptr]]
#     stack_depth[[st_et_ptr]] <- st_stack_ptr
#     st_stack_ptr <- st_stack_ptr - 1
#     et_idx[[st_et_ptr]] <- vec_idx
#   }
# }

# origin <- as.numeric(min(y[["period_time"]][st_idx]))

# df <- data.frame(
#   label_st = y[["period_label"]][st_idx],
#   label_et = y[["period_label"]][et_idx],
#   st = as.numeric(y[["period_time"]][st_idx]) - origin,
#   et = as.numeric(y[["period_time"]][et_idx]) - origin,
#   depth = stack_depth,
#   imputed = FALSE
# )
# df <- df[order(df[["st"]]), , drop = FALSE]
# df[["duration"]] <- df[["et"]] - df[["st"]]

# # Plot
# ggplot2::ggplot(df, ggplot2::aes(y = depth, color = label_st)) +
#   ggplot2::geom_segment(aes(x = st, xend = et, yend = depth), linewidth = 1) +
#   ggplot2::geom_text(aes(x = st, label = label_st), hjust = -0.1, vjust = -1, size = 3, color = "black") +
#   ggplot2::labs(x = "Time", y = NULL, title = "Function execution periods") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank())
