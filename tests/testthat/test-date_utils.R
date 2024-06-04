# nolint start

test_that(
  vdoc[["add_spec"]]("get_range_date should return the earliest and latest date from a vector", c(specs$modification_date_display)),
  {
    get_date_range(
      list(
        a = lubridate::ymd_hms("2021-01-13 00:00:00"),
        b = lubridate::ymd_hms("2021-01-14 00:00:00"),
        c = lubridate::ymd_hms("2021-01-16 00:00:00")
      )
    ) %>%
      expect_equal(
        c(
          lubridate::ymd_hms("2021-01-13 00:00:00"),
          lubridate::ymd_hms("2021-01-16 00:00:00")
        )
      )
  }
)

test_that(
  vdoc[["add_spec"]]("get_range_date should warn when the list contains a NULL entry", c(specs$data_table_meta_check)),
  {
    get_date_range(
      list(
        a = lubridate::ymd_hms("2021-01-13 00:00:00"),
        b = NULL,
        c = lubridate::ymd_hms("2021-01-16 00:00:00")
      )
    ) %>% expect_warning(
      regexp = "b has no date. no meta attribute or no mtime entry"
    )
  }
)

# Testing add_date_range ----

test_that(
  vdoc[["add_spec"]]("add_date_range should add date_range attribute to a dataset based on the dates of the data tables", c(specs$modification_date_display)),
  {
    domain_list <- setNames(rep(list(mtcars), 3), c("a", "b", "c")) %>%
      purrr::map2(
        list(
          a = lubridate::ymd_hms("2021-01-13 00:00:00"),
          b = lubridate::ymd_hms("2021-01-14 00:00:00"),
          c = lubridate::ymd_hms("2021-01-16 00:00:00")
        ),
        ~ {
          attr(.x, "meta") <- list(mtime = .y)
          .x
        }
      )

    expected <-
      expect_equal(
        attr(add_date_range(domain_list), "date_range"),
        c(lubridate::ymd_hms("2021-01-13 00:00:00"), lubridate::ymd_hms("2021-01-16 00:00:00"))
      )
  }
)

# nolint end
