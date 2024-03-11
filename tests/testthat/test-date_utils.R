component <- "get_range_date"

test_that(
  paste(
    component,
    "should return the earliest and latest date from a vector
    
    "
  ),
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
  paste(
    component,
    "should warn when the list contains a NULL entry
    
    "
  ),
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

component <- "add_date_range"

test_that(
  paste(
    component,
    "should add date_range attribute to a dataset based on the dates of the data tables
    
    "
  ),
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
