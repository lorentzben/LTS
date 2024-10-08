test_that("test d1t0 has a length of 1 (0 trans)", {
  library(zoo)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(tsibble)

  d1t0 <- read.csv("../data/one_day_zero_trans_r2.csv")

  bird_ids_d1t0 <- unique(d1t0$tagname)
  bird_ids_d1t0 <- na.trim(sort(bird_ids_d1t0))

  d1t0["DateTime"] <- as.POSIXct(d1t0$access, origin="1970-01-01", tz="UTC")


  d1t0$subzone[d1t0$subzone == "Bottom"] <- "bottom"
  d1t0$subzone[d1t0$subzone == "Middle"] <- "middle"
  d1t0$subzone[d1t0$subzone == "Top"] <- "top"


  # This is a hack to work with the downloaded data from excel and onedrive
  d1t0$accessdate <- lubridate::ymd_hms(d1t0$DateTime)

  # TODO change this block to remove the duplicate check
  d1t0_struct <- d1t0 |> nest(data = - tagname) |>
    na.exclude() |>
    mutate(tsibble = map(data, ~tsibble::tsibble(datetime = lubridate::ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

  d1t0_all_analysis <- d1t0_struct |>
    mutate(slicedTsibble = map(tsibble, ~ LTS::slice_tsibble(.x, "2021-02-19 T04:00:00", "2021-05-06 T22:00:00")))


  # TODO can we delete the sampled?
  d1t0_regular <- d1t0_all_analysis |>
    select(c(tagname, slicedTsibble)) |>
    mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
    mutate(perSec = map(near_5, ~ fill_gaps(.x))) |>
    mutate(sampled = map(perSec, ~ na.locf(.x)))

  d1t0_overall_interval <- d1t0_regular |>
    mutate(interval = map(sampled, ~LTS::time_to_intervals(.x)))

  # check that time budget says that it spent 100% in bottom
  expect_equal(length(d1t0_overall_interval$interval[[1]]$t1), 1, label='d1t0 has a length of 1 (0 trans)')

})
