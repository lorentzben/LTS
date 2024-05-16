test_that("nested_time_to_intervals works", {
  library(zoo)
  library(purrr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(tsibble)
  library(hms)

  d1t0 <- read.csv("../data/one_day_zero_trans_r2.csv")
  bird_ids_d1t0 <- unique(d1t0$tagname)
  bird_ids_d1t0 <- na.trim(sort(bird_ids_d1t0))

  d1t0["DateTime"] <- as.POSIXct(d1t0$access, origin="1970-01-01", tz="GMT")


  d1t0$subzone[d1t0$subzone == "Bottom"] <- "bottom"
  d1t0$subzone[d1t0$subzone == "Middle"] <- "middle"
  d1t0$subzone[d1t0$subzone == "Top"] <- "top"


  # This is a hack to work with the downloaded data from excel and onedrive
  d1t0$accessdate <- ymd_hms(d1t0$DateTime)

  # TODO change this block to remove the duplicate check
  d1t0_struct <- d1t0 |> nest(data = - tagname) |>
    na.exclude() |>
    mutate(tsibble = map(data, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

  d1t0_all_analysis <- d1t0_struct |>
    mutate(slicedTsibble = map(tsibble, ~ slice_tsibble(.x, "2021-02-19 T04:00:00", "2021-05-06 T22:00:00")))


  # TODO can we delete the sampled?
  d1t0_regular <- d1t0_all_analysis |>
    select(c(tagname, slicedTsibble)) |>
    mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60))) |>
    mutate(perSec = map(near_5, ~ fill_gaps(.x))) |>
    mutate(sampled = map(perSec, ~ na.locf(.x)))


  # first item to compare to day+night
  d1t0_overall_interval <- d1t0_regular |>
    mutate(interval = map(sampled, ~time_to_intervals(.x)))

  d1t0_all_room_time_budget <- d1t0_overall_interval |>
    mutate(tb = map(interval, ~ get_time_budget_prop(.x))) |>
    unnest(tb)

  d1t0_overall_tb <- d1t0_all_room_time_budget |>
    select("Interval.1.", "Interval.2.", "X1", "X2", "X3")

  Interval <- c(ymd_hms(as.POSIXct.numeric(as.numeric(head(d1t0_overall_interval$interval[[1]],n=1)$t1),origin=origin)),ymd_hms(as.POSIXct.numeric(as.numeric(tail(d1t0_overall_interval$interval[[1]],n=1)$t2),origin=origin)))


  # TODO change the code to be slicedTsibble as opposed to sampled
  d1t0_all_room_day <- d1t0_overall_interval |>
    mutate(day = map(slicedTsibble, ~ get_day_records(.x,"04:00:00","22:00:00")))


  d1t0_all_room_day <- d1t0_all_room_day |>
    mutate(day_int = map(day, ~ nested_time_to_intervals(.x)))

  # check 0 trans in day

  n_trans <- length(d1t0_all_room_day$day_int[[1]]$daily_int[[1]]$to_zone)-1

  expect_equal(n_trans, 0 , label='d1t0 expect 0 trans in day')
})
