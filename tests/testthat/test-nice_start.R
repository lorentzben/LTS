test_that("test nice start - d1t0 ", {
  d1t0 <- read.csv("../data/one_day_zero_trans_r2.csv")

  bird_ids_d1t0 <- unique(d1t0$tagname)
  bird_ids_d1t0 <- na.trim(sort(bird_ids_d1t0))

  d1t0["DateTime"] <- as.POSIXct(d1t0$access, origin="1970-01-01", tz="UTC")

  print("what makes up subzone col")
  unique(d1t0$subzone)

  d1t0$subzone[d1t0$subzone == "Bottom"] <- "bottom"
  d1t0$subzone[d1t0$subzone == "Middle"] <- "middle"
  d1t0$subzone[d1t0$subzone == "Top"] <- "top"


  print("what makes up subzone col")
  unique(d1t0$subzone)

  print("how many NAs in DateTime and Subzone")
  sum(is.na(d1t0$DateTime))
  sum(is.na(d1t0$subzone))

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
    mutate(near_5 = map(slicedTsibble, ~ nice_start(.x, "5 seconds",5/60,datetime="datetime")))

  # TODO determine how to check equality
  expect_equal(lubridate::second(d1t0_regular$near_5[[1]][["datetime"]][1]),5)
})
