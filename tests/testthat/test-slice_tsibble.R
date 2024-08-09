test_that("check slice_tsibble - d1t0", {
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
    mutate(slicedTsibble = map(tsibble, ~ slice_tsibble(.x,  "2021-03-21 T12:43:00","2021-03-21 T19:56:00")))

  expect_equal(utils::head(d1t0_all_analysis$slicedTsibble[[1]]$datetime,n=1),LTS::my_ymd_hms("2021-03-21 T12:43:00"))
  expect_equal(utils::tail(d1t0_all_analysis$slicedTsibble[[1]]$datetime,n=1),LTS::my_ymd_hms("2021-03-21 T19:56:00"))
})

test_that("check slice_tsibble for flipped start and stop", {
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

  expect_error(d1t0_all_analysis <- d1t0_struct |>
    mutate(slicedTsibble = map(tsibble, ~ slice_tsibble(.x,  "2021-03-21 T19:56:00","2021-03-21 T12:43:00"))))


})

test_that("check slice_tsibble for cutoffs outside table", {
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

  expect_warning(d1t0_all_analysis <- d1t0_struct |>
    mutate(slicedTsibble = map(tsibble, ~ slice_tsibble(.x,  "2021-03-20 T12:43:00","2021-03-23 T19:56:00"))))
})

test_that("check slice_tsibble - d1t0 explicit params", {
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
    mutate(slicedTsibble = map(tsibble, ~ slice_tsibble(.x,  "2021-03-21 T12:43:00","2021-03-21 T19:56:00",datetime="datetime")))

  expect_equal(utils::head(d1t0_all_analysis$slicedTsibble[[1]]$datetime,n=1),LTS::my_ymd_hms("2021-03-21T12:43:00"))
  expect_equal(utils::tail(d1t0_all_analysis$slicedTsibble[[1]]$datetime,n=1),LTS::my_ymd_hms("2021-03-21T19:56:00"))
})


