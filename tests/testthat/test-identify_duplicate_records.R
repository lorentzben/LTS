test_that("test identify duplicate records with id_dupes-R3.csv - Explicit params", {
  require(zoo)
  require(lubridate)
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(tsibble)

  dupe <- read.csv("../data/id_dupes-R3.csv")

  dupe$tagname <- dupe$LegBand

  bird_ids_dupe <- unique(dupe$tagname)
  bird_ids_dupe <- na.trim(sort(bird_ids_dupe))

  dupe["DateTime"] <- as.POSIXct(dupe$access, origin="1970-01-01", tz="GMT")


  unique(dupe$subzone)

  dupe$subzone[dupe$subzone == "Bottom"] <- "bottom"
  dupe$subzone[dupe$subzone == "Middle"] <- "middle"
  dupe$subzone[dupe$subzone == "Top"] <- "top"


  unique(dupe$subzone)


  sum(is.na(dupe$DateTime))
  sum(is.na(dupe$subzone))

  # This is a hack to work with the downloaded data from excel and onedrive
  dupe$accessdate <- ymd_hms(dupe$DateTime)

  dupe_dupe_struct <- dupe |> nest(data = - tagname) |>
    na.exclude() |>
    mutate(id_dupes = map(data ,~identify_duplicate_records(.x,datetime="accessdate",value="subzone"))) |>
    mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
    mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

  dupe_struct <- dupe |>
    nest(data = - tagname) |>
    na.exclude() |>
    mutate(rmDupe = map(data, ~distinct(.x, accessdate, .keep_all=TRUE))) |>
    mutate(tsibble = map(rmDupe, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

  #these are probably not actually equal because of the ID dupes function
  expect_gt(length(dupe_struct$tsibble[[2]]$datetime), length(dupe_dupe_struct$tsibble[[2]]$datetime))

})

test_that("test identify duplicate records with id_dupes-R3.csv - Implied params", {
  require(zoo)
  require(lubridate)
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(tsibble)

  dupe <- read.csv("../data/id_dupes-R3.csv")

  dupe$tagname <- dupe$LegBand

  bird_ids_dupe <- unique(dupe$tagname)
  bird_ids_dupe <- na.trim(sort(bird_ids_dupe))

  dupe["DateTime"] <- as.POSIXct(dupe$access, origin="1970-01-01", tz="GMT")


  unique(dupe$subzone)

  dupe$subzone[dupe$subzone == "Bottom"] <- "bottom"
  dupe$subzone[dupe$subzone == "Middle"] <- "middle"
  dupe$subzone[dupe$subzone == "Top"] <- "top"


  unique(dupe$subzone)


  sum(is.na(dupe$DateTime))
  sum(is.na(dupe$subzone))

  # This is a hack to work with the downloaded data from excel and onedrive
  dupe$accessdate <- ymd_hms(dupe$DateTime)

  dupe_dupe_struct <- dupe |> nest(data = - tagname) |>
    na.exclude() |>
    mutate(id_dupes = map(data ,~identify_duplicate_records(.x))) |>
    mutate(cleaned = map(id_dupes, ~.x[! .x$duplicate == 1,])) |>
    mutate(tsibble = map(cleaned, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

  dupe_struct <- dupe |>
    nest(data = - tagname) |>
    na.exclude() |>
    mutate(rmDupe = map(data, ~distinct(.x, accessdate, .keep_all=TRUE))) |>
    mutate(tsibble = map(rmDupe, ~tsibble(datetime = ymd_hms(.x$accessdate), value = .x$subzone, index = datetime) ))

  #these are probably not actually equal because of the ID dupes function
  expect_gt(length(dupe_struct$tsibble[[2]]$datetime), length(dupe_dupe_struct$tsibble[[2]]$datetime))

})
