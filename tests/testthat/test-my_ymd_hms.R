test_that("my_ymd_hms works for basic value", {
  expect_type(my_ymd_hms("2024-03-14T15:22:45"),"double")
})

test_that("my_ymd_hms works for midnight value", {
  test_dat <- read.csv("../data/interval_for_my_ymd_hms.csv")
  print(as.POSIXct.numeric(as.numeric(head(test_dat,n=1)$t1),origin="1970-01-01",tz = "UTC"))
  expect_type(my_ymd_hms(as.POSIXct.numeric(as.numeric(head(test_dat,n=1)$t1),origin="1970-01-01",tz="UTC")),"double")
})
