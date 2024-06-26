
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LTS

<!-- badges: start -->

[![R-CMD-check](https://github.com/lorentzben/LTS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lorentzben/LTS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of LTS is to package and extend functions useful for
time-series datasets.

## Installation

You can install the development version of LTS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lorentzben/LTS")
```

## Docker installation

``` bash
docker run -it -v .:/mnt -p 8888:8787 -e PASSWORD="$PASSWORD" lorentzb/rfid:2.0
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LTS)

de_duplicated_record_table <- identify_duplicate_records(record_table)
slice_tsibble(record_table, begin, end)
my_ymd_hms(date_1)
nice_start()
time_to_intervals()
get_time_budget_prop()
get_day_records()
get_night_records()
nested_time_to_intervals()
get_night_zone_from_TB()
```
