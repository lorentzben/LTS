#' Function to round the start so that records start nicely
#'
#' @param dataframe tsibble to be rounded
#' @param units from lubridate "a character string specifying a time unit or a multiple of a unit to be rounded to. Valid base units are second, minute, hour, day, week, month, bimonth, quarter, season, halfyear and year. Arbitrary unique English abbreviations as in the period() constructor are allowed. Rounding to multiples of units (except weeks) is supported."
#' @param interval_min sampling frequency in minutes (5/60 for 5 seconds)
#' @param datetime String that is the columnname where the ymd_hms formatted datetime
#'
#' @return a tsibble that was rounded
#' @export
#'
#' @examples
#' subzone <- c("Middle","Middle","Middle","Top")
#' datetime <- c("2021-03-21 T23:13:00","2021-03-21 T23:12:00", "2021-03-21 T23:11:00","2021-03-21 T23:10:00")
#' accessdate <- c("2021-03-21 T23:13:00","2021-03-21 T23:12:00", "2021-03-21 T23:11:00","2021-03-21 T23:10:00")
#' LegBand <- c(6992,6992,6992,6992)
#' record_table <- data.frame(cbind(subzone,datetime,accessdate,LegBand))
#' colnames(record_table) <- c('subzone','datetime','accessdate','LegBand')
#' record_table <- dplyr::tibble(record_table)
#' record_table$datetime <- lubridate::as_datetime(record_table$datetime)
#'
#' nice_start(record_table, "5 seconds", 5/60, datetime="datetime")
nice_start <- function(dataframe, units, interval_min, datetime="datetime"){


  dataframe[1,][[datetime]] <- lubridate::round_date(my_ymd_hms(dataframe[1,][[datetime]]), unit=units)

  # remove duplicate entries
  #dataframe <- dataframe[!duplicates(dataframe)]
  dataframe <- dataframe |>
    dplyr::distinct(datetime, .keep_all=TRUE)
  dataframe <- tsibble::tsibble(dataframe,index=datetime)

  attr(dataframe, 'interval') <- tsibble::new_interval(min=interval_min)
  return(dataframe)
}
