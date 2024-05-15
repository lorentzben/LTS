#' get_night_records
#'
#' @param data tsibble to select for day records
#' @param start time in "HH:MM:SS" format for day start
#' @param end time in "HH:MM:SS" format for day end
#'
#' @return night sliced tsibble containing night records as well as day and week of study from first data entry
#' @export
#'
#' @examples
#' columns <- c("datetime", "value")
#' row1 <- c("2021-03-21 12:15:05", "bottom")
#' row2 <- c("2021-03-21 12:15:10", "bottom")
#' row3 <- c("2021-03-21 12:15:15", "bottom")
#' row4 <- c("2021-03-21 12:15:20", "bottom")
#' row5 <- c("2021-03-21 12:15:25", "bottom")
#' row6 <- c("2021-03-21 12:15:30", "bottom")
#' data2 <- data.frame(rbind(row1,row2,row3,row4,row5,row6),row.names=c())
#' colnames(data2) <- columns
#' get_night_records(data2, "04:00:00", "12:25:00")


get_night_records <- function(data, start, end){
  # Splits out night records based on day time points given returns dataframe with day and week of study embedded

  requireNamespace("hms")
  tryCatch(
    lubridate::hms(start),
    error=function(e) {
      message('Failed to parse, ensure HH:MM:SS format')
      print(e)
    }
  )
  tryCatch(
    lubridate::hms(end),
    error=function(e) {
      message('Failed to parse, ensure HH:MM:SS format')
      print(e)
    }
  )

  first_day_of_study <- lubridate::ymd(data[1]$datetime)

  data$day <- (hms::as_hms(lubridate::ymd_hms(data$datetime)) >= lubridate::hms(start) & hms::as_hms(lubridate::ymd_hms(data$datetime)) < lubridate::hms(end))

  night <- data[data$day != TRUE,]

  night$date <- lubridate::ymd(night$datetime)

  night_dates <- c(first_day_of_study, night$date)

  date_to_day <- data.frame(cbind(unique(night_dates),1:length(unique(night_dates))))

  colnames(date_to_day) <- c('datetime','dos')

  night$dos <- ifelse(lubridate::hour(night$datetime) %in% 0:(lubridate::hour(lubridate::hms(start))-1) ,
                      as.numeric(date_to_day[match(lubridate::ymd(night$datetime), date_to_day$datetime),2])-1,
                      as.numeric(date_to_day[match(lubridate::ymd(night$datetime), date_to_day$datetime),2]))


  week_offset <- as.numeric(lubridate::week(night$datetime)[1])

  night$wos <- (as.numeric(lubridate::week(night$datetime))-week_offset)+1

  return(night)
}
