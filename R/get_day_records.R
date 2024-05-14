#' get_day_records
#'
#' @param data tsibble to select for day records
#' @param start time in "HH:MM:SS" format for day start
#' @param end time in "HH:MM:SS" format for day end
#'
#' @return day sliced tsibble containing day records as well as day and week of study from first data entry
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
#' get_day_records(data2, "04:00:00", "12:25:00")

get_day_records <- function(data, start, end){
  # Splits out day records based on day time points given returns dataframe with day and week of study embedded
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

  data$day <- (hms::as_hms(lubridate::ymd_hms(data$datetime)) >= lubridate::hms(start) & hms::as_hms(lubridate::ymd_hms(data$datetime)) < lubridate::hms(end))

  day <- data[data$day == TRUE,]

  day$date <- lubridate::ymd(day$datetime)

  date_to_day <- data.frame(cbind(unique(day$date),1:length(unique(day$date))))

  colnames(date_to_day) <- c('datetime','dos')

  day$dos <- as.numeric(date_to_day[match(lubridate::ymd(day$datetime), date_to_day$datetime),2])

  # I modified this from "%-V" to "%V" so if there is an issue you can revert.
  week_offset <- as.numeric(lubridate::week(day$datetime)[1])

  day$wos <- (as.numeric(lubridate::week(day$datetime))-week_offset)+1

  return(day)
}
