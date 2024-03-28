#' select Tsibble between timepoints given (used to get contiguous time series of records)
#'
#' @param data Tsibble with data between start and stop
#' @param start A lubridate::ymd() formatted datetime
#' @param stop A lubridate::ymd() formatted datetime
#' @param datetime String that is the columnname where the ymd_hms formatted datetime is
#'
#' @return a sliced Tsibble for values between start and stop
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
#' begin <- "2021-03-21 T23:10:00"
#' end <- "2021-03-21 T23:12:00"
#'
#' slice_tsibble(record_table, start=begin, stop=end)
slice_tsibble <- function(data, start, stop, datetime="datetime"){

  stopifnot(start<stop)

  if(start > utils::head(data[[datetime]],n=1)){
    warning("Start provided is outside of series, will use first value")
  }
  if(stop > utils::tail(data[[datetime]],n=1)){
    warning("End provided is outside of series, will use last value")
  }

    #Check to make sure start and stop are not outside the dataframe
    previous_state <- utils::tail(data[data[[datetime]] <= LTS::my_ymd_hms(start),],n=1)
    greater_than_start <- data[data[[datetime]] >= LTS::my_ymd_hms(start),]
    less_than_end <- greater_than_start[greater_than_start[[datetime]] <= LTS::my_ymd_hms(stop),]
    final_state <- utils::head(greater_than_start[greater_than_start[[datetime]] >= LTS::my_ymd_hms(stop),],n=1)


    if(nrow(less_than_end) == 0){
      less_than_end <- NA
      return(less_than_end)
    }

    previous_state[[datetime]] <- LTS::my_ymd_hms(start)
    final_state[[datetime]] <- LTS::my_ymd_hms(stop)
    less_than_end <- dplyr::bind_rows(less_than_end, previous_state)
    less_than_end <- dplyr::bind_rows(less_than_end, final_state)
    return(less_than_end)




}
