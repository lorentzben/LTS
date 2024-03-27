#' select Tsibble between timepoints given (used to get contiguous time series of records)
#'
#' @param data Tsibble with data between start and stop
#' @param start A lubridate::ymd() formatted datetime
#' @param stop
#'
#' @return a sliced Tsibble for values between start and stop
#' @export
#'
#' @examples
#'
sliceTsibble <- function(data, start, stop){
  require(utils)

  previous_state <- utils::tail(data[data$datetime <= LTS::my_ymd_hms(start),],n=1)
  greater_than_start <- data[data$datetime >= LTS::my_ymd_hms(start),]
  less_than_end <- greater_than_start[greater_than_start$datetime <= LTS::my_ymd_hms(stop),]
  final_state <- utils::head(greater_than_start[greater_than_start$datetime >= LTS::my_ymd_hms(stop),],n=1)


  if(nrow(less_than_end) == 0){
    less_than_end <- NA
    return(less_than_end)
  }

  previous_state$datetime <- LTS::my_ymd_hms(start)
  final_state$datetime <- LTS::my_ymd_hms(stop)
  less_than_end <- bind_rows(less_than_end, previous_state)
  less_than_end <- bind_rows(less_than_end, final_state)
  return(less_than_end)

}
