#' Function to round the start so that records start nicely
#'
#' @param dataframe
#' @param units
#' @param interval_min
#'
#' @return
#' @export
#'
#' @examples
nice_start <- function(dataframe, units, interval_min, datetime){
  require("lubridate")
  dataframe[1,1][[datetime]] <- lubridate::round_date(my_ymd_hms(dataframe[1,1][[datetime]]), unit=units)

  # remove duplicate entries
  #dataframe <- dataframe[!duplicates(dataframe)]
  dataframe <- dataframe |>
    distinct(datetime, .keep_all=TRUE)
  dataframe <- tsibble(dataframe,index=datetime)

  attr(dataframe, 'interval') <- new_interval(min=interval_min)
  return(dataframe)
}
