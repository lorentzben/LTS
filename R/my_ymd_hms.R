#' Midnight Aware YMD-HMS formatting
#'
#' @inheritParams lubridate::ymd_hms
#'
#' @return a numerically formatted datetime
#' @export
#'
#' @examples
#' date_1 <- "2021-03-10 T04:00:00"
#' date_2 <- "2021-03-15 T00:00:00"
#' my_ymd_hms(date_1)
#' my_ymd_hms(date_2)
my_ymd_hms <- function(...){
  requireNamespace("lubridate")

  tryCatch(
    {
      parsed <- lubridate::ymd_hms(...)
      return(parsed)
    },
    warning=function(w) {
      message('Likely a Midnight Value')
      # hour <- hour(date)
      # minute <- minute(date)
      # second <- second(date)
      # year <- year(date)
      # month <- month(date)
      # day <- day(date)

      # date_char <- paste0(year,"/",month,"/",day," ",hour,":",minute,":",second)
      # parsed <-  format(as.POSIXct(date_char, tz="UTC"), format="%Y-%m-%d %H:%M:%S")
      parsed <- as.numeric(...)
      return(parsed)
    }

  )
}
