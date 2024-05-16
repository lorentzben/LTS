#' nested_time_to_intervals
#'
#' @param data dataframe containing records with the column dos (day of study)
#'
#' @return daily_time_budget nested interval tables, where records are nested on day of study
#' @export
#'
#' @examples
#' columns <- c("datetime","value","day","date","dos","wos")
#' row1 <- c("2021-03-21 12:15:05", "bottom", TRUE, 2021-03-21, 1, 1)
#' row2 <- c("2021-03-21 12:15:10", "bottom", TRUE, 2021-03-21, 1, 1)
#' row3 <- c("2021-03-21 12:15:15", "bottom", TRUE, 2021-03-21, 1, 1)
#' row4 <- c("2021-03-21 12:15:20", "bottom", TRUE, 2021-03-21, 1, 1)
#' row5 <- c("2021-03-21 12:15:25", "bottom", TRUE, 2021-03-21, 1, 1)
#' row6 <- c("2021-03-21 12:15:30", "bottom", TRUE, 2021-03-21, 1, 1)
#' data2 <- data.frame(rbind(row1,row2,row3,row4,row5,row6),row.names=c())
#' colnames(data2) <- columns
#' nested_time_to_intervals(data2)
#'
nested_time_to_intervals <- function(data){

  # I'm going to remove date as the nesting function
  daily_time_budget <- data |>
    tidyr::nest(by_day = -c(dos)) |>
    dplyr::mutate(daily_int = purrr::map(by_day, ~time_to_intervals(.x))) #|>
  #unnest(daily_int)

  return(daily_time_budget)
}
