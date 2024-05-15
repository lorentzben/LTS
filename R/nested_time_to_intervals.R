#' nested_time_to_intervals
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
nested_time_to_intervals <- function(data){
  #TODO decide how to return this and/or make a special time budget function that nests the data before it calculates the budget.
  # I'm going to remove date as the nesting function
  tmp <- data |>
    nest(by_day = -c(dos)) |>
    mutate(daily_int = map(by_day, ~timeToIntervals(.x))) #|>
  #unnest(daily_int)

  return(tmp)
}
