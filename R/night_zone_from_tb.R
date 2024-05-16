#' night zone from time budget
#'
#' @param data a night time budget that you want to know the zone that most time was spent in
#'
#' @return a two item vector of zones and propotional time spent in it
#' @export
#'
#' @examples
#' columns <- c("t1","t2", "from_zone", "to_zone")
#' rows <- c("1616328905", "1616368409", NA, "bottom")
#' data <- data.frame(rbind(rows))
#' colnames(data) <- columns
#' data2 <- get_time_budget_prop(data)
#' night_zone_from_TB(data2)
#'
night_zone_from_TB <- function(data){
  overall_zone_sum <- data[,3:5] |>
    na.exclude() |>
    colSums()
  nest_zone <- sort(overall_zone_sum,decreasing=T)[1]

  return(c(names(nest_zone), nest_zone))

}
