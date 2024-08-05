#' Function to turn time table into interval table, handles case of 0,1,2,...n transitions
#'
#' @param data tsibble to be turned into a interval table
#'
#' @return interval table where nrows is equal to number of transitions + 1
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
#' data <- data.frame(rbind(row1,row2,row3,row4,row5,row6))
#' colnames(data) <- columns
#' time_to_intervals(data)
#'
time_to_intervals <- function(data){
  requireNamespace("dplyr")

  interval_table <- data.frame()

  first_entry <- utils::head(data,n=1)
  transition_into <- data[which(data$value != dplyr::lag(data$value)),]
  transition_from <- data[which(data$value != dplyr::lag(data$value))-1,]
  last_entry <- utils::tail(data,n=1)

  if(length(transition_into$datetime) == 0){

    new_row <- cbind(as.POSIXct.numeric(as.numeric(first_entry$datetime),origin="1970-01-01"),as.POSIXct.numeric(as.numeric(last_entry$datetime),origin="1970-01-01"),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)
    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")

  } else if( length(transition_into$value) == 1){

    new_row <- cbind(as.POSIXct.numeric(as.numeric(first_entry$datetime),origin="1970-01-01"),as.POSIXct.numeric(as.numeric(transition_into[1,]$datetime),origin="1970-01-01"),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)

    new_row <- cbind(as.POSIXct.numeric(as.numeric(transition_into[1,]$datetime),origin="1970-01-01"), as.POSIXct.numeric(as.numeric(last_entry[1,]$datetime),origin="1970-01-01"),as.character(transition_from[1,2]),as.character(transition_into[1,2]))
    interval_table <- rbind(interval_table, new_row)
    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")

  } else if(length(transition_into$value) == 2){

    new_row <- cbind(as.POSIXct.numeric(as.numeric(first_entry$datetime),origin="1970-01-01"),as.POSIXct.numeric(as.numeric(transition_into[1,]$datetime),origin="1970-01-01"),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)

    new_row <- cbind(as.POSIXct.numeric(as.numeric(transition_into[1,]$datetime),origin="1970-01-01"), as.POSIXct.numeric(as.numeric(transition_into[2,]$datetime),origin="1970-01-01"),as.character(transition_from[1,2]),as.character(transition_into[1,2]))
    interval_table <- rbind(interval_table, new_row)
    new_row <- cbind(as.POSIXct.numeric(as.numeric(transition_into[2,]$datetime),origin="1970-01-01"), as.POSIXct.numeric(as.numeric(last_entry$datetime),origin="1970-01-01"),as.character(transition_from[2,2]),as.character(last_entry[,2]))
    interval_table <- rbind(interval_table, new_row)
    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")

  } else if(length(transition_into$value) > 2) {


    new_row <- cbind(as.POSIXct.numeric(as.numeric(first_entry$datetime),origin="1970-01-01"),as.POSIXct.numeric(as.numeric(transition_into[1,]$datetime),origin="1970-01-01"),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)

    for(i in 1:length(transition_into$value)){
      new_row <- cbind(as.POSIXct.numeric(as.numeric(transition_into[i,]$datetime),origin="1970-01-01"), as.POSIXct.numeric(as.numeric(transition_into[i+1,]$datetime),origin="1970-01-01"),as.character(transition_from[i,2]),as.character(transition_into[i,2]))
      interval_table <- rbind(interval_table, new_row)
    }

    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")
    interval_table[length(interval_table$t2),]$t2 <- last_entry$datetime

  } else {

    print("this shouldn't happen")
    quit("no")

  }

  return(interval_table)

}
