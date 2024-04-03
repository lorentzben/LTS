#' Function to turn time table into interval table, handles case of 0,1,2,...n transitions
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
time_to_intervals <- function(data){
  require(dplyr)

  interval_table <- data.frame()

  first_entry <- head(data,n=1)
  transition_into <- data[which(data$value != dplyr::lag(data$value)),]
  transition_from <- data[which(data$value != dplyr::lag(data$value))-1,]
  last_entry <- tail(data,n=1)

  if(length(transition_into$datetime) == 0){

    new_row <- cbind(my_ymd_hms(first_entry$datetime),my_ymd_hms(last_entry$datetime),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)
    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")

  } else if( length(transition_into$value) == 1){

    new_row <- cbind(my_ymd_hms(first_entry$datetime),my_ymd_hms(transition_into[1,]$datetime),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)

    new_row <- cbind(my_ymd_hms(transition_into[1,]$datetime), my_ymd_hms(last_entry[1,]$datetime),as.character(transition_from[1,2]),as.character(transition_into[1,2]))
    interval_table <- rbind(interval_table, new_row)
    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")

  } else if(length(transition_into$value) == 2){

    new_row <- cbind(my_ymd_hms(first_entry$datetime),my_ymd_hms(transition_into[1,]$datetime),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)

    new_row <- cbind(my_ymd_hms(transition_into[1,]$datetime), my_ymd_hms(transition_into[2,]$datetime),as.character(transition_from[1,2]),as.character(transition_into[1,2]))
    interval_table <- rbind(interval_table, new_row)
    new_row <- cbind(my_ymd_hms(transition_into[2,]$datetime), my_ymd_hms(last_entry$datetime),as.character(transition_from[2,2]),as.character(last_entry[,2]))
    interval_table <- rbind(interval_table, new_row)
    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")

  } else if(length(transition_into$value) > 2) {


    new_row <- cbind(my_ymd_hms(first_entry$datetime),my_ymd_hms(transition_into[1,]$datetime),NA, as.character(first_entry$value))
    interval_table <- rbind(interval_table, new_row)

    for(i in 1:length(transition_into$value)){
      new_row <- cbind(my_ymd_hms(transition_into[i,]$datetime), my_ymd_hms(transition_into[i+1,]$datetime),as.character(transition_from[i,2]),as.character(transition_into[i,2]))
      interval_table <- rbind(interval_table, new_row)
    }

    colnames(interval_table) <- c("t1","t2","from_zone","to_zone")
    interval_table[length(interval_table$t2),]$t2 <- last_entry$datetime

  } else {

    print("this shouldn't happen")
    exit(0)

  }

  return(interval_table)

}
