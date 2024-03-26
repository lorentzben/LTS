#' perform sliding window analysis to remove transitions into current zone
#'
#' @param records tibble with colnamed subzone
#'
#' @return de-duplicated transtion record table
#' @export
#'
#' @examples
#' require(dplyr)
#' subzone <- c("Middle","Middle","Middle","Top")
#' accessdate <- c(1620412230,1620411973,1620411937,1620411910)
#' LegBand <- c(6992,6992,6992,6992)
#' record_table <- data.frame(cbind(subzone,accessdate,LegBand))
#' colnames(record_table) <- c('subzone','accessdate','LegBand')
#' record_table <- dplyr::tibble(record_table)
#'
#' de_duplicated_record_table <- identify_duplicate_records(record_table)


identify_duplicate_records <- function(records){
  records$duplicate <- rep(0, length(records$subzone))
  records <- records[order(records$accessdate),]

  for(i in 3:length(records$subzone)){

    primary <- records[i-2,]
    secondary <- records[i-1,]
    tertiary <- records[i,]
    #print(primary)

    #print(paste(primary$subzone," : ",secondary$subzone))


    #check if primary zone is same as secondary zone
    if(length(unique(c(primary$subzone,secondary$subzone,tertiary$subzone)))!=3){
      if(primary$subzone != secondary$subzone){
        records[[i-2,"duplicate"]] <- 1
      } else if(secondary$subzone != tertiary$subzone){
        records[[i-1,"duplicate"]] <- 1
      }
    }

  }
  return(records)
}
