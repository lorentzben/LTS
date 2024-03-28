#' perform sliding window analysis to remove transitions into current zone
#'
#' @param records tibble with colnamed subzone
#' @param datetime String colname containing epoch formatted datetime
#' @param value String colname containing factor of interest
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
#' de_duplicated_record_table <- identify_duplicate_records(record_table,datetime="accessdate",value="subzone")


identify_duplicate_records <- function(records,datetime="accessdate",value="subzone"){
  records$duplicate <- rep(0, length(records[[value]]))
  records <- records[order(records[[datetime]]),]

  for(i in 3:length(records[[value]])){

    primary <- records[i-2,]
    secondary <- records[i-1,]
    tertiary <- records[i,]
    #print(primary)

    #check if primary zone is same as secondary zone
    if(length(unique(c(primary[[value]],secondary[[value]],tertiary[[value]])))!=3){
      if(primary[[value]] != secondary[[value]]){
        records[[i-2,"duplicate"]] <- 1
      } else if(secondary[[value]] != tertiary[[value]]){
        records[[i-1,"duplicate"]] <- 1
      }
    }

  }
  return(records)
}
