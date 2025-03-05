#' Checks if the columns entered by the user match the columns in the data source
#'
#' @param userColNames a vector of column names entered by the user
#' @param dataColNames a vector names presents in a dataset
#' @return returns an error if on or several colum names
#' in userColNames are not present in dataColNames, except if they are present but in lower case, then the 
#' correspondent lower case name is substituted
#' @export
#' @examples 
#' userColNames<-c("ID", "LCZ_PRIMARY")
#' dataColNames<-c("ID", "LCZ_PRIMARY")
#' checkColnameCase(userColNames, dataColNames)
#' dataColNames<-c("ID", "lcz_primary")
checkColnameCase<-function(userColNames, dataColNames){
  inCol <- userColNames %in% dataColNames
  badCol <- userColNames[!inCol]
  colErr <- c("It seems that some of the columns you try to import do not exist in the source file,
              are you sure you meant ",
              paste(badCol), "?")
  if (prod(inCol) == 0) {
    if(tolower(badCol)%in%dataColNames){
      message(paste0(badCol,
                     " doesn't seem to be a column of your dataset, but ",
                     tolower(badCol),
                     " is and was loaded instead. If this was not the desired column, please check your source data. "))
      userColNames[userColNames == badCol]<-tolower(badCol)  
    } else {
      stop(colErr)
    }
  }
  
  return(userColNames)
  
}