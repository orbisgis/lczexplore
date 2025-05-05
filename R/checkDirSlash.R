#' Simply adds a slash at then end of a directory path if there is none
#' @param dirPath is a directory pas as a string
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @return the same string if it ended with a slash, else the string with a slash at its end
#' @export
#' @examples
#' dirPathTest<-"test"
#' checkDirSlash(dirPathTest)
checkDirSlash<-function(dirPath){
  if ( substring(dirPath, nchar(dirPath), nchar(dirPath)) != "/"){
    dirPath<-paste0(dirPath, "/")
  }
  return(dirPath)
}

