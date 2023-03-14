#' This function is deprecated, the refactoring is embedded in othe functions
#'
#' @param lcz a column from an sf object, containing the lcz values
#' @param orig a workflow from which to deduce the possible lcz encoding. For wudapt the land use lcz are coded 11 to 17, for geoclimate they are coded 101 to 107
#'
#' @return the function returns the original lcz column, recoded with the 101 to 107 levels
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' codeNivLCZ(lcz=redonBDT$LCZ_PRIMARY, orig="geoClimate")
codeNivLCZ<-function(lcz,orig){
  # this is deprecated and taken care of in import functions
  print("this is deprecated and taken care of in import functions")
  if (orig=="wudapt"){
    lcz<-lcz %>% as.character %>% as.factor
    levels(lcz)<-list("1"="1","2"="2","3"="3","4"="4","5"="5",
                      "6"="6","7"="7","8"="8","9"="9","10"="10",
                      "101"="11","102"="12","103"="13","104"="14",
                      "105"="15", "106"="16","107"="17")
    return(lcz)
  } else {

      if (orig=="geoClimate"){
        lcz<-lcz%>% as.factor
        levels(lcz)<-list("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                  "9"="9","10"="10","101"="101","102"="102","103"="103","104"="104",
                  "105"="105","106"="106","107"="107")
        return(lcz)
      } else {stop("orig must be wudapt or geoClimate")}

  }

}
