#' Detects standard LCZ levels and associate standard colors to them
#'
#' @param levels is the vector of the LCZ levels present in a dataset
#' @param colors is the vector of colors one intends to use to visualize the LCZ of the dataset
#' @param useStandCol allows to say if standard colors should replace user specified colors
#' when standard levels are detected.
#' @import dplyr sf
#' @importFrom grDevices palette.colors
#'
#' @return output is a list containing levelColors, a named vector, which names are the levels
#' present in the data and which values are the associated colors,
#' and case, a string spcifying what case was encountered when producing the levels and colors.
#' @export
#'
#' @examples
#' # standLevCol is not to be used directly by user. 
#' # It deals with levels and colors provided by the user and 
#' # tries to replace user color by standard color 
#' # when a standard level of LCZ is recognized
standLevCol<-function(levels,colors="", useStandCol=FALSE){
# alienColors is a standard palette, chosen for its readibility : Polychrome 36
  # It will be used if no colors are specified and if levels are not known standard levels 
  alienColors<-palette.colors(n=length(levels), palette="Polychrome 36")

  standardLevels<-c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
               "9"="9","10"="10",
               "101"="101","102"="102","103"="103","104"="104", "105"="105","106"="106","107"="107",
               "101"="11","102"="12","103"="13","104"="14", "105"="15", "106"="16","107"="17",
               "101"="A","102"="B","103"="C","104"="D","105"="E","106"="F","107"="G")
  standCorresp<-c("1"="#8b0101","2"="#cc0200","3"="#fc0001","4"="#be4c03","5"="#ff6602","6"="#ff9856",
                  "7" ="#fbed08","8"="#bcbcba","9"="#ffcca7","10"="#57555a",
                  "11"="#006700","101"="#006700","A"="#006700",
                  "12"="#05aa05","102"="#05aa05","B"="#05aa05",
                  "13"="#648423","103"="#648423","C"="#648423",
                  "14"="#bbdb7a", "104"="#bbdb7a","D"="#bbdb7a",
                  "15"="#010101","105"="#010101","E"="#010101",
                  "16"="#fdf6ae", "106" = "#fdf6ae" , "F" = "#fdf6ae",
                  "17"="#6d67fd","107"="#6d67fd","G"="#6d67fd")

  if (length(colors)==length(levels) && prod(areColors(colors))==1){
    typeLevels<-colors
    names(typeLevels)<-levels

  } else {
    print("Please, check your vectors of levels and colors")
    typeLevels<-alienColors
    names(typeLevels)<-levels
    typeLevels
  }

  if(useStandCol==TRUE){
    message("As useStandCol is set to TRUE, some of the specified colors may have been over-ridden to use standard colors if standard LCZ values have been detected")
    names(typeLevels)<-levels
    typeLevels[levels%in%standardLevels]<-
      standCorresp[names(standCorresp)%in%levels[levels%in%standardLevels]]
    typeLevels
    } else {return(typeLevels)}



}