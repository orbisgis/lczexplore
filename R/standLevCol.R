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

  standardLevels<-.lczenv$typeLevelsConvert 
  standCorresp<- .lczenv$colorMapDefault
  names(standCorresp) <- .lczenv$typeLevelsDefault

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