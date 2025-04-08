#' For a given LCZ sf object, returns the number of geometries and their mean area per level of LCZ
#' @param sf contains the geometry and LCZ levels
#' @param columns a the LCZ column name
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return the number of geometries (Reference Spatial units or RSUs) 
#' and their mean area per level of LCZ, and the same after agregatting geometries 
#' with same level of LCZ which touch each other
#' @export
#' @examples
summariseRSUs<-function(sfIn, aggregatingColumns = "lcz_primary", trimValue = 0 ){
  if (!"sf"%in%class(sfIn)){sfIn<-st_as_sf(sfIn)}
  
  DTin<-sfIn
  data.table::setDT(DTin)
  sfOut<-DTin[,
    as.list(
      c(
        number = .N,
        meanArea = round(mean(area / 10000, trim = trimValue), digits = 2),
        sdArea = round(sd(area / 10000), digits = 2),
        totalArea=round(sum(area/10000), digits = 2),
        meanLogArea = round(mean(log(area / 10000), trim = trimValue), digits = 2),
        sdLogArea = round(sd(log(area / 10000)), digits = 2)
      )
    ), by = aggregatingColumns]
  
 return(sfOut)
}



