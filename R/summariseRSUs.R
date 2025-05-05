#' For a given LCZ sf object, returns the number of geometries and their mean area per LCZ type
#' @param sfIn an sf objects that contains the geometry and LCZ levels
#' @param aggregatingColumns tshould be the name of the LCZ types column, but can be any factor variable names
#' @param trimValue a trim parameter to feed the mean function in order to avoid letting artifially small or big
#' geometries to influence the average area results
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return the number of geometries (Reference Spatial units or RSUs) 
#' and their mean area per level of LCZ, and the same after agregatting geometries 
#' with same level of LCZ which touch each other
#' @export
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"))[-1]
#' allLocAllWfs<-concatAllLocationsAllWfs(
#'  dirList = dirList, locations = c("Blaru", "Goussainville"), 
#' workflowNames = c("osm","bdt","iau","wudapt"),
#'  missingGeomsWf = "iau",
#'  refWf = NULL,
#'  refLCZ = "Unclassified",
#'  residualLCZvalue = "Unclassified",
#'  column = "lcz_primary"
#')
#' summarisedRSUs<-summariseRSUs(allLocAllWfs, aggregatingColumns = "wf")
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



