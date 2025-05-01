#' In a given directory (or a list of directories) the function looks for LCZ datafiles, intersects them and return a datasets with intersected geometries and LCZ values for each workflow
#' @param dirList the list of directories for which the different LCZ files will be intersected
#' @param workflowNames sets the names of workflows and define the name of the files which will be loaded and intersected
#' @param locations : for each diretory from dirList, a location name must be fed to the function
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return returns an sf object with all intersections
#' @export
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"))[-1]
#' allLocIntersected<-concatIntersectedLocations(
#' dirList = dirList, locations = c("Blaru", "Goussainville"))
concatIntersectedLocations<-function(dirList, locations, workflowNames = c("osm","bdt","iau","wudapt")){
  concatIntersectedDf<-data.frame(
    matrix(ncol=length(workflowNames)+3, nrow=0)
  )
  names(concatIntersectedDf)<-c(workflowNames,"area", "location", "geometry")

  for (i in 1:length(dirList)){
    print(locations[i])
    concatIntersectedDf<-rbind(concatIntersectedDf,
                               intersectAlocation(
                                 dirPath = dirList[i], workflowNames = workflowNames, location = locations[i])
    )
  }
  concatIntersectedDf$location<-factor(concatIntersectedDf$location, levels = .lczenv$typeLevelsDefault)
  concatIntersectedSf<-concatIntersectedDf %>% st_as_sf()
  
  return(concatIntersectedSf)
}