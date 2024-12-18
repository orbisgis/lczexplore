#' In a given directory (or a list of directories) the function looks for LCZ datafiles, intersects them and return a datasets with intersected geometries and LCZ values for each workflow
#' @param dirList the list of directories for which the different LCZ files will be intersected
#' @param workflowNames sets the names of workflows and define the name of the files which will be loaded and intersected
#' @param for each diretory from dirList, a location name must be fed to the function
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form, 
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
#' 
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
  concatIntersectedDf$location<-factor(concatIntersectedDf$location)
  concatIntersectedSf<-concatIntersectedDf %>% st_as_sf()
  return(concatIntersectedSf)
}