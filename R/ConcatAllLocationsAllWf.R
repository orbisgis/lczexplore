#' In a given list of directories the function looks for LCZ datafiles, return a datasets LCZ values for each geom and workflow
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
concatAllLocationsAllWfs<-function(dirList, locations, workflowNames = c("osm","bdt","iau","wudapt"),
                                   missingGeomsWf = "iau", refWf = NULL, refLCZ = NA,
                                   residualLCZvalue = NA, column = "lcz_primary"){
  allLocAllWfSf<-matrix(ncol = length(workflowNames)+1, nrow = 0)
  allLocAllWfSf<-as.data.frame(allLocAllWfSf)
  names(allLocAllWfSf)<- c("lcz_primary", "location", "wf", "area", "geometry")
for( i in 1:length(dirList)){
    dirPath<-dirList[1]
    aLocation<-locations[i]
    sfList<-loadMultipleSfs(dirPath = dirPath,
                           workflowNames = workflowNames , location = aLocation )
    if(substr(dirPath, nchar(dirPath), nchar(dirPath))!="/"){dirPath<-paste0(dirPath, "/")}
    zoneSfPath<-paste0(dirPath,"zone.fgb")
    zoneSf<-read_sf(zoneSfPath)
    sfList<-addMissingRSUs(sfList = sfList,
                           missingGeomsWf="iau", zoneSf, refWf = refWf, refLCZ = refLCZ, 
                           residualLCZvalue = residualLCZvalue,
                           column = "lcz_primary", location = aLocation)
    concatSf<-concatAlocationWorkflows(sfList = sfList,
                                       location = aLocation, refCrs = 1)
    allLocAllWfSf<-rbind(allLocAllWfSf, concatSf)
  }
  
  allLocAllWfSf<- st_as_sf(allLocAllWfSf)
}

