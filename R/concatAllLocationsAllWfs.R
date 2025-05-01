#' In a given list of directories the function looks for LCZ datafiles, return a datasets LCZ values for each geom and workflow
#' @param dirList the list of directories for which the different LCZ files will be intersected
#' @param workflowNames sets the names of workflows and define the name of the files which will be loaded and intersected
#' @param locations for each diretory from dirList, a location name must be fed to the function
#' @param missingGeomsWf the name of the workflow where some areas were not classified
#' @param refWf the name of the workflow one uses as a reference to complete the incomplete workflow.
#' if NULL then the geometries added will get the level specified in residualLCZvalue, else, the level in refLCZ
#' @param refLCZ the reference level the completed geometries will receive
#' @param residualLCZvalue the value the completed geometries where the reference workflow does not classify 
#' the geometry with refLCZ level
#' @param column a parameter to feed addMissingRSUs function
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf data.table dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form, 
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
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
concatAllLocationsAllWfs<-function(dirList, locations, workflowNames = c("osm","bdt","iau","wudapt"),
                                   missingGeomsWf = "iau", refWf = NULL, refLCZ = NA,
                                   residualLCZvalue = NA, column = "lcz_primary"){
  # allLocAllWfSf<-matrix(ncol = 5, nrow = 0)
  tmp <- st_sfc()
  class(tmp)[1] <- "sfc_POLYGON" 
  allLocAllWfSf<- data.frame(
    lcz_primary=character(0),location=character(0),
  wf=character(0), area=numeric(0)) %>% 
  st_as_sf(geometry = st_sfc(),  # Initialize with an empty geometry column
           crs = 4326)

for( i in 1:length(dirList)){
    dirPath<-dirList[i]
    if (substring(text = dirPath, first = nchar(dirPath))!="/"){dirPath<-paste0(dirPath, "/")} 
    aLocation<-locations[i]
    print(aLocation)
    sfList<-loadMultipleSfs(dirPath = dirPath,
                           workflowNames = workflowNames , location = aLocation )
    if(substr(dirPath, nchar(dirPath), nchar(dirPath))!="/"){dirPath<-paste0(dirPath, "/")}
    zoneSfPath<-paste0(dirPath,"zone.fgb")
    zoneSf<-read_sf(zoneSfPath)
    sfList<-addMissingRSUs(sfList = sfList,
                           missingGeomsWf="iau", zoneSf, refWf = refWf, refLCZ = refLCZ, 
                           residualLCZvalue = residualLCZvalue,
                           column = "lcz_primary")
    concatSf<-concatAlocationWorkflows(sfList = sfList,
                                       location = aLocation, refCrs = 1)
  if(i==1 && st_crs(allLocAllWfSf)!=st_crs(concatSf)){
    allLocAllWfSf<-st_transform(allLocAllWfSf, crs = st_crs(concatSf))
  }
  allLocAllWfSf<-rbind(allLocAllWfSf, concatSf)

  }
  
  return(allLocAllWfSf)
}

