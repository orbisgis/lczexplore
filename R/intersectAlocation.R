#' Intersects several sf files for a given location identified by a given directory path and name 
#' @param dirPath is the directory where the original data are
#' @param workflowNames are the names of the workflows : they are used to identify the files
#' @param location is the name of the location (string)
#' @param addMissingRSUs if TRUE calls the addMissingRSUs function to avoid areas with no geometries
#' @param missingGeomsWf is passed to addMissingRSU function
#' @param refWf  is passed to addMissingRSU function
#' @param refLCZ  is passed to addMissingRSU function
#' @param residualLCZvalue is passed to addMissingRSU function
#' @param column the column name of the LCZ classification
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return an sf file with values of LCZ from all the input 
#' are assigned to geometries resulting from intersection of all input geometries
#' @details This function is not generic, it expects the data files to be named wf_rsu_lcz; wf varying among 
#' the values of workflownames, and the LCZ columns are expected to be lcz_primary (but lower and upper cases are accepted)
#' @export
#' @examples
#' # deprecated : use concatIntersectedLocations with the proper arguments
intersectAlocation<-function(dirPath, workflowNames = c("osm","bdt","iau","wudapt"), location,
                             addMissingRSUs = TRUE,
                             missingGeomsWf="iau", refWf = NULL, refLCZ = "Unclassified",
                             residualLCZvalue = "Unclassified",
                             column = "lcz_primary"){
  dirPath<-checkDirSlash(dirPath)
  zoneSfPath<-paste0(dirPath, "zone.fgb")
  zoneSf<-read_sf(zoneSfPath)

  sfList<-loadMultipleSfs(dirPath = dirPath, workflowNames = c("osm","bdt","iau","wudapt"), location = location )
  sfList<-addMissingRSUs(sfList = sfList,
                         missingGeomsWf="iau", zoneSf =zoneSf, refWf = refWf, refLCZ = refLCZ,
                         residualLCZvalue = residualLCZvalue,
                         column = "lcz_primary")
  intersecSf<-createIntersect(sfList=sfList, columns=rep("lcz_primary", length(workflowNames)),
                              refCrs=NULL, sfWf=workflowNames, minZeroArea=0.0001)
  if ("character"%in%class(location)) {intersecSf$location<-location}
return(intersecSf)
}

