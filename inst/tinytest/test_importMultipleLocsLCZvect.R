# library(dplyr)
# library(sf)
# library(ggplot2)

# sans addmissingRSU
 sfList<-importMultipleLocsLCZvect(dirPath = paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
 workflowNames = c("osm","bdt","wudapt"), location = c("Arville", "Redon"))

showLCZ(sfList$Arville$osm)

# with addMissingRSUs

sfList2<-importMultipleLocsLCZvect(dirPath = paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
                                  workflowNames = c("osm","bdt","wudapt"), location = c("Arville", "Redon"),
                                  missingGeomsWf = "osm",
                                  refWf = NULL,
                                  refLCZ = "Unclassified",
                                  residualLCZvalue = "Unclassified",
                                  column = "LCZ_PRIMARY")

showLCZ(sfList2$Arville$osm)