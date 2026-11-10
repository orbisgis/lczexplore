# library(dplyr)
# library(sf)
# library(ggplot2)

# sans addmissingRSU
 sfList<-importMultipleLocsLCZvect(dirPath = paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
 workflowNames = c("osm","bdt","wudapt"), location = c("Arville", "Redon"))

showLCZ(sfList$Arville$osm)

class(sfList$Arville$osm)

test1<-concatAllLocsWorkflows(sfList)
test2<-dplyr::filter(test1, location == "Arville" & wf == "osm")
showLCZ(test2)

# with addMissingRSUs

sfList2<-importMultipleLocsLCZvect(dirPath = paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
                                  workflowNames = c("osm","bdt","wudapt"),
                                   location = c("Arville", "Redon"),
                                  missingGeomsWf = "osm",
                                  refWf = NULL,
                                  refLCZ = "Unclassified",
                                  residualLCZvalue = "Unclassified",
                                  columns = "LCZ_PRIMARY")

showLCZ(sfList2$Arville$osm)
showLCZ(sfList2$Redon$osm)

allLocAllWfs<-concatAllLocsWorkflows(sfList2)
test<-dplyr::filter(allLocAllWfs, location == "Redon" & wf == "osm")
showLCZ(test)

sfList3<-importMultipleLocsLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTreeBDT2012",
                                   workflowNames = c("osm","bdt","wudapt"),
                                   missingGeomsWf = "osm",
                                   refWf = NULL,
                                   refLCZ = "Unclassified",
                                   residualLCZvalue = "Unclassified",
                                   columns = "LCZ_PRIMARY")

showLCZ(sfList3$Arville$osm)