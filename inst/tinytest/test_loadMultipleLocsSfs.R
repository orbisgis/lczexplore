# library(dplyr)
# library(sf)
# library(ggplot2)

 sfList<-loadMultipleLocsSfs(dirPath = paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
 workflowNames = c("osm","bdt","wudapt"), inLocation = c("Arville", "Redon"))
