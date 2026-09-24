# library(dplyr)
# library(sf)
# library(ggplot2)

 sfList<-importMultipleLCZvect(dirPath = paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
                               columns = c("LCZ_PRIMARY", "LCZ_PRIMARY", "lcz_primary"),
                               workflowNames = c("osm","bdt","wudapt"), location = "Arville")
showLCZ(sfList$wudapt, column = "lcz_primary")

expect_silent(
  sfList<-importMultipleLCZvect(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
                          workflowNames = c("osm","bdt","wudapt"),
                          location = "Arville", column = "lcz_primary")
)
