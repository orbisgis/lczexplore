# library(dplyr)
# library(sf)
# library(ggplot2)

 sfList<-loadMultipleSfs(dirPath = paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
 workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville"  )

expect_silent(
  sfList<-loadMultipleSfs(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
                          workflowNames = c("osm","bdt","wudapt"),
                          inLocation = "Arville", column = "lcz_primary")
)
