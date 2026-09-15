# library(sf)

 sfList<-loadMultipleSfs(dirPath = paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
 workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville"  )

 zoneSf <- sf::read_sf(
 paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville/zone.fgb")
 )
 ArvilleAllWfs <-  concatAlocationWorkflows(
 sfList = sfList, location = "Arville")







