 dirList <- list.dirs(paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs"), recursive = FALSE)
 allLocIntersected<-concatIntersectedLocations(
 dirList = dirList, locations = c("Arville", "Redon"))

 consensus <- computeConsensus(inDf = allLocIntersected,
 wfNames = c("bdt","osm", "wudapt"= "wud"))

 fList<-loadMultipleSfs(dirPath = paste0(system.file("extdata/multipleWfs/Redon", package = "lczexplore")),
                        workflowNames = c("osm","bdt","wudapt"), location = "Redon"  )

 intersected<-createIntersect(sfList = sfList, columns = rep("lcz_primary", 3),
                              workflowNames = c("osm", "bdt", "wudapt"), refCrs = 1)

 consensus <- computeConsensus(inDf = intersected,
                               wfNames = c("bdt","osm", "wudapt"= "wud"))

 multicompare_test<-compareMultipleLCZ(intersected,
                                       columns = c("osm","bdt","wudapt"),
                                       trimPerc = 0.0)
