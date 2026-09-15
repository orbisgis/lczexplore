 dirPath<-paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs")
 allLocAllWfs<-loadConcatAllLocsAllWfs(
  dirPath = dirPath, locations = c("Arville", "Redon"),
 workflowNames = c("osm","bdt","wudapt"),
  missingGeomsWf= "osm",
  refWf = NULL,
  refLCZ = "Unclassified",
  residualLCZvalue = "Unclassified",
  column = "lcz_primary"
)
 ASUallLocAllWfs <- aggregateRSUsByLCZ(
 allLocAllWfs,
 LCZcolumn = "lcz_primary", wfColumn = "wf", locationColumn = "location", aggregateBufferSize = 0.5)