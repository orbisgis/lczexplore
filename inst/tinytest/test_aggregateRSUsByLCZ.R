 dirPath<-paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs")
 allLocAllWfs<-importConcatMultipleLocsLCZvect(
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
column = "lcz_primary", wfColumn = "wf", locationColumn = "location", aggregateBufferSize = 0.5)