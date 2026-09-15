 sfList<-loadMultipleSfs(dirPath = paste0(
 system.file("extdata", package = "lczexplore"),
 "/multipleWfs/Arville"),
 workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville")
 ArvilleIntersect <- createIntersect(
  sfList = sfList, columns = rep("lcz_primary", 4),
  workflowNames = c("osm","bdt","wudapt"))
 ArvilleMultipleComparison<-compareMultipleLCZ(
  sfInt = ArvilleIntersect,
  columns = c("osm","bdt","wudapt"),
  trimPerc = 0.5)
 ArvilleWorkflowAgreement<-workflowAgreeAreas(ArvilleMultipleComparison$sfIntLong)