dirPath<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs")
allLocSfList<-loadMultipleLocsSfs(
  dirPath = dirPath, locations = c("Arville", "Redon"), workflowNames = c("osm","bdt","wudapt"))

# test<-concatAllLocsWorkflows(allLocSfList)

allLocIntersected<-createIntersect(allLocSfList, columns = rep("lcz_primary", 4),
                                   workflowNames = c("osm", "bdt", "wudapt"))

testSankey<-prepareSankeyLCZ(intersectedDf = allLocIntersected
  , wf1 = "wudapt", wf2 = "osm")

plotSankeyfiedLCZ(
  sankeyfied = testSankey, plotNow=TRUE)

sfList<-loadMultipleSfs(
  dirPath = paste0(
    system.file("extdata", package = "lczexplore"),
    "/multipleWfs/Arville"),
  workflowNames = c("osm","bdt","wudapt"), location = "Arville")
ArvilleIntersect <- createIntersect(
  sfList = sfList, columns = rep("lcz_primary", 4),
  workflowNames = c("osm", "bdt", "wudapt"))
testSankey<-prepareSankeyLCZ(intersectedDf = ArvilleIntersect
  , wf1 = "wudapt", wf2 = "osm")
plotSankeyfiedLCZ(
  sankeyfied = testSankey, plotNow=TRUE)


dirPath<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs")
allLocConcatenated<-loadMultipleLocsSfs(
  dirPath = dirPath, locations = c("Arville", "Redon"))
allLocIntersected<-createIntersect(allLocConcatenated, columns = rep("lcz_primary", 4),
                                   workflowNames = c("osm", "bdt", "wudapt") )
testSankey<-prepareSankeyLCZ(intersectedDf = allLocIntersected
  , wf1 = "wudapt", wf2 = "osm")
plotSankeyfiedLCZ(
  sankeyfied = testSankey, plotNow=TRUE)