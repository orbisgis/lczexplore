sfList2<-loadMultipleLocsSfs(
  dirPath = paste0(
    system.file("extdata", package = "lczexplore"),
    "/multipleWfs"),
  workflowNames = c("osm","bdt","wudapt"))

twoLocsIntersect <- createIntersect(
  sfList = sfList2, columns = rep("lcz_primary", 3),
  workflowNames = c("osm","bdt","wudapt"))
example<-barplotLCZfromIntersect(twoLocsIntersect,
                                 columns = c("osm", "bdt", "wudapt"),
                                 workflowNames = c("osm", "bdt", "wudapt"), stat = "sum")
example<-barplotLCZfromIntersect(twoLocsIntersect,
                                 columns = c("osm", "bdt", "wudapt"),
                                 workflowNames = c("osm", "bdt", "wudapt"), stat = "perc")
exampleNoWf<-barplotLCZfromIntersect(twoLocsIntersect,
                                 columns = c("osm", "bdt", "wudapt"),
                                 stat = "perc")
exampleNoCol<-barplotLCZfromIntersect(twoLocsIntersect,
                                 workflowNames = c("osm", "bdt", "wudapt"), stat = "perc")
exampleNoColNoWf<-barplotLCZfromIntersect(twoLocsIntersect, stat = "perc")