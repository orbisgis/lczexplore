 rootDir<-system.file("extdata/multipleWfs", package = "lczexplore")

allLCZDirNames <- list.dirs(rootDir)[-1]
allLocationsNames <- gsub(
  x = allLCZDirNames,
  pattern = "(.*)(/)(\\w+)",
  replacement = "\\3")

for (i in 1:2){
  barplotLCZaLocation(
    dirPath = allLCZDirNames[i],
    location = allLocationsNames[i], plotNow = TRUE, plotSave = FALSE)
}

