 dirList<-list.dirs(paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs"), recursive = FALSE)
 allLocIntersected<-concatIntersectedLocations(
 dirList = dirList, inLocations = c("Arville", "Redon"), columns = "lcz_primary")

