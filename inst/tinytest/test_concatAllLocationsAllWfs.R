rootDir<-"/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree"
setwd(rootDir)
allLCZDirNames <- list.dirs()[-1]
allLCZDirNames <- substr(allLCZDirNames, start = 2, stop = 1000)
allLocationsNames<-substr(allLCZDirNames, start = 2, stop = 1000)
allLCZDirNames<-paste0(rootDir, allLCZDirNames, "/")
allLocallWfs<-concatAllLocationsAllWfs(
  dirList = allLCZDirNames, locations = allLocationsNames , workflowNames = c("osm","bdt","iau","wudapt"))
sf::write_sf(allLocallWfs, paste0(rootDir, "/allLocAllWfs.fgb"))