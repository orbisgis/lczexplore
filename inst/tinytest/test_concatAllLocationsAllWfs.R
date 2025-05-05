library(ggplot2)
library(sf)

wd<-getwd()
rootDir<-"/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree"
setwd(rootDir)
allLCZDirNames <- list.dirs()[-1]
allLCZDirNames <- substr(allLCZDirNames, start = 2, stop = 1000)
allLocationsNames<-substr(allLCZDirNames, start = 2, stop = 1000)
allLCZDirNames<-paste0(rootDir, allLCZDirNames, "/")
setwd(wd)





# tmp <- st_sfc()
# class(tmp)[1] <- "sfc_POLYGON" # for points
# template <- st_sf(field1=integer(0),field2=character(0),geometry=tmp)
# allLocAllWfSf <- st_sf(
#   lcz_primary=character(0),location=character(0), 
#   wf=character(0), area=numeric(0), geometry=tmp)
# st_set_crs(allLocAllWfSf, 4326)
# st_crs(allLocAllWfSf)
# 
# empty_df<-data.frame(lcz_primary=character(0),location=character(0),
#                      wf=character(0), area=numeric(0))
# sf_object <- st_as_sf(empty_df,
#                       geometry = st_sfc(),  # Initialize with an empty geometry column
#                       crs = 4326)
# st_crs(sf_object)
# 
# st_transform(allLocAllWfSf, crs = 32631)

allLocAllWfs<-concatAllLocationsAllWfs(
  dirList = allLCZDirNames, locations = allLocationsNames , workflowNames = c("osm","bdt","iau","wudapt"))

# sf::write_sf(allLocAllWfs, paste0(rootDir, "/allLocAllWfs.fgb"))