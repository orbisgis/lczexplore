# library(ggplot2) ; library(sf); library(dplyr) ; library(lczexplore) ; library(ggspatial) ; library(units)
# 
# 
# osm<-importLCZvect(
#   dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Conflans-Sainte-Honorine/",
#   file = "osm_lcz.fgb")
# bdt<-importLCZvect(
#   dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Conflans-Sainte-Honorine/",
#   file = "bdt_lcz.fgb")
# 
# crsRef<-st_crs(bdt)
# osm<-osm %>% mutate(area = st_area(geometry))
# 
# 
# showLCZ(osm) + geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))
# 
# test<-osm %>% group_by(LCZ_PRIMARY) %>%  summarise() %>% ungroup %>%
#   st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% mutate(area=st_area(geometry))
# 
# 
# showLCZ(test, column = "LCZ_PRIMARY")+ geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))
# 
# 
# 
# wudapt<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Conflans-Sainte-Honorine/",
#                       file = "wudapt_lcz.fgb", column="lcz_primary")
# st_crs(wudapt)
# crsRef
# 
# wudapt<- wudapt %>% st_transform(crs = crsRef) %>% mutate(area = st_area(geometry))
# st_crs(wudapt)
# 
# hist(wudapt$area)
# 
# 
# showLCZ(wudapt, column="lcz_primary") + geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))
#   
# wudapt$geometry<-st_make_valid(wudapt$geometry)
# 
# test <- wudapt %>% group_by(lcz_primary) %>%  summarise() %>% ungroup %>%
#   st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% mutate(area=st_area(geometry))
# 
# showLCZ(test, column="lcz_primary") + geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))
# 
# bufferDist <- 0.00001
# 
# # test2 <-wudapt %>% group_by(lcz_primary) %>% 
# #   st_buffer(dist = bufferDist) %>% 
# #   dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep") %>%
# #   mutate(geometry = st_make_valid(geometry)) %>% 
# #   dplyr::summarise(across(geometry, ~ sf::st_union(., by_feature = TRUE)), .groups = "drop") %>%
# #   st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% mutate(area=st_area(geometry))
# 
# test4 <-wudapt %>% group_by(lcz_primary) %>%
#    st_buffer(dist=0.00001) %>%
# group_by(lcz_primary) %>% summarise() %>% 
#   st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% mutate(area=st_area(geometry)) %>%  ungroup
# 
# showLCZ(test4, column = "lcz_primary") + geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))
# 
#   # write_sf(test, "/tmp/wudapt.fgb")
# 
# test3 <-wudapt %>% mutate(area=st_area(geometry))%>%
#   aggregate(by = list(lcz = "lcz_primary"), FUN = unique) %>% mutate(area = st_area(geometry))
# showLCZ(test3) + geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))
# 
# showLCZ(test3, column = "lcz_primary") + geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))
# 
# library(sf)
# allLocAllWfs<-read_sf("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/allLocAllWfs.fgb")
# # summary(allLocAllWfs)
# RSUsSummarisedAllLoc<-matrix(ncol=6, nrow=0) %>% as.data.frame
# names(RSUsSummarisedAllLoc)<-c("lcz",   "numberRSUs", "meanArea", "numberRSUsClust", "meanAreaClust", "wf")
# 
# for (wf in unique(allLocAllWfs$wf)){
#   print(wf)
#   sfIn<-subset(allLocAllWfs, wf==wf)
#   dfOut<-summariseRSUs(sfIn, column = "lcz_primary" )
#   dfOut$wf<-wf
#   RSUsSummarisedAllLoc<-rbind(RSUsSummarisedAllLoc, dfOut)
# }
