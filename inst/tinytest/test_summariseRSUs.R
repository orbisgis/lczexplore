osm<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                   file = "osm_lcz.fgb")


test<-osm %>% group_by(LCZ_PRIMARY) %>%  summarise() %>% ungroup %>%
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% mutate(area=st_area(geometry))


showLCZ(test)+geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))

test<-lczexplore::summariseRSUs(redonBDT, column ="LCZ_PRIMARY")

wudapt<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                      file = "wudapt_lcz.fgb", column="lcz_primary")
showLCZ(wudapt, column="lcz_primary")
wudapt$geometry<-st_make_valid(wudapt$geometry)
test<-wudapt %>% group_by(lcz_primary) %>%  summarise() %>% ungroup %>%
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% mutate(area=st_area(geometry))
showLCZ(test, column="lcz_primary")+geom_sf_text(aes(label = round(drop_units(area)/100, digits = 0)))


library(sf)
allLocAllWfs<-read_sf("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/AllLocAllWorkflows.fgb")
# summary(allLocAllWfs)
RSUsSummarisedAllLoc<-matrix(ncol=6, nrow=0) %>% as.data.frame
names(RSUsSummarisedAllLoc)<-c("lcz",   "numberRSUs", "meanArea", "numberRSUsClust", "meanAreaClust", "wf")

for (wf in unique(allLocAllWfs$wf)){
  print(wf)
  sfIn<-subset(allLocAllWfs, wf==wf)
  dfOut<-summariseRSUs(sfIn, column = "lcz_primary" )
  dfOut$wf<-wf
  RSUsSummarisedAllLoc<-rbind(RSUsSummarisedAllLoc, dfOut)
}
