# library(sf)
# library(ggplot2)
# 
# sfList<-loadMultipleSfs(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Les Mureaux/",
#                        workflowNames = c("osm","bdt","iau","wudapt"), location = "Les Mureaux"  )
# 
# zoneSf<-read_sf("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Les Mureaux/zone.fgb")
# zoneSf<-st_transform(zoneSf,
#                      crs=st_crs(sfList$iau))
# missingDiff<-st_difference(
#   st_union(zoneSf),
#   st_union(sfList[["iau"]])) %>% st_as_sf
# str(missingDiff)
# 
# waterRef<-sfList[["bdt"]][sfList[["bdt"]]$lcz_primary=="107",] %>% st_transform(crs=st_crs(sfList[["iau"]]))
# 
# ggplot(waterRef) +
#   geom_sf(aes(fill=lcz_primary)) +
#   scale_fill_manual(values=c("107"="blue"))
# 
# waterNew<-st_intersection(missingDiff, waterRef)
# st_set_geometry(waterNew, waterNew$x)
# names(waterNew)[names(waterNew)=="x"]<-"geometry"
# st_geometry(waterNew)<-"geometry"
# 
# 
# 
# 
# ggplot(waterNew) +
#   geom_sf(aes(fill=lcz_primary))+
#   scale_fill_manual(values=c("107"="blue"))
# 
# residualDiff<-st_difference(
#   st_union(missingDiff),
#   st_union(waterNew)) %>% st_as_sf
# st_set_geometry(residualDiff, residualDiff$x)
# names(residualDiff)[names(residualDiff)=="x"]<-"geometry"
# st_geometry(residualDiff)<-"geometry"
# residualDiff[["lcz_primary"]]<-"105"
# residualDiff[["location"]]<-"Les Mureaux"
# residualDiff[["wf"]]<-"iau"
# 
# test<-rbind(sfList$iau,
#             waterNew)
# 
# ggplot() +
#   geom_sf(data = residualDiff, aes(fill=lcz_primary))+
#   geom_sf(data = waterNew, aes(fill=lcz_primary)) +
#   scale_fill_manual(values=c("107"="blue","105" =" gray"))
#   
# 
# 
# 
# 
# newWater<-st_intersection(
#   missingDiff,
#   waterRef
# )
# 
# 
# ggplot() +
#   geom_sf(data=newWater, aes(fill=lcz_primary)) +
#   scale_fill_manual(values=c("107"="blue"))
# 
# 
# test<-addMissingRSUs(sfList, missingGeomsWf="iau", zoneSf = zoneSf, refWf = NULL, refLCZ = "107",
#                      residualLCZvalue=NA, location = "Les Mureaux", column = "lcz_primary")
# test<-addMissingRSUs(sfList, missingGeomsWf="iau", zoneSf = zoneSf, refWf = "bdt", refLCZ = "107",
#                      residualLCZvalue="105", location = "Les Mureaux", column = "lcz_primary")
# 
# showLCZ(test$iau, column="lcz_primary")