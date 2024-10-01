#' Compares several sets of geographical classifications, especially Local Climate Zones classifications
#' @param sfList a list which contains the classifications to compare, as sf objects
#' @param LCZcolumns a vector which contains, for eacfh sf of sfList, the name of the column of the classification to compare
#' @param refCrs a number which indicates which sf object from sfList will provide the CRS in which all the sf objects will be projected before comparison
#' By defautl it is set to an empty string and no ID is loaded.
#' @param sfWf a vector of strings which contains the names of the workflows used to produce the sf objects
#' @param trimPerc this parameters indicates which percentile to drop out of the smallest geometries resulting 
#' from the intersection of the original sf geometries intersection. 
#' It allows to account for numeric precision errors and to speed up computations at the cost of not considering the smallest geometries. 
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return returns graphics of comparison and an object called matConfOut which contains :
#'TO DO
#' @export
#' @examples
#' 
compareMultipleLCZ<-function(sfList, LCZcolumns, refCrs=NULL, sfWf=NULL, trimPerc=0.05){
  intersec_sf<-createIntersec(sfList = sfList, LCZcolumns = LCZcolumns , refCrs= refCrs, sfWf = sfWf)
  print(nrow(intersec_sf))
  intersec_sf$area<-st_area(intersec_sf)
  intersec_sf <- intersec_sf %>% subset(area>quantile(intersec_sf$area, probs=trimPerc) & !is.na(area))
  print(nrow(intersec_sf))
  intersec_sfnogeom<-st_drop_geometry(intersec_sf)
  for (i in 1:(length(sfList) - 1)) {
    for(j in (i+1):length(sfList)){
      compName<-paste0(i,"_",j)
      print(compName)
      intersec_sfnogeom[,compName]<-intersec_sfnogeom[,i] == intersec_sfnogeom[,j]
    }
  }
  rangeCol<-(length(sfList)+3):ncol(intersec_sfnogeom)
  print(rangeCol)
  # print(names(intersec_sfnogeom[,rangeCol]))
  intersec_sfnogeom$nbAgree<-apply(intersec_sfnogeom[,rangeCol],MARGIN=1,sum)
  intersec_sfnogeom$maxAgree<-apply(
    X = intersec_sfnogeom[,1:length(sfList)], MARGIN = 1, function(x) max(table(x) ))
  intersec_sf<-cbind(intersec_sfnogeom,intersec_sf$geometry)  %>% st_as_sf()
  intersec_sf
  intersec_sfLong<-pivot_longer(intersec_sfnogeom,cols=rangeCol, names_to = "whichWfs", values_to = "agree")
  intersec_sfLong$LCZref<-substr(intersec_sfLong$whichWfs,start = 1, stop=1 )
  print(head(intersec_sfLong[,c(1,2,9:10)]))
  whichLCZagree <- names(intersec_sfLong)[as.numeric(intersec_sfLong$LCZref)]
  indRow<- seq_len(nrow(intersec_sfLong))
  z<-data.frame(indRow, whichLCZagree)
  intersec_sfLong$LCZvalue<-apply(z, 1, function(x) unlist(st_drop_geometry(intersec_sfLong)[x[1], x[2]]))
  print(head(intersec_sfLong[,c(1,2,9:11)]))

  output<-list(intersec_sf=intersec_sf, intersec_sfLong=intersec_sfLong)
}


sfBDT_11_78030<-importLCZvect(dirPath="/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2011/bdtopo_2_78030/",
                   file="rsu_lcz.fgb", column="LCZ_PRIMARY")
class(sfBDT_11_78030)
sfBDT_22_78030<-importLCZvect(dirPath="/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2022/bdtopo_3_78030/",
                              file="rsu_lcz.fgb", column="LCZ_PRIMARY")
sf_OSM_11_Auffargis<-importLCZvect(dirPath="//home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2011/osm_Auffargis/",
                                   file="rsu_lcz.fgb", column="LCZ_PRIMARY")
sf_OSM_22_Auffargis<-importLCZvect(dirPath="/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2022/osm_Auffargis/",
                                   file="rsu_lcz.fgb", column="LCZ_PRIMARY")
sf_WUDAPT_78030<-importLCZvect("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/WUDAPT/",
                               file ="wudapt_Auffargis.fgb", column="lcz_primary")

sfList<-list(BDT11 = sfBDT_11_78030, BDT22 = sfBDT_22_78030, OSM11= sf_OSM_11_Auffargis, OSM22 = sf_OSM_22_Auffargis,
             WUDAPT = sf_WUDAPT_78030)


multicompare_test<-compareMultipleLCZ(sfList = sfList, LCZcolumns = c(rep("LCZ_PRIMARY",4),"lcz_primary"),
                                      sfWf = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"),trimPerc = 0.5)
multicompare_test


ggplot(data=multicompare_test$intersec_sf) +
  geom_sf(aes(fill=maxAgree, color=after_scale(fill)))+
  scale_fill_gradient(low = "red" , high = "green", na.value = NA)

# hist(st_area(multicompare_test$intersec_sf$geometry))

# allSameList<-list(OSM11= sf_OSM_11_Auffargis, OSM11.2 = sf_OSM_11_Auffargis, 
#   OSM11.3 = sf_OSM_11_Auffargis, OSM11.4 = sf_OSM_11_Auffargis, OSM_11.5 = sf_OSM_11_Auffargis)
# showLCZ(sfList[[1]])

# sf_OSM_11_Auffargis[which.max(st_area(sf_OSM_11_Auffargis)),] 

# max(st_area(sf_OSM_11_Auffargis))

# multicompare_test_all_same<-compareMultipleLCZ(sfList = allSameList, 
#   LCZcolumns = rep("LCZ_PRIMARY",5),
#   sfWf = c("OSM1","OSM2", "OSM3", "OSM4", "OSM5"),
#   trimPerc = 0.5)



# areas_test<-st_area(multicompare_test_all_same$intersec_sf)
# hist(areas_test)
# hist(st_area(sf_OSM_11_Auffargis$geometry))


# quantile(areas_test, prob = 0.5)
# test2<-multicompare_test_all_same$intersec_sf[
#   st_area(multicompare_test_all_same$intersec_sf) == 
#     max(st_area(multicompare_test_all_same$intersec_sf)),
# ]


# ggplot() + 
#     geom_sf(data=multicompare_test_all_same$intersec_sf, aes(fill=maxAgree))+
#     scale_fill_gradient(low = "red" , high = "green", na.value = NA)

# ggplot() + 
#     geom_sf(data=test2, aes(color = "gray", fill=maxAgree)) +
#       scale_fill_gradient(low = "red" , high = "green", na.value = NA) + 
#     scale_linewidth(range=c(8))

# ggplot() +
#   geom_sf(data = sf_OSM_11_Auffargis[which.max(st_area(sf_OSM_11_Auffargis)),], 
# aes(color = LCZ_PRIMARY) 
# )
