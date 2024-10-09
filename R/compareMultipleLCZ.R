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
  nbWfs<-length(sfList)
  
  intersec_sf<-computeAgreements(intersec_sf = intersec_sf, nbWfs = nbWfs)
  
  return(intersec_sf)

}


sfBDT_11_78030<-importLCZvect(dirPath="/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2011/bdtopo_2_78030/",
                   file="rsu_lcz.fgb", column="LCZ_PRIMARY")

sfBDT_22_78030<-importLCZvect(dirPath="/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2022/bdtopo_3_78030/",
                              file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# sf_OSM_11_Auffargis<-importLCZvect(dirPath="//home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2011/osm_Auffargis/", file="rsu_lcz.fgb", column="LCZ_PRIMARY")
sf_OSM_22_Auffargis<-importLCZvect(dirPath="/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2022/osm_Auffargis/",
                                   file="rsu_lcz.fgb", column="LCZ_PRIMARY")
sf_WUDAPT_78030<-importLCZvect("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/WUDAPT/",
                               file ="wudapt_Auffargis.fgb", column="lcz_primary")
sf_IAU_auffargis <- importLCZvect("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/IAU", file = "IAU_Auffargis.fgb", column = "lcz_primary")

sfList<-list(BDT11 = sfBDT_11_78030, BDT22 = sfBDT_22_78030, # OSM11= sf_OSM_11_Auffargis, 
  OSM22 = sf_OSM_22_Auffargis,
             WUDAPT = sf_WUDAPT_78030, IAU = sf_IAU_auffargis)


multicompare_test<-compareMultipleLCZ(sfList = sfList, LCZcolumns = c(rep("LCZ_PRIMARY",3), rep("lcz_primary", 2)),
                                      sfWf = c("BDT11","BDT22",
                                      # "OSM11",
                                      "OSM22","WUDAPT", "IAU"),trimPerc = 0.25)
multicompare_test %>% summary()

require(ggplot2)
ggplot(data=multicompare_test) +
  geom_sf(aes(fill=maxAgree, color=after_scale(fill)))+
  scale_fill_gradient(low = "red" , high = "green", na.value = NA)

ggplot(data=multicompare_test) +
  geom_sf(aes(fill=nbAgree, color=after_scale(fill)))+
  scale_fill_gradient(low = "red" , high = "green", na.value = NA)

