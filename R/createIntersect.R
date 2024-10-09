createIntersec<-function(sfList, LCZcolumns, refCrs=NULL, sfWf=NULL){
  intersec_sf<-sfList[[1]] %>% select(LCZcolumns[1])
  if (is.null(refCrs)){refCrs<-st_crs(intersec_sf)}
  for (i in 2:length(sfList)){
    sfProv<-sfList[[i]] %>% select(LCZcolumns[i])
    if (st_crs(sfProv) != refCrs ) {sfProv<-st_transform(sfProv, crs=refCrs)}
    intersec_sf<-st_intersection(intersec_sf,sfProv)
  }
  if (!is.null(sfWf) & length(sfWf) == length(sfList)){
    names(intersec_sf)[1:(ncol(intersec_sf)-1)]<-paste0("LCZ",sfWf)
  } else { names(intersec_sf)[1:(ncol(intersec_sf)-1)]<-paste0("LCZ",1:length(sfList)) }
  
return(intersec_sf)
  
  # intersec_sf[,1:(ncol(intersec_sf)-1)]
  # test<- apply(
  #    X = st_drop_geometry(intersec_sf)[,1:(ncol(intersec_sf)-1)], 
  #    MARGIN = 2,  FUN = as.factor)
  # return(test)
  
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
sf_IAU_auffargis <- importLCZvect("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/IAU", file = "IAU_Auffargis.fgb", column = "lcz_primary")

sfList<-list(BDT11 = sfBDT_11_78030, BDT22 = sfBDT_22_78030, OSM11= sf_OSM_11_Auffargis, OSM22 = sf_OSM_22_Auffargis,
             WUDAPT = sf_WUDAPT_78030, IAU = sf_IAU_auffargis)



intersected<-createIntersec(sfList = sfList, LCZcolumns = c(rep("LCZ_PRIMARY",4),rep("lcz_primary",2)), 
                            sfWf = c("BDT11","BDT22","OSM11","OSM22","WUDAPT", "IAU"))
is.factor(sf_OSM_22_Auffargis$LCZ_PRIMARY)

# 
# 
# test_list<-list(a=c(1,2),b="top",c=TRUE)
# length(test_list)
# for (i in test_list[2:3]) print(str(i))