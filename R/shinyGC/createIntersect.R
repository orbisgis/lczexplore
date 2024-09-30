createIntersec<-function(sfList, columns, refCrs=NULL, sfWf=NULL){
  intersec_sf<-sfList[[1]] %>% select(columns[1])
  if (is.null(refCrs)){refCrs<-st_crs(intersec_sf)}
  for (i in 2:length(sfList)){
    sfProv<-sfList[[i]] %>% select(columns[i])
    if (st_crs(sfProv) != refCrs ) {sfProv<-st_transform(sfProv, crs=refCrs)}
    intersec_sf<-st_intersection(intersec_sf,sfProv)
  }
  if (!is.null(sfWf) & length(sfWf) == length(sfList)){
    names(intersec_sf)[1:(ncol(intersec_sf)-1)]<-paste0("LCZ",sfWf)
  } else { names(intersec_sf)[1:(ncol(intersec_sf)-1)]<-paste0("LCZ",1:length(sfList)) }
  intersec_sf 
}

# sfBDT_11_78030<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/BDT/2011/bdtopo_2_78030",
#                    file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# class(sfBDT_11_78030)
# sfBDT_22_78030<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/BDT/2022/bdtopo_3_78030",
#                               file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# sf_OSM_11_Auffargis<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/OSM/2011/osm_Auffargis/",
#                                    file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# sf_OSM_22_Auffargis<-importLCZvect(dirPath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/OSM/2022/osm_Auffargis/",
#                                    file="rsu_lcz.fgb", column="LCZ_PRIMARY")
# sf_WUDAPT_78030<-importLCZvect("/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/WUDAPT", 
#                                file ="wudapt_78030.geojson", column="lcz_primary")
# 
# sfList<-list(BDT11 = sfBDT_11_78030, BDT22 = sfBDT_22_78030, OSM11= sf_OSM_11_Auffargis, OSM22 = sf_OSM_22_Auffargis, 
#              WUDAPT = sf_WUDAPT_78030)
# showLCZ(sfList[[1]])
# 
# 
# 
# intersected<-createIntersec(sfList = sfList, columns = c(rep("LCZ_PRIMARY",4),"lcz_primary"), 
#                             sfWf = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"))
# 
# 
# test_list<-list(a=c(1,2),b="top",c=TRUE)
# length(test_list)
# for (i in test_list[2:3]) print(str(i))