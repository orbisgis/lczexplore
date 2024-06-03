compareMultipleLCZ<-function(sfList, columns, refCrs=NULL, sfWf=NULL, trimPerc=0.05){
  echInt<-createIntersec(sfList = sfList, columns = columns , refCrs= refCrs, sfWf = sfWf)
  print(nrow(echInt))
  echInt$area<-st_area(echInt)
  echInt <- echInt %>% subset(area>quantile(echInt$area, probs=trimPerc) & !is.na(area))
  print(nrow(echInt))
  echIntnogeom<-st_drop_geometry(echInt)
  for (i in 1:(length(sfList) - 1)) {
    for(j in (i+1):length(sfList)){
      compName<-paste0(i,"_",j)
      print(compName)
      echIntnogeom[,compName]<-echIntnogeom[,i] == echIntnogeom[,j]
    }
  }
  rangeCol<-(length(listSfs)+3):ncol(echIntnogeom)
  print(rangeCol)
  # print(names(echIntnogeom[,rangeCol]))
  echIntnogeom$nbAgree<-apply(echIntnogeom[,rangeCol],MARGIN=1,sum)
  echInt<-cbind(echIntnogeom,echInt$geometry)  %>% st_as_sf()
  echInt
  
  return(echInt)
  # print(length(listSfs)+2:(ncol(echInt)-1))
  # echInt
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

multicompare_test<-compareMultipleLCZ(sfList = sfList, columns = c(rep("LCZ_PRIMARY",4),"lcz_primary"),
                                      sfWf = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"),trimPerc = 0.5)
multicompare_test

plot1<-showLCZ(sf = multicompare_test, column="LCZBDT22", wf="22")
plot2<-showLCZ(sf = multicompare_test, column="LCZBDT11", wf="11")

ggplot(data=multicompare_test) +
  geom_sf(aes(fill=as.factor(nbAgree), color=after_scale(fill)))

