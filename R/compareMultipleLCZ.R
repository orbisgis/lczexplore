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
  echIntnogeom$maxAgree<-apply(
    X = echIntnogeom[,1:length(sfList)], MARGIN = 1, function(x) max(table(x) ))
  echInt<-cbind(echIntnogeom,echInt$geometry)  %>% st_as_sf()
  echInt
  echIntLong<-pivot_longer(st_drop_geometry(echInt),cols=rangeCol, names_to = "whichWfs", values_to = "agree")
  echIntLong$LCZref<-substr(echIntLong$whichWfs,start = 1, stop=1 )
  print(head(echIntLong[,c(1,2,9:10)]))
  whichLCZagree <- names(echIntLong)[as.numeric(echIntLong$LCZref)]
  indRow<- seq_len(nrow(echIntLong))
  z<-data.frame(indRow, whichLCZagree)
  echIntLong$LCZvalue<-apply(z, 1, function(x) unlist(st_drop_geometry(echIntLong)[x[1], x[2]]))
  print(head(echIntLong[,c(1,2,9:11)]))
  
  output<-list(echInt=echInt, echIntLong=echIntLong)
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

test<-multicompare_test$echIntLong
test2<-test %>% subset(agree==TRUE) %>% group_by(LCZvalue) %>% summarize(agreementArea=sum(area)) %>% mutate(percAgreementArea=agreementArea/sum(agreementArea))

test<-multicompare_test$echInt[,1:5] %>% st_drop_geometry()
prov1<-apply(X = test, MARGIN = 1, table )
prov2<-apply(X = test, MARGIN = 1, function(x) max(table(x)) )

head(prov1)
head(prov2)

plot1<-showLCZ(sf = multicompare_test$echInt, column="LCZBDT22", wf="22")
plot2<-showLCZ(sf = multicompare_test$echInt, column="LCZBDT11", wf="11")

ggplot(data=multicompare_test$echInt) +
  geom_sf(aes(fill=maxAgree, color=after_scale(fill)))+
  scale_fill_gradient(low = "red" , high = "green", na.value = NA)

