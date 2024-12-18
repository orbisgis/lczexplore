# This tests the function createIntersect
library(tinytest)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cowplot)
library(forcats)

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
# showLCZ(sfList[[1]])



intersected<-createIntersect(sfList = sfList, columns = c(rep("LCZ_PRIMARY",4),"lcz_primary"),
                             sfWf = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"))
names(intersected) %>% reformulate()


# test_list<-list(a=c(1,2),b="top",c=TRUE)
# length(test_list)
# for (i in test_list[2:3]) print(str(i))

multicompare_test<-compareMultipleLCZ(intersected, 
                                      LCZcolumns = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"),trimPerc = 0.5)

testAreas<-workflowAgreeAreas(multicompare_test$sfIntLong)
testAreas$disagreeAreas
testAreas$agreeAreas

multicompare_test

test<-multicompare_test$sfIntLong
test2<-test %>% subset(agree==TRUE) %>% group_by(LCZvalue) %>% summarize(agreementArea=sum(area)) %>% 
  mutate(percAgreementArea=agreementArea/sum(agreementArea))

testWfAgree<-test %>% subset(agree==TRUE) %>% group_by(whichWfs) %>% summarize(agreementArea=sum(area))

test<-multicompare_test$sfInt[,1:5] %>% st_drop_geometry()
prov1<-apply(X = test, MARGIN = 1, table )
prov2<-apply(X = test, MARGIN = 1, function(x) max(table(x)) )

head(prov1)
head(prov2)

plot1<-showLCZ(sf = multicompare_test$sfInt, column="BDT22", wf="BDT22")
plot2<-showLCZ(sf = multicompare_test$sfInt, column="BDT11", wf="BDT1111")
plot3<-showLCZ(sf = multicompare_test$sfInt, column="OSM22", wf="OSM22")
plot4<-showLCZ(sf = multicompare_test$sfInt, column="WUDAPT", wf="WUDAPT")
plot5<-ggplot(data=multicompare_test$sfInt) +
  geom_sf(aes(fill=nbAgree, color=after_scale(fill)))+
  scale_fill_gradient(low = "red" , high = "green", na.value = NA)
plot_grid(plot1, plot2, plot3, plot4, plot5)
