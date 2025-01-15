# This tests the function createIntersect
library(tinytest)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(cowplot)
library(forcats)

sfList<-loadMultipleSfs(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Drancy/", 
                       workflowNames = c("osm","bdt","iau","wudapt"), location = "Drancy"  )

intersected<-createIntersect(sfList = sfList, columns = rep("lcz_primary", 4),
                             sfWf = c("osm","bdt","iau","wudapt"))

multicompare_test<-compareMultipleLCZ(intersected, 
                                      LCZcolumns = c("osm","bdt","iau","wudapt"),
                                      trimPerc = 0.5)

testAreas<-workflowAgreeAreas(multicompare_test$sfIntLong)
testAreas$disagreeAreas
testAreas$agreeAreas

osm<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                          file = "osm_lcz.fgb")
bdt<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                   file = "bdt_lcz.fgb")
iau<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                   file = "iau_lcz.fgb")
wudapt<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                   file = "wudapt_lcz.fgb")

# multicompare_test
# 
# test<-multicompare_test$sfIntLong
# test2<-test %>% subset(agree==TRUE) %>% group_by(LCZvalue) %>% summarize(agreementArea=sum(area)) %>% 
#   mutate(percAgreementArea=agreementArea/sum(agreementArea))
# 
# testWfAgree<-test %>% subset(agree==TRUE) %>% group_by(whichWfs) %>% summarize(agreementArea=sum(area))
# 
# test<-multicompare_test$sfInt[,1:5] %>% st_drop_geometry()
# prov1<-apply(X = test, MARGIN = 1, table )
# prov2<-apply(X = test, MARGIN = 1, function(x) max(table(x)) )
# 
# head(prov1)
# head(prov2)
# 
# plot1<-showLCZ(sf = multicompare_test$sfInt, column="BDT22", wf="BDT22")
# plot2<-showLCZ(sf = multicompare_test$sfInt, column="BDT11", wf="BDT1111")
# plot3<-showLCZ(sf = multicompare_test$sfInt, column="OSM22", wf="OSM22")
# plot4<-showLCZ(sf = multicompare_test$sfInt, column="WUDAPT", wf="WUDAPT")
# plot5<-ggplot(data=multicompare_test$sfInt) +
#   geom_sf(aes(fill=nbAgree, color=after_scale(fill)))+
#   scale_fill_gradient(low = "red" , high = "green", na.value = NA)
# plot_grid(plot1, plot2, plot3, plot4, plot5)
