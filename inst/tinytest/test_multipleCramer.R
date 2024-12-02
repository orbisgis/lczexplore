# This tests the function multipleCramer
# library(tinytest)
#
# library(sf)

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
intersectedSample


testVs<-multipleCramer(intersected, columns = names(intersected)[names(intersected)!="geometry"], nbOutAssociations = 5)
testVs$signifAssoc
str(testVs)

names6<-grep(x = rownames(testVs), pattern = "6$", value = TRUE)

testVsSampled<-testVs[1:6,1:6]

testMask<-testVsSampled
ncol(testVsSampled)
nrow(testVsSampled)
testMask<-(testMask>0.004 & testMask<1)
testMask[!testMask]<-NA

keptRows<-apply(
  X = apply(X = testMask, MARGIN = 1, is.na), 
  MARGIN = 1, sum)<ncol(testVs)
keptCols<-apply(
  X = apply(X = testMask, MARGIN = 2, is.na ),
  MARGIN = 2, sum)<nrow(testVs)
signifAssoc<-testVsSampled[keptRows, keptCols]
signifAssoc[(signifAssoc>=1 | signifAssoc<0.004)]<-NA

