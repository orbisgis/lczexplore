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
                             sfWf = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"), minZeroArea=0.1)
summary(intersected$area)
min(intersected$area)
intersected[intersected$area==0,]

library(caret)
library(confintr)
testVs<-multipleCramer(intersected, 
                       columns = names(intersected)[names(intersected)!="geometry" &names(intersected)!="area"],
                       nbOutAssociations = 30)
# testVs$signifAssoc
# testVs$cramerLong %>% head(10)
# 
# intersectedDf<-st_drop_geometry(intersected)
# str(intersectedDf)
# summary(intersectedDf$area)
# min(intersectedDf$area)
# names(intersectedDf)
# dataTest<-intersectedDf[ ,names(intersectedDf)!="area"]
# dataTest <- as.data.frame(lapply(X = dataTest, factor))
# summary(dataTest)
# str(dataTest)
# weights<-(intersectedDf$area/sum(intersectedDf$area))
# length(weights)
# auffargisMCA<-MCA(X = dataTest[, names(dataTest)!="area"], ncp = 5, row.w = weights)
# plot.MCA(auffargisMCA, invisible = c("ind"))
# 
# dataTestNo107<-dataTest[apply(dataTest, 1, function(x) all(x!="107")),]
# nrow(dataTestNo107)
# weightsNo107<-(intersectedDf$area/sum(intersectedDf$area))[
#   apply(dataTest, 1, function(x) all(x!="107"))]
# length(weightsNo107)
# 
# auffargisMCANo107<-MCA(X = dataTestNo107[,names(dataTest)!="area"], ncp = 10, graph = FALSE)
# # plot.MCA(auffargisMCANo107, invisible= c("ind"))
# auffargisMCANo107Weights<-MCA(X = dataTestNo107[,names(dataTest)!="area"], ncp = 10, row.w = weightsNo107, graph = FALSE)
# # plot.MCA(auffargisMCANo107Weights, invisible= c("ind"))
# # plot.MCA(auffargisMCANo107Weights, invisible= c("ind"), axes=c(3,4))
# # plot.MCA(auffargisMCANo107Weights, invisible= c("ind"), axes=c(5,6))
# 
# library(factoextra)
# fviz_mca_var(
#   auffargisMCANo107Weights,
#   choice = c("var.cat"),
#   axes = c(1, 2),
#   geom = c("point", "text"),
#   repel = TRUE,
#   col.var = "red",
#   alpha.var = 1,
#   shape.var = 17,
#   col.quanti.sup = "blue",
#   col.quali.sup = "darkgreen",
#   map = "symmetric",
#   select.var = list(name = NULL, cos2 = NULL, contrib = NULL)
# )
# 
# fviz_mca_var(
#   auffargisMCANo107Weights,
#   choice = c("var.cat"),
#   axes = c(3, 4),
#   geom = c("point", "text"),
#   repel = TRUE,
#   col.var = "red",
#   alpha.var = 1,
#   shape.var = 17,
#   col.quanti.sup = "blue",
#   col.quali.sup = "darkgreen",
#   map = "symmetric",
#   select.var = list(name = NULL, cos2 = NULL, contrib = NULL)
# )
# 
# fviz_mca_var(
#   auffargisMCANo107Weights,
#   choice = c("var.cat"),
#   axes = c(5, 6),
#   geom = c("point", "text"),
#   repel = TRUE,
#   col.var = "red",
#   alpha.var = 1,
#   shape.var = 17,
#   col.quanti.sup = "blue",
#   col.quali.sup = "darkgreen",
#   map = "symmetric",
#   select.var = list(name = NULL, cos2 = NULL, contrib = NULL)
# )
# 
# 
# str(auffargisMCANo107)
# 

