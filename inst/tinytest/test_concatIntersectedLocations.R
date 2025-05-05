dirList<-list.dirs("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree")[-1]
library(dplyr)
# allLCZDirNames2022<-list.dirs("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2022")
# 
# osmTownNames2022<-gsub(
#   pattern  = "(.*osm_)|(.)|(, Ile-de-France)",
#   replacement = "\\2",
#   x = grep(
#     pattern = ".*osm_",
#     x = allLCZDirNames2022,
#     value = TRUE)
# ) %>% unique()
# 
# allLocationNamesSource<-read.csv(
#   "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/List_by_dep/List_by_dep_concat.csv",
#   sep="\t", header = TRUE)
# allLocationCodes<-allLocationNamesSource$insee %>% as.character
# allLocationNames<-gsub(
#   pattern  = "(.)|(, Ile-de-France)",
#   replacement = "\\1",
#   x = allLocationNamesSource$Nom) %>% sort

# 
# allLocIntersected<-concatIntersectedLocations(
#   dirList = dirList[1:3],
#   locations = allLocationNames[1:3])
# 
# dirList[1]
# nchar(dirList[1])
# substring(dirList[1], nchar(dirList[1]), nchar(dirList[1]))
# 
# checkDirSlash(dirList[1])
# 
# checkDirSlash("test/test")
# sfList<-loadMultipleSfs(dirList[1])
# 
# intersected<-createIntersect(sfList = sfList, columns = rep("lcz_primary", 4),
#                              sfWf = c("osm","bdt","iau","wudapt"))
# 
# multicompare_test<-compareMultipleLCZ(allLocIntersected,
#                                       LCZcolumns = c("osm","bdt","iau","wudapt"),
#                                       trimPerc = 0.5)