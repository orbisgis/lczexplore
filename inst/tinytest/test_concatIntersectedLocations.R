dirList<-list.dirs("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree")[-1]
allLCZDirNames2022<-list.dirs("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/GeoClimate/2022")

osmTownNames2022<-gsub(
  pattern  = "(.*osm_)|(.)|(, Ile-de-France)",
  replacement = "\\2",
  x = grep(
    pattern = ".*osm_",
    x = allLCZDirNames2022,
    value = TRUE)
) %>% unique()



allLocIntersected<-concatIntersectedLocations(
  dirList = dirList[1:3],
  locations = osmTownNames2022)

intersected<-createIntersect(sfList = sfList, columns = rep("lcz_primary", 4),
                             sfWf = c("osm","bdt","iau","wudapt"))

multicompare_test<-compareMultipleLCZ(allLocIntersected,
                                      LCZcolumns = c("osm","bdt","iau","wudapt"),
                                      trimPerc = 0.5)