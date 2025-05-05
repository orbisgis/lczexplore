library(sf)
sfList<-loadMultipleSfs(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Drancy/",
                       workflowNames = c("osm","bdt","iau","wudapt"), location = "Drancy"  )

test<-concatAlocationWorkflows(sfList = sfList, location = "Drancy", refCrs = 1)
write_sf(test, "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/testDrancy.fgb")

