test<-intersectAlocation(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Argenteuil/", 
                   workflowNames = c("osm","bdt","iau","wudapt"), location = "Argenteuil",
                             addMissingRSUs = TRUE,
                             missingGeomsWf="iau", refWf = NULL, refLCZ = "Unclassified",
                             residualLCZvalue = "Unclassified",
                             column = "lcz_primary")