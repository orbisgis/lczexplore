library(dplyr)
library(sf)
library(ggplot2)

sfList<-loadMultipleSfs(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Les Mureaux/",
                       workflowNames = c("osm","bdt","iau","wudapt"), location = "Les Mureaux"  )
