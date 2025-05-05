library(dplyr)
library(sf)
library(ggplot2)

sfList<-loadMultipleSfs(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Goussainville"),
                       workflowNames = c("osm","bdt","iau","wudapt"), location = "Goussainville"  )
