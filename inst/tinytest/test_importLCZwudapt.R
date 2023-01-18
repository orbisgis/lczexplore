#This tests teh function importLCZgen
# library(tinytest)
#
# library(sf)

redonBbox<-importLCZgen(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
  geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="bBox")

redonWudapt<-importLCZwudapt("/home/gousseff/Documents/2_CodesSources/Wudapt/WudaptEurope/",bBox=redonBbox)
redonWudapt<-importLCZwudapt(
  paste0(dirPath=system.file("extdata", package = "lczexplore"),"/"),bBox=redonBbox)
summary(redonWudapt)

showLCZ(sf=redonWudapt,column="EU_LCZ_map",repr="brut")
