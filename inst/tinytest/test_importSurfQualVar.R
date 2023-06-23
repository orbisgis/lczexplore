#This tests the function importSurfQualVar
# library(tinytest)
#
library(sf)
library(dplyr)
# test<-st_read(
#   "/home/gousseff/Documents/2_CodesSources/R/lczexplore/lczexplore/inst/extdata/bdtopo_2_2/Redon/rsu_lcz.geojson")
# colonnes<-c("LCZ_PRIMARY","ID_RSU","LCZ_UNIQUENESS_VALU")
# tetest<-try(test[colonnes]) %>% class
# "try-error"%in%tetest

utrfRedonBDT<-importSurfQualVar(dirPath=paste0(
  system.file("extdata", package = "lczexplore"), "/bdtopo_2_2/Redon"),
  file="rsu_utrf_area.geojson", column="TYPO_MAJ")
  
showLCZ(sf=utrfRedonBDT, column="TYPO_MAJ",repr="alter")
  utrfRedonOSM<-importSurfQualVar(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/osm/2022/Redon"),
  file="rsu_utrf_area.geojson", column="TYPO_MAJ",geomID="ID_RSU",confid="UNIQUENESS_VALUE")
  
  utrfComparison<-compareLCZ(sf1=utrfRedonBDT, column1="TYPO_MAJ", sf2=utrfRedonOSM, column2="TYPO_MAJ",wf1=" UTRF BDT", wf2 = " UTRF OSM",
  location = " Redon",exwrite=FALSE,repr="alter")
  # Plot the confusion matrix of thes two classifications  
  print(utrfComparison$matConfPlot)




expect_silent(
  utrfRedonBDT<-importSurfQualVar(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), 
                                                         file="rsu_utrf_area.geojson", column="TYPO_MAJ",geomID="ID_RSU",confid="UNIQUENESS_VALUE")
)

showLCZ(utrfRedonBDT,column = "TYPO_MAJ",repr="alter")

expect_silent(
  utrfRedonOSM<-importSurfQualVar(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/osm/2022/Redon"),
                                  file="rsu_utrf_area.geojson", column="TYPO_MAJ",geomID="ID_RSU",confid="UNIQUENESS_VALUE")
)

#summary(st_geometry_type(utrfRedonOSM))
utrfComparison<-compareLCZ(sf1=utrfRedonBDT, column1="TYPO_MAJ", sf2=utrfRedonOSM, column2="TYPO_MAJ",wf1=" UTRF BDT", wf2 = " UTRF OSM", 
           location = " Redon",exwrite=FALSE,repr="alter")

utrfComparison$matConfPlot %>% print
utrfComparison$data
utrfComparison$matConf
utrfComparison$matConfLarge

library(tidyr)
pivot_wider(utrfComparison$matConf, names_from = TYPO_MAJ.1, values_from = agree)
expect_equal(utrfComparison$matConf[1,3],62.96)
# 
# expect_silent(importLCZgen(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon")
#                            ,file="rsu_lcz.geojson",
#                            column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"))
# 
# 
# # Tests if the imported version of Redon test data matches the Redon test data in the package
# redonBDT2<-importLCZgen(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),
#   column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE")
# st_crs(redonBDT2)$wkt<-gsub("é","e",st_crs(redonBDT2)$wkt)
# 
# expect_identical(redonBDT,redonBDT2)
# 
# # Error if file doesn't exist in location
# 
# expect_error(importLCZgen(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="chaussure.geojson",
#   column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"),"The file doesn't seem to exist.")
# 
# ## Error if directory doesn't exist
# 
# 
# expect_error(importLCZgen(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redono"),file="rsu_lcz.geojson",
#   column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"),
#   "The directory set in dirPath doesn't seem to exist")
# 
# # test if one column doesn't exist
# 
# expect_error(
# importLCZgen(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",
#   column="LCZ_PRIMAR",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"),
#   "It seems that some of the columns you try to import do not exist in the source file")
# 
# expect_error(
#   importLCZgen(dirPath=paste0(
#     system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",
#     column="LCZ_PRIMARY",geomID="ID_RSY",confid="LCZ_UNIQUENESS_VALUE"),
#   "It seems that some of the columns you try to import do not exist in the source file")
# 
# expect_error(
#   importLCZgen(dirPath=paste0(
#     system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",
#     column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALU"),
#   "It seems that some of the columns you try to import do not exist in the source file")
# 
# # test if the column argument is missing
# expect_error(
#   importLCZgen(dirPath=paste0(
#     system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="",
#     geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"),
#   "You must specify the column containing the LCZ")
# 
# # test if the output is a bounding box or a sfFile and if none other output is asked for
# #
# # test<-class(importLCZgen(dirPath=paste0(
# #   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
# #   geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="bBox"))
# 
# expect_equal(class(importLCZgen(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
#   geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="sfFile")),
#   c("sf","data.frame"))
# 
# expect_equal(class(importLCZgen(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
#   geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="bBox")),
#   c("sfc_POLYGON","sfc"))
# 
# expect_error(importLCZgen(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
#   geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="chaussure de ski"),
# "Output must be sfFile to return geoms and LCZ or bBox to return the bounding box")
# 
# 
# # test what happens if the levels of LCZ are not coherent ?
# expect_warning(importLCZgen(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),
#              file="rsu_lcz.geojson", output="sfFile", column="LCZ_PRIMARY",
#              geomID="", confid="",
#              typeLevels=c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
#                        "9"="9","10"="10","101"="101","102"="102","103"="103","104"="104",
#                        "105"="105","106"="106","101"="11","102"="12","103"="13","104"="14",
#                        "105"="15", "106"="16","107"="17"),drop=T),
#              "The levels you specified with the typeLevels argument don't cover the LCZ values")