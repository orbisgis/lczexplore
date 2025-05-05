#This tests the function importLCZvect
# library(tinytest)
#
library(sf)
# test<-st_read(
#   "/home/gousseff/Documents/2_CodesSources/R/lczexplore/lczexplore/inst/extdata/bdtopo_2_2/Redon/rsu_lcz.geojson")
# colonnes<-c("LCZ_PRIMARY","ID_RSU","LCZ_UNIQUENESS_VALU")
# tetest<-try(test[colonnes]) %>% class
# "try-error"%in%tetest




expect_silent(importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),
                          column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",verbose=T))

expect_silent(importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon")
                           ,file="rsu_lcz.geojson",
                           column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"))


# Tests if the imported version of Redon test data matches the Redon test data in the package
redonBDT2<-importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"), "/bdtopo_2_2/Redon"),
  column="LCZ_PRIMARY", geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE", naAsUnclassified = FALSE)
st_crs(redonBDT2)$wkt<-gsub("é","e",st_crs(redonBDT2)$wkt)

summary(redonBDT$LCZ_PRIMARY)
summary(redonBDT2$LCZ_PRIMARY)

expect_identical(redonBDT,redonBDT2)

# Error if file doesn't exist in location

expect_error(importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), file="chaussure.geojson",
  column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"),"The file doesn't seem to exist.")

## Error if directory doesn't exist


expect_error(importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"), "/bdtopo_2_2/Redono"), file="rsu_lcz.geojson",
  column="LCZ_PRIMARY", geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE"),
  "The directory set in dirPath doesn't seem to exist")

# test if one column doesn't exist

expect_error(
importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"), "/bdtopo_2_2/Redon"), file="rsu_lcz.geojson",
  column="LCZ_PRIMAR",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE"),
  "It seems that some of the columns you try to import do not exist in the source file")

expect_error(
  importLCZvect(dirPath=paste0(
    system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), file="rsu_lcz.geojson",
    column="LCZ_PRIMARY",geomID="ID_RSY",confid="LCZ_UNIQUENESS_VALUE"),
  "It seems that some of the columns you try to import do not exist in the source file")

expect_error(
  importLCZvect(dirPath=paste0(
    system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), file="rsu_lcz.geojson",
    column="LCZ_PRIMARY", geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALU"),
  "It seems that some of the columns you try to import do not exist in the source file")

# test if the column argument is missing
expect_error(
  importLCZvect(dirPath=paste0(
    system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), file="rsu_lcz.geojson", column="",
    geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE"),
  "You must specify the column containing the LCZ")

# test if the output is a bounding box or a sfFile and if none other output is asked for
#
# test<-class(importLCZvect(dirPath=paste0(
#   system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
#   geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="bBox"))

expect_equal(class(importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), file="rsu_lcz.geojson",column="LCZ_PRIMARY",
  geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE",output="sfFile")),
  c("sf","data.frame"))

expect_equal(class(importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), file="rsu_lcz.geojson",column="LCZ_PRIMARY",
  geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE", output="bBox")),
  c("sfc_POLYGON","sfc"))

expect_error(importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"), file="rsu_lcz.geojson", column="LCZ_PRIMARY",
  geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE", output="chaussure de ski"),
"Output must be sfFile to return geoms and LCZ or bBox to return the bounding box")


# test what happens if the levels of LCZ are not coherent ?
expect_warning(importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"), "/bdtopo_2_2/Redon"),
             file="rsu_lcz.geojson", output="sfFile", column="LCZ_PRIMARY",
             geomID="", confid="",
             typeLevels=c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                       "9"="9","10"="10","101"="101","102"="102","103"="103","104"="104",
                       "105"="105","106"="106","101"="11","102"="12","103"="13","104"="14",
                       "105"="15", "106"="16","107"="17"),drop=T),
             "The levels you specified with the typeLevels argument don't cover the LCZ values")


# test if the drop argument allows to keep or drop comun other than specified
test<-importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",
              column="LCZ_PRIMARY", geomID="ID_RSU", confid="LCZ_UNIQUENESS_VALUE", drop=FALSE)
expect_equal("LCZ_SECONDARY"%in%names(test),TRUE)

# Special test with a flatgeobuffer file. 
test<-importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),
                            column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",verbose=T)
if (file.exists("test.fgb")) file.remove("test.fgb")
write_sf(test, "test.fgb")
expect_silent(test<-importLCZvect(dirPath=getwd(),file="test.fgb",
                    column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",verbose=T))
rm(test)
if (file.exists("test.fgb")) file.remove("test.fgb")
