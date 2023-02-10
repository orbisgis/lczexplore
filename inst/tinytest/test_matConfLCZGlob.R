#This tests teh function matConfLCZ
# library(tinytest)
#
# library(sf)
require(tidyr)

#showLCZ(redonBDT)
#showLCZ(redonOSM)
expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="brut", saveG="", exwrite=TRUE, location="Redon", plot=FALSE),
               "Both sf datasets need to live in the same crs projection \\(srid / epsg\\),")

expect_silent(
  testGlob<-matConfLCZGlob(filePath= paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2_osm.csv"),
                                          file="bdtopo_2_2_osm.csv", wf1="bdt", wf2="osm",
                                geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                         geomID2="ID_RSU.1", column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1", sep=";", repr="brut",
                         niveaux="", plot=T)

  )

testSource<-read.csv(paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2_osm.csv"), sep=";",header=T)

expect_silent(
matConfLCZGlob(filePath="",
               inputDf = testSource, wf1="bdt", wf2="osm",
               geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
               geomID2="ID_RSU.1", column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1", sep=";", repr="brut",
               niveaux="", plot=T)
)
