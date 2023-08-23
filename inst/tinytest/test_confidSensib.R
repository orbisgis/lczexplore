# library(tinytest)
# system.file("extdata/", package = "lczexplore") %>% list.files()
# system.file("extdata/bdtopo_2_2", package = "lczexplore") %>% list.files()
# file.exists(system.file("extdata/bdtopo_2_2/Redon", package = "lczexplore"))
#
# system.file("extdata/osm/2022", package = "lczexplore") %>% list.files()
#
#
# confidSensib

mainPath<-system.file("extdata", package = "lczexplore")
testSourceFact<-read.csv(paste0(mainPath,"/bdtopo_2_2_osm.csv"), sep=";",header=T,stringsAsFactors = T)
expect_warning(
confidTest1<-confidSensib(inputDf=testSourceFact, filePath="", nPoints=5,
                       wf1="bdtopo_2_2", wf2="osm",
                       geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                       geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
                       sep=";", repr="standard",
                       plot=TRUE, saveG=mainPath),
"containing missing values")

expect_warning(
  confidTest2<-confidSensib(filePath=paste0(
    system.file("extdata", package = "lczexplore"),
    "/bdtopo_2_2_osm.csv"), 
                           nPoints=5,
                           wf1="bdtopo_2_2", wf2="osm",
                           geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                           geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
                           sep=";", repr="standard",
                           plot=TRUE,  saveG=mainPath),
  "containing missing values")

expect_equal(confidTest1,confidTest2)