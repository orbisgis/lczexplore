library(tinytest)


############################################
##
## Usage with outputs of compareLCZ directed to a csv rather than to a R file 
## is only useful for comparison on many locations
## It is still possible but not tested here as compareLCZ now ooutputs the data needed to proceed 
## 
############################################

redonCompare<-compareLCZ(sf1=redonBDT,wf1="bdt", geomID1 = "ID_RSU", column1 ="LCZ_PRIMARY",
                         confid1 = "LCZ_UNIQUENESS_VALUE",
                         sf2=redonOSM,wf2="osm",geomID2 = "ID_RSU", column2="LCZ_PRIMARY",
                         confid2 ="LCZ_UNIQUENESS_VALUE", exwrite=FALSE, plot=FALSE)



expect_warning(
confidTest1<-confidSensib(inputDf=redonCompare$data, filePath="", nPoints=5,
                       wf1="bdtopo_2_2", wf2="osm",
                       geomID1="ID_RSU", column1="bdt", confid1="LCZ_UNIQUENESS_VALUE",
                       geomID2="ID_RSU.1",column2="osm", confid2="LCZ_UNIQUENESS_VALUE.1",
                       sep=";", repr="standard",
                       plot=TRUE, saveG=""),
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
                           plot=TRUE,  saveG=""),
  "containing missing values")

confidTest1<-confidSensib(inputDf=redonCompare$data, filePath="", nPoints=5,
                          wf1="bdtopo_2_2", wf2="osm",
                          geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                          geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
                          sep=";", repr="standard",
                          plot=TRUE, saveG="")
confidTest2<-confidSensib(filePath=paste0(
  system.file("extdata", package = "lczexplore"),
  "/bdtopo_2_2_osm.csv"),
                          nPoints=5,
                          wf1="bdtopo_2_2", wf2="osm",
                          geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
                          geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
                          sep=";", repr="standard",
                          plot=TRUE,  saveG="")

expect_equal(confidTest1,confidTest1)