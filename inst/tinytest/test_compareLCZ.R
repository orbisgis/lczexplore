# This tests the function compareLCZ
# library(tinytest)
#
# library(sf)

######################################
### Test value
#######################################

redonBDT2<-importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon"),
                         file = "bdt_lcz.fgb",
                         column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",verbose=T)


autocompareBDT<-compareLCZ(sf1 = redonBDT, sf2 = redonBDT2)
compareLCZ(sf1 = redonBDT, sf2 = redonBDT2)

expect_true(unlist(autocompareBDT$percAgg)==100)

redonOSM2<-importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon"),
                         file = "osm_lcz.fgb",
                         column="LCZ_PRIMARY",geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",verbose=T)
autocompareOSM<-compareLCZ(sf1 = redonOSM, sf2 = redonOSM2)
expect_true(unlist(autocompareOSM$percAgg)==100)

OsmBdtCompare<-compareLCZ(sf1 = redonOSM, sf2 = redonBDT)
test_value<-round(
  OsmBdtCompare$matConf[
    redonOSM == 9 & redonBDT == 9,"agreePercArea"], 0)
expect_true(test_value==71)

test_value<-round(
  OsmBdtCompare$matConf[
    redonOSM == 6 & redonBDT == 9,"agreePercArea"], 0)
expect_true(test_value==46)

OsmBdtCompare$percAgg

######################################
### Test some grouped LCZ files
########################################

expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdt_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", saveG="", exwrite=FALSE, location="Redon", plotNow = TRUE, confPlot = "sankey"),
          "Both sf datasets need to live in the same crs projection \\(srid / epsg\\),")

expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", saveG="", exwrite=FALSE, location="Redon", plotNow = TRUE, minZeroArea = 0.00),
               "Both sf datasets need to live in the same crs projection \\(srid / epsg\\),")

expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", saveG="", exwrite=TRUE, location="Redon", plotNow = TRUE),
               "\\(redonBDT\\)")
if (file.exists("bdtopo_2_2_osm.csv")) {file.remove("bdtopo_2_2_osm.csv")}


expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", ref=2, saveG="", exwrite=TRUE, location="Redon", plotNow = T, confPlot = "sankey"),
               "\\(redonOSM\\)")
if (file.exists("bdtopo_2_2_osm.csv")) {file.remove("bdtopo_2_2_osm.csv")}

# compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
#            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
#            repr="alter", ref=2, saveG="", exwrite=FALSE, location="Redon", plotNow = TRUE, urban=c("1","2","3","4","5","6","7","8","9"),
#            tryGroup=TRUE, industry="10",
#            vegetation=c("101","102","103","104"),
#            impervious="105",pervious="106",water="107",
#            colors=c("orange","black","darkGreen","grey","burlywood","blue"))

expect_message(compareRedonBDTsquare<-
  compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
             sf2=redonBDT, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="bdt",
             repr="standard", ref=2, saveG="", exwrite=FALSE, location="Redon", plotNow = F),
  "The column  LCZ_PRIMARY  of the dataset redonBDT is the reference against which the  LCZ_PRIMARY  column")

#names(compareRedonBDTOSM)
compareRedonBDTOSM<-
  compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
             sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
             repr="standard", saveG="", exwrite=TRUE, location="Redon", plotNow = TRUE)
expect_equal("data.frame"%in%class(compareRedonBDTOSM$matConf),TRUE)
expect_equal("data.frame"%in%class(compareRedonBDTOSM$areas),TRUE)
expect_equal("data.frame"%in%class(compareRedonBDTOSM$data),TRUE)
expect_equal("ggplot"%in%class(compareRedonBDTOSM$matConfPlot),TRUE)


#compareRedonBDTOSMPlot$matConfPlot %>% class


redonBbox <- importLCZvect(dirPath = paste0(
  system.file("extdata", package = "lczexplore"), "/multipleWfs/Redon"), file = "bdt_lcz.fgb", column = "LCZ_PRIMARY",
                           geomID = "ID_RSU", confid = "LCZ_UNIQUENESS_VALUE", output = "bBox")

redonWudapt <- importLCZraster(system.file("extdata", package = "lczexplore"),
                               fileName = "redonWudapt.tif", bBox = redonBbox)

# system.file("extdata", package = "lczexplore","/redonWudapt.tif")
# redonWudapt %>% summary
# showLCZ(redonWudapt,column = "EU_LCZ_map")

expect_message(
compareRedonBDTwudaptPlot<-compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", wf1="bdtopo_2_2",
                                   sf2=redonWudapt, column2="EU_LCZ_map", wf2="wudapt",
                                   repr="standard", ref=2, saveG="", exwrite=FALSE, location="Redon", plotNow = TRUE),
"they will be coerced to the specified reference \\(redonWudapt\\)"
)

######################################
### Test some grouped LCZ files
########################################


redonBDTgrouped <- groupLCZ(
  redonBDT, column = "LCZ_PRIMARY", urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
  industry = "10",
  vegetation = c("101", "102", "103", "104"),
  impervious = "105", pervious = "106", water = "107",
  colors = c("red", "black", "green", "grey", "burlywood", "blue"))

redonOSMgrouped <- groupLCZ(
  redonOSM, column = "LCZ_PRIMARY", urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
  industry = "10",
  vegetation = c("101", "102", "103", "104"),
  impervious = "105", pervious = "106", water = "107", colors = c("red", "black", "green", "grey", "burlywood", "blue"))

# levCol(redonOSMgrouped,"LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
#        industry="10",
#        vegetation=c("101","102","103","104"),
#        impervious="105",pervious="106",water="107",
#        colors=c("red","black","green","grey","burlywood","blue"))


# useless as it doesn't use grouped columns, could be done from original Redon sf objects

expect_warning(compareRedonBDTOSMgrouped <-
                 compareLCZ(sf1 = redonBDTgrouped, column1 = "LCZ_PRIMARY", geomID1 = "ID_RSU",
                            confid1 = "LCZ_UNIQUENESS_VALUE", wf1 = "groupedBDT",
                            sf2 = redonOSM, column2 = "LCZ_PRIMARY", geomID2 = "ID_RSU",
                            confid2 = "LCZ_UNIQUENESS_VALUE", wf2 = "groupedOSM",
                            repr = "alter", ref = 2, saveG = "", exwrite = FALSE, location = "Redon",
                            urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                            industry = "10",
                            vegetation = c("101", "102", "103", "104"),
                            impervious = "105", pervious = "106", water = "107",
                            colors = c("red", "black", "green", "grey", "burlywood", "blue"),
                            tryGroup = TRUE, plotNow = TRUE, confPlot = "sankey"),
               "attribute variables are assumed to be spatially constant throughout all geometries")

# showLCZ(redonBDTgrouped, column = "LCZ_PRIMARY", repr = "standard")


redonBDTgrouped2 <-
  groupLCZ(redonBDT, column = "LCZ_PRIMARY", urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), outCol = "groupedLCZ",
           industry = "10", vegetation = c("101", "102", "103", "104"), impervious = "105", pervious = "106", water = "107",
           colors = c("red", "black", "green", "grey", "burlywood", "blue"))
redonOSMgrouped2 <-
  groupLCZ(redonOSM, column = "LCZ_PRIMARY", urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), outCol = "otherName",
           industry = "10", vegetation = c("101", "102", "103", "104"), impervious = "105", pervious = "106", water = "107",
           colors = c("red", "black", "green", "grey", "burlywood", "blue"))
redonWudaptGrouped <-
  groupLCZ(redonWudapt, column = "EU_LCZ_map", urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), outCol = "otherName",
           industry = "10", vegetation = c("101", "102", "103", "104"), impervious = "105", pervious = "106", water = "107",
           colors = c("red", "black", "green", "grey", "burlywood", "blue"))


expect_message(compareLCZ(sf1=redonBDTgrouped2, column1="groupedLCZ", wf1="BDT",
                          sf2=redonWudaptGrouped, column2="otherName", wf2="osm", exwrite=FALSE, repr="alter", plotNow = T,
                          urban="urban",industry="industry",vegetation="vegetation",
                          impervious="impervious",pervious="pervious",water="water",
                          colors=c("red","black","green","grey","burlywood","blue"), saveG="", plotNow = FALSE),
               "they will be coerced to the specified reference \\(redonBDTgrouped2\\)"
)

expect_message(compareLCZ(sf1=redonBDTgrouped2, column1="groupedLCZ", wf1="BDT",
           sf2=redonOSMgrouped2, column2="otherName", wf2="osm", exwrite=FALSE, repr="alter", plotNow = T,
           urban="urban",industry="industry",vegetation="vegetation",impervious="impervious",pervious="pervious",water="water",
           colors=c("red","black","green","grey","burlywood","blue"), saveG="", plotNow = FALSE),
               "they will be coerced to the specified reference \\(redonBDTgrouped2\\)"
)

expect_warning(compareRedonBDTOSMgrouped <-
                 compareLCZ(sf1 = redonBDTgrouped,
                            column1 = "grouped", geomID1 = "ID_RSU", confid1 = "LCZ_UNIQUENESS_VALUE", wf1 = "groupedBDT",
                            sf2 = redonOSM,
                            column2 = "LCZ_PRIMARY", geomID2 = "ID_RSU", confid2 = "LCZ_UNIQUENESS_VALUE", wf2 = "groupedOSM",
                            repr = "alter", ref = 2, saveG = "", exwrite = FALSE, location = "Redon", plotNow = FALSE,
                            urban = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "chaussure"),
                            industry = "10",
                            vegetation = c("101", "102", "103", "104"),
                            impervious = "105", pervious = "106", water = "107",
                            colors = c("red", "black", "green", "grey", "burlywood", "blue"), tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")

expect_warning(compareRedonBDTOSMgrouped <-
                 compareLCZ(sf1 = redonBDTgrouped,
                            column1 = "grouped", geomID1 = "ID_RSU", confid1 = "LCZ_UNIQUENESS_VALUE", wf1 = "groupedBDT",
                            sf2 = redonOSM,
                            column2 = "LCZ_PRIMARY", geomID2 = "ID_RSU", confid2 = "LCZ_UNIQUENESS_VALUE", wf2 = "groupedOSM",
                            repr = "alter", ref = 2, saveG = "", exwrite = FALSE, location = "Redon", plotNow = FALSE,
                            urban = c("1", "2", "3", "4", "5", "6", "7", "8", "chaussure"),
                            industry = "10",
                            vegetation = c("101", "102", "103", "104"),
                            impervious = "105", pervious = "106", water = "107",
                            colors = c("red", "black", "green", "grey", "burlywood", "blue"), tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")