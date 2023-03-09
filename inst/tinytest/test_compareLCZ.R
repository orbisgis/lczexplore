# This tests teh function compareLCZ
# library(tinytest)
#
# library(sf)

# st_crs(redonBDT)
# st_crs(redonOSM)


expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="brut", saveG="", exwrite=TRUE, location="Redon", plot=TRUE),
          "Both sf datasets need to live in the same crs projection \\(srid / epsg\\),")

expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="brut", saveG="", exwrite=TRUE, location="Redon", plot=FALSE),
               "\\(redonBDT\\)")


expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="brut", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=F),
               "\\(redonOSM\\)")

expect_message(compareRedonBDTsquare<-
  compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
             sf2=redonBDT, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="bdt",
             repr="brut", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=F),
  "The column  LCZ_PRIMARY  of the dataset redonBDT is the reference against which the  LCZ_PRIMARY  column")

#names(compareRedonBDTOSM)
expect_equal(class(compareRedonBDTOSM$matConf),"data.frame")
expect_equal(class(compareRedonBDTOSM$aires),"data.frame")
expect_equal(class(compareRedonBDTOSM$data),"data.frame")
expect_equal(class(compareRedonBDTOSM$matConfPlot)[2],"ggplot")

compareRedonBDTOSMPlot<-compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
           sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
           repr="brut", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=TRUE)

expect_equal(class(compareRedonBDTOSM$matConfPlot)[2],"ggplot")

#compareRedonBDTOSMPlot$matConfPlot %>% class


redonBbox<-importLCZgen(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
  geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="bBox")

redonWudapt<-importLCZraster("/home/gousseff/Documents/2_CodesSources/Wudapt/WudaptEurope/",bBox=redonBbox)
# redonWudapt %>% summary
# showLCZ(redonWudapt,column = "EU_LCZ_map")

expect_message(
compareRedonBDTwudaptPlot<-compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", wf1="bdtopo_2_2",
                                   sf2=redonWudapt, column2="EU_LCZ_map", wf2="wudapt",
                                   repr="brut", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=TRUE),
"they will be coerced to the specified reference \\(redonWudapt\\)"
)

######################################
### Test some grouped LCZ files
########################################


redonBDTgrouped<-LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",cols=c("red","black","green","grey","burlywood","blue"))

redonOSMgrouped<-LCZgroup2(redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",cols=c("red","black","green","grey","burlywood","blue"))

compareRedonBDTOSMgrouped<-
  compareLCZ(sf1=redonBDTgrouped, column1="grouped", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
             sf2=redonOSMgrouped, column2="grouped", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
             repr="grouped", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=T,urban=c("1","2","3","4","5","6","7","8","9"),
             industry="10",
             vegetation=c("101","102","103","104"),
             impervious="105",pervious="106",water="107", cols=c("red","black","green","grey","burlywood","blue"))

