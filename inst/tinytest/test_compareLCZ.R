# This tests the function compareLCZ
# library(tinytest)
#
# library(sf)



expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", saveG="", exwrite=FALSE, location="Redon", plot=TRUE),
          "Both sf datasets need to live in the same crs projection \\(srid / epsg\\),")


expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", saveG="", exwrite=TRUE, location="Redon", plot=TRUE),
               "\\(redonBDT\\)")
file.remove("bdtopo_2_2_osm.csv")


expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=F),
               "\\(redonOSM\\)")
file.remove("bdtopo_2_2_osm.csv")

# compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
#            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
#            repr="alter", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE, urban=c("1","2","3","4","5","6","7","8","9"),
#            tryGroup=TRUE, industry="10",
#            vegetation=c("101","102","103","104"),
#            impervious="105",pervious="106",water="107",
#            colors=c("orange","black","darkGreen","grey","burlywood","blue"))

expect_message(compareRedonBDTsquare<-
  compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
             sf2=redonBDT, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="bdt",
             repr="standard", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=F),
  "The column  LCZ_PRIMARY  of the dataset redonBDT is the reference against which the  LCZ_PRIMARY  column")

#names(compareRedonBDTOSM)
compareRedonBDTOSM<-
  compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
             sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
             repr="standard", saveG="", exwrite=TRUE, location="Redon", plot=TRUE)
expect_equal("data.frame"%in%class(compareRedonBDTOSM$matConf),TRUE)
expect_equal("data.frame"%in%class(compareRedonBDTOSM$areas),TRUE)
expect_equal("data.frame"%in%class(compareRedonBDTOSM$data),TRUE)
expect_equal("ggplot"%in%class(compareRedonBDTOSM$matConfPlot),TRUE)


#compareRedonBDTOSMPlot$matConfPlot %>% class


redonBbox<-importLCZvect(dirPath=paste0(
  system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon"),file="rsu_lcz.geojson",column="LCZ_PRIMARY",
  geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE",output="bBox")

redonWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
                             fileName = "redonWudapt.tif", bBox=redonBbox)

# system.file("extdata", package = "lczexplore","/redonWudapt.tif")
# redonWudapt %>% summary
# showLCZ(redonWudapt,column = "EU_LCZ_map")

expect_message(
compareRedonBDTwudaptPlot<-compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", wf1="bdtopo_2_2",
                                   sf2=redonWudapt, column2="EU_LCZ_map", wf2="wudapt",
                                   repr="standard", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE),
"they will be coerced to the specified reference \\(redonWudapt\\)"
)

######################################
### Test some grouped LCZ files
########################################


redonBDTgrouped<-groupLCZ(
  redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           colors=c("red","black","green","grey","burlywood","blue"))

redonOSMgrouped<-groupLCZ(
  redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
  industry="10",
  vegetation=c("101","102","103","104"),
  impervious="105",pervious="106",water="107",colors=c("red","black","green","grey","burlywood","blue"))

# levCol(redonOSMgrouped,"LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
#        industry="10",
#        vegetation=c("101","102","103","104"),
#        impervious="105",pervious="106",water="107",
#        colors=c("red","black","green","grey","burlywood","blue"))




expect_warning(compareRedonBDTOSMgrouped<-
  compareLCZ(sf1=redonBDTgrouped, column1="grouped", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
             sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
             repr="alter", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE,
             urban=c("1","2","3","4","5","6","7","8","9"),
             industry="10",
             vegetation=c("101","102","103","104"),
             impervious="105",pervious="106",water="107",
             colors=c("red","black","green","grey","burlywood","blue"),tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")

# showLCZ(redonBDTgrouped, column = "LCZ_PRIMARY", repr = "standard")


redonBDTgrouped2<-
  groupLCZ(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),outCol="groupedLCZ",
  industry="10",vegetation=c("101","102","103","104"),impervious="105",pervious="106",water="107",
            colors=c("red","black","green","grey","burlywood","blue"))
redonOSMgrouped2<-
  groupLCZ(redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),outCol="otherName",
            industry="10",vegetation=c("101","102","103","104"),impervious="105",pervious="106",water="107",
            colors=c("red","black","green","grey","burlywood","blue"))
redonWudaptGrouped<-  
  groupLCZ(redonWudapt,column="EU_LCZ_map",urban=c("1","2","3","4","5","6","7","8","9"),outCol="otherName",
                                industry="10",vegetation=c("101","102","103","104"),impervious="105",pervious="106",water="107",
                                colors=c("red","black","green","grey","burlywood","blue"))


expect_message(compareLCZ(sf1=redonBDTgrouped2, column1="groupedLCZ", wf1="BDT",
                          sf2=redonWudaptGrouped, column2="otherName", wf2="osm", exwrite=FALSE, repr="alter", plot=T,
                          urban="urban",industry="industry",vegetation="vegetation",
                          impervious="impervious",pervious="pervious",water="water",
                          colors=c("red","black","green","grey","burlywood","blue"), saveG=""),
               "they will be coerced to the specified reference \\(redonBDTgrouped2\\)"
)

expect_message(compareLCZ(sf1=redonBDTgrouped2, column1="groupedLCZ", wf1="BDT",
           sf2=redonOSMgrouped2, column2="otherName", wf2="osm", exwrite=FALSE, repr="alter", plot=T,
           urban="urban",industry="industry",vegetation="vegetation",impervious="impervious",pervious="pervious",water="water",
           colors=c("red","black","green","grey","burlywood","blue"), saveG=""),
               "they will be coerced to the specified reference \\(redonBDTgrouped2\\)"
)

expect_warning(compareRedonBDTOSMgrouped<-
                 compareLCZ(sf1=redonBDTgrouped, 
                            column1="grouped", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
                            sf2=redonOSM, 
                            column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
                            repr="alter", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE,
                            urban=c("1","2","3","4","5","6","7","8","9","chaussure"),
                            industry="10",
                            vegetation=c("101","102","103","104"),
                            impervious="105",pervious="106",water="107",
                            colors=c("red","black","green","grey","burlywood","blue"),tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")

expect_warning(compareRedonBDTOSMgrouped<-
                 compareLCZ(sf1=redonBDTgrouped, 
                            column1="grouped", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
                            sf2=redonOSM, 
                            column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
                            repr="alter", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE,
                            urban=c("1","2","3","4","5","6","7","8","chaussure"),
                            industry="10",
                            vegetation=c("101","102","103","104"),
                            impervious="105",pervious="106",water="107",
                            colors=c("red","black","green","grey","burlywood","blue"),tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")