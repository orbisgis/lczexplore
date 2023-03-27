# This tests the function compareLCZ
# library(tinytest)
#
# library(sf)

# st_crs(redonBDT)
# st_crs(redonOSM)


expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", saveG="", exwrite=TRUE, location="Redon", plot=TRUE),
          "Both sf datasets need to live in the same crs projection \\(srid / epsg\\),")
file.remove("bdtopo_2_2_osm.csv")

expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", saveG="", exwrite=TRUE, location="Redon", plot=FALSE),
               "\\(redonBDT\\)")
file.remove("bdtopo_2_2_osm.csv")

expect_message(compareRedonBDTOSM<-
                 compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
                            repr="standard", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=F),
               "\\(redonOSM\\)")
file.remove("bdtopo_2_2_osm.csv")

# compareRedonBDTOSM<-
#   compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
#              sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
#              repr="standard", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=T)

expect_message(compareRedonBDTsquare<-
  compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
             sf2=redonBDT, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="bdt",
             repr="standard", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=F),
  "The column  LCZ_PRIMARY  of the dataset redonBDT is the reference against which the  LCZ_PRIMARY  column")
file.remove("bdtopo_2_2_bdt.csv")
#names(compareRedonBDTOSM)
expect_equal(class(compareRedonBDTOSM$matConf),"data.frame")
expect_equal(class(compareRedonBDTOSM$areas),"data.frame")
expect_equal(class(compareRedonBDTOSM$data),"data.frame")
expect_equal(class(compareRedonBDTOSM$matConfPlot),"ggplot_built")

compareRedonBDTOSM<-
  compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="bdtopo_2_2",
           sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="osm",
           repr="standard", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE)
expect_equal(file.remove("bdtopo_2_2_OSM.csv"),FALSE)

expect_equal(class(compareRedonBDTOSM$matConfPlot),"ggplot_built")

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
                                   repr="standard", ref=2, saveG="", exwrite=TRUE, location="Redon", plot=TRUE),
"they will be coerced to the specified reference \\(redonWudapt\\)"
)
file.remove("bdtopo_2_2_wudapt.csv")
######################################
### Test some grouped LCZ files
########################################


redonBDTgrouped<-LCZgroup2(
  redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
                           industry="10",
                           vegetation=c("101","102","103","104"),
                           impervious="105",pervious="106",water="107",
                           cols=c("red","black","green","grey","burlywood","blue"))

redonOSMgrouped<-LCZgroup2(
  redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
  industry="10",
  vegetation=c("101","102","103","104"),
  impervious="105",pervious="106",water="107",cols=c("red","black","green","grey","burlywood","blue"))

# levCol(redonOSMgrouped,"LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),
#        industry="10",
#        vegetation=c("101","102","103","104"),
#        impervious="105",pervious="106",water="107",
#        cols=c("red","black","green","grey","burlywood","blue"))


###
# compareLCZ(sf1=redonBDT, column1="LCZ_PRIMARY", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
#            sf2=redonOSMgrouped, column2="grouped", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
#            repr="grouped", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE,
#            urban=c("1","2","3","4","5","6","7","8","9"),
#            industry="10",
#            vegetation=c("101","102","103","104"),
#            impervious="105",pervious="106",water="107",
#            cols=c("red","black","green","grey","burlywood","blue"),tryGroup = TRUE)

expect_warning(compareRedonBDTOSMgrouped<-
  compareLCZ(sf1=redonBDTgrouped, column1="grouped", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
             sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
             repr="grouped", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE,
             urban=c("1","2","3","4","5","6","7","8","9"),
             industry="10",
             vegetation=c("101","102","103","104"),
             impervious="105",pervious="106",water="107",
             cols=c("red","black","green","grey","burlywood","blue"),tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")

file.remove("groupedBDT_groupedOSM.csv")

redonBDTgrouped2<-
  LCZgroup2(redonBDT,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),outCol="groupedLCZ",
  industry="10",vegetation=c("101","102","103","104"),impervious="105",pervious="106",water="107",
            cols=c("red","black","green","grey","burlywood","blue"))
redonOSMgrouped2<-
  LCZgroup2(redonOSM,column="LCZ_PRIMARY",urban=c("1","2","3","4","5","6","7","8","9"),outCol="otherName",
            industry="10",vegetation=c("101","102","103","104"),impervious="105",pervious="106",water="107",
            cols=c("red","black","green","grey","burlywood","blue"))

# showLCZ(redonBDTgrouped2,column="groupedLCZ",repr='grouped',
#         typeLevels=c("urban","industry","vegetation","impervious","pervious","water"),
#         cols=c("red","black","green","grey","burlywood","blue"))


expect_message(compareLCZ(sf1=redonBDTgrouped2, column1="groupedLCZ", wf1="BDT",
           sf2=redonOSMgrouped2, column2="otherName", wf2="osm", exwrite=FALSE, repr="grouped", plot=T,
           urban="urban",industry="industry",vegetation="vegetation",impervious="impervious",pervious="pervious",water="water",
           cols=c("red","black","green","grey","burlywood","blue")),
               "they will be coerced to the specified reference \\(redonBDTgrouped2\\)"
)

expect_warning(compareRedonBDTOSMgrouped<-
                 compareLCZ(sf1=redonBDTgrouped, column1="grouped", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
                            repr="grouped", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE,
                            urban=c("1","2","3","4","5","6","7","8","9","chaussure"),
                            industry="10",
                            vegetation=c("101","102","103","104"),
                            impervious="105",pervious="106",water="107",
                            cols=c("red","black","green","grey","burlywood","blue"),tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")

expect_warning(compareRedonBDTOSMgrouped<-
                 compareLCZ(sf1=redonBDTgrouped, column1="grouped", geomID1 = "ID_RSU", confid1="LCZ_UNIQUENESS_VALUE", wf1="groupedBDT",
                            sf2=redonOSM, column2="LCZ_PRIMARY", geomID2 = "ID_RSU", confid2="LCZ_UNIQUENESS_VALUE", wf2="groupedOSM",
                            repr="grouped", ref=2, saveG="", exwrite=FALSE, location="Redon", plot=TRUE,
                            urban=c("1","2","3","4","5","6","7","8","chaussure"),
                            industry="10",
                            vegetation=c("101","102","103","104"),
                            impervious="105",pervious="106",water="107",
                            cols=c("red","black","green","grey","burlywood","blue"),tryGroup = TRUE),
               "attribute variables are assumed to be spatially constant throughout all geometries")

