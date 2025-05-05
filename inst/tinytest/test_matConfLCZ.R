#This tests teh function matConfLCZ
# library(tinytest)
#
# library(sf)
library(tidyr)
require(dplyr)

#showLCZ(redonBDT)
#showLCZ(redonOSM)

########### Test if the agree of an sf file with itself is 100 % (or 0 for LCZ present in no geom)
expect_warning(
matConfRedonBDTBDT<-matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
                               sf2=redonBDT,column2='LCZ_PRIMARY', plot = TRUE),
"attribute variables are assumed to be spatially constant throughout all geometries"
)


matConfLongAuto<-matConfRedonBDTBDT$matConf


matConfLargeAuto<-matConfLongAuto %>%
  pivot_wider(names_from=LCZ_PRIMARY, values_from=agreePercArea, values_fill=0)

realMatConfLargeAuto<-as.matrix(matConfLargeAuto[,!is.na(as.numeric(names(matConfLargeAuto)))])
testAuto<-prod((round(diag(realMatConfLargeAuto), 5)==100)|(round(diag(realMatConfLargeAuto), 5) ==0))
expect_equal(testAuto,1)

######## test that different classification do not agree everywhere

matConfRedonBDTOSM<-matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
                            sf2=redonOSM,column2='LCZ_PRIMARY', plot=TRUE, 
                               wf1 = "GC BDTOPO", wf2 = "GC OSM")

matConfRedonOSMBDT<-matConfLCZ(sf1=redonOSM,column1='LCZ_PRIMARY',
                               sf2=redonBDT,column2='LCZ_PRIMARY', plot=TRUE,
                               wf1 = "GC OSM", wf2 = "GC BDTOPO")

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

matConfRedonOSMBDTGrouped<-matConfLCZ(sf1 = redonOSMgrouped, column1 = "grouped",
                               sf2 = redonBDTgrouped, column2 = "grouped", plot=TRUE,
                               wf1 = "GC OSM", wf2 = "GC BDTOPO")


matConfLargeHetero<-matConfRedonBDTOSM$matConf %>%
  pivot_wider(names_from=LCZ_PRIMARY, values_from=agreePercArea, values_fill=0)

unorderedNames<-names(matConfLargeHetero)[!names(matConfLargeHetero)%in%c("LCZ_PRIMARY.1", "Unclassified")]
orderedNames<-unorderedNames[order(as.numeric(unorderedNames))]

matConfLargeHetero[
  order(matConfLargeHetero$LCZ_PRIMARY.1),
  orderedNames]


realMatConfLargeHeteroClean<-matConfLargeHetero[
  order(matConfLargeHetero$LCZ_PRIMARY.1),
  orderedNames] %>% as.matrix

diagHetero<-round(diag(realMatConfLargeHeteroClean), 2)
diagHeteroRef<-c(0.00, 89.17, 6.35, 0.00, 0.00, 65.14,  0.00, 83.55, 66.84,  0.00, 48.19,
                 1.02,  0.00, 87.57, 29.93,  0.00, 89.46)

testHetero<-prod((diag(realMatConfLargeHeteroClean)==100)|(diag(realMatConfLargeHeteroClean)==0))

# names(matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
#                  sf2=redonOSM,column2='LCZ_PRIMARY',plot=FALSE))

expect_equal(testHetero,0)
expect_equal(diagHetero,diagHeteroRef)
expect_equal(names(matConfRedonBDTOSM),c("matConf","matConfPlot","areas","percAgg"))






