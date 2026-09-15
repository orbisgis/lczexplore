# This tests the function createIntersect
# library(tinytest)
#
# library(sf)

# intersected<-createIntersect(sfList = sfList, columns = c(rep("LCZ_PRIMARY",4),"lcz_primary"),
#                             workflowNames = c("BDT11","BDT22","OSM11","OSM22","WUDAPT"))
 sfList<-loadMultipleSfs(
   dirPath = paste0(
   system.file("extdata", package = "lczexplore"),
   "/multipleWfs/Arville"),
 workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville")

collapse::ldepth(sfList)

ArvilleIntersect <- createIntersect(
  sfList = sfList, columns = rep("lcz_primary", 4),
  workflowNames = c("osm", "bdt", "wudapt"), minZeroArea = 0)

LCZtype<-105
test<-subset(ArvilleIntersect, bdt == LCZtype)
agreeArea <- subset(test, osm == LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
disagreeArea <- subset(test, osm != LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
percAgree<-agreeArea/(agreeArea + disagreeArea)
percAgree

ArvilleBDT<-sfList$bdt
ArvilleOSM<-sfList$osm

arvilleCompare<-compareLCZ(sf1 = ArvilleBDT, column1 = "lcz_primary", sf2 = ArvilleOSM, column2 = "lcz_primary")



sfListTwoLocs<-loadMultipleLocsSfs(dirPath = paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
                            workflowNames = c("osm","bdt","wudapt"), inLocation = c("Arville", "Redon"))

collapse::ldepth(sfListTwoLocs)
twoLocsIntersec <- createIntersect(sfList = sfListTwoLocs, columns = rep("lcz_primary", 4),
                                   workflowNames = c("osm", "bdt", "wudapt"))

# Test avec une quali autre que LCZ
utrfRedonBDT<-
  importQualVar(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/utrfFiles"),
                file="bdt_utrf_area.fgb", column="TYPO_MAJ", geomID="ID_RSU", confid="UNIQUENESS_VALUE")
utrfRedonOSM<-
  importQualVar(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/utrfFiles"),
                file="osm_utrf_area.fgb", column="TYPO_MAJ",geomID="ID_RSU",confid="UNIQUENESS_VALUE")

utrfRedonRandom<-utrfRedonBDT
#utrfRedonRandom$TYPO_MAJ<- sample(utrfRedonRandom$TYPO_MAJ,
# size = length(utrfRedonRandom$TYPO_MAJ), replace = FALSE)
compareLCZ(sf1 = utrfRedonBDT, column1 = "TYPO_MAJ",
           sf2 = utrfRedonRandom, column2 = "TYPO_MAJ", repr = "alter")

## Check if all sf the same

sfListTestAllBDT<-list(
  bdt1= utrfRedonBDT,
  bdt2 = utrfRedonBDT,
  bdt3 = utrfRedonBDT)

testIntersectAllBDT<-
  createIntersect(
    sfList = sfListTestAllBDT,
    columns = rep("TYPO_MAJ", 3),
    workflowNames=c("bdt1", "bdt2", "bdt3"),
    minZeroArea=0)

LCZtype <- "pd"
test<-subset(testIntersectAllBDT, bdt1 == LCZtype)
agreeArea <- subset(test, bdt2 == LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
disagreeArea <- subset(test, bdt2 != LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
percAgree<-agreeArea/(agreeArea + disagreeArea)
percAgree


# not all the same, right order

sfListTest<-list(
  osm = utrfRedonOSM,
  bdt = utrfRedonBDT,
  rand = utrfRedonBDT
)

testIntersect<-
  createIntersect(
    sfList = sfListTest,
    columns = rep("TYPO_MAJ", 3),
    workflowNames=c("osm", "bdt", "rand"),
    minZeroArea=0)

LCZtype <- "pd"
test<-subset(testIntersect, bdt == LCZtype)
agreeArea <- subset(test, rand == LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
disagreeArea <- subset(test, rand != LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
percAgree<-agreeArea/(agreeArea + disagreeArea)
percAgree






