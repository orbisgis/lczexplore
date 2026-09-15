require(lczexplore)

# Tests on one location only to ease the checkings
oneLocDir<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon")
oneLocSfList<-loadMultipleSfs(dirPath = oneLocDir, workflowNames = c("osm","bdt","wudapt"),
                                   inLocation = "Redon")
oneLocSfIntersected <- createIntersect(sfList = oneLocSfList, columns = rep("lcz_primary", 4),
                                        refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.001)
redon_bdt<-oneLocSfList$bdt
class(redon_bdt)
redon_osm<-oneLocSfList$osm
redon_wud<-oneLocSfList$wud
showLCZ(sf = redon_bdt, column = "lcz_primary")
showLCZ(sf = redon_osm, column = "lcz_primary")
showLCZ(sf = redon_wud, column = "lcz_primary")

bdt_wud_compare<-compareLCZ(sf1 = redon_bdt, column1 = "lcz_primary", wf1 = "bdt",
           sf2 = redon_wud, column2 = "lcz_primary", wf2 = "wudapt")
bdt_wud_compare$matConfPlot
bdt_osm_compare<-compareLCZ(sf1 = redon_bdt, column1 = "lcz_primary", wf1 = "bdt",
                            sf2 = redon_osm, column2 = "lcz_primary", wf2 = "osm")

oneLocWeightedFlux<-createWeightedFlux(oneLocSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))
drawChordDiagram(oneLocWeightedFlux, colorMapIn = NULL, labelMatch = NULL)



## With Two Locations


twoLocsDir<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs")
twoLocsSfList<-loadMultipleLocsSfs(dirPath = twoLocsDir, workflowNames = c("osm","bdt","wudapt"),
                                   inLocation = c("Arville", "Redon"))



twoLocsSfIntersected <- createIntersect(sfList = twoLocsSfList, columns = rep("lcz_primary", 4),
                                        refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.001)

twoLocsWeightedFlux<-createWeightedFlux(twoLocsSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))

# test <- makeSectorsAndGroups(twoLocsWeightedFlux)

drawChordDiagram(twoLocsWeightedFlux, colorMapIn = NULL, labelMatch = NULL)

twoLocsWeightedFluxNo104no101<-subset(twoLocsWeightedFlux,
                             !grepl("101", twoLocsWeightedFlux$orig) &
                               !grepl("104", twoLocsWeightedFlux$orig) &
                               !grepl("101", twoLocsWeightedFlux$dest) &
                               !grepl("104", twoLocsWeightedFlux$dest))

drawChordDiagram(twoLocsWeightedFluxNo104no101, colorMapIn = NULL, labelMatch = NULL)


aggregMatch<-c("acompact"="Compact", "blessCompact" = "Less Compact", "cfewToNoBuild" = "Few to No Buildings at all",
               "dunclass" = "Unclassified")
drawChordDiagram(twoLocsWeightedFluxNo104no101, labelMatch = aggregMatch,
                 acompact = c("1", "2", "3"),
                 blessCompact = c("4", "5", "6", "7", "8", "10"),
                 cfewToNoBuild = c("9", "101", "102", "103", "104", "105", "106", "107"),
                 dunclass = "Unclassified",
                 groupColors = c(
                   "acompact" = "#8b0101",
                   "blessCompact" = "#ff9856",
                   "cfewToNoBuild" = "#bbdb7a",
                   "dunclass" = "grey"
                 )
)

# Sans labelMatch c'est bien l'ordre alphabétique qui induit l'ordre de représentation
drawChordDiagram(twoLocsWeightedFluxNo104no101,
                 acompact = c("1", "2", "3"),
                 blessCompact = c("4", "5", "6", "7", "8", "10"),
                 cfewToNoBuild = c("9", "101", "102", "103", "104", "105", "106", "107"),
                 dunclass = "Unclassified",
                 groupColors = c(
                   "acompact" = "#8b0101",
                   "blessCompact" = "#ff9856",
                   "cfewToNoBuild" = "#bbdb7a",
                   "dunclass" = "grey"
                 )
)

aggregMatch2<-c("compact"="Compact", "lessCompact" = "Less Compact", "fewToNoBuild" = "Few to No Buildings",
                "unclass" = "Unclassified")

drawChordDiagram(twoLocsWeightedFluxNo104no101,
                 labelMatch = aggregMatch2,
                 compact = c("1", "2", "3"),
                 lessCompact = c("4", "5", "6", "7", "8", "10"),
                 fewToNoBuild = c("101", "102", "103", "104", "105", "106", "107", "9"),
                 unclass = "Unclassified",
                 groupColors = c(
                   "compact" = "#8b0101",
                   "lessCompact" = "#ff9856",
                   "fewToNoBuild" = "#bbdb7a",
                   "unclass" = "grey"
                 )
)


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
agreeArea <- subset(test, bdt3 == LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
disagreeArea <- subset(test, bdt3 != LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
percAgree<-agreeArea/(agreeArea + disagreeArea)
percAgree

allSameFlux<-createWeightedFlux(testIntersectAllBDT, wfNamesIn = c("bdt1","bdt2","bdt3"))
drawChordDiagram(allSameFlux, )

# not all the same, right order

sfListTest<-list(
  bdt = utrfRedonBDT,
  rand = utrfRedonBDT,
  osm = utrfRedonOSM
  )

testIntersect<-
  createIntersect(
    sfList = sfListTest,
    columns = rep("TYPO_MAJ", 3),
    workflowNames=c("bdt", "rand", "osm"),
    minZeroArea=0)
testIntersect$bdt %>% summary
testIntersect$rand %>% summary
testIntersect$osm %>% summary


LCZtype <- "pd"
test<-subset(testIntersect, bdt == LCZtype)
agreeArea <- subset(test, rand == LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
disagreeArea <- subset(test, rand != LCZtype) %>% st_drop_geometry() %>% select(area) %>% sum
percAgree<-agreeArea/(agreeArea + disagreeArea)
percAgree

levelUTRF<-  c("icio", "pd", "id", "psc", "pcio", "local", "pcif", "ba", "icif")
colorMapInUTRF<-palette.colors(n = length(levelUTRF) )
names(colorMapInUTRF)<-levelUTRF
colorMapInUTRF


testWeightedFlux<-createWeightedFlux(
  testIntersect, wfNamesIn = c("osm","bdt","rand"), typeLevelsDefaultIn = NULL)

drawChordDiagram(testWeightedFlux, colorMapIn = colorMapInUTRF)
drawChordDiagram(testWeightedFlux)

drawChordDiagram(testWeightedFlux,
                 colorMapIn = colorMapInUTRF, labelMatch = NULL)

