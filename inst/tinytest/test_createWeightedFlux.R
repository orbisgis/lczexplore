require(lczexplore)
# One location
oneLocDir<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon")
oneLocSfList<-loadMultipleSfs(dirPath = oneLocDir, workflowNames = c("osm","bdt","wudapt"),
                              inLocation = "Redon")
oneLocSfIntersected <- createIntersect(sfList = oneLocSfList, columns = rep("lcz_primary", 4),
                                       refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.00)
redon_bdt<-oneLocSfList$bdt
redon_osm<-oneLocSfList$osm

showLCZ(redon_bdt, column = "lcz_primary")
showLCZ(redon_osm, column = "lcz_primary")

testComp<-compareLCZ(sf1 = redon_bdt, column1 = "lcz_primary",
                     sf2 = redon_osm, column2 = "lcz_primary" )
testComp$matConf

testComp$matConfLarge
# MatConfLarge semble OK
testComp$areas

oneLocWeightedFlux<-createWeightedFlux(oneLocSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))
subset(oneLocWeightedFlux, orig == "bdt_8" & dest == "osm_8")

subset(oneLocWeightedFlux, orig == "bdt_8" & dest == "osm_6")

subset(testComp$areas, marginLevels==8)$percArea1 *
  subset(testComp$matConf,redon.bdt == "8" & redon.osm == "6")$agreePercArea

drawChordDiagram(oneLocWeightedFlux, colorMapIn = NULL, labelMatch = NULL, inFacing = "clockwise")

# Two locations
twoLocsDir<-paste0(
  system.file("extdata", package = "lczexplore"),"/multipleWfs")
twoLocsSfList<-loadMultipleLocsSfs(dirPath = twoLocsDir, workflowNames = c("osm","bdt","wudapt"),
                                   inLocation = c("Arville", "Redon"))

twoLocsSfIntersected <- createIntersect(sfList = twoLocsSfList, columns = rep("lcz_primary", 4),
                                        refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.00)

twoLocsWeightedFlux<-createWeightedFlux(twoLocsSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))

test <- makeSectorsAndGroups(twoLocsWeightedFlux)

drawChordDiagram(twoLocsWeightedFlux, colorMapIn = NULL, labelMatch = NULL, inFacing = "clockwise")