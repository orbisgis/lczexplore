# This tests the function createIntersect
# library(tinytest)
# library(dplyr)
# library(tidyr)
# library(sf)
# library(ggplot2)
# library(forcats)

sfList<-loadMultipleSfs(dirPath = paste0(system.file("extdata/multipleWfs/Redon", package = "lczexplore")),
                        workflowNames = c("osm","bdt","wudapt"), inLocation = "Redon"  )

intersected<-createIntersect(sfList = sfList, columns = rep("lcz_primary", 3),
                             workflowNames = c("osm", "bdt", "wudapt"), refCrs = 1)
multicompare_test<-compareMultipleLCZ(intersected,
                                      columns = c("osm","bdt","wudapt"),
                                      trimPerc = 0.0)

aggregMatch<-c("acompact"="Compact", "blessCompact" = "Less Compact", "cfewToNoBuild" = "Few to No Buildings at all",
               "dunclass" = "Unclassified")
multicompare_test<-compareMultipleLCZ(intersected,
                                      columns = c("osm","bdt","wudapt"),
                                      trimPerc = 0.0,
                                      labelMatch = aggregMatch,
                                      acompact = c("1", "2", "3"),
                                      blessCompact = c("4", "5", "6", "7", "8", "10"),
                                      cfewToNoBuild = c("9", "101", "102", "103", "104", "105", "106", "107"),
                                      dunclass = "Unclassified",
                                      groupColors = c("acompact" = "#8b0101",
                                                      "blessCompact" = "#ff9856","cfewToNoBuild" = "#bbdb7a",
                                                      "dunclass" = "grey")
)


testAreas<-workflowAgreeAreas(multicompare_test$sfIntLong)
testAreas
#####################################
### compare results with matconf
#####################################

testMatConf<-matConfLCZ(sf1 = sfList$osm, column1 = "lcz_primary", sf2 = sfList$bdt, column2 = "lcz_primary")
testMatConf$percAgg

multicompare_test<-compareMultipleLCZ(intersected,
                                      columns = c("osm","bdt","wudapt"),
                                      trimPerc = 0.5)
testAreas$percAgree
testAreas<-workflowAgreeAreas(multicompare_test$sfIntLong)

expect_false(round(testAreas[1,4],1) == round(testMatConf$percAgg,1))

# They differ because there are 3 workflows. Now let's test with only 2.
sfList2<-loadMultipleSfs(dirPath = paste0(system.file("extdata/multipleWfs/Redon", package = "lczexplore")),
                        workflowNames = c("osm","bdt"), inLocation = "Redon"  )

intersected2<-createIntersect(sfList = sfList2, columns = rep("lcz_primary", 2),
                             workflowNames = c("osm", "bdt"), refCrs = 1)
multicompare_test2<-compareMultipleLCZ(intersected2,
                                      columns = c("osm","bdt"),
                                      trimPerc = 0.0)
testAreas2<-workflowAgreeAreas(multicompare_test2$sfIntLong)
expect_true(round(testAreas2[1,4],1) == round(testMatConf$percAgg,1))
# They agree



expect_equal(testAreas$areaAgree[1], 7733495)
expect_equal(round(testAreas$areaDisagree[1], 2), 80362.35)

osm<-importLCZvect(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon"),
                          file = "osm_lcz.fgb")
bdt<-importLCZvect(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon"),
                   file = "bdt_lcz.fgb")
wudapt<-importLCZvect(dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Redon"),
                   file = "wudapt_lcz.fgb", column = "lcz_primary")

sfList2<-list(osm = osm, bdt = bdt, wudapt = wudapt)

test3<- loadmultipleSfsFromSession(sfList = sfList2,
                                   workflowNames = c("osm", "bdt", "wudapt"),
                                   location = "Redon",
                                   columns = c("LCZ_PRIMARY", "LCZ_PRIMARY", "lcz_primary" ))

intersected<-createIntersect(sfList = test3, columns = rep("lcz_primary", 3),
                             workflowNames = c("osm", "bdt", "wudapt"))

expect_silent(multicompare_test<-compareMultipleLCZ(intersected,
                                                    columns = c("osm","bdt","wudapt"),
                                                    trimPerc = 0.5))

expect_silent(testAreas<-workflowAgreeAreas(multicompare_test$sfIntLong))


# multicompare_test

test<-multicompare_test$sfIntLong
test2<-test %>% subset(agree==TRUE) %>% group_by(LCZvalue) %>% summarize(agreementArea=sum(area)) %>%
  mutate(percAgreementArea=agreementArea/sum(agreementArea))

testWfAgree<-test %>% subset(agree==TRUE) %>% group_by(whichWfs) %>% summarize(agreementArea=sum(area))

test<-multicompare_test$sfInt[,1:5] %>% st_drop_geometry()
prov1<-apply(X = test, MARGIN = 1, table )
prov2<-apply(X = test, MARGIN = 1, function(x) max(table(x)) )

head(prov1)
head(prov2)

plot1<-showLCZ(sf = multicompare_test$sfInt, column="bdt", wf="bdt")
plot2<-showLCZ(sf = multicompare_test$sfInt, column="osm", wf="osm")
plot4<-showLCZ(sf = multicompare_test$sfInt, column="wudapt", wf="wud")
plot5<-ggplot(data=multicompare_test$sfInt) +
  geom_sf(aes(fill=nbAgree, color=after_scale(fill)))+
  scale_fill_gradient(low = "red" , high = "green", na.value = NA)
cowplot::plot_grid(plot1, plot2, plot4, plot5)
