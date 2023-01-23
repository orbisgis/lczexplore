#This tests teh function matConfLCZ
# library(tinytest)
#
# library(sf)
require(tidyr)

#showLCZ(redonBDT)
#showLCZ(redonOSM)

########### Test if the accord of an sf file with itself is 100 % (or 0 for LCZ present in no geom)
expect_warning(
matConfRedonBDTBDT<-matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
                               sf2=redonBDT,column2='LCZ_PRIMARY',plot=FALSE),
"attribute variables are assumed to be spatially constant throughout all geometries"
)


matConfLongAuto<-matConfRedonBDTBDT$matConf


matConfLargeAuto<-matConfLongAuto %>%
  pivot_wider(names_from=LCZ_PRIMARY, values_from=accord, values_fill=0)

realMatConfLargeAuto<-as.matrix(matConfLargeAuto[,!is.na(as.numeric(names(matConfLargeAuto)))])
testAuto<-prod((diag(realMatConfLargeAuto)==100)|(diag(realMatConfLargeAuto)==0))
expect_equal(testAuto,1)

######## test that different classification do not agreee everytime

matConfRedonBDTOSM<-matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
                            sf2=redonOSM,column2='LCZ_PRIMARY',plot=FALSE)
matConfLargeHetero<-matConfRedonBDTOSM$matConf %>%
  pivot_wider(names_from=LCZ_PRIMARY, values_from=accord, values_fill=0)

realMatConfLargeHetero<-as.matrix(matConfLargeHetero[,!is.na(as.numeric(names(matConfLargeHetero)))])
diagHetero<-diag(realMatConfLargeHetero)
diagHeteroRef<-c(0.00, 89.17, 6.35, 0.00, 0.00, 65.14,  0.00, 83.55, 66.84,  0.00, 48.19,
                 1.02,  0.00, 87.57, 29.93,  0.00, 89.46)

testHetero<-prod((diag(realMatConfLargeHetero)==100)|(diag(realMatConfLargeHetero)==0))

# names(matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
#                  sf2=redonOSM,column2='LCZ_PRIMARY',plot=FALSE))

expect_equal(testHetero,0)
expect_equal(diagHetero,diagHeteroRef)
expect_equal(names(matConfRedonBDTOSM),c("matConf","matConfPlot","aires","pourcAcc"))






