library(FactoMineR)
library(Factoshiny)
# factorialPlanWfs<-function(intersec_sf, sfWfs = NULL){ 
#   columnNames<-names(intersec_sf)
#   LCZwfsNames<-grep( pattern = "LCZ*",  x = names(intersec_sf), value = TRUE)


# }

test_sf_for_MCA<-st_drop_geometry(multicompare_test[
  , grep( pattern = "LCZ*",  x = names(multicompare_test), value = ) ])


test.MCA<-FactoMineR::MCA(X = test_sf_for_MCA, ncp = 4913)

Factoshiny::MCAshiny(test.MCA)
