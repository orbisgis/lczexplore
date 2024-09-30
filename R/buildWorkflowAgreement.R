buildWorkflowAgreement <- function(intersec_sf, sfWfs = NULL){ 
  if ( !is.null(intersec_sf$geometry)) {
    intersec_sf<-st_drop_geometry(intersec_sf)
  }
  columnNames<-names(intersec_sf)
  pairNames<-grep(pattern = "[1-9]_[1-9]", x = columnNames)
  agreement_by_pair<- t(
    intersec_sf[,pairNames]) %*% as.matrix(drop_units(intersec_sf$area)) / 
    sum(drop_units(intersec_sf$area))
    

  if (!is.null(sfWfs)) {
    lengthSfWfs<-length(sfWfs)
    testLengthSfWfs<-factorial(lengthSfWfs)/(2*factorial(lengthSfWfs-2))
    compNames<-NULL
    if ( nrow(agreement_by_pair)==testLengthSfWfs ) {
      for (firstWfIndice in 1:(length(sfWfs)-1)) {
        for(secondWfIndice in (firstWfIndice + 1):length(sfWfs)){
          compNames<-c(compNames,paste0(sfWfs[firstWfIndice],"_",sfWfs[secondWfIndice]))    
          }
        }
      }
    row.names(agreement_by_pair) <- compNames

     }
 return(sort(agreement_by_pair[,1], decreasing = TRUE))
}

tetest<-buildWorkflowAgreement(intersec_sf = multicompare_test$intersec_sf, 
  sfWfs = c("BDT11", "BDT22", "OSM11", "OSM22", "WUDAPT"))
