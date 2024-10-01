LCZmodeByAgreementLevel <- function(intersec_sf, sfWfs = NULL){ 
  if ( !is.null(intersec_sf$geometry)) {
    intersec_sf<-st_drop_geometry(intersec_sf)
  }
  columnNames<-names(intersec_sf)
  pairNames<-grep(pattern = "[1-9]_[1-9]", x = columnNames)
  agreement_by_pair<- t(
    intersec_sf[,pairNames]) %*% as.matrix(drop_units(intersec_sf$area)) / 
    sum(drop_units(intersec_sf$area))
    
  LCZwfsNames<-grep( pattern = "LCZ*",  x = names(intersec_sf), value = TRUE)
  intersec_sf$LCZmode<-apply(intersec_sf[,LCZwfsNames], 1, Mode)
  
  modeLCZSurfbyAgreement <- intersec_sf %>% group_by(maxAgree, LCZmode) %>% summarize(modeLCZsurf = sum(area)) %>% mutate(modeLCZSurfPerc = modeLCZsurf/sum(modeLCZsurf)*100)

  generalProp<-intersec_sf %>%select(area, LCZmode) %>% mutate(totalArea=sum(area)) %>% 
    group_by(LCZmode) %>% 
    summarize(modeLCZGenSurfPerc = sum(area), totalArea = mean(totalArea)) %>% 
    mutate(modeLCZGenSurfPerc = modeLCZGenSurfPerc / totalArea *100 ) %>% 
    select(LCZmode, modeLCZGenSurfPerc)

  modeLCZSurfbyAgreement<-left_join(modeLCZSurfbyAgreement, generalProp, by = "LCZmode") %>% 
    arrange(desc(maxAgree),desc(modeLCZSurfPerc))

  # if (!is.null(sfWfs)) {
  #   lengthSfWfs<-length(sfWfs)
  #   testLengthSfWfs<-factorial(lengthSfWfs)/(2*factorial(lengthSfWfs-2))
  #   compNames<-NULL
  #   if ( nrow(agreement_by_pair)==testLengthSfWfs ) {
  #     for (firstWfIndice in 1:(length(sfWfs)-1)) {
  #       for(secondWfIndice in (firstWfIndice + 1):length(sfWfs)){
  #         compNames<-c(compNames,paste0(sfWfs[firstWfIndice],"_",sfWfs[secondWfIndice]))    
  #         }
  #       }
  #     }
  #   row.names(agreement_by_pair) <- compNames

  #    }
 return(modeLCZSurfbyAgreement )
}

Mode <- function(x) {
  ux <- unique(x)
  unlist(ux[which.max(tabulate(match(x, ux)))])
}


LCZmodeTest <- LCZmodeByAgreementLevel(multicompare_test$intersec_sf)
LCZmodeTest[601:610,c(LCZwfsNamesTest,"LCZmode")]

test1<-multicompare_test$intersec_sf
LCZwfsNamesTest<-grep( pattern = "LCZ*",  x = names(test1), value = TRUE) 
apply(test1[,LCZwfsNamesTest], 1, Mode)



grep( pattern = "LCZ*", names(multicompare_test$intersec_sf), value = TRUE)
