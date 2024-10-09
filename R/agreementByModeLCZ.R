agreementByLCZlevel <- function(intersec_sf, sfWfs = NULL){ 
  if ( !is.null(intersec_sf$geometry)) {
    intersec_sf<-st_drop_geometry(intersec_sf)
  }
  columnNames<-names(intersec_sf)
      
  LCZwfsNames<-grep( pattern = "LCZ*",  x = names(intersec_sf), value = TRUE)
  intersec_sf$LCZmode<-apply(intersec_sf[,LCZwfsNames], 1, Mode)
  
agreementByModeLCZ<- intersec_sf %>% group_by(LCZmode, maxAgree) %>% summarize(agreeByLCZsurf = sum(drop_units(area))) %>% mutate(LCZmode = as.integer(LCZmode), agreeByLCZPerc = round( agreeByLCZsurf/sum(agreeByLCZsurf)*100, digits = 2))

  # areaByAgreement <- intersec_sf %>% group_by(maxAgree) %>% summarize ( areaByAgree = sum(drop_units(area))) %>% mutate ( agreementSurfPerc = round(areaByAgree / sum(areaByAgree) * 100, digits = 3))

  generalProp<-intersec_sf %>% select(area, LCZmode) %>% mutate(totalArea=sum(drop_units(area))) %>% 
    group_by(LCZmode) %>% 
    summarize(modeLCZGenSurfPerc = sum(drop_units(area)), totalArea = mean(totalArea)) %>% 
    mutate(modeLCZGenSurfPerc = round(modeLCZGenSurfPerc / totalArea *100 , digits = 3)) %>% 
    select(LCZmode, modeLCZGenSurfPerc) %>% mutate(LCZmode = as.integer(LCZmode))

  # modeLCZSurfbyAgreement<-left_join(modeLCZSurfbyAgreement, generalProp, by = "LCZmode") %>% 
  #   arrange(desc(maxAgree),desc(modeLCZSurfPerc))
  # modeLCZSurfbyAgreement<-left_join(modeLCZSurfbyAgreement, areaByAgreement, by = "maxAgree")

 agreementByModeLCZ <- agreementByModeLCZ %>%  
   dplyr::arrange(LCZmode,desc(maxAgree)) %>% select(LCZmode, maxAgree, agreeByLCZPerc)

 agreementByModeLCZ <- left_join(agreementByModeLCZ, generalProp, by = "LCZmode")

p<-ggplot(agreementByModeLCZ, aes(x = maxAgree, y = agreeByLCZPerc)) +
  geom_bar(stat = "identity") +
      facet_wrap(~ LCZmode)
print(p)
return(agreementByModeLCZ)
}

Mode <- function(x) {
  ux <- unique(x)
  unlist(ux[which.max(tabulate(match(x, ux)))])
}

library(units)
library(dplyr)
library(sf)
library(ggplot2)
library(lczexplore)
agreeByLCZmodeTest <- agreementByLCZlevel(multicompare_test$intersec_sf)

head(agreeByLCZmodeTest)
