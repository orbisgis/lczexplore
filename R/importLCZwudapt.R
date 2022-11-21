importLCZwudapt<-function(dirPath,zone="europe",bBox){
  paquets<-c("sf","dplyr","terra","forcats")
  lapply(paquets, require, character.only = TRUE)
  fileName<-paste0(dirPath,"EU_LCZ_map.tif")
  sfFile<-rast(fileName)
  bBox<-st_transform(bBox,st_crs(sfFile,proj=T))


  sfFile<-sfFile %>% crop(bBox) %>% as.polygons(dissolve=F) %>%
    st_as_sf() %>% st_intersection(bBox)

  niveaux<-c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                 "9"="9","10"="10","101"="11","102"="12","103"="13","104"="14",
                 "105"="15", "106"="16","107"="17")
  # niveaux2<-as.character(c(1:10,101:107))

  sfFile<-sfFile %>% mutate(EU_LCZ_map=factor(subset(sfFile,select='EU_LCZ_map',drop=T)))
  temp<-subset(sfFile,select='EU_LCZ_map',drop=T)
  # the use of !!! is a special way to parse the elements in the tidyverse packages
  temp<-fct_recode(temp,!!!niveaux)
  sfFile<-sfFile %>% mutate(EU_LCZ_map=temp)

  cat(levels(subset(sfFile,select='EU_LCZ_map',drop=T)))
  #plot(sfFile)
  sfFile
}
