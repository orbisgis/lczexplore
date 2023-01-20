#' Imports the LCZ produced by the WUDAPT algorithm.
#' For now, the function only allows import from the european tiff
#' produced by WUDAPT. Users MSUT DOWNLOAD the WUDAPT tiff file
#' at the following url : https://figshare.com/articles/dataset/European_LCZ_map/13322450
#' A future version may include the world data once a strategy is defined to deal with CRS.
#'
#' @param dirPath is the path to the directory where the
#' @param zone set to europe by default, may include world once a strategy is defined
#' @param bBox bBox is the bounding box needed to crop the wudapt tiff file.
#' It can be produced bu the importLCZgen function
#' @return an sf file containing the geom and LCZ levels from the WUDAPT Europe tiff within the bBox bounding box
#' @import sf dplyr terra forcats
#' @export
#'
#' @examples
importLCZwudapt<-function(dirPath,zone="europe",bBox){
# internal import function used in two loops
  effectiveImport<-function(fileName,bBox){
    sfFile<-rast(fileName)
    bBox<-st_transform(bBox,st_crs(sfFile,proj=T))

    cropTry<-try(sfFile %>% crop(bBox))
    if(is(cropTry,"try-error")){stop("The bounding box doesn't intersect with the Wudapt tiff : \n
                                     maybe it's out of the Europe zone covered by this package. \n Future versions may include other zones.")}
    else{
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
   }




  if (!file.exists(dirPath)){stop(message="The directory set in dirPath doesn't seem to exist")}
  else{
    fileName<-paste0(dirPath,"EU_LCZ_map.tif")
    if (!file.exists(fileName)){
      choice<-readline(prompt="The wudapt Europe map tiff file doesn't exist in the specified directory. \n
              If you want lczexplore to try and download it type 1, else type 2 to exit and get the tiff map by yourself")
      print("choice: ");print(choice);
      if (choice==1){
        url<-"https://figshare.com/ndownloader/files/35069446"
        fetchTry<-try(download.file(url=url,method="auto",destfile=fileName))
        if(is(fetchTry,"try-error")){
          stop("The file couldn't be downloaded, maybe try to dowload another way.")
        } else {
          sfFile<-effectiveImport(fileName = fileName,bBox=bBox)
          sfFile
          }
      }
    }
    else{
      sfFile<-effectiveImport(fileName = fileName,bBox=bBox)
      sfFile
      }
  }


  #paquets<-c("sf","dplyr","terra","forcats")
  #lapply(paquets, require, character.only = TRUE)



}
