#' Imports the rsu_lcz geojson file produced by GeoClimate. 
#' Use the more generic importLCZvect function
#'
#' @param dirPath : the path where the rsu_lcz.geojson file is.
#' @param output : if sfFile the rsu_lcz.geojson is imported as an sf file, if bBox, a bounding box of the area is returned
#' @return : if output is set to sfFile, the function returns an sf objects containing all the colums
#' of the rsu_lcz.geojson found in the dirPath directory.
#' If output is bBox a bounding box (seef sf package) of the area contained in the rsu_lcz file in the dirPath directory is returned.
#' This is useful to select geoms analysed in other sf objects.
#' @import sf dplyr
#' @export
#'
#' @examples 
#' importLCZgc(dirPath=paste0(system.file("extdata", package = "lczexplore"),"/bdtopo_2_2/Redon/"))
importLCZgc<-function(dirPath,output="sfFile"){
  # output can be sfFile, bBox or Contour

  # dependancies should be dealt with @import
  #paquets<-c("sf","dplyr")
  #lapply(paquets, require, character.only = TRUE)

  #
  #
  # typeLevels<-typeLevels<-list("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
  #                        "9"="9","10"="10","101"="101","102"="102","103"="103","104"="104",
  #                        "105"="105","106"="106","107"="107","101"="11","102"="12","103"="13","104"="14",
  #                        "105"="15", "106"="16","107"="17")
  fileName<-paste0(dirPath,"rsu_lcz.geojson")
  sfFile<-st_read(dsn=fileName)
  #sfFile<-sfFile %>% mutate(LCZ_PRIMARY<-factor(subset(sfFile,select=LCZ_PRIMARY,drop=T),levels=typeLevels))


  if(output=="sfFile"){return(sfFile)} else {
    if(output=="bBox"){bBox<-st_bbox(sfFile,crs=st_crs(sfFile)) %>% st_as_sfc

    return(bBox)} else {
      if(output=="contour"){
        # fileName2<-paste0(dirPath,"zones.geojson")
        sfContour<-st_read(dsn=fileName) %>% st_geometry %>% st_union()
        return(sfContour)
      }
      else {
      stop("output must be sfFile to return geoms and LCZ or bBox to return the bounding box")}
    }
  }

}

