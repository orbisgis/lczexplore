#' Imports LCZ from a standard geographical file (tested : geojson, shp, more to come)
#'
#' @param dirPath is the path of the directory of the file
#' @param file is the name of the file from which the LCZ are imported
#' @param column indicates the name of the column containing LCZ values, all other
#' columns will be dropped. If empty string, all the colums will be kept
#' @param geomID is the name of the column containing the ID of each geom to load. If an empty string, no column is loaded.
#' @param confid is the name of the column containing a confidence indicator to filter geoms,
#' for instance the uniqueness of the LCZ level of each geom
#' @param output : if sfFile, the function returns an sfFile with the LCZ colums,
#' if bBox, returns a bounding box one can use to crop a raster file or to intersect another sf file
#' @param niveaux the levels of the imported LCZ classification
#' @import dplyr forcats
#' @return
#' @export
#'
#' @examples
importLCZgen<-function(dirPath, file, output, column, geomID="", confid="",
                       niveaux=c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                                                        "9"="9","10"="10","101"="101","102"="102","103"="103","104"="104",
                                                         "105"="105","106"="106","107"="107","101"="11","102"="12","103"="13","104"="14",
                                                        "105"="15", "106"="16","107"="17"),drop=T){
  fileName<-paste0(dirPath,file)
  # select only the needed column that is the unempty strings among column, geomID and confid
  colonnes<-c(geomID,column,confid)
  colonnes<-colonnes[sapply(colonnes,nchar)!=0]
  sfFile<-st_read(dsn=fileName)[,colonnes]

  if (length(niveaux)==1){
    niveaux<-unique(subset(sfFile,select=column))
  }

  if (column!=""){
    if(drop==T){sfFile<-subset(sfFile,select=colonnes)}
    sfFile <-
    sfFile%>% mutate(!!column:=fct_recode(factor(subset(sfFile,select=column,drop=T),levels=niveaux),!!!niveaux))
    return(sfFile)}
  else {stop("You must specify the coolumn containing the LCZ")}


  #sfFile <- sfFile%>% mutate(!!column:=fct_recode(subset(sfFile,select=column,drop=T),!!!niveaux))

  if(output=="sfFile"){return(sfFile)} else {
    if(output=="bBox"){bBox=st_bbox(sfFile,crs=st_crs(sfFile)) %>% st_as_sfc
    return(bBox)}
    else {
      stop("output must be sfFile to return geoms and LCZ or bBox to return the bounding box")}

    }
}


