#' Imports the LCZ classifications produced on raster maps.  
#' For now, the european tif map produced by WUDAP is used as a reference example. 
#' Users can DOWNLOAD the WUDAPT tif file at the following url : 
#' https://figshare.com/articles/dataset/European_LCZ_map/13322450
#'
#' @param dirPath is the path to the directory where the
#' @param fileName is the name of the raster file (tif or geotif), by default \'EU_LCZ_map.tif\'.
#' @param LCZband defines the band of the raster file which contains the LCZ types. 
#' It can be a number or a band name (default = 1)
#' @param LCZcolumn the name of the column of the output sf File in which the LCZ types will be stored
#' (default = "LCZ")
#' @param confidenceBand the band of the raster files which contains a measure of confidence. 
#' Can be a number or a band name (default = "", that is no measure of confidence provided)
#' @param confidenceColumn the name of the column of the output sf File in which the confidence values will be stored
#' (default = "", no confidence value specified)
#' @param typeLevels indicates a named vector of the unique values contained in LCZcolumn,
#' @param bBox bBox is the bounding box needed to crop the raster file.
#' It can be produced bu the importLCZvect function. It can either be of class bBox or of class sfc
#' @return an sf file containing the geom and LCZ levels from theraster tiff within the bBox bounding box
#' @import sf dplyr forcats
#' @importFrom terra crop
#' @importFrom terra rast
#' @importFrom terra as.polygons
#' @export
#'
#' @examples
#' redonBbox<-importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),
#' "/bdtopo_2_2/Redon"), file="rsu_lcz.geojson", column="LCZ_PRIMARY", output="bBox")
#'
#' redonWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
#' fileName="redonWudapt.tif",bBox=redonBbox)
#' 
#' # another way to get the bounding box when one explores a given city would be the use of the 
#' # getbb() function from the osmdata package. 
#' # This exaample requires the osmdata package and therefore is not executed here
#' # redonBbox<-osmdata::getbb("Redon")
#' # redonWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
#' # fileName="redonWudapt.tif",bBox=redonBbox)
#' 
#' # another way to get the bounding box when one doesn't want 
#' #to compare to a vector map is to enter it's coordinates 
#' # and feed them to st_bbox() of the sf package.
#' 
#' # imporLCZraster also allows to choose which band of a raster file to import
#' redonWudapt<-importLCZraster(system.file("extdata", package = "lczexplore"),
#' fileName="redonWudapt.tif",bBox=redonBbox, LCZband=1, LCZcolumn='EU_LCZ_map')
importLCZraster<-function(dirPath, bBox, fileName="EU_LCZ_map.tif", LCZband=1, LCZcolumn='EU_LCZ_map',
                          confidenceBand = "", confidenceColumn = "confidence",
                          typeLevels=c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                           "9"="9","10"="10","101"="11","102"="12","103"="13","104"="14",
                            "105"="15", "106"="16","107"="17")){
# internal import function used in two loops
  effectiveImport<-function(fileName,bBox,LCZband,confidenceBand,LCZcolumn, confidenceColumn){

     if(confidenceBand=="") {
    lyrs=list(LCZband)
    sfFile<-do.call(rast, list(x=fileName, lyrs=lyrs))
  } else if (confidenceBand!="") {
    lyrs=list(LCZband,confidenceBand)
    sfFile<-do.call(rast, list(x=fileName, lyrs=lyrs))
  }

    if (sum(class(bBox)%in%c("sfc_POLYGON","sfc" ))==0) {
      bBox<-st_as_sfc(bBox)
    }
    bBox<-st_transform(bBox, st_crs(sfFile,proj=TRUE))
    

    cropTry<-try(sfFile %>% crop(bBox))
    if(is(cropTry,"try-error")) { 
      stop("The bounding box doesn't intersect with the provided raster file.")}
    else{
    sfFile<-sfFile %>% crop(bBox) %>% as.polygons(dissolve=FALSE) %>%
      st_as_sf() %>% st_intersection(bBox)

      if (length(typeLevels==1)){
    typeLevels<-c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
               "9"="9","10"="10","101"="11","102"="12","103"="13","104"="14",
               "105"="15", "106"="16","107"="17")}
    # typeLevels2<-as.character(c(1:10,101:107))
    
    names(sfFile)[names(sfFile)==LCZband]<-LCZcolumn
    names(sfFile)[names(sfFile)==confidenceBand]<-confidenceColumn
      print(str(sfFile))
    sfFile<-sfFile%>% mutate(!!LCZcolumn:=fct_recode(factor(subset(sfFile,select=LCZcolumn,drop=T),levels=typeLevels),
                                    !!!typeLevels)) %>%
        drop_na(LCZcolumn)

    cat(levels(subset(sfFile,select=LCZcolumn,drop=T)))
    #plot(sfFile)
    sfFile
    }
   }




  if (!file.exists(dirPath)){stop(message="The directory set in dirPath doesn't seem to exist")}
  else{
    fileName<-paste0(dirPath,"/",fileName)
    print(fileName)
    if (!file.exists(fileName)) {
        stop("The raster file doesn't exist in the specified directory")
        } else  { # if the user specifies the number of the layer and not its name
          if (is.numeric(LCZband) | is.numeric(confidenceBand)){
            sfFile<-rast(fileName)
            layerNames <- names(sfFile)
            if (is.numeric(LCZband)) { LCZband<-layerNames[LCZband] }
            if (is.numeric(confidenceBand)) { confidenceBand<-layerNames[confidenceBand] }
          }
          
          sfFile<-effectiveImport(fileName  =  fileName,  bBox = bBox,  
                                  LCZband = LCZband,  LCZcolumn = LCZcolumn, 
                                  confidenceBand =  confidenceBand, 
                                  confidenceColumn = confidenceColumn)
          sfFile
    } 
  }

sfFile$geomID<-row.names(sfFile)
  return(sfFile)

}