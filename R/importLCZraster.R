#' Imports the LCZ classifications produced on raster maps, mainly by the WUDAPT algorithm.
#' For now, the function  import from the european tiff
#' produced by WUDAPT. Users can DOWNLOAD the WUDAPT tiff file
#' at the following url : https://figshare.com/articles/dataset/European_LCZ_map/13322450
#' A future version may include the world data once a strategy is defined to deal with CRS.
#'
#' @param dirPath is the path to the directory where the
#' @param fileName is the name of the raster file (tif or geotif), by default \'EU_LCZ_map.tif\' . 
#' Will be useful when other zones will be added
#' @param column indicates the name of the column which will contain the LCZ in the output file
#' @param typeLevels indicates a named vector of the unique values contained in column,
#' @param zone set to europe by default, may include world once a strategy is defined
#' @param bBox bBox is the bounding box needed to crop the wudapt tiff file.
#' It can be produced bu the importLCZvect function. It can either be of class bBox or of class sfc
#' @return an sf file containing the geom and LCZ levels from the WUDAPT Europe tiff within the bBox bounding box
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
#' # the following example can only be executed when user has downloaded 
#' # CONUS-wide LCZ map and Training Areas on WUDAPT website
#' # sanDiegobBoxCoord<-st_sf(a=1:2, geom=st_sfc(
#' #st_point(c(-117.175198,32.707289)),
#' #st_point(c(-117.112198,32.750900)),crs = 4326
#' #))
#' #sanDiegoBbox<-st_bbox(sanDiegobBoxCoord)
#' #sanDiegoWudapt<-importLCZraster(
#' #dirPath="path_of_the_tiff",
#' #fileName="CONUS_LCZ_map_NLCD_v1.0_epsg4326.tif",
#' #column="CONUS_LCZ_map_NLCD_v1.0_epsg4326"
#' #  ,bBox=sanDiegoBbox)
#' #showLCZ(sanDiegoWudapt, column="CONUS_LCZ_map_NLCD_v1.0_epsg4326")
importLCZraster<-function(dirPath,zone="europe",bBox,fileName="EU_LCZ_map.tif", column='EU_LCZ_map',
                          typeLevels=c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                           "9"="9","10"="10","101"="11","102"="12","103"="13","104"="14",
                            "105"="15", "106"="16","107"="17")){
# internal import function used in two loops
  effectiveImport<-function(fileName,bBox){
    sfFile<-rast(fileName)
    if (sum(class(bBox)%in%c("sfc_POLYGON","sfc" ))==0) {
      bBox<-st_as_sfc(bBox)
    }
    bBox<-st_transform(bBox,st_crs(sfFile,proj=T))
    

    cropTry<-try(sfFile %>% crop(bBox))
    if(is(cropTry,"try-error")){stop("The bounding box doesn't intersect with the Wudapt tiff : \n
                                     maybe it's out of the Europe zone covered by this package. \n Future versions may include other zones.")}
    else{
    sfFile<-sfFile %>% crop(bBox) %>% as.polygons(dissolve=F) %>%
      st_as_sf() %>% st_intersection(bBox)

      if (length(typeLevels==1)){
    typeLevels<-c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
               "9"="9","10"="10","101"="11","102"="12","103"="13","104"="14",
               "105"="15", "106"="16","107"="17")}
    # typeLevels2<-as.character(c(1:10,101:107))

    sfFile<-sfFile%>%
        mutate(!!column:=fct_recode(factor(subset(sfFile,select=column,drop=T),levels=typeLevels),
                                    !!!typeLevels)) %>%
        drop_na(column)

    cat(levels(subset(sfFile,select=column,drop=T)))
    #plot(sfFile)
    sfFile
    }
   }




  if (!file.exists(dirPath)){stop(message="The directory set in dirPath doesn't seem to exist")}
  else{
    fileName<-paste0(dirPath,"/",fileName)
    print(fileName)
    if (!file.exists(fileName)){
      choice<-readline(prompt="The wudapt Europe map tiff file doesn't exist in the specified directory. \n
              If you want lczexplore to try and download it type 1, else type 2 to exit and get the tiff map by yourself")
      print("choice: "); print(choice)
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



}