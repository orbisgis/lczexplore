#' Imports Local Climate Zone classifications from a standard geographical file (tested : geojson, shp, more to come)
#'
#' @param dirPath is the path of the directory of the file
#' @param file is the name of the file from which the LCZ are imported
#' @param column indicates the name of the column containing LCZ values. 
#' LCZ values are expected to be of a standard LCZ format (1 to 17, or 1 to 10 and 101 to 107 or 1 to G),
#' else, use the importQualVar function
#' @param geomID is the name of the column containing the ID of each geom to load. 
#' If an empty string, no column is loaded.
#' @param confid is the name of the column containing a confidence indicator to filter geoms,
#' for instance the uniqueness of the LCZ level of each geom
#' @param output : if sfFile, the function returns an sfFile with the specified columns,
#' if bBox, returns a bounding box one can use to crop a raster file or to intersect another sf file
#' @param typeLevels the levels of the imported LCZ classification
#' @param verbose if TRUE show the discrepancies between specified levels of LCZ and
#' levels actually present in column
#' @param drop : the default is TRUE, which means all the column are 
#' dropped excepted those specified in previous parameters
#' @import dplyr forcats rlang sf
#' @importFrom terra crop
#' @importFrom tidyr drop_na
#' @importFrom terra rast
#' @return returns an sf object containing at least the geoms and the LCZ values, 
#' and if specified, columns for the IDs of the geoms and the confidence value of the LCZ levels.
#' @export
#' @examples 
#' redonBDTex<-importLCZvect(dirPath=paste0(system.file("extdata", package = "lczexplore"),
#' "/bdtopo_2_2/Redon"), file="rsu_lcz.geojson", column="LCZ_PRIMARY",
#' geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE")
#' showLCZ(redonBDTex)
importLCZvect<-function(dirPath, file="rsu_lcz.geojson", output="sfFile", column="LCZ_PRIMARY",
                       geomID="", confid="",
                       typeLevels=c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                                 "9"="9","10"="10",
                                 "101"="101","102"="102","103"="103","104"="104", "105"="105","106"="106","107"="107",
                                 "101"="11","102"="12","103"="13","104"="14", "105"="15", "106"="16","107"="17",
                                  "101"="A","102"="B","103"="C","104"="D","105"="E","106"="F","107"="G"),
                       drop=T, verbose=FALSE){
  if (!file.exists(dirPath)){stop(message="The directory set in dirPath doesn't seem to exist")}
  if ( substr(dirPath, start=nchar(dirPath), stop = nchar(dirPath)) == "/") { 
    fileName<-paste0(dirPath,file)} else {
      fileName<-paste0(dirPath,"/",file)
  }
  
  # select only the needed column, that is the unempty strings among column, geomID and confid
  colonnes<-c(geomID,column,confid)
  colonnes<-colonnes[sapply(colonnes,nchar)!=0]

  # Check if all the desired columns are present in the source file and only loads the file if the columns exist
  ### DOESN'T WORK WITH flatgeobuffer
  nom<-gsub(pattern="(.+?)(\\.[^.]*$|$)",x=file,replacement="\\1")
  extension<-gsub(pattern="(.+?)(\\.[^.]*$|$)",x=file,replacement="\\2")
  if (extension != ".fgb"){ # Some metadata for fgb files do not specify table/layer names
    query<-paste0("select * from ",nom," limit 0") # So this query wouldn't work with such fgb files
    sourceCol<-st_read(dsn=fileName, query=query, quiet=!verbose) %>% names
    inCol<-colonnes%in%sourceCol
    badCol<-colonnes[!inCol]
    colErr<-c("It seems that some of the columns you try to import do not exist in the source file,
              are you sure you meant ",
                   paste(badCol),"?")
    if (prod(inCol)==0){ stop(colErr) } else { 
      if (drop== TRUE) {sfFile<-sf::st_read(dsn=fileName,quiet=!verbose)[,colonnes] } else {
        sfFile<-sf::st_read(dsn=fileName,quiet=!verbose)[,]}
    }
  } else {if (extension == ".fgb") {
    sfFile<-sf::st_read(dsn=fileName,quiet=!verbose)[,]
    sourceCol<-names(sfFile)
    inCol<-colonnes%in%sourceCol
    badCol<-colonnes[!inCol]
    colErr<-c("It seems that some of the columns you try to import do not exist in the source file,
              are you sure you meant ",
              paste(badCol),"?")
    if (prod(inCol)==0){ stop(colErr) }
       
  }
  
  }

  # if typeLevels is empty
  if (length(typeLevels)==1){
    typeLevels<-unique(subset(sfFile,select=all_of(column),drop=TRUE))
    names(typeLevels)<-typeLevels
  }

  # if typeLevels is not specified it will be set to default and we need to capture this later
  # typeLevelsDefault<-c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
  #                   "9"="9","10"="10","101"="101","102"="102","103"="103","104"="104",
  #                   "105"="105","106"="106","107"="107","101"="11","102"="12","103"="13","104"="14",
  #                   "105"="15", "106"="16","107"="17")
# Select columns from original file
if (column!=""){
   prov<-as.character(unique((st_drop_geometry(subset(sfFile,select=column,drop=T))))) %>% as.character
  names(prov)<-prov
  if( prod(prov%in%typeLevels)==0 ) {
    if (verbose==TRUE){
        print("levels in typeLevels are : ")
        print(typeLevels)
        print("levels in original data set are ")
        print(unique(subset(sfFile,select=column,drop=T)))
      }
      warning("The levels you specified with the typeLevels argument don't cover the LCZ values in your source file.
              Some geoms have been dropped,this could seriously alter your analysis, please check the levels or enter an empty string as typeLevels")

    }
 if( sum(prov%in%typeLevels)==0 ){
      stop(
        paste0("none of the levels present in ",column,
               " is covered by the levels you specified.",
               "Check your choice of column and your choice of levels",
               " If you let typeLevels set by default, ", column,
               " must contain LCZ types in a standard format"))
    }

    sfFile <-
    sfFile%>%
      mutate(!!column:=fct_recode(factor(subset(sfFile,select=column,drop=T),levels=typeLevels),!!!typeLevels)) %>%
      drop_na(column)
    }
  else {stop("You must specify the column containing the LCZ")}


  #sfFile <- sfFile%>% mutate(!!column:=fct_recode(subset(sfFile,select=column,drop=T),!!!typeLevels))

  if(output=="sfFile"){return(sfFile)} else {
    if(output=="bBox"){ 
      bBox<-st_bbox(sfFile,crs=st_crs(sfFile)) %>% st_as_sfc %>% st_make_valid(geos_keep_collapsed = FALSE)
      
      return(bBox) }
    else {
      stop("Output must be sfFile to return geoms and LCZ or bBox to return the bounding box")}

    }
}