#' Imports a qualitative variable associated with  polygons
#' from a geographical dataset (tested : geojson, shp, more to come)
#'
#' @param dirPath is the path of the directory of the file
#' @param file is the name of the file from which the variable is imported
#' @param column indicates the name of the column containing the qualitative variable is imported
#' @param geomID is the name of the column containing the ID of each geom to load. 
#' If an empty string, no column is loaded.
#' @param confid is the name of the column containing a confidence indicator, 
#' for instance the uniqueness of the LCZ level of each geom, in order to filter geoms,
#' @param output : if sfFile, the function returns an sfFile with the qualitative variable,
#' if bBox, returns a bounding box one can use to crop a raster file or to intersect another sf file
#' @param typeLevels the levels of the imported qualitative variable
#' @param drop : the default is TRUE, which means all the column are dropped excepted those specified in previous parameters
#' @param verbose allows to hide some messages when set to false.
#' @import dplyr forcats rlang sf
#' @importFrom forcats fct_recode
#' @return returns an sf object containing at least the geoms and the qualitative variable,
#' and if specified, columns for the IDs of the geoms and the confidence value of 
#' thevalues of the variable.
#' @export
#' @examples 
#' utrfRedonBDT<-importQualVar(dirPath=paste0(
#' system.file("extdata", package = "lczexplore"), "/bdtopo_2_2/Redon"),
#' file="rsu_utrf_area.geojson", column="TYPO_MAJ")
#' showLCZ(sf=utrfRedonBDT, column="TYPO_MAJ",repr="alter")
#' utrfRedonOSM<-importQualVar(dirPath=
#' paste0(system.file("extdata", package = "lczexplore"),"/osm/2022/Redon"),
#' file="rsu_utrf_area.geojson", column="TYPO_MAJ",geomID="ID_RSU",confid="UNIQUENESS_VALUE")
#' # One can now compare these 2 classifications and store the result in an object...
#' utrfComparison<-compareLCZ(
#'  sf1=utrfRedonBDT, column1="TYPO_MAJ", wf1=" UTRF BDT",
#'  sf2=utrfRedonOSM, column2="TYPO_MAJ", wf2 = " UTRF OSM",
#' location = " Redon",exwrite=FALSE,repr="alter")
#'  # ... then plot the confusion matrix of these two classifications  
#' print(utrfComparison$matConfPlot)
#' 
#' 
importQualVar<-function(dirPath, file="rsu_utrf_area.geojson", output="sfFile", column="TYPO_MAJ",
                       geomID="ID_RSU", confid="UNIQUENESS_VALUE",
                       typeLevels="",
                       drop=T, verbose=TRUE){
  if (!file.exists(dirPath)){stop(message="The directory set in dirPath doesn't seem to exist")}

  fileName<-paste0(dirPath,"/",file)
  # select only the needed column, that is the unempty strings among column, geomID and confid
  colonnes<-c(geomID,column,confid)
  colonnes<-colonnes[sapply(colonnes,nchar)!=0]

  # Check if all the desired columns are present in the source file and only loads the file if the columns exist
  nom<-gsub(pattern="(.+?)(\\.[^.]*$|$)",x=file,replacement="\\1")
  query<-paste0("select * from ",nom," limit 0")
  sourceCol<-st_read(dsn=fileName,query=query) %>% names
  inCol<-colonnes%in%sourceCol
  badCol<-colonnes[!inCol]
  colErr<-c("It seems that some of the columns you try to import do not exist in the source file,
            are you sure you meant ",
            paste(badCol)," ?")
  if (prod(inCol)==0){ stop(colErr) } else { sfFile<-st_read(dsn=fileName)[,colonnes] }

  if (column!=""){
    if(drop==T){sfFile<-subset(sfFile,select=colonnes)}
  

    
    
  # if typeLevels is empty
  if (length(typeLevels)<=1){
    typeLevels<-unique(subset(sfFile,select=column,drop=TRUE))
    names(typeLevels)<-typeLevels
  }
 else { 

    prov<-as.character(unique((st_drop_geometry(subset(sfFile,select=column,drop=T)))))
    names(prov)<-prov

    if( prod(prov%in%typeLevels)==0 ){
      if (verbose==T){
        print("levels in typeLevels are : ")
        print(typeLevels)
        print("levels in the original dataset are ")
        print(unique(subset(sfFile,select=column,drop=T)))
      }
      warning("The levels you specified with the typeLevels argument don't cover the values in the column of your source file.
              Some geoms have been dropped,this could seriously alter your analysis, please check the levels or enter an empty string as typeLevels")

    }
    if( sum(prov%in%typeLevels)==0 ){
      stop(
        paste0("none of the levels present in ",column,
               " is covered by the levels you specified.",
               "Check your choice of column and your choice of levels"))
    }
  }


    sfFile[[column]]<-factor(sfFile[[column]],levels=typeLevels)
      sfFile<-drop_na(sfFile, column)
  }
  else {stop("You must specify the column containing your qualitative variable")}


  #sfFile <- sfFile%>% mutate(!!column:=fct_recode(subset(sfFile,select=column,drop=T),!!!typeLevels))

  if(output=="sfFile"){return(sfFile)} else {
    if(output=="bBox"){bBox<-st_bbox(sfFile,crs=st_crs(sfFile)) %>% st_as_sfc
      return(bBox)}
    else {
      stop("Output must be sfFile to return geoms and qualitative variable or bBox to return the bounding box")}

  }
}