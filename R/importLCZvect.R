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
#' redonBDTex<-importLCZvectFromFile(dirPath=paste0(system.file("extdata", package = "lczexplore"),
#' "/bdtopo_2_2/Redon"), file="rsu_lcz.geojson", column="LCZ_PRIMARY",
#' geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE")
importLCZvectFromFile <- function(
  dirPath, file = "rsu_lcz.geojson", column, geomID = "", confid = "", verbose = TRUE, drop = TRUE) {
  if (!file.exists(dirPath)) { stop(message = "The directory set in dirPath doesn't seem to exist") }

  fileName <- paste0(dirPath, "/", file)
  # select only the needed column, that is the unempty strings among column, geomID and confid
  colonnes <- c(geomID, column, confid)
  colonnes <- colonnes[sapply(colonnes, nchar) != 0]

  # Check if all the desired columns are present in the source file and only loads the file if the columns exist
  ### DOESN'T WORK WITH flatgeobuffer
  nom <- gsub(pattern = "(.+?)(\\.[^.]*$|$)", x = file, replacement = "\\1")
  extension <- gsub(pattern = "(.+?)(\\.[^.]*$|$)", x = file, replacement = "\\2")
  if (extension != ".fgb") { # Some metadata for fgb files do not specify table/layer names
    query <- paste0("select * from ", nom, " limit 0") # So this query wouldn't work with such fgb files
    sourceCol <- st_read(dsn = fileName, query = query, quiet = !verbose) %>% names
    colonnes<-checkColnameCase(colonnes, sourceCol)
    sfFile <- sf::st_read(dsn = fileName, quiet = !verbose)[, colonnes]    
  } else { 
      if (extension == ".fgb") {
        sfFile <- sf::st_read(dsn = fileName, quiet = !verbose)[,]
        sourceCol <- names(sfFile)
        colonnes<-checkColnameCase(colonnes, sourceCol)
        sfFile<-sfFile[,colonnes]
      }
  }

  return(sfFile)
}


#' Imports Local Climate Zone classifications from a standard geographical file (tested : geojson, shp, more to come)
#'
#' @param dirPath is the path of the directory of the file
#' @param sfIn is the sf object containing the LCZ map
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
#' redonBDTex2<-importLCZvectFromSf(sfIn = redonBDTex , column="LCZ_PRIMARY",
#'                                  geomID="ID_RSU",confid="LCZ_UNIQUENESS_VALUE")
importLCZvectFromSf <- function(sfIn, column, geomID = "", confid = "") {
  colonnes <- c(geomID, column, confid)
  colonnes <- colonnes[sapply(colonnes, nchar) != 0]
  sourceCol <- names(sfIn)
  colonnes<-checkColnameCase(userColNames = colonnes, dataColNames =sourceCol)
  sfFile<-sfIn[,colonnes]  
  
  return(sfFile)
}


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
importLCZvect <- function(dirPath, file = "rsu_lcz.geojson", output = "sfFile", column = "LCZ_PRIMARY",
                    geomID = "", confid = "",
                    typeLevels =  .lczenv$typeLevelsDefault,
                    drop = T, verbose = FALSE, sfIn = NULL, naAsUnclassified = TRUE) {
  
  if (is.null(sfIn)) {
    sfFile <- importLCZvectFromFile(
      dirPath = dirPath, file = file, column = column, geomID = geomID, confid = confid,
      drop = drop, verbose = verbose)
  } else {
    sfFile <- importLCZvectFromSf(sfIn, column, geomID = "", confid = "")
  }


  # if typeLevels is empty
  if (length(typeLevels) == 1) {
    typeLevels <- unique(subset(sfFile, select = all_of(column), drop = TRUE))
    names(typeLevels) <- typeLevels
  }

  if (column != "") {
    prov <- as.character(unique((st_drop_geometry(subset(sfFile, select = column, drop = T))))) %>% as.character
    names(prov) <- prov
    if (prod(prov %in% typeLevels) == 0) {
      if (verbose == TRUE) {
        print("levels in typeLevels are : ")
        print(typeLevels)
        print("levels in original data set are ")
        print(unique(subset(sfFile, select = column, drop = T)))
      }
      warning(
        paste0("The levels you specified with the typeLevels argument don't cover the LCZ values in your source file. \n",
               "Some geoms have been dropped, this could seriously alter your analysis, \n",
               "please check the levels or enter an empty string as typeLevels")
      )

    }
    if (sum(prov %in% typeLevels) == 0) {
      stop(
        paste0("none of the levels present in ", column,
               " is covered by the levels you specified.",
               "Check your choice of column and your choice of levels",
               " If you let typeLevels set by default, ", column,
               " must contain LCZ types in a standard format"))
    }

    sfFile <-
      sfFile %>%
        mutate(!!column := fct_recode(
          factor(subset(sfFile, select = column, drop = T), levels = typeLevels), !!!typeLevels))  #%>%
        # 
    if (naAsUnclassified){ sfFile[[column]]<- forcats::fct_na_value_to_level(sfFile[[column]], "Unclassified") }
    else { sfFile <- drop_na(sfFile, column)}
  }
  else { stop("You must specify the column containing the LCZ") }


  #sfFile <- sfFile%>% mutate(!!column:=fct_recode(subset(sfFile,select=column,drop=T),!!!typeLevels))

  if (output == "sfFile") { return(sfFile) } else {
    if (output == "bBox") {
      bBox <- st_bbox(sfFile, crs = st_crs(sfFile)) %>%
        st_as_sfc %>%
        st_make_valid(geos_keep_collapsed = FALSE)

      return(bBox) }
    else {
      stop("Output must be sfFile to return geoms and LCZ or bBox to return the bounding box") }

  }
}


