#' In a given directory (or a list of directories) the function looks for LCZ datafiles
#' and load them in a list. In each directory, files must have names built as follow :
#' wf_lcz.fileExtension, where wf are the values specified in workflowNames parameter and
#' fileExtension is a file extension known by sf drivers, like fgb, geojson...
#' @param dirPath is the place where the files are
#' @param workflowNames sets the names of workflows
#' @param location is the name of the location at which all LCZ are created
#' @param fileExtension is the extensions of the files to load (.fgb is the recommended format)
#' @param columns the name (string) of the column containing LCZ types.
#' If the different workflows do no use the same column names, a vector of names is passed
#' @param typeLevels allows to pass the expected levels of the lcz columns
#' @importFrom forcats fct_recode
#' @importFrom dplyr mutate
#' @import sf units
#' @return returns a list containing one sf by workflow for a given locations
#' @export
#' @examples
#' sfList<-importMultipleLCZvect(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
#' workflowNames = c("osm","bdt","wudapt"), location = "Arville", columns = "lcz_primary")
importMultipleLCZvect <- function(
  dirPath, workflowNames = c("osm", "bdt", "wudapt"),
  location = NA,
  fileExtension = ".fgb",
  columns = "lcz_primary",
  typeLevels = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8",
                  "9" = "9", "10" = "10",
                  "101" = "101", "102" = "102", "103" = "103", "104" = "104", "105" = "105", "106" = "106", "107" = "107",
                  "101" = "11", "102" = "12", "103" = "13", "104" = "14", "105" = "15", "106" = "16", "107" = "17",
                  "101" = "A", "102" = "B", "103" = "C", "104" = "D", "105" = "E", "106" = "F", "107" = "G")) {

  if (is.null(typeLevels)){
    message("Levels for LCZ types in LLCZ columns were unspecified, standard levels will be tried")
    typeLevels <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8",
                    "9" = "9", "10" = "10",
                    "101" = "101", "102" = "102", "103" = "103", "104" = "104", "105" = "105",
                    "106" = "106", "107" = "107",
                    "101" = "11", "102" = "12", "103" = "13", "104" = "14",
                    "105" = "15", "106" = "16", "107" = "17",
                    "101" = "A", "102" = "B", "103" = "C", "104" = "D", "105" = "E", "106" = "F",
                    "107" = "G", "Unclassified" = "unclassified")

  }
   if (is.null(location) | prod(!is.na(location)) == 0) {
    print("location")
    print(location)
    location <- gsub(pattern = "(.*)(/)(.+)(/$)", replacement = "\\3", x = dirPath)
    print(location)
  }
  if (length(columns) == 1) { columns <- rep(columns, length(workflowNames)) }
  dirPath <- checkDirSlash(dirPath)
  print(dirPath)
  sfList <- list()
  for (i in seq_along(workflowNames)) {
    inName <- paste0(dirPath, workflowNames[i], "_lcz", fileExtension)
    inSf <- read_sf(inName)

    inSf[[columns[i]]] <- factor(inSf[[columns[i]]], levels = typeLevels)

    inSf$lcz_primary <- inSf[[columns[i]]]

    inSf <- select(inSf, lcz_primary) #%>% mutate(
     # lcz_primary = factor(lcz_primary, levels = typeLevels))
    inSf <- dplyr::mutate(inSf, wf = workflowNames[i], location = location, .before = geometry)
    sfList[[workflowNames[i]]] <- inSf

  }
  return(sfList)
}

#' Take a list of sf objects within the session and format them to allow
#' the use of createIntersect and multipleCompare functions.
#' @param sfList the list of LCZ sf objects
#' @param location the name of the location, all files from sfList must regard the same location
#' @param refCRS a number telling which sf
#' of the sfList will be the reference in termes of Coordinate Reference System
#' @param workflowNames a vector containing the workflow names of each sf object of sfList
#' @param columns contain the names of the columns where the sf objects of sfList are stored
#' @importFrom sf st_transform st_crs st_drop_geometry
#' @return returns a list of sf objects, each of them has columns lcz_primary, wf and location
#' @export
#' @examples
#' osm<-importLCZvect(dirPath = paste0(
#'      system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
#'      file = "osm_lcz.fgb")
#' bdt<-importLCZvect(dirPath = paste0(
#'      system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
#'  file = "bdt_lcz.fgb")
#' wudapt<-importLCZvect(dirPath = paste0(
#'      system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
#'  file = "wudapt_lcz.fgb", column = "lcz_primary")
#' sfList<-list(osm = osm, bdt = bdt, wudapt = wudapt)
#' sfListFormatted<- importMultipleLCZvectFromSession(sfList = sfList,
#' workflowNames = c("osm", "bdt", "wudapt"),location = "Arville",
#' columns = c("LCZ_PRIMARY", "LCZ_PRIMARY", "lcz_primary" ))
#' intersected<-createIntersect(sfList = sfListFormatted, columns = rep("lcz_primary", 3),
#' workflowNames = c("osm", "bdt", "wudapt"))
#' multicompare_test<-compareMultipleLCZ(intersected,
#' columns = c("osm","bdt","wudapt"),trimPerc = 0.5)
importMultipleLCZvectFromSession <- function(sfList, workflowNames, columns, location, refCRS = 1) {
  refCRS <- st_crs(sfList[[refCRS]])
  sfList <- lapply(sfList, st_transform, crs = refCRS)
  locations <- rep(location, length(workflowNames))

  if (is.na(location) | is.null(location)) {
    location <- tryCatch(
    { st_drop_geometry(sfList[[1]][[1]][1, "location"]) %>% as.character },
    error = function(...) {
      message("No location column or location column that contains not character")
    })
  }

  if( length(columns)== 1 & min(nchar(columns)>1) ) {
    message("You specified only one column name, it will be used for all sf files of sfList")
    columns <- rep( columns, times = length(sfList)) }

  if(is.null(columns)){
    message("LCZ column names are missing,
    the function will try to look for lcz_primary columns instead")
    columns<- rep("lcz_primary", rep = length (sfList)) }

  if (prod(!is.na(columns))==0){
    if (length(columns) == length(sfList)){
      message ("One of the column names is missing, lcz_primary will be tried instead")
      columns[is.na(columns)]<-"lcz_primary"
    } else {
      stop("Column vector should contain either on column name to be replicated or
        as many names as datasets in sfList")}
  }

  if( prod(columns == "lcz_primary")){ message("LCZ are now in the lcz_primary of resulting sf")}


  addLocWf <- function(sfIn, wf, location, column) {
    sfIn$wf <- wf
    sfIn$location <- location
    sfIn$lcz_primary <- sfIn[[column]]
    sfIn <- sfIn[, c("lcz_primary", "wf", "location", "geometry")]
    print(names(sfIn))
    return(sfIn)
  }

  sfListAugmented <- mapply(addLocWf, sfList, wf = workflowNames, location = locations, column = columns, SIMPLIFY = FALSE)
  return(sfListAugmented)
}