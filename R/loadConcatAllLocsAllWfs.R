#' In a given list of directories the function looks for LCZ datafiles, return a datasets LCZ values for each geom and workflow
#' @param dirPath is the path to the root directory where are the
#' subdirectories containing  different location LCZ files
#' @param workflowNames sets the names of workflows and define the name of the files which will be loaded and intersected
#' @param locations for each diretory from dirList, a location name must be fed to the function
#' @param missingGeomsWf the name of the workflow where some areas were not classified
#' @param refWf the name of the workflow one uses as a reference to complete the incomplete workflow.
#' if NULL then the geometries added will get the level specified in residualLCZvalue, else, the level in refLCZ
#' @param refLCZ the reference level the completed geometries will receive
#' @param residualLCZvalue the value the completed geometries where the reference workflow does not classify 
#' the geometry with refLCZ level
#' @param column a parameter to feed addMissingRSUs function
#' @import sf utils
#' @importFrom magrittr "%>%"
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form, 
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
#' dirPath<-paste0(
#' system.file("extdata", package = "lczexplore"),
#' "/multipleWfs")
#' allLocAllWfs<-loadConcatAllLocsAllWfs(
#'  dirPath = dirPath, locations = c("Redon", "Arville"),
#' workflowNames = c("osm","bdt","wudapt"),
#'  missingGeomsWf= "osm",
#'  refWf = NULL,
#'  refLCZ = "Unclassified",
#'  residualLCZvalue = "Unclassified",
#'  column = "lcz_primary"
#' )
loadConcatAllLocsAllWfs <- function(dirPath, locations = NA, workflowNames = c("osm", "bdt", "wud"),
                                    missingGeomsWf = "osm", refWf = NULL, refLCZ = NA,
                                    residualLCZvalue = NA, column = "lcz_primary") {
  # allLocAllWfSf<-matrix(ncol = 5, nrow = 0)
  dirList <- list.dirs(dirPath, recursive = FALSE)
  if (is.null(locations) || (length(locations) == 1 && is.na(locations))) {
    locations <- gsub(pattern = "(.*)(/)(.+)(/$|/{0})", replacement = "\\3", x = dirList)
  } else {
    # Remplacer uniquement les NA dans locations par les noms extraits de dirList
    extracted_names <- gsub(pattern = "(.*)(/)(.+)(/$|/{0})", replacement = "\\3", x = dirList)
    locations <- ifelse(is.na(locations), extracted_names, locations)
  }


  tmp <- st_sfc()
  class(tmp)[1] <- "sfc_POLYGON"
  allLocAllWfSf <- data.frame(
    lcz_primary = character(0), location = character(0),
    wf = character(0), area = numeric(0)) %>%
    st_as_sf(geometry = st_sfc(),  # Initialize with an empty geometry column
             crs = 4326)

  for (i in seq_along(dirList)) {
    dirPath <- dirList[i]
    if (substring(text = dirPath, first = nchar(dirPath)) != "/") { dirPath <- paste0(dirPath, "/") }
    aLocation <- locations[i]
    print(aLocation)
    sfList <- loadMultipleSfs(dirPath = dirPath,
                              workflowNames = workflowNames, inLocation = aLocation)
    if (substr(dirPath, nchar(dirPath), nchar(dirPath)) != "/") { dirPath <- paste0(dirPath, "/") }
    zoneSfPath <- paste0(dirPath, "zone.fgb")
    zoneSf <- read_sf(zoneSfPath)
    sfList <- addMissingRSUs(sfList = sfList,
                             missingGeomsWf = missingGeomsWf, zoneSf, refWf = refWf, refLCZ = refLCZ,
                             residualLCZvalue = residualLCZvalue,
                             column = "lcz_primary")
    concatSf <- concatAlocationWorkflows(sfList = sfList,
                                         location = aLocation, refCrs = 1)
    if (st_crs(allLocAllWfSf) != st_crs(concatSf)) {
      allLocAllWfSf <- st_transform(allLocAllWfSf, crs = st_crs(concatSf))
    }
    allLocAllWfSf <- rbind(allLocAllWfSf, concatSf)
  }
  return(allLocAllWfSf)
}

