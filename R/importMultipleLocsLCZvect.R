#' In a given directory the function looks different locations as subdirectories and LCZ datafiles
#' in each subdirectory, and load them in a list
#' In each directory, files must have names built as follow :
#' wf_lcz.<fileExtension>, where wf are the values specified in workflowNames parameter and
#' fileExtension is a file extension known by sf drivers, like fgb, geojson...
#' @param dirPath is the place where the files are
#' @param workflowNames sets the names of workflows
#' @param locations is the name of the location at which all LCZ are created
#' @param fileExtension is the extensions of the files to load (.fgb is the recommended format)
#' @param columns contains the names of the columns containing the LCZ types
#' @importFrom forcats fct_recode
#' @importFrom dplyr mutate
#' @import sf units RColorBrewer utils grDevices
#' @return a list of sf, containing one element per location,
#' each of the containing one sf per werkflow
#' @export
#' @examples
#' sfList<-importMultipleLocsLCZvect(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
#' workflowNames = c("osm","bdt","wudapt"), location = c("Arville", "Redon"))
importMultipleLocsLCZvect <- function(
  dirPath = paste0(
    system.file("extdata", package = "lczexplore"), "/multipleWfs/"),
  workflowNames = c("osm", "bdt", "wudapt"), locations = c("Arville", "Redon"),
  fileExtension = ".fgb", columns = NULL) {

  dirList <- list.dirs(dirPath, recursive = FALSE)
  print(dirList)
  print(locations)

  if (length(locations) < length(dirList) | prod(!is.na(locations)) == 0) {
    locations <- gsub(pattern = "(.*)(/)(.+)", replacement = "\\3", x = dirList)
    message(
      paste0("Some location names are missing, the following directory names will replace location names: "
        , paste0(locations, collapse = ", ")))
  }

  if (is.null(columns) | prod(is.na(columns)) == 0) {
    message("The names of the LCZ types columns were mis-specified, attempt to load with default 'lcz_primary' name")
    columns <- rep("lcz_primary", length(dirList))
  }

  allLocAllWfs <- vector("list", length = length(locations))
  names(allLocAllWfs) <- locations
  for (loc_i in seq_along(dirList)) {
    allLocAllWfs[[locations[loc_i]]] <-
      importMultipleLCZvect(
        dirPath = dirList[loc_i],
        location = locations[loc_i],
        workflowNames = workflowNames, fileExtension = fileExtension, columns = columns[loc_i]
      )
  }
  return(allLocAllWfs)
}

# sfListAll<-importMultipleLocsLCZvect(dirPath = paste0(
# system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
# workflowNames = c("osm","bdt","wudapt"), locations = c(NA, "Redon"))