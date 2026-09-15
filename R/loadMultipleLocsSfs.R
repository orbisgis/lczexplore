#' In a given directory the function looks different locations as subdirectories and LCZ datafiles
#' in each subdirectory, and load them in a list
#' In each directory, files must have names built as follow :
#' <wf>_lcz.<fileExtension>, where wf are the values specified in workflowNames parameter and
#' fileExtension is a file extension known by sf drivers, like fgb, geojson...
#' @param dirPath is the place where the files are
#' @param workflowNames sets the names of workflows
#' @param inLocations is the name of the location at which all LCZ are created
#' @param fileExtension is the extensions of the files to load (.fgb is the recommended format)
#' @param columns contains the names of the columns containing the LCZ types
#' @importFrom forcats fct_recode
#' @importFrom dplyr mutate
#' @import sf units RColorBrewer utils grDevices
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form,
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
#' sfList<-loadMultipleLocsSfs(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
#' workflowNames = c("osm","bdt","wudapt"), inLocation = c("Arville", "Redon"))
loadMultipleLocsSfs <- function(
  dirPath = paste0(
    system.file("extdata", package = "lczexplore"), "/multipleWfs/"),
  workflowNames = c("osm", "bdt", "wudapt"), inLocations = c("Arville", "Redon"),
  fileExtension = ".fgb", columns = NULL) {

  dirList <- list.dirs(dirPath, recursive = FALSE)
  print(dirList)
  print(inLocations)

  if (length(inLocations) < length(dirList) | prod(!is.na(inLocations)) == 0) {
    inLocations <- gsub(pattern = "(.*)(/)(.+)", replacement = "\\3", x = dirList)
    message(
      paste0("Some location names are missing, the following directory names will replace location names: "
        , paste0(inLocations, collapse = ", ")))
  }

  if (is.null(columns) | prod(is.na(columns)) == 0) {
    message("The names of the LCZ types columns were mis-specified, attempt to load with default 'lcz_primary' name")
    columns <- rep("lcz_primary", length(dirList))
  }

  allLocAllWfs <- vector("list", length = length(inLocations))
  names(allLocAllWfs) <- inLocations
  for (loc_i in seq_along(dirList)) {
    allLocAllWfs[[inLocations[loc_i]]] <-
      loadMultipleSfs(
        dirPath = dirList[loc_i],
        inLocation = inLocations[loc_i],
        workflowNames = workflowNames, fileExtension = fileExtension, columns = columns[loc_i]
      )
  }
  return(allLocAllWfs)
}

# sfListAll<-loadMultipleLocsSfs(dirPath = paste0(
# system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
# workflowNames = c("osm","bdt","wudapt"), inLocations = c(NA, "Redon"))