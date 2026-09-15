#' In a given directory (or a list of directories) the function looks for LCZ datafiles,
#' intersects them and return a dataset with intersected geometries and LCZ values for each workflow
#' @param dirList the list of directories for which the different LCZ files will be intersected
#' @param workflowNames sets the names of workflows and define the name of the files
#' which will be loaded and intersected
#' @param columns the name (string) of the column containing LCZ types.
#' If the different workflows do no use the same column names, a vector of names is passed
#' @param inLocations : for each diretory from dirList, a location name must be fed to the function
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf
#' @return returns an sf object with all intersections
#' @export
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"), recursive = FALSE)
#' allLocIntersected<-concatIntersectedLocations(
#'  dirList = dirList, inLocations = c("Arville", "Redon"), columns = "lcz_primary")
concatIntersectedLocations <- function(dirList, workflowNames = c("osm", "bdt", "wudapt"),
                                       inLocations,
                                       columns = "lcz_primary") {
  sfList <- list()
  if (length(inLocations) != length(dirList) | is.null(inLocations)) {
    inLocations <- gsub(pattern = "(.*)(/)(.+)(/$|/{0})", replacement = "\\3", x = dirList) }

  if (length(columns == 1)) { columns <- rep(columns, length(workflowNames)) }
  intersectedList <- list()

  for (i in seq_along(dirList)) {
    intersectedList[[i]] <- loadMultipleSfs(
      dirPath = dirList[i],
      workflowNames = c("osm", "bdt", "wudapt"),
      inLocation = inLocations[i], columns = columns[i]) %>%
      createIntersect(columns = columns, refCrs = NULL, workflowNames = workflowNames,
                      minZeroArea = 0.0001
      )
  }

  concatIntersectedSf <- do.call(rbind, intersectedList)

  # concatIntersectedSf$location<-factor(
  #   concatIntersectedSf$location, levels = .lczenv$typeLevelsDefault)
  return(concatIntersectedSf)
}