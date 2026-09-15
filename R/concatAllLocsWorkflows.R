#' loadConcatAllLocsAllWfs is recommended instead as it loadsand concatenate input files
#' and can even fill missing geometries with "unclassified" type.
#' But if maps are already loaded in a List, concatAllLocsWorkflows is
#' the equivalent of concatenateAlocationWorkflows, but for several Locations
#' Take a list of locations, each list contains a list of sf files
#' with an lcz_primary column, and concatenates them into a single sf object,
#' adding a column for location and workflow names 
#' @param sfList contains a list of locations containing list of sf for each workflow
#' @param refCrs a number telling which sf of the sfList will be the reference in termes of Coordinate Reference System
#' @importFrom sf st_transform st_crs st_drop_geometry
#' @return returns a single sf object containing all geometries for all locations and workflows, and the the
#' lcz types in the column lcz_primary
#' @export
#' @examples
#' sfListAll<-loadMultipleLocsSfs(dirPath = paste0(
#'  system.file("extdata", package = "lczexplore"),"/multipleWfs/"),
#'                               workflowNames = c("osm","bdt","wudapt"),
#'                               inLocation = c("Arville", "Redon"))
#' allLocsAllWfs <-  concatAllLocsWorkflows(
#'  sfList = sfListAll)
concatAllLocsWorkflows <- function(sfList, refCrs = 1) {

  locations <- names(sfList)
  concatSf <- vector(mode = "list", length = length(locations))
  names(concatSf) <- locations
  for (loc_i in locations) {
    concatSf[[loc_i]] <- concatAlocationWorkflows(
      sfList = sfList[[loc_i]], location = loc_i, refCrs = refCrs
    )
  }
  concatSf <- do.call(rbind, concatSf)
  # concatSf<-unlist2d(sfList)
  return(concatSf)
}

