#' For an sf object containing LCZ types, aggregates touching geometries with same value of LCZ 
#' @param sf contains the geometry and LCZ levels
#' @param column the name of the column containing LCZ types
#' @param wfColumn is the column where the workflow used to produce LCZ classif is precised.
#' @param aggregateBufferSize is the size of a buffer you can add to the geometries before agregation. 
#' Useful when you suspect numeric precision to create false disjunction of geometries.
#' @param locationColumn is the name of the column where the location is stored
#' @import sf
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise mutate ungroup all_of across
#' @return an sf object containing the agregated geometries, their LCZ types and the wf and location columns 
#' if present in the initial sf
#' @export
#' @examples
#' dirPath<-paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs")
#' allLocAllWfs<-loadConcatAllLocsAllWfs(
#'  dirPath = dirPath, locations = c("Redon", "Arville"),
#' workflowNames = c("osm","bdt","wudapt"),
#'  missingGeomsWf= "osm",
#'  refWf = NULL,
#'  refLCZ = "Unclassified",
#'  residualLCZvalue = "Unclassified",
#'  column = "lcz_primary"
#' )
#' ASUallLocAllWfs <- aggregateRSUsByLCZ(
#' allLocAllWfs,
#' column = "lcz_primary", wfColumn = "wf", locationColumn = "location", aggregateBufferSize = 0.5)
aggregateRSUsByLCZ <- function(sf, aggregateBufferSize = 0, column, wfColumn, locationColumn = "location") {
  groupCols <- as.list(environment())[c("wfColumn", "locationColumn", "column")]
  presentColArgs <- !c(missing(wfColumn), missing(locationColumn), missing(column))
  groupCols <- unname(unlist(groupCols[presentColArgs]))
  print(groupCols)
  clustered <- sf %>%
    st_buffer(dist = aggregateBufferSize) %>%
    dplyr::group_by(across(all_of(groupCols))) %>%
    dplyr::summarise() %>%
    ungroup %>%
    st_cast("MULTIPOLYGON") %>%
    st_cast("POLYGON") %>%
    dplyr::mutate(area = drop_units(st_area(geometry))) %>%
    ungroup %>%
    ungroup %>%
    ungroup
  return(clustered)
}