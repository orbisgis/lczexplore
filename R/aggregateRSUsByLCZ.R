#' For an sf object containing LCZ types, aggregates touching geometries with same value of LCZ 
#' @param sf contains the geometry and LCZ levels
#' @param LCZcolumn the name of the column coontaining LCZ types
#' @param wfColumn is the column where the workflow used to produce lcz is precised. 
#' @param aggregateBufferSize is the size of a buffer you can add to the geometries before agregation. 
#' Useful when you suspect numeric precision to create false disjunction of geometries.
#' @param locationColumn is the name of the column where the location is stored
#' @import sf dplyr 
#' @return an sf object containing the agregated geometries, their LCZ types and the wf and location columns 
#' if present in the initial sf
#' @export
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"))[-1]
#' allLocAllWfs<-concatAllLocationsAllWfs(
#'  dirList = dirList, locations = c("Blaru", "Goussainville"), 
#' workflowNames = c("osm","bdt","iau","wudapt"),
#'  missingGeomsWf = "iau",
#'  refWf = NULL,
#'  refLCZ = "Unclassified",
#'  residualLCZvalue = "Unclassified",
#'  column = "lcz_primary"
#')
#' ASUallLocAllWfs <- aggregateRSUsByLCZ(
#' allLocAllWfs,
#' LCZcolumn = "lcz_primary", wfColumn = "wf", location = "location", aggregateBufferSize = 0.5)
aggregateRSUsByLCZ<-function(sf, aggregateBufferSize = 0, LCZcolumn, wfColumn, locationColumn="location"){
  groupCols<-as.list(environment())[c("wfColumn", "locationColumn", "LCZcolumn")]
  presentColArgs<-!c(missing(wfColumn), missing(locationColumn), missing(LCZcolumn))
  groupCols<-unname(unlist(groupCols[presentColArgs]))
  print(groupCols)
  clustered <- sf %>% st_buffer(dist = aggregateBufferSize) %>% 
    dplyr::group_by(across(all_of(groupCols))) %>%
    dplyr::summarise()  %>% ungroup %>% 
     st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% 
     dplyr::mutate(area = drop_units(st_area(geometry))) %>% ungroup %>% ungroup %>% ungroup
  return(clustered)
}