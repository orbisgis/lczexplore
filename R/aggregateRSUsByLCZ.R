#' For an sf object containing LCZ types, aggregates touching geometries with same value of LCZ 
#' @param sf contains the geometry and LCZ levels
#' @param LCZcolumn the name of the column coontaining LCZ types
#' @param wfColumn is the column where the workflow used to produce lcz is precised. 
#' @param aggregateBufferSize is the size of a buffer you can add to the geometries before agregation. 
#' Useful when you suspect numeric precision to create false disjunction of geometries.
#' @import sf dplyr 
#' @return an sf object containing the agregated geometries, their LCZ types and the wf and location columns 
#' if present in the initial sf
#' @export
#' @examples
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