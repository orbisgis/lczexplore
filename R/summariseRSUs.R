#' For a given LCZ sf object, returns the number of geometries and their mean area per level of LCZ
#' @param sf contains the geometry and LCZ levels
#' @param columns a the LCZ column name
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return the number of geometries (Reference Spatial units or RSUs) 
#' and their mean area per level of LCZ, and the same after agregatting geometries 
#' with same level of LCZ which touch each other
#' @export
#' @examples
summariseRSUs<-function(sf, column = "lcz_primary", aggregateBufferSize = 0.00001, trim = 0.1 ){
  # sf<-sf %>% mutate(area=st_area(geometry)) %>% st_drop_geometry() %>% as.data.frame
  
  unClustered <-sf %>% group_by(.data[[column]])  %>%
    dplyr::summarise(numberRSUs=n(), meanArea = round(mean(st_area(geometry), na.rm = TRUE, trim = trim), digits = 0)) %>% st_drop_geometry()
  
  clustered <-sf %>% group_by(.data[[column]]) %>%  st_buffer(dist=aggregateBufferSize) %>%
    dplyr::summarise() %>% ungroup %>% 
    st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>%  group_by(.data[[column]]) %>%
    dplyr::summarise(numberRSUsClust=n(), 
                     meanAreaClust = round(mean(st_area(geometry), na.rm = TRUE, , trim = trim), digits = 0),
                     totalAreaClust = round(sum(st_area(geometry), na.rm = TRUE), digits = 0) ) %>% st_drop_geometry
  output <- full_join(unClustered, clustered, by = column)
  names(output)[1]<-"lcz"
  return(output)
}

