summariseRSUs<-function(sf, column ){
  # sf<-sf %>% mutate(area=st_area(geometry)) %>% st_drop_geometry() %>% as.data.frame
  
  unClustered <-sf %>% group_by(.data[[column]])  %>%
    summarise(numberRSUs=n(), meanArea = mean(st_area(geometry))) %>% st_drop_geometry()
  
  clustered <-sf %>% group_by(.data[[column]]) %>%  summarise(geometry = st_union(geometry))
  clustered <- lapply(1:nrow(clustered), function(i) {
    st_cast(clustered[i, ], "POLYGON")
  }) %>%
    do.call(rbind, .)
  clustered <- clustered %>%group_by(.data[[column]]) %>%  summarise(numberRSUs=n(), meanArea = mean(st_area(geometry))) %>% st_drop_geometry
  output <- list(unClustered = unClustered, clustered = clustered)
  return(output)
}

osm<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                   file = "osm_lcz.fgb")
osm %>% group_by(LCZ_PRIMARY) %>% summarise(geometry = st_union(geometry))
showLCZ(osm)
test<-summariseRSUs(osm, column ="LCZ_PRIMARY")

osm2<-osm %>% group_by(LCZ_PRIMARY) %>% summarise() %>% ungroup 

osm3<-lapply(1:nrow(osm2), function(i) {
  st_cast(osm2[i, ], "POLYGON")
}) %>%
  do.call(rbind, .)

osm3 %>% group_by(LCZ_PRIMARY) %>% summarise(numberRSUs=n(), meanArea = mean(st_area(geometry)))

showLCZ(osm3)


osm3 <-aggregate(osm, by = osm2, do_union = FALSE)
  x,
  by,
  FUN,
  ...,
  do_union = TRUE,
  simplify = TRUE,
  join = st_intersects
)
  
  
  group_by(LCZ_PRIMARY) %>% summarise %>% ungroup %>% st_cast("POLYGON")

st_area(osm2)
showLCZ(osm2)