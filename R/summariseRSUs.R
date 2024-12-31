summariseRSUs<-function(sf, column ){
  # sf<-sf %>% mutate(area=st_area(geometry)) %>% st_drop_geometry() %>% as.data.frame
  
  unClustered <-sf %>% group_by(.data[[column]])  %>%
    summarise(numberRSUs=n(), meanArea = mean(st_area(geometry))) %>% st_drop_geometry()
  
  clustered <-sf %>% group_by(.data[[column]]) %>%  summarise() %>% ungroup %>% 
    st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>%  group_by(.data[[column]]) %>% 
    summarise(numberRSUs=n(), meanArea = mean(st_area(geometry))) %>% st_drop_geometry
  output <- list(unClustered = unClustered, clustered = clustered)
  return(output)
}

osm<-importLCZvect(dirPath = "/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree/Dourdan/",
                   file = "osm_lcz.fgb")
write_sf(osm, "inst/extdata/example.fgb")

# osm %>% group_by(LCZ_PRIMARY) %>% summarise(geometry = st_union(geometry))
# showLCZ(osm)
# test<-summariseRSUs(osm, column ="LCZ_PRIMARY")
# 
# osm2<-osm %>% group_by(LCZ_PRIMARY) %>% summarise() %>% ungroup 
# 
# osm3<-lapply(1:nrow(osm2), function(i) {
#   st_cast(osm2[i, ], "POLYGON")
# }) %>%
#   do.call(rbind, .)
# 
# osm3 %>% group_by(LCZ_PRIMARY) %>% summarise(numberRSUs=n(), meanArea = mean(st_area(geometry)))
# 
# showLCZ(osm3)
#  
# prov <- osm %>%  group_by(LCZ_PRIMARY) %>% summarise %>% ungroup %>% st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
# showLCZ(prov)
# 
# class(prov)
# 
# prov[11,]%>% st_cast("POLYGON")
# 
# st_area(osm2)
# showLCZ(osm2)
dput(osm)
options(timeout=1000)
tmp <- tempdir(check = TRUE)
zf <- file.path(tmp, "example2.geojson")
if(file.exists(zf)){file.remove(zf)}
if(!file.exists(zf)){
download.file(
  "https://raw.githubusercontent.com/MGousseff/lczexplore/multipleComparison/inst/extdata/dourdan_osm_lcz.fgb",
destfile = zf)
}

test<-read_sf(zf)
test %>% group_by(LCZ_PRIMARY) %>% summarise %>% ungroup %>% st_cast("POLYGON")
test %>% group_by(LCZ_PRIMARY) %>% summarise %>% ungroup %>% st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")


