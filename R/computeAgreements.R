computeAgreements<-function(intersec_sf, nbWfs) {
  intersec_sfnogeom<-st_drop_geometry(intersec_sf)
  for (i in 1:(nbWfs - 1)) {
    for(j in (i+1):nbWfs){
      compName<-paste0(i,"_",j)
      print(compName)
      intersec_sfnogeom[,compName]<-intersec_sfnogeom[,i] == intersec_sfnogeom[,j]
    }
  } # TODO : refactor with a matrix reduction operation
  
  rangeCol<-( (nbWfs+2):ncol(intersec_sfnogeom) ) # nbWfs+2 because of the area column
  print(names(intersec_sfnogeom)[rangeCol])
  # print(names(intersec_sfnogeom[,rangeCol]))
  intersec_sfnogeom$nbAgree<-apply(intersec_sfnogeom[,rangeCol],MARGIN=1,sum)
  intersec_sfnogeom$maxAgree<-apply(
    X = intersec_sfnogeom[,1:nbWfs], MARGIN = 1, function(x) max(table(x) ))
  intersec_sf<-cbind(intersec_sfnogeom,intersec_sf$geometry)  %>% st_as_sf()
}
