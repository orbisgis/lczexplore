repairRoadsIAU<-function(sfList, zoneSf, location){
  zoneSf<-st_transform(zoneSf,
                       crs=st_crs(sfList$iau))
  iauDiff<-st_difference(
    st_union(zoneSf$geometry),
    st_union(sfList$iau$geometry))

  if(length(iauDiff)==0){stop("st_difference returns no features, maybe the roads were already repaired")}


  iauDiff<-st_as_sf(iauDiff)
  st_set_geometry(iauDiff, iauDiff$x)
  iauDiff$lcz_primary<-105
  iauDiff$location<-location
  iauDiff$wf<-"iau"

  names(iauDiff)[names(iauDiff)=="x"]<-"geometry"
  st_geometry(iauDiff)<-"geometry"
  iauDiff<-iauDiff[,c("lcz_primary", "location", "wf", "geometry")]
  iauDiff<-st_as_sf(iauDiff)
  sfList$iau<-rbind(sfList$iau, iauDiff)
  return(sfList)
}