#' Compares several sets of geographical classifications, especially Local Climate Zones classifications
#' @param sfInt an sf objects with intersected geometries and the LCZ columns for each workflow LCZ
#' @param LCZcolumns a vector which contains, the name of the columns of the classification to compare
#' @param sfWf a vector of strings which contains the names of the workflows used to produce the sf objects
#' @param trimPerc this parameters indicates which percentile to drop out of the smallest geometries resulting 
#' from the intersection of the original sf geometries intersection. 
#' It allows to account for numeric precision errors and to speed up computations at the cost of not considering the smallest geometries. 
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a longer form, 
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' pseudoK is a heuristic estimate of a Cohen's kappa coefficient of agreement between classifications
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' @export
#' @examples
#' 
compareMultipleLCZ<-function(sfInt, LCZcolumns, sfWf=NULL, trimPerc=0.05){
  if (is.null(LCZcolumns)) { 
    LCZcolumns<-names(sfInt)[!names(sfInt)%in%c("area", "geometry")]
  }
  sfInt <- sfInt %>% subset(area>quantile(sfInt$area, probs=trimPerc) & !is.na(area))
  # if input intersected file comes from a concatenation, it will have a location column that is not needed
  if("location" %in% names(sfInt)){ sfInt<-sfInt[,!names(allLocIntersected)=="location"]}
  
  sfIntnogeom<-st_drop_geometry(sfInt)
  
  if (is.null(sfWf) | length(sfWf)!=length(LCZcolumns)){sfWf<-LCZcolumns}
  
  for (i in 1:(length(LCZcolumns) - 1)) {
    for(j in (i+1):length(LCZcolumns)){
      compName<-paste0(sfWf[i],"_",sfWf[j])
      print(compName)
      sfIntnogeom[,compName]<-sfIntnogeom[ , LCZcolumns[i]] == sfIntnogeom[ , LCZcolumns[j]]
    }
  }
  rangeCol<-(length(LCZcolumns)+2):ncol(sfIntnogeom)
  print(rangeCol)
  # print(names(sfIntnogeom[,rangeCol]))
  sfIntnogeom$nbAgree<-apply(
     X = sfIntnogeom[,rangeCol],MARGIN=1,sum)
  sfIntnogeom$maxAgree<-apply(
    X = sfIntnogeom[,1:length(LCZcolumns)], MARGIN = 1, function(x) max(table(x) ))
 print(head(sfIntnogeom))
  
  # long format
  sfIntLong<-pivot_longer(sfIntnogeom, cols=names(sfIntnogeom)[rangeCol], names_to = "whichWfs", values_to = "agree")
  
  # Get the reference LCZ column on which 2 wf agree
  
  whichLCZagree <- gsub( x = sfIntLong$whichWfs, pattern = "(.*)(_)(.*)", replacement = "\\1")
  indRow<- seq_len(nrow(sfIntLong))
  z<-data.frame(indRow, whichLCZagree)

  sfIntLong$LCZvalue<-apply(z, 1, function(x) unlist(st_drop_geometry(sfIntLong)[x[1], x[2]]))

  sfInt<-cbind(sfIntnogeom,sfInt$geometry)  %>% st_as_sf()
  
  
  output<-list(sfInt=sfInt, sfIntLong=sfIntLong)
}


