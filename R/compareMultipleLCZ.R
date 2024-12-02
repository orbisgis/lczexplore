#' Compares several sets of geographical classifications, especially Local Climate Zones classifications
#' @param sfList a list which contains the classifications to compare, as sf objects
#' @param LCZcolumns a vector which contains, for eacfh sf of sfList, the name of the column of the classification to compare
#' @param refCrs a number which indicates which sf object from sfList will provide the CRS in which all the sf objects will be projected before comparison
#' By defautl it is set to an empty string and no ID is loaded.
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
compareMultipleLCZ<-function(sfList, LCZcolumns, refCrs=NULL, sfWf=NULL, trimPerc=0.05){
  echInt<-createIntersect(sfList = sfList, columns = LCZcolumns , refCrs= refCrs, sfWf = sfWf)
  print(nrow(echInt))
  echInt$area<-st_area(echInt)
  echInt <- echInt %>% subset(area>quantile(echInt$area, probs=trimPerc) & !is.na(area))
  print(nrow(echInt))
  echIntnogeom<-st_drop_geometry(echInt)
  for (i in 1:(length(sfList) - 1)) {
    for(j in (i+1):length(sfList)){
      compName<-paste0(i,"_",j)
      print(compName)
      echIntnogeom[,compName]<-echIntnogeom[,i] == echIntnogeom[,j]
    }
  }
  rangeCol<-(length(sfList)+3):ncol(echIntnogeom)
  print(rangeCol)
  # print(names(echIntnogeom[,rangeCol]))
  echIntnogeom$nbAgree<-apply(echIntnogeom[,rangeCol],MARGIN=1,sum)
  echIntnogeom$maxAgree<-apply(
    X = echIntnogeom[,1:length(sfList)], MARGIN = 1, function(x) max(table(x) ))
  echInt<-cbind(echIntnogeom,echInt$geometry)  %>% st_as_sf()
  echInt
  echIntLong<-pivot_longer(st_drop_geometry(echInt),cols=rangeCol, names_to = "whichWfs", values_to = "agree")
  echIntLong$LCZref<-substr(echIntLong$whichWfs,start = 1, stop=1 )
  print(head(echIntLong[,c(1,2,9:10)]))
  whichLCZagree <- names(echIntLong)[as.numeric(echIntLong$LCZref)]
  indRow<- seq_len(nrow(echIntLong))
  z<-data.frame(indRow, whichLCZagree)
  echIntLong$LCZvalue<-apply(z, 1, function(x) unlist(st_drop_geometry(echIntLong)[x[1], x[2]]))
  print(head(echIntLong[,c(1,2,9:11)]))

  output<-list(echInt=echInt, echIntLong=echIntLong)
}


