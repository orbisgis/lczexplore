#' Computes Cramer's V for all pairs of one hot encoded levels of input LCZ classifications
#' @param sfInt an sf object with several LCZ classifications to compare on the same intersected geometries, 
#' typically an output of the createIntersec function
#' @param columns a vector which contains names of LCZ classification columns
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom caret dummyVars
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return an sf file with values of LCZ from all the input 
#' are assigned to geometries resulting from intersection of all input geometries
#' @export
#' @examples
multipleCramer<-function(sfInt, columns, nbOutAssociations ){ 
   if("sf"%in%class(sfInt)){sfInt<-as.data.frame(st_drop_geometry(sfInt))}
  sfInt<-sfInt[,columns]
  formula_dummy<-reformulate(columns)
  sfDummy<-caret::dummyVars(formula = formula_dummy, data = sfInt) %>% predict(newdata = sfInt) %>% as.data.frame
  notAlwaysZero<-apply(X = sfDummy, MARGIN = 2, sum)!=0
  sfDummy<-sfDummy[,notAlwaysZero]
  # print(summary(sfDummy))
  
  
  dimVs<-ncol(sfDummy)
  Vs<-matrix(ncol = dimVs, nrow = dimVs)
  for (i in 1:length(names(sfDummy))){
    for (j in i:length(names(sfDummy))){
      Vs[i,j]<-confintr::cramersv(chisq.test(table(sfDummy[,c(i,j)]), correct = FALSE))
      # print(table(sfDummy[,c(i,j)]))
    }
  }
  rownames(Vs)<-names(sfDummy)
  colnames(Vs)<-names(sfDummy)

  threshold <- Vs %>% as.vector %>% unique %>% sort(decreasing=TRUE) %>% head(n = (nbOutAssociations+1)) %>% min
  print(paste0("threshold = ", threshold))
  
  Mask<-Vs
  Mask<-(Mask>=threshold)
  Mask[Vs<threshold]<-NA
  Mask[Vs==1]<- NA

  keptRows<-apply(
    X = apply(X = Mask, MARGIN = 1, is.na),
    MARGIN = 1, sum)<ncol(Vs)
  keptCols<-apply(
    X = apply(X = Mask, MARGIN = 2, is.na ),
    MARGIN = 2, sum)<nrow(Vs)
  print("keptRows = ") ; print(keptRows)
  print("keptCols = ") ; print(keptCols)
  signifAssoc<-Vs[keptRows, keptCols]
  signifAssoc[(signifAssoc>=1 | signifAssoc<threshold)]<-NA
  
  output<-list(cramerMatrix = Vs, signifAssoc = signifAssoc)
  
  return(output)
}