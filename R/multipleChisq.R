#' Computes Cramer's V for all pairs of onehot encoded levels of input LCZ classifications
#' @param sfInt an sf object with several LCZ classifications on the same (intersected) geometries, 
#' typically an output of the createIntersect function
#' @param columns a vector which contains names of LCZ classification columns
#' @param nbOutAssociations the number of significant associations we want to extract, ie pair of levels 
#' from two different workflows whose Cramer's V is high (association between LCZ 101 from workflow 1
#' and LCZ 101 from workflow 2 are exclude, for instance, but can be read from the cramerLong output)
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom caret dummyVars
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return Cramer's V between pairs of levels, in a matrix (cramerMatrix) or long form (cramerLong), 
#' and a dataframe with the nbOutAssociation most significant association
#' @export
#' @examples
multipleChisq<-function(sfInt, columns, nbOutAssociations, areaColumn = "area" ){ 
   if("sf"%in%class(sfInt)){sfInt<-as.data.frame(st_drop_geometry(sfInt))}
  weights<-sfInt[[areaColumn]]
  sfInt<-sfInt[ , columns]
  formula_dummy<-reformulate(columns)
  sfDummy<-caret::dummyVars(formula = formula_dummy, data = sfInt) %>% predict(newdata = sfInt) %>% as.data.frame
  # notAlwaysZero<-apply(X = sfDummy, MARGIN = 2, sum)!=0
  # sfDummy<-sfDummy[,notAlwaysZero]
  # print(summary(sfDummy))
  
  
  dimWChisqs<-ncol(sfDummy)
  WChisqs<-matrix(ncol = dimWChisqs, nrow = dimWChisqs)
  for (i in 1:length(names(sfDummy))){
    for (j in i:length(names(sfDummy))){
      WChisqs[i,j]<-weights::wtd.chi.sq(var1 = sfDummy[i], var2 = sfDummy[j], weight = weights, na.rm=TRUE,
                                   drop.missing.levels=TRUE, mean1=TRUE)[3]
      # print(table(sfDummy[,c(i,j)]))
    }
  }

  rownames(WChisqs)<-names(sfDummy)
  colnames(WChisqs)<-names(sfDummy)

  WChisqsLong<-data.frame(
    LCZ_type_1 = rownames(WChisqs),
    LCZ_type_2 = rep(colnames(WChisqs), each = nrow(WChisqs)),
    WChisqs = as.vector(WChisqs)) %>% arrange(desc(WChisqs))%>% na.omit()
  
  # Remove association between modalities of a same LCZ classification
  WChisqsLong$wf1<-gsub(x = WChisqsLong$LCZ_type_1,
         pattern = "(\\w[0-9]*)\\.([0-9]+|Unclassified)", replacement = "\\1")
  WChisqsLong$wf2<-gsub(x = WChisqsLong$LCZ_type_2,
         pattern="(\\w[0-9]*)\\.([0-9]+|Unclassified)", replacement = "\\1")
  WChisqsLong$LCZ1<-gsub(x = WChisqsLong$LCZ_type_1,
                   pattern = "([A-Za-z]*\\d*)(\\.)(\\d+|Unclassified)", replacement = "\\3")
  WChisqsLong$LCZ2<-gsub(x = WChisqsLong$LCZ_type_2,
                   pattern = "([A-Za-z]*\\d*)(\\.)([0-9]+|Unclassified)", replacement = "\\3")
  WChisqsLong<-WChisqsLong[,c("wf1", "LCZ1", "wf2", "LCZ2", "WChisqs", "LCZ_type_1","LCZ_type_2")]
  WChisqsLong<-WChisqsLong[WChisqsLong$wf1!=WChisqsLong$wf2,]



  WChisqsLong<-WChisqsLong[WChisqsLong$LCZ_type_1!=WChisqsLong$LCZ_type_2,]
  threshold <- 
    WChisqsLong$WChisqs[WChisqsLong$LCZ1!=WChisqsLong$LCZ2] %>% as.vector %>% 
      unique %>% sort(decreasing = FALSE) %>% head(n = (nbOutAssociations+1)) %>% min
  signifAssoc<-WChisqsLong[WChisqsLong$WChisqs <= threshold & WChisqsLong$LCZ1!=WChisqsLong$LCZ2,]
  print(paste0("threshold = ", threshold))
  
  output<-list(WChisqsMatrix = WChisqs, WChisqsLong = WChisqsLong, threshold = threshold, signifAssoc = signifAssoc)
  
  return(output)
}