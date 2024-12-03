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

  VsLong<-data.frame(
    LCZ_type_1 = rownames(Vs),
    LCZ_type_2 = rep(colnames(Vs), each = nrow(Vs)),
    cramerVs = as.vector(Vs)) %>% arrange(desc(cramerVs))%>% na.omit()
  
  # Remove association between modalities of a same LCZ classification
  VsLong$wf1<-gsub(x = VsLong$LCZ_type_1,
         pattern = "(\\w[0-9]*)\\.([0-9]+)", replacement = "\\1")
  VsLong$wf2<-gsub(x = VsLong$LCZ_type_2,
         pattern="(\\w[0-9]*)\\.([0-9]+)", replacement = "\\1")
  VsLong$LCZ1<-gsub(x = VsLong$LCZ_type_1,
                   pattern = "([A-Za-z]*\\d*)(\\.)(\\d+)", replacement = "\\3")
  VsLong$LCZ2<-gsub(x = VsLong$LCZ_type_2,
                   pattern = "([A-Za-z]*\\d*)(\\.)([0-9]+)", replacement = "\\3")
  VsLong<-VsLong[,c("wf1", "LCZ1", "wf2", "LCZ2", "cramerVs", "LCZ_type_1","LCZ_type_2")]
  VsLong<-VsLong[VsLong$wf1!=VsLong$wf2,]



  VsLong<-VsLong[VsLong$LCZ_type_1!=VsLong$LCZ_type_2,]
  threshold <- 
    VsLong$cramerVs[VsLong$LCZ1!=VsLong$LCZ2] %>% as.vector %>% 
      unique %>% sort(decreasing=TRUE) %>% head(n = (nbOutAssociations+1)) %>% min
  signifAssoc<-VsLong[VsLong$cramerVs>=threshold & VsLong$LCZ1!=VsLong$LCZ2,]
  print(paste0("threshold = ", threshold))
  
  output<-list(cramerMatrix = Vs, cramerLong = VsLong, threshold = threshold, signifAssoc = signifAssoc)
  
  return(output)
}