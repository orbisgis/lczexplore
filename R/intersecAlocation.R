#' Intersects several sf files for a given location identified by a given directory path and name 
#' @param dirPath is the directory where the original data are
#' @param workflowNames are the names of the workflows : they are used to identify the files
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return an sf file with values of LCZ from all the input 
#' are assigned to geometries resulting from intersection of all input geometries
#' @details This function is not generic, it expects the data files to be named wf_rsu_lcz; wf varying among 
#' the values of workflownames, and the LCZ columns are expected to be lcz_primary (but lower and upper cases are accepted)
#' @export
#' @examples
intersectAlocation<-function(dirPath, workflowNames = c("osm","bdt","iau","wudapt"), location){
  lastPos<-nchar(dirPath)
  if(substr(dirPath, start = lastPos, stop = lastPos)!="/"){dirPath<-paste0(dirPath,"/")}
  
  typeLevels<-c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                "9"="9","10"="10",
                "101"="101","102"="102","103"="103","104"="104", "105"="105","106"="106","107"="107",
                "101"="11","102"="12","103"="13","104"="14", "105"="15", "106"="16","107"="17",
                "101"="A","102"="B","103"="C","104"="D","105"="E","106"="F","107"="G")
  sfList<-list()
  for (i in workflowNames){
    inName<-paste0(dirPath, i, "_lcz.fgb")
    inSf<-read_sf(inName)
    names(inSf)<-tolower(names(inSf))
    inSf<-select(inSf,lcz_primary) %>% mutate(
      lcz_primary=factor(lcz_primary, levels = typeLevels))
    sfList[[i]]<-inSf
    # sfName<-paste0(zoneName,i)
    # assign(sfName,inSf)    
    # print(summary(inSf))
  }
intersecSf<-createIntersect(sfList=sfList, columns=rep("lcz_primary", length(workflowNames)),
                              refCrs=NULL, sfWf=workflowNames, minZeroArea=0.0001)
  if ("character"%in%class(location)) {intersecSf$location<-location}
return(intersecSf)
}

