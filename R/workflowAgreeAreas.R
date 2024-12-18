
#' From the output of compare multiple, cmputes which workflows agree the most regarding the area of agreement
#' @param sfMultiCompLong the output of multiple comparison function
#' @param LCZcolumns a vector which contains, the name of the columns of the classification to compare
#' @param Wfs a vector of strings which contains the names of the workflows used to produce the sf objects
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return the pairwise agreement between workflows, sorted by decreasing agreeing areas
#' @export
#' @examples
#' 
workflowAgreeAreas<-function(sfMultiCompLong){
  agreeAreas<- sfMultiCompLong%>% subset(agree) %>% group_by(whichWfs) %>% summarise(area=sum(area)) %>% arrange(desc(area))
  disagreeAreas<-sfMultiCompLong%>% subset(!agree) %>% group_by(whichWfs) %>% summarise(area=sum(area)) %>% arrange(desc(area))
  output<-list(agreeAreas=agreeAreas, disagreeAreas = disagreeAreas)
  return(output)
}