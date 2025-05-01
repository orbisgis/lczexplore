
#' From the output of compare multiple, cmputes which workflows agree the most regarding the area of agreement
#' @param sfMultiCompLong the sfLong output of compareMultipleLCZ function
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return the pairwise agreement between workflows, sorted by decreasing agreeing areas
#' @export
#' @examples
#' sfList<-loadMultipleSfs(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),
#' "/multipleWfs/Goussainville"),
#' workflowNames = c("osm","bdt","iau","wudapt"), location = "Goussainville")
#' GoussainvilleIntersect <- createIntersect(
#'  sfList = sfList, columns = rep("lcz_primary", 4),  
#'  sfWf = c("osm","bdt","iau","wudapt"))
#' GoussainvilleMultipleComparison<-compareMultipleLCZ(
#'  sfInt = GoussainvilleIntersect,
#'  LCZcolumns = c("osm","bdt","iau","wudapt"),
#'  trimPerc = 0.5)
#' GoussainvilleWorkflowAgreement<-workflowAgreeAreas(GoussainvilleMultipleComparison$sfIntLong)
workflowAgreeAreas<-function(sfMultiCompLong){
    agreeAreas<- sfMultiCompLong%>% subset(agree) %>% group_by(whichWfs) %>% 
    dplyr::summarise(area=sum(area))
  disagreeAreas<-sfMultiCompLong%>% subset(!agree) %>% group_by(whichWfs) %>% 
    dplyr::summarise(area=sum(area))
  output<-merge(agreeAreas, disagreeAreas, by = "whichWfs",
suffixes = c("Agree", "Disagree")) %>% arrange(desc(areaAgree)) %>%
mutate(percAgree = areaAgree/(areaAgree+areaDisagree)*100)
  return(output)
}