#' From the output of compare multiple, cmputes which workflows agree the most regarding the area of agreement
#' @param sfMultiCompLong the sfLong output of compareMultipleLCZ function
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom dplyr group_by summarise arrange mutate desc
#' @importFrom magrittr "%>%"
#' @import sf units
#' @return the pairwise agreement between workflows, sorted by decreasing agreeing areas
#' @export
#' @examples
#' sfList<-loadMultipleSfs(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),
#' "/multipleWfs/Arville"),
#' workflowNames = c("osm","bdt","wudapt"), location = "Arville")
#' ArvilleIntersect <- createIntersect(
#'  sfList = sfList, columns = rep("lcz_primary", 4),  
#'  workflowNames = c("osm","bdt","wudapt"))
#' ArvilleMultipleComparison<-compareMultipleLCZ(
#'  sfInt = ArvilleIntersect,
#'  columns = c("osm","bdt","wudapt"),
#'  trimPerc = 0.5)
#' ArvilleWorkflowAgreement<-workflowAgreeAreas(ArvilleMultipleComparison$sfIntLong)
workflowAgreeAreas <- function(sfMultiCompLong) {
  agreeAreas <- sfMultiCompLong %>%
    subset(agree) %>%
    dplyr::group_by(.data$whichWfs) %>%
    dplyr::summarise(area = sum(area))
  disagreeAreas <- sfMultiCompLong %>%
    subset(!agree) %>%
    dplyr::group_by(.data$whichWfs) %>%
    dplyr::summarise(area = sum(area))
  output <- merge(agreeAreas, disagreeAreas, by = "whichWfs",
                  suffixes = c("Agree", "Disagree")) %>%
    dplyr::arrange(dplyr::desc(.data$areaAgree)) %>%
    dplyr::mutate(percAgree = .data$areaAgree / (.data$areaAgree + .data$areaDisagree) * 100)
  return(output)
}