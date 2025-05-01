
#' Prepares the data to produce a sankey graph between two LCZ workflows 
#' @param intersectedDf is an sf object or a data frame which contains lcz values for at least two workflows on the same geometries, 
#' and the area of the geometries
#' @param wf1 is the column name where the LCZ value of the first workflow are stored
#' @param wf2 wf1 is the column name where the LCZ value of the first workflow are stored
#' @return an object to feed plotSankeyLCZ
#' @import sf ggplot2 dplyr ggsankeyfier
#' @export
#'
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"))[-1]
#' allLocIntersected<-concatIntersectedLocations(
#' dirList = dirList, locations = c("Blaru", "Goussainville"))
#' testSankey<-prepareSankeyLCZ(intersectedDf = allLocIntersected
#'  , wf1 = "wudapt", wf2 = "osm")
prepareSankeyLCZ<-function(intersectedDf, wf1, wf2){
  intersectedDf<-intersectedDf[,c(wf1, wf2, "area")]
  internRecode<-function(LCZvect){
    case_when(
      nchar(as.character(LCZvect))==1 ~ paste0("00",LCZvect),
      nchar(as.character(LCZvect))==2 ~ paste0("0",LCZvect),
      .default = as.character(LCZvect))
  }
  intersectedDf[[wf1]]<-internRecode(intersectedDf[[wf1]])
  intersectedDf[[wf2]]<-internRecode(intersectedDf[[wf2]])
  sankeyfied<-ggsankeyfier::pivot_stages_longer(
    data = st_drop_geometry(intersectedDf),
    stages_from = c(wf1, wf2),
    values_from = "area"
  )
  sankeyfied$node<-ordered(sankeyfied$node,
    levels = c("001", "002","003", "004", "005", "006", "007", "008", "009", "010",
               "101", "102", "103", "104", "105", "106", "107", "Unclassified")
  )
  return(sankeyfied)
}