#' In a given directory (or a list of directories) the function looks for LCZ datafiles
#' and load them in a list
#' @param dirPath is the place where the files are
#' @param workflowNames sets the names of workflows
#' @param location is the name of the location at which all LCZ are created
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
#' sfList<-loadMultipleSfs(dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs/Goussainville"),
#' workflowNames = c("osm","bdt","iau","wudapt"), location = "Goussainville"  )
loadMultipleSfs<-function(dirPath, workflowNames = c("osm","bdt","iau","wudapt"), location ){
  typeLevels<-c("1"="1","2"="2","3"="3","4"="4","5"="5","6"="6","7"="7","8"="8",
                "9"="9","10"="10",
                "101"="101","102"="102","103"="103","104"="104", "105"="105","106"="106","107"="107",
                "101"="11","102"="12","103"="13","104"="14", "105"="15", "106"="16","107"="17",
                "101"="A","102"="B","103"="C","104"="D","105"="E","106"="F","107"="G")
  dirPath<-checkDirSlash(dirPath)
  print(dirPath)
  sfList<-list()
  for (i in workflowNames){
    inName<-paste0(dirPath, i, "_lcz.fgb")
    inSf<-read_sf(inName)
    names(inSf)<-tolower(names(inSf))
    inSf<-select(inSf,lcz_primary) %>% mutate(
      lcz_primary=factor(lcz_primary, levels = typeLevels))
    inSf<-mutate(inSf,location = location, wf = i, .before = geometry)
    # inSf$location<-location
    # inSf$wf<-i
    inSf$lcz_primary<-fct_recode(inSf$lcz_primary, !!!typeLevels)
    sfList[[i]]<-inSf

  }
  return(sfList)
}
