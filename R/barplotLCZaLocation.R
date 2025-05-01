#' in a given directory, if several LCZ files are present, plots the repartition 
#' of LCZ regarding of their source (workflow)
#' NOTE: to represent the map of LCZ for a given fil, use `showLCZ` function instead
#' @param dirPath is the path where the datasets are stored
#' @param location is the name of the locations for the plot is produced
#' @param refWf is a reference workflow name, passed to the function addMissingRSUs when needed
#' @param refLCZ is a reference LCZtype, passed to the function addMissingRSUs when needed
#' @param residualLCZvalue a LCZ default type, passed to the function addMissingRSUs when needed
#' @param workflowNames is a vector of prefixes. The LCZ files must be named workflow_rsu.fgb
#' where workflow is on of the values in workflowNames vector 
#' @param plotNow If TRUE, the boxplot of the repartition will be printed
#' @param plotSave If TRUE, the plot will be saved in the directory pointed by dirPath 
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom caret dummyVars
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer units utils grDevices rlang
#' @return Cramer's V between pairs of levels, in a matrix (cramerMatrix) or long form (cramerLong), 
#' and a dataframe with the nbOutAssociation most significant association
#' @export
#' @examples
#' barplotLCZaLocation(
#' dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Goussainville"),
#' refWf = NULL, refLCZ = NA, residualLCZvalue = "Unclassified",
#' location = "Goussainville", plotNow = TRUE)
barplotLCZaLocation<-function(dirPath, location, workflowNames = c("osm", "bdt", "iau", "wudapt"),
                              refWf = NULL, refLCZ = NA, residualLCZvalue=NA,
                              plotNow = FALSE, plotSave = TRUE){
  colorMap<-rev(c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
                  "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
                  "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd", "ghostwhite"))
  names(colorMap)<-rev(c(1:10,101:107, "Unclassified"))
  etiquettes<-rev(c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                    "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                    "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                    "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                    "LCZ A: Dense trees", "LCZ B: Scattered trees",
                    "LCZ C: Bush,scrub","LCZ D: Low plants",
                    "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand",
                    "LCZ G: Water", "Unclassified"))

  sfList<-loadMultipleSfs(dirPath = dirPath,
                         workflowNames = workflowNames , location = location )
  if(substr(dirPath, nchar(dirPath), nchar(dirPath))!="/"){dirPath<-paste0(dirPath, "/")}
  zoneSfPath<-paste0(dirPath,"zone.fgb")
  zoneSf<-read_sf(zoneSfPath)
  sfList<-addMissingRSUs(sfList, missingGeomsWf="iau", zoneSf = zoneSf, refWf = refWf, 
                                    refLCZ = refLCZ,
                         residualLCZvalue = residualLCZvalue, column = "lcz_primary")
  concatSf<-concatAlocationWorkflows(sfList = sfList,
                                     location = location, refCrs = 1)
  surfaces<-concatSf %>%
    mutate(wf = factor(wf, levels = c("bdt", "osm", "wudapt", "iau"))) %>%
    mutate(lcz_primary = factor(lcz_primary, levels = names(colorMap))) %>%
    mutate(lcz_primary = tidyr::replace_na(lcz_primary, "Unclassified")) %>%
    dplyr::group_by(wf, lcz_primary) %>% dplyr::summarise(area=drop_units(sum(area)), location=unique(location))

  location<-unique(surfaces$location)
  outPlot<-ggplot(surfaces) +
    geom_col(aes(fill=lcz_primary, y=area, x=wf, color = after_scale(fill))) +
    # scale_fill_viridis(discrete = T) +
    scale_fill_manual(values=colorMap, breaks = names(colorMap), labels = etiquettes, na.value = "ghostwhite") +
    ggtitle(paste0("LCZ repartition by workflow for ", location))

  
  if(is.logical(plotSave) && plotSave){
    plotName<-paste0(dirPath, "LCZbyWfBarplot.png")
    ggsave(plotName, outPlot)}
  
  if (is.character(plotSave)){
    if(substring( plotSave, first = nchar(plotSave), last = nchar(plotSave)) !="/"){ plotSave<-paste0(plotSave, "/") }
    plotName<-paste0(plotSave, location,"_LCZbyWfBarplot.png")
    ggsave(plotName, outPlot)
  }
  if (plotNow){print(outPlot)}

  return(outPlot)
}