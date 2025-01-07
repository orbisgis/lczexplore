#' in a given directory, if several LCZ files are present, it plots the repartition of LCZ regarding of their source (workflow)
#' NOTE: to represent the map of LCZ for a given fil, use `showLCZ` function instead
#' @param workflownames is a vector of prefixes. The LCZ files must benames workflow_rsu.fgb  
#' @param plotNow. If TRUE, the boxplot of the repartition will be printed
#' @param plotSave. If TRUE, the plot will be saved in the directory pointed by dirPath 
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom caret dummyVars
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return Cramer's V between pairs of levels, in a matrix (cramerMatrix) or long form (cramerLong), 
#' and a dataframe with the nbOutAssociation most significant association
#' @export
#' @examples
boxplotLCZaLocation<-function(dirPath, location, workflowNames = c("osm","bdt","iau","wudapt"),
                           plotNow = FALSE, plotSave = TRUE){
  colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
              "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
              "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd")
  names(colorMap)<-c(1:10,101:107)
  etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                "LCZ A: Dense trees", "LCZ B: Scattered trees",
                "LCZ C: Bush,scrub","LCZ D: Low plants",
                "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water")

  sfList<-loadMultipleSf(dirPath = dirPath,
                         workflowNames = workflowNames , location = location )
  if(substr(dirPath, nchar(dirPath), nchar(dirPath))!="/"){dirPath<-paste0(dirPath, "/")}
  zoneSfPath<-paste0(dirPath,"zone.fgb")
  zoneSf<-read_sf(zoneSfPath)
  sfList<-repairRoadsIAU(sfList = sfList, zoneSf = zoneSf, location = location)
  concatSf<-concatAlocationWorkflows(sfList = sfList,
                                     location = location, refCrs = 1)

  surfaces<-concatSf %>%
    mutate(wf = factor(wf, levels = c("bdt", "osm", "wudapt", "iau"))) %>%
    dplyr::group_by(wf, lcz_primary) %>% dplyr::summarise(area=sum(area), location=unique(location))

  location<-unique(surfaces$location)
  outPlot<-ggplot(surfaces, aes(fill=lcz_primary, y=area, x=wf)) +
    geom_col() +
    # scale_fill_viridis(discrete = T) +
    scale_fill_manual(values=colorMap, breaks = names(colorMap), labels = etiquettes) +
    ggtitle(paste0("LCZ repartition by workflow for ", location))

  plotName<-paste0(dirPath, "LCZbyWfBoxplot.png")
  if(plotSave){ggsave(plotName, outPlot)}
  if (plotNow){print(outPlot)}

  return(outPlot)
}