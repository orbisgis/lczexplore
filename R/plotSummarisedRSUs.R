#' For a given LCZ sf object, plots the number of geometries and an average area indicator per level of LCZ
#' @param summarisedSfIn the output of summariseRSU function
#' @param plotNow : if TRUE the plot will be displayed in the session
#' @param workflowNames contain the names of the workflows
#' @param graphPath : a valid directory path where th plot will be saved 
#' (for now an empty string to avoid saving in the working directory)
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang patchwork
#' @return the number of geometries (Reference Spatial units or RSUs) 
#' and their mean area per level of LCZ, and the same after agregatting geometries 
#' with same level of LCZ which touch each other
#' @export
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"))[-1]
#' allLocAllWfs<-concatAllLocationsAllWfs(
#'  dirList = dirList, 
#'     locations = c("Blaru", "Goussainville"), 
#'     workflowNames = c("osm","bdt","iau","wudapt"),
#'  missingGeomsWf = "iau",
#'  refWf = NULL,
#'  refLCZ = "Unclassified",
#'  residualLCZvalue = "Unclassified",
#'  column = "lcz_primary"
#')
#' summarisedRSUs<-summariseRSUs(allLocAllWfs, aggregatingColumns = c("wf", "lcz_primary"))
#' plotSummarisedRSUs(summarisedSfIn = summarisedRSUs)
plotSummarisedRSUs<-function(summarisedSfIn, workflowNames = c("wudapt", "iau", "osm", "bdt"),
                             plotNow = TRUE, graphPath = ""){
  
  colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
              "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
              "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd", "ghostwhite")
  names(colorMap)<-as.character(c(1:10,101:107, "Unclassified"))
  etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                "LCZ A: Dense trees", "LCZ B: Scattered trees",
                "LCZ C: Bush,scrub","LCZ D: Low plants",
                "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water", "Unclassified")
  
  graphPath<-checkDirSlash(graphPath)
  initalAlphas<-rep(0.1,length(workflowNames))
  names(initalAlphas)<-workflowNames
  allPlotNames<-NULL
  wf2<-c(5,1,2,0)
  wfNamedVector<-c('bdt' = "GC/BDT", 'osm' = "GC/OSM", 'wudapt' = "WUDAPT", 'iau' = "IAU")
  
  for (wf in workflowNames) {
    wfAlphas<-initalAlphas
    wfAlphas[wf]<-1
    plotName<-paste0("plot_",wf)
    allPlotNames<-c(allPlotNames,plotName)
   assign( plotName, 
   {ggplot(data = summarisedSfIn) +
    geom_point(
      aes(x=number, y = meanLogArea,
          shape = wf, color = lcz_primary, fill = lcz_primary, alpha = wf,
          size = totalArea), stroke = 1.5) +
    scale_alpha_manual(values = wfAlphas)+
    scale_fill_manual(
      values= colorMap, breaks = names(colorMap),
      labels = etiquettes, na.value = "ghostwhite") +
    scale_color_manual(
      values = colorMap, breaks = names(colorMap),
      labels = etiquettes, na.value = "ghostwhite") +
    scale_shape_manual(values = wf2, name = "workflow",
                       labels = wfNamedVector,
                       breaks = c("wudapt","iau", "osm", "bdt")) +
    labs( x = "number of Spatial units", y = "Avereage mean of log areas of ASU") +
     guides(alpha = "none") + 
     labs(subtitle = wfNamedVector[wf]) +
     theme(legend.position = "right") }
   )
 }
  
 allPlots<-do.call(list, mget(allPlotNames))
 outPlot <- allPlots[[1]] + allPlots[[2]] + allPlots[[3]] + allPlots[[4]] + plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(
      title = "Overview of aggregating behavior", 
      subtitle = "Average log area x Average number of spatial units per LCZ types and workflow",
    caption = "Along the y axis : coarser map, along the x axis, patchworky map ")
  if (plotNow) {print(outPlot)}
  if (nchar(graphPath)>1){ggsave(graphPath, outPlot)}
  return(outPlot)
 
}


# plotSummarisedRSUs(aggregatedSF = allLocAllWfs)