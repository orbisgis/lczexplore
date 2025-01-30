#' For a given LCZ sf object, plots the number of geometries and their median area per level of LCZ
#' @param longSf contains the geometry and LCZ levels
#' @param workflows the names of workflows (they will indicate the names of the columns containing LCZ levels)
#' @param plotNow if TRUE the plot will be displayed in the session
#' @graphPath : a valid directory path where th plot will be saved 
#' (for now an empty string to avoid saving in the working directory)
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf dplyr cowplot forcats units tidyr RColorBrewer utils grDevices rlang
#' @return the number of geometries (Reference Spatial units or RSUs) 
#' and their mean area per level of LCZ, and the same after agregatting geometries 
#' with same level of LCZ which touch each other
#' @export
#' @examples
plotSummarisedRSUs<-function(longSf, workflows = c("osm", "bdt", "iau", "wudapt"), plotNow = TRUE, locations = NULL, graphPath = ""){
  print(locations)
  # colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
  #             "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
  #             "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd", "ghostwhite")
  # names(colorMap)<-as.character(c(1:10,101:107, "Unclassified"))
  # etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
  #               "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
  #               "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
  #               "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
  #               "LCZ A: Dense trees", "LCZ B: Scattered trees",
  #               "LCZ C: Bush,scrub","LCZ D: Low plants",
  #               "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water", "Unclassified")
  
  
  summarisedRSUs<-matrix(ncol=6, nrow=0) %>% as.data.frame
  names(summarisedRSUs)<-c("lcz",   "numberRSUs", "medianArea", "numberRSUsClust", "medianAreaClust", "wf")
  if(is.null(locations) | length(locations)==0){
    locations<-unique(longSf$location)
  }

  for (wfi in workflows){
    print(wfi)
    sfIn<-longSf[longSf$location%in%locations & longSf$wf==wfi,]
    dfOut<-summariseRSUs(sfIn, column = "lcz_primary" )
    dfOut$wf<-wfi
    summarisedRSUs<-rbind(summarisedRSUs, dfOut)
  }

  summarisedRSUs$lcz<-factor(summarisedRSUs$lcz,
                                   levels = c(as.character(1:10),as.character(101:107)))

  summarisedRSUs$medianArea<-round(summarisedRSUs$medianArea)
  summarisedRSUs$medianAreaClust<-round(summarisedRSUs$medianAreaClust)
  summarisedRSUs<-summarisedRSUs %>% arrange(wf,lcz)


  # 
  outPlot<-ggplot() +
    # geom_point(data = summarisedRSUs, aes(x=numberRSUs, y = medianArea, color = lcz), size = 2) +
    geom_point(data = summarisedRSUs, aes(x=numberRSUsClust, y = medianAreaClust, color = lcz), size = 2) +
    scale_color_manual(values=colorMap, breaks = names(colorMap), labels = etiquettes, na.value = "ghostwhite")+
    facet_wrap(vars(wf))
  if(plotNow){print(outPlot)}
  
  if(!is.null(graphPath) && length(graphPath)==1 && nchar(graphPath)>1){
    if(substr(graphPath, nchar(graphPath), nchar(graphPath))!="/"){graphPath<-paste0(graphPath, "/")}
    graphName<-paste0(graphPath, paste0(locations, collapse = "_"), ".png")
    ggsave(graphName, outPlot)
  }
  
  output<-list(outPlot=outPlot, summarisedRSUs=summarisedRSUs)
  return(output)
  
}

# plotSummarisedRSUs(longSf = allLocAllWfs)