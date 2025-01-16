plotSummarisedRSUs<-function(longSf, workflows = c("osm", "bdt", "iau", "wudapt"), plot = TRUE, locations = NULL, graphPath = ""){
  print(locations)
  colorMap<-c("#8b0101","#cc0200","#fc0001","#be4c03","#ff6602","#ff9856",
              "#fbed08","#bcbcba","#ffcca7","#57555a","#006700","#05aa05",
              "#648423","#bbdb7a","#010101","#fdf6ae","#6d67fd", NA)
  names(colorMap)<-as.character(c(1:10,101:107, NA))
  etiquettes<-c("LCZ 1: Compact high-rise","LCZ 2: Compact mid-rise","LCZ 3: Compact low-rise",
                "LCZ 4: Open high-rise","LCZ 5: Open mid-rise","LCZ 6: Open low-rise",
                "LCZ 7: Lightweight low-rise","LCZ 8: Large low-rise",
                "LCZ 9: Sparsely built","LCZ 10: Heavy industry",
                "LCZ A: Dense trees", "LCZ B: Scattered trees",
                "LCZ C: Bush,scrub","LCZ D: Low plants",
                "LCZ E: Bare rock or paved","LCZ F: Bare soil or sand","LCZ G: Water", "ghostwhite")
  
  
  summarisedRSUs<-matrix(ncol=6, nrow=0) %>% as.data.frame
  names(summarisedRSUs)<-c("lcz",   "numberRSUs", "meanArea", "numberRSUsClust", "meanAreaClust", "wf")
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

  summarisedRSUs$meanArea<-round(summarisedRSUs$meanArea)
  summarisedRSUs$meanAreaClust<-round(summarisedRSUs$meanAreaClust)
  summarisedRSUs<-summarisedRSUs %>% arrange(wf,lcz)


  # 
  outPlot<-ggplot() +
    # geom_point(data = summarisedRSUs, aes(x=numberRSUs, y = meanArea, color = lcz), size = 2) +
    geom_point(data = summarisedRSUs, aes(x=numberRSUsClust, y = meanAreaClust, color = lcz), size = 2) +
    scale_color_manual(values=colorMap, breaks = names(colorMap), labels = etiquettes, na.value = "ghostwhite")+
    facet_wrap(vars(wf))
  if(plot){print(outPlot)}
  
  if(!is.null(graphPath) && length(graphPath)==1 && nchar(graphPath)>1){
    if(substr(graphPath, nchar(graphPath), nchar(graphPath))!="/"){graphPath<-paste0(graphPath, "/")}
    graphName<-paste0(graphPath, paste0(locations, collapse = "_"), ".png")
    ggsave(graphName, outPlot)
  }
  
  output<-list(outPlot=outPlot, summarisedRSUs=summarisedRSUs)
  return(output)
  
}

# plotSummarisedRSUs(longSf = allLocAllWfs)