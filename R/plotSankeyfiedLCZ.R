#' Plots a sankey graph between two LCZ workflows 
#' @param sankeyfied is the data as produced by the prepareSankeyLCZ function
#' @param colorMap is a vector of colors whose names cover the valus of the nodes in sankeyfied
#' @param plotNow is set to TRUE by default, if set to FAULT the graph is not plotted. 
#' @return returns a sanky plot of LCZ workflows on the same areas
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
#' testSankeyPlot<-plotSankeyfiedLCZ(
#' sankeyfied = testSankey, plotNow=TRUE)
plotSankeyfiedLCZ<-function(sankeyfied, plotNow=TRUE, colorMap = NULL){
  if(is.null(colorMap)) {
    colorMap<-.lczenv$colorMapDefault
  names(colorMap)<-case_when(
    nchar(names(colorMap))==1 ~ paste0("00",names(colorMap)),
    nchar(names(colorMap))==2 ~ paste0("0",names(colorMap)),
    .default = names(colorMap)
  )
  } else colorMap<-colorMap
  
  colorMap<-colorMap[sort(names(colorMap))]

  # pos <- position_sankey(split_nodes = TRUE, align = "top",
  #                        width = 0.2, v_space = 0.15, h_space = 0.25)
  sankeyPlot<-ggplot(sankeyfied,
         aes(x = stage, y = area, group = node, connector = connector,
             edge_id = edge_id, fill = node)) +
    ggsankeyfier::geom_sankeyedge(order = "ascending") +
    ggsankeyfier::geom_sankeynode(order = "ascending") +
    guides(
      # fill   = guide_legend(ncol = 1),
      #      alpha  = guide_legend(ncol = 1),
           colour = guide_legend(title = "LCZ type", ncol = 1)) +
    theme(legend.position = "top") +
    scale_color_manual(
      values = colorMap
    ) +
    theme(legend.position = "right") +
    labs(x = paste0(
      "Break up of LCZ areas from workflow ", 
      levels(sankeyfied$stage)[1], 
      " to workflow ",
      levels(sankeyfied$stage)[2]))
  
  if(plotNow) { print(sankeyPlot) }
  
  return(sankeyPlot)
 }