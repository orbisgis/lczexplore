#' Plots a sankey graph between two LCZ workflows 
#' @param sankeyfied is the data as produced by the prepareSankeyLCZ function
#' @param colorMap is a vector of colors whose names cover the valus of the nodes in sankeyfied
#' @param plotNow is set to TRUE by default, if set to FAULT the graph is not plotted.
#' @param v_space allows a separation between LCZ type bars
#' @return returns a sanky plot of LCZ workflows on the same areas
#' @import sf ggplot2 ggsankeyfier
#' @export
#' @examples
#' dirList<-list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"))[-1]
#' allLocIntersected<-concatIntersectedLocations(
#' dirList = dirList, inLocations = c("Redon", "Arville"))
#' testSankey<-prepareSankeyLCZ(intersectedDf = allLocIntersected
#'  , wf1 = "wudapt", wf2 = "osm")
#' testSankeyPlot<-plotSankeyfiedLCZ(
#' sankeyfied = testSankey, plotNow=TRUE)
plotSankeyfiedLCZ <- function(sankeyfied, plotNow = TRUE, colorMap = NULL,
                              v_space = "auto") {
  if (is.null(colorMap) | prod(names(colorMap) %in% .lczenv$typeLevelsDefault) == 1) {
    colorMap <- .lczenv$colorMapDefault
    # colorMap<-lczexplore:::.lczenv$colorMapDefault

    names(colorMap) <- case_when(
      nchar(names(colorMap)) == 1 ~ paste0("00", names(colorMap)),
      nchar(names(colorMap)) == 2 ~ paste0("0", names(colorMap)),
      .default = names(colorMap)
    )
    colorMap <- colorMap[sort(names(colorMap))]
  } else colorMap <- colorMap



  sharedPosition <- position_sankey(
    v_space = v_space,
    h_space = "auto",
    split_nodes = FALSE,
    align = "top",
    order = "as_is"
  )

  sankeyPlot <- ggplot(
    data = sankeyfied,
    aes(x = stage, y = area, group = node, connector = connector,
        edge_id = edge_id, fill = node)) +
    ggsankeyfier::geom_sankeyedge(
      position = sharedPosition) +
    ggsankeyfier::geom_sankeynode(
      position = sharedPosition) +
    guides(
      # fill   = guide_legend(ncol = 1),
      #      alpha  = guide_legend(ncol = 1),
      colour = guide_legend(title = "LCZ type", ncol = 1)) +
    scale_fill_manual(
      values = colorMap, breaks = names(colorMap)
    ) +
    theme(legend.position = "right") +
    labs(x = paste0(
      "Break up of LCZ areas from workflow ",
      levels(sankeyfied$stage)[1],
      " to workflow ",
      levels(sankeyfied$stage)[2]))

  if (plotNow) { print(sankeyPlot) }

  return(sankeyPlot)
}