#' For a given LCZ sf object, plots the number of geometries and an average area indicator per level of LCZ
#' @param summarisedSfIn the output of summariseRSU function
#' @param plotNow : if TRUE the plot will be displayed in the session
#' @param workflowNames contain the names of the workflows
#' @param graphPath : a valid directory path where th plot will be saved 
#' (for now an empty string to avoid saving in the working directory)
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @import sf units utils grDevices  patchwork
#' @importFrom magrittr "%>%"
#' @return the number of geometries (Reference Spatial units or RSUs) 
#' and their mean area per level of LCZ, and the same after agregatting geometries 
#' with same level of LCZ which touch each other
#' @export
#' @examples
#' dirPath<-paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs")
#' allLocAllWfs<-loadConcatAllLocsAllWfs(
#'  dirPath = dirPath,
#'     locations = c("Redon", "Arville"),
#'     workflowNames = c("osm","bdt","wudapt"),
#'  missingGeomsWf= "osm",
#'  refWf = NULL,
#'  refLCZ = "Unclassified",
#'  residualLCZvalue = "Unclassified",
#'  column = "lcz_primary"
#'  )
#' summarisedRSUs<-summariseRSUs(allLocAllWfs, aggregatingColumns = c("wf", "lcz_primary"))
#' plotSummarisedRSUs(summarisedSfIn = summarisedRSUs, workflowNames = c("wudapt"= "wud", "osm", "bdt"))
plotSummarisedRSUs <- function(summarisedSfIn, workflowNames = c("wudapt", "osm", "bdt"),
                               plotNow = TRUE, graphPath = "") {

  colorMap <- .lczenv$colorMapDefault
  etiquettes <- .lczenv$etiquettesDefault
  graphPath <- checkDirSlash(graphPath)
  initalAlphas <- rep(0.1, length(workflowNames))
  names(initalAlphas) <- workflowNames
  allPlotNames <- NULL
  wfShapeNumber <- if (length(workflowNames) < 5) { c(5, 1, 2, 0) } else { c(c(5, 1, 2, 0), 6:(6 + length(workflowNames) - 4)) }
  # if (prod(workflowNames == c("wudapt"= "wud", "osm", "bdt"))==0){
  #   wfNamedVector <- c(bdt = "GC/BDT", osm = "GC/OSM", wudapt = "WUDAPT")
  # } else {wfNamedVector<-workflowNames}

  wfNamedVector <- workflowNames
  wfNamedVector[nchar(names(workflowNames)) > 1] <- names(workflowNames)[nchar(names(workflowNames)) > 1]
  print(wfNamedVector)
  for (wf in workflowNames) {
    wfAlphas <- initalAlphas
    wfAlphas[wf] <- 1
    plotName <- paste0("plot_", wf)
    allPlotNames <- c(allPlotNames, plotName)
    assign(plotName, {
      ggplot(data = summarisedSfIn) +
        geom_point(aes(x = .data$number, y = .data$meanLogArea,
                       shape = .data$wf, color = .data$lcz_primary,
                       fill = .data$lcz_primary, alpha = .data$wf, size = .data$totalArea), stroke = 1.5) +
        scale_alpha_manual(values = wfAlphas) +
        scale_fill_manual(values = colorMap, breaks = names(colorMap), labels = etiquettes, na.value = "ghostwhite") +
        scale_color_manual(name = "LCZ type", values = colorMap, breaks = names(colorMap),
                           labels = etiquettes, na.value = "ghostwhite") +
        scale_shape_manual(values = wfShapeNumber, name = "Workflows", labels = wfNamedVector, breaks = workflowNames) +
        guides(fill = "none", colour = guide_legend(order = 1),
               shape = guide_legend(order = 2),
               size = guide_legend(order = 3)) +
        scale_size_continuous(name = "Total area for \n a workflow and a LCZ type") +
        labs(x = "Number of Aggregated Spatial units", y = "Mean of log areas of ASU") +
        guides(alpha = "none") +
        labs(subtitle = wfNamedVector[workflowNames == wf]) +
        theme(legend.position = "right",
              axis.text = element_text(size = rel(1)),
              axis.title = element_text(size = rel(1.2), face = "bold"),
              legend.text = element_text(size = rel(1), face = "bold"),
              legend.title = element_text(size = rel(1.2), face = "bold"),
              plot.subtitle = element_text(size = rel(1.2), face = "bold"))
    })
  }
  allPlots <- do.call(list, mget(allPlotNames))
  # outPlot <- allPlots[[1]] + allPlots[[2]] + allPlots[[3]] + allPlots[[4]] +
  outPlot <- wrap_plots(allPlots, ncol = 2,) +
    plot_layout(ncol = 2, guides = "collect") +
    plot_annotation(title = "Overview of aggregating behavior",
                    subtitle = "Average log area x Average number of spatial units per LCZ types and workflow",
                    caption = "Along the y axis : coarser map, along the x axis, patchworky map ",
                    theme = theme(plot.title = element_text(size = rel(1.7), face = "bold"),
                                  plot.subtitle = element_text(size = rel(1.3), face = "bold")))
  if (plotNow) {
    print(outPlot)
  }
  if (nchar(graphPath) > 1) {
    ggsave(graphPath, outPlot)
  }
  return(outPlot)
}

# plotSummarisedRSUs(aggregatedSF = allLocAllWfs)