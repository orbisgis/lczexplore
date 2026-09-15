#' in a given directory, if several LCZ files are present, plots the repartition 
#' of LCZ regarding of their source (workflow)
#' NOTE: to represent the map of LCZ for a given file, use `showLCZ` function instead
#' @param dirPath is the path where the datasets are stored
#' @param inLocation is the name of the locations for the plot is produced
#' @param refWf is a reference workflow name, passed to the function addMissingRSUs when needed
#' @param missingGeom allows to indicate which workflow is supposed to have missing geometries
#' (not used most of the time)
#' @param refLCZ is a reference LCZtype, passed to the function addMissingRSUs when needed
#' @param residualLCZvalue a LCZ default type, passed to the function addMissingRSUs when needed
#' @param workflowNames is a vector of prefixes. The LCZ files must be named workflow_rsu.fgb
#' where workflow is on of the values in workflowNames vector 
#' @param plotNow If TRUE, the boxplot of the repartition will be printed
#' @param plotSave If TRUE, the plot will be saved in the directory pointed by dirPath 
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom caret dummyVars
#' @importFrom dplyr mutate group_by summarise
#' @importFrom tidyr  replace_na
#' @import sf forcats units RColorBrewer units utils grDevices
#' @return A barplot of LCZ area percentage by LCZ type and workflow
#' @export
#' @examples
#' barplotLCZaLocation(
#' dirPath = paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
#' refWf = NULL, refLCZ = NA, residualLCZvalue = "Unclassified",
#' inLocation = "Arville", plotSave = "/tmp", plotNow = TRUE)
barplotLCZaLocation <- function(dirPath, inLocation, workflowNames = c("osm", "bdt", "wudapt"),
                                refWf = NULL, refLCZ = NA, residualLCZvalue = NA, missingGeom = "osm",
                                plotNow = FALSE, plotSave = "\tmp") {
  colorMap <- rev(c("#8b0101", "#cc0200", "#fc0001", "#be4c03", "#ff6602", "#ff9856",
                    "#fbed08", "#bcbcba", "#ffcca7", "#57555a", "#006700", "#05aa05",
                    "#648423", "#bbdb7a", "#010101", "#fdf6ae", "#6d67fd", "ghostwhite"))
  names(colorMap) <- rev(c(1:10, 101:107, "Unclassified"))
  etiquettes <- rev(c("LCZ 1: Compact high-rise", "LCZ 2: Compact mid-rise", "LCZ 3: Compact low-rise",
                      "LCZ 4: Open high-rise", "LCZ 5: Open mid-rise", "LCZ 6: Open low-rise",
                      "LCZ 7: Lightweight low-rise", "LCZ 8: Large low-rise",
                      "LCZ 9: Sparsely built", "LCZ 10: Heavy industry",
                      "LCZ A: Dense trees", "LCZ B: Scattered trees",
                      "LCZ C: Bush,scrub", "LCZ D: Low plants",
                      "LCZ E: Bare rock or paved", "LCZ F: Bare soil or sand",
                      "LCZ G: Water", "Unclassified"))

  sfList <- loadMultipleSfs(dirPath = dirPath,
                            workflowNames = workflowNames, inLocation = inLocation)
  if (substr(dirPath, nchar(dirPath), nchar(dirPath)) != "/") { dirPath <- paste0(dirPath, "/") }
  zoneSfPath <- paste0(dirPath, "zone.fgb")
  zoneSf <- read_sf(zoneSfPath)
  sfList <- addMissingRSUs(sfList, missingGeomsWf = missingGeom, zoneSf = zoneSf, refWf = refWf,
                           refLCZ = refLCZ,
                           residualLCZvalue = residualLCZvalue, column = "lcz_primary")
  concatSf <- concatAlocationWorkflows(sfList = sfList,
                                       location = inLocation, refCrs = 1)

  if (!("area" %in% names(concatSf))) {
    concatSf$area <- st_area(concatSf)
  }

  surfaces <- concatSf %>%
    dplyr::mutate(wf = factor(wf, levels = c("bdt", "osm", "wudapt" = "wud"))) %>%
    dplyr::mutate(lcz_primary = factor(lcz_primary, levels = names(colorMap))) %>%
    dplyr::mutate(lcz_primary = tidyr::replace_na(lcz_primary, "Unclassified")) %>%
    dplyr::group_by(wf, lcz_primary) %>%
    dplyr::summarise(area = drop_units(sum(area)), location = unique(inLocation))

  inLocation <- unique(surfaces$location)

  #utils::globalVariables(c("fill")) # Trick to avoid R CMD check to raise a note a bout no binding for glob var fill

  outPlot <- ggplot(surfaces) +
    geom_col(aes(fill = .data$lcz_primary, y = .data$area, x = .data$wf, color = after_scale(fill))) +
    # scale_fill_viridis(discrete = T) +
    scale_fill_manual(
      values = colorMap,
      breaks = names(colorMap),
      labels = etiquettes, na.value = "ghostwhite") +
    ggtitle(paste0("LCZ repartition by workflow for ", inLocation))


  if (is.logical(plotSave) && plotSave) {
    plotName <- paste0(dirPath, "LCZbyWfBarplot.png")
    ggsave(plotName, outPlot) }

  if (is.character(plotSave)) {
    if (substring(plotSave, first = nchar(plotSave), last = nchar(plotSave)) != "/") {
      plotSave <- paste0(plotSave, "/") }
    plotName <- paste0(plotSave, inLocation, "_LCZbyWfBarplot.png")
    ggsave(plotName, outPlot)
  }
  if (plotNow) { print(outPlot) }

  return(outPlot)
}