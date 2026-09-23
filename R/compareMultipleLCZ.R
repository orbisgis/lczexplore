#' Compares several sets of geographical classifications, especially Local Climate Zones classifications
#' @param sfInt an sf objects with intersected geometries and the LCZ columns for each workflow LCZ
#' @param columns a vector which contains, the name of the columns of the classification to compare
#' @param workflowNames a vector of strings which contains the names of the workflows used to produce the sf objects
#' @param trimPerc this parameters indicates which percentile to drop out of the smallest geometries resulting
#' It allows to account for numeric precision errors and to speed up computations at the cost of not considering the smallest geometries.
#' @param labelMatch feed matching between levels of the data and some labels
#' from the intersection of the original sf geometries intersection.
#' @param ... areguments to be passed for groupint, in the form of
#' groupLevel = levels to group to this group level, and a possible groupColors named vectors,
#' which names are the grouped levels and values are the desired colors.
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom tidyr pivot_longer
#' @import sf forcats units RColorBrewer utils grDevices
#' @return returns graphics of comparison and an object called matConfOut which contains :
#' matConfLong, a confusion matrix in a long form (each line has the percentage of LCZ type i from workflow j
#' which is classified in LCZ type i' by workflow j'),
#' matConfPlot is a ggplot2 object showing the confusion matrix.
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' If saveG is not an empty string, graphics are saved under "saveG.png"
#' agreements a dtaframe with the pairs of workflows, areas on which they agree, disagree, and the percentage of agreement
#' @export
#' @examples
#' sfList<-importMultipleLCZvect(dirPath =
#' paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs/Arville"),
#' workflowNames = c("osm","bdt","wudapt"), location = "Arville")
#' ArvilleIntersect <- createIntersect(
#'  sfList = sfList, columns = rep("lcz_primary", 4),  
#'  workflowNames = c("osm","bdt","wudapt"))
#' ArvilleMultipleComparison<-compareMultipleLCZ(
#'  sfInt = ArvilleIntersect,
#'  columns = c("osm","bdt","wudapt"),
#'  trimPerc = 0.5)
compareMultipleLCZ <- function(sfInt, columns, workflowNames = NULL, trimPerc = 0.0, labelMatch = NULL, ...) {
  if (is.null(columns)) {
    columns <- names(sfInt)[!names(sfInt) %in% c("area", "geometry")]
  }
  sfInt <- sfInt[sfInt$area > quantile(sfInt$area, probs = trimPerc) & !is.na(sfInt$area),]

  # if input intersected file comes from a concatenation, it will have a location column that is not needed
  if ("location" %in% names(sfInt)) { sfInt <- sfInt[, !names(sfInt) == "location"] }

  sfIntNoGeom <- st_drop_geometry(sfInt)

  if (is.null(workflowNames) | length(workflowNames) != length(columns)) { workflowNames <- columns }

  allLevels <- sfIntNoGeom[, columns] %>%
    lapply(levels) %>%
    unlist %>%
    unique()
  sfIntNoGeom[, columns] <- sfIntNoGeom[, columns] %>% lapply(function(x) factor(x, levels = allLevels))

  # Compute and sums pairwise agreeing surfaces

  for (i in 1:(length(columns) - 1)) {
    for (j in (i + 1):length(columns)) {
      compName <- paste0(workflowNames[i], "_", workflowNames[j])
      print(compName)
      sfIntNoGeom[, compName] <- sfIntNoGeom[, columns[i]] == sfIntNoGeom[, columns[j]]
    }
  }
  rangeCol <- (length(columns) + 2):ncol(sfIntNoGeom)
  print(rangeCol)
  # print(names(sfIntnogeom[,rangeCol]))
  sfIntNoGeom$nbAgree <- apply(
    X = sfIntNoGeom[, rangeCol], MARGIN = 1, sum)
  sfIntNoGeom$maxAgree <- apply(
    X = sfIntNoGeom[, seq_along(columns)], MARGIN = 1, function(x) max(table(x), na.rm = TRUE))
  print(head(sfIntNoGeom))

  # long format
  sfIntLong <- tidyr::pivot_longer(sfIntNoGeom, cols = names(sfIntNoGeom)[rangeCol], names_to = "whichWfs", values_to = "agree")

  # Get the reference LCZ column on which 2 wf agree

  whichLCZagree <- gsub(x = sfIntLong$whichWfs, pattern = "(.*)(_)(.*)", replacement = "\\1")
  indRow <- seq_len(nrow(sfIntLong))
  z <- data.frame(indRow, whichLCZagree)

  sfIntLong$LCZvalue <- apply(z, 1, function(x) unlist(st_drop_geometry(sfIntLong)[x[1], x[2]]))
  sfInt <- cbind(sfIntNoGeom, sfInt$geometry) %>% st_as_sf()

  agreements<-workflowAgreeAreas(sfIntLong)

  consensus <- computeConsensus(sfInt, wfNames = workflowNames)

  weightedFlux<-createWeightedFlux(intersectSfWide = sfInt, columns = columns, wfNamesIn = workflowNames,
                                   typeLevelsDefaultIn = NULL)

  drawChordDiagram(weightedFluxIn = weightedFlux, labelMatch = labelMatch,...)

  output <- list(sfInt = sfInt, sfIntLong = sfIntLong,
                 agreements = agreements, consensus = consensus, weightedFlux = weightedFlux
  )
}


