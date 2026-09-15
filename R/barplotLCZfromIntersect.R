#' plots the repartition of LCZ types regarding of their source (map/workflow)
#' from an sf object containing all spatial units and LCZ value, typically the output of createIntersect
#' @param sfIn is the file containing the spatial units and LCZ type per workflow
#' @param workflowNames is a vector of prefixes. The LCZ files must be named workflow_rsu.fgb
#' where workflow is on of the values in workflowNames vector
#' @param columns is the vector of names of the columns containing the LCZ types. If not specified, the function
#' will try to use the workflowNames instead
#' @param stat can take the value "perc" to plot percentage of area for each LCZ type or "sum" for total
#' area for each LCZ type
#' @param plotNow If TRUE, the boxplot of the repartition will be printed
#' @importFrom ggplot2 geom_sf guides ggtitle aes
#' @importFrom collapse fmutate fselect frename fgroup_by fsummarise fsum
#' @importFrom caret dummyVars
#' @importFrom dplyr mutate group_by summarise
#' @importFrom tidyr  replace_na
#' @import sf forcats units RColorBrewer units utils grDevices
#' @return A barplot of LCZ area percentage by LCZ type and workflow
#' @export
#' @examples
#'
#' # Two Locations
#' sfList2<-loadMultipleLocsSfs(
#'  dirPath = paste0(
#'      system.file("extdata", package = "lczexplore"),
#'      "/multipleWfs"),
#'  workflowNames = c("osm","bdt","wudapt"))
#'
#' twoLocsIntersect <- createIntersect(
#'  sfList = sfList2, columns = rep("lcz_primary", 3),
#'  workflowNames = c("osm","bdt","wudapt"))
#' example<-barplotLCZfromIntersect(sfIn = twoLocsIntersect,
#'                               columns = c("osm", "bdt", "wudapt"),
#'                             workflowNames = c("osm", "bdt", "wudapt"))
barplotLCZfromIntersect <- function(sfIn, workflowNames = NULL, columns = NULL, stat = "perc", plotNow = TRUE) {
  checkedWfCol <- checkColumnWorkflowNames(columns = columns, workflowNames = workflowNames, sfIn = sfIn)
  columns <- checkedWfCol$columns
  workflowNames <- checkedWfCol$workflowNames


  if (!("area" %in% names(sfIn))) {
    concatSf$area <- st_area(sfIn)
  }
  df <- st_drop_geometry(sfIn)

  if (stat == "perc") {
    percSurf <- lapply(seq_along(columns), function(g) {
      groupCol <- columns[g]
      tmp <- df %>%
        fgroup_by(groupCol) %>%
        fsummarise(area = fsum(area)) %>%
        fmutate(pct = area / sum(area) * 100) %>%
        fselect(groupCol, "pct") %>%
        frename(lcz_primary = groupCol, .nse = FALSE) %>%
        fmutate(wf = workflowNames[g])
      return(tmp)
    })
    percSurf <- do.call(rbind, percSurf)

    outPlot <- ggplot(data = percSurf) +
      geom_col(aes(x = wf, y = pct, fill = lcz_primary)) +
      scale_fill_manual(
        values = .lczenv$colorMapDefault,
        breaks = names(.lczenv$colorMapDefault),
        labels = .lczenv$shortEtiquettesDefault,
        na.value = "ghostwhite") +
      labs(x = "Workflow", y = "Percentage of Area")
    if (plotNow) print(outPlot)
    return(percSurf) } else if (stat == "sum") {
    sumSurf <- lapply(seq_along(columns), function(g) {
      groupCol <- columns[g]
      tmp <- df %>%
        fgroup_by(groupCol) %>%
        fsummarise(sumArea = fsum(area)) %>%
        fselect(groupCol, "sumArea") %>%
        frename(lcz_primary = groupCol, .nse = FALSE) %>%
        fmutate(wf = workflowNames[g])
    })

    sumSurf <- do.call(rbind, sumSurf)

    outPlot <- ggplot(data = sumSurf) +
      geom_col(aes(x = wf, y = sumArea, fill = lcz_primary)) +
      scale_fill_manual(
        values = .lczenv$colorMapDefault,
        breaks = names(.lczenv$colorMapDefault),
        labels = .lczenv$shortEtiquettesDefault,
        na.value = "ghostwhite") +
      labs(x = "Workflow", y = "Summed Area")
    if (plotNow) print(outPlot)
    return(sumSurf)
  }

}


checkColumnWorkflowNames <- function(columns, workflowNames, sfIn) {
  if (
    (is.null(columns) | prod(!is.na(columns)) == 0) &
      (!is.null(workflowNames)) &
      prod(!is.na(workflowNames)) == 1 &
      length(workflowNames) > 1) {
    message(paste0("The names of the columns of the lcz types for each workflow are missing, ",
                   "an attempt will be made with workflowNames instead "))
    columns <- workflowNames
  }

  if ((is.null(workflowNames) | prod(!is.na(workflowNames)) == 0) &
    (!is.null(columns) | prod(!is.na(columns)) == 1)) {
    message(paste0("The names of the workflows are missing, ",
                   "an attempt will be made with column names instead "))
    workflowNames <- columns
  }

  if (
    (is.null(workflowNames) | prod(!is.na(workflowNames)) == 0) &&
      (is.null(columns) | prod(!is.na(columns) == 0))) {
    message(paste0("The names of the workflows and of the columns are missing.",
                   "Will try to replace them with names of columns other than location, area and geometry,",
                   "but this is hazardous."))
    workflowNames <- columns <- names(sfIn)[!names(sfIn) %in% c("location", "area", "geometry")]
  }
  output <- list(columns = columns, workflowNames = workflowNames)
  return(output)

}
