#' Compute an indicator of consensus among workflows for each LCZ type
#' @param inDf a data frame, a data table or even an sf object, typically
#' the output of the function createMultipleIntersect. It must contain
#' values of LCZ types for different workflows in columns names after wf names,
#' a column area which contains the area of each spatial unit
#' @param wfNames a vector of strings containing the name of the workflows. The columns
#' containing the LCZ type for each workflow bare the same names.
#' @import data.table
#' @export
#' @examples
#' dirList <- list.dirs(paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs"), recursive = FALSE)
#' allLocIntersected<-concatIntersectedLocations(
#' dirList = dirList, inLocations = c("Arville", "Redon"))
#' consensus <- computeConsensus(inDf = allLocIntersected,
#' wfNames = c("bdt","osm", "wudapt"= "wud"))
computeConsensus <- function(inDf, wfNames) {
  setDT(inDf)
  d1 <- CJ(
    names(inDf)[names(inDf) %in% wfNames],
    names(inDf)[names(inDf) %in% wfNames]
  )[V1 != V2]


  d2 <- d1[, list(LCZ_value = inDf[, get(V1)], LCZ_alter = inDf[, get(V2)], area = inDf[, area]), list(V1, V2)][
    , list(LCZ_value, LCZ_alter, area, agree = LCZ_value == LCZ_alter),][
    , list(LCZ_value, LCZ_alter, area, agree, agreeArea = agree * area, disagreeArea = (!agree) * area),]
  consensus <- d2[, list(percAgree = sum(agreeArea) / (sum(agreeArea) + sum(disagreeArea))), keyby = list(LCZ_value)][order(percAgree), ,]
}