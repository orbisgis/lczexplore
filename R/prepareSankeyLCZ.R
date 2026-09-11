#' Prepares the data to produce a sankey graph between two LCZ workflows
#' @param intersectedDf is an sf object or a data frame which contains lcz values for
#' at least two workflows on the same geometries,
#' and the area of the geometries
#' @param wf1 is the column name where the LCZ value of the first workflow are stored
#' @param wf2 wf1 is the column name where the LCZ value of the first workflow are stored
#' @return an object to feed plotSankeyLCZ
#' @importFrom dplyr case_when
#' @import sf ggplot2 ggsankeyfier
#' @export
#' @examples
#' dirPath<-paste0(
#' system.file("extdata", package = "lczexplore"),"/multipleWfs")
#' allLocConcatenated<-loadMultipleLocsSfs(
#'   dirPath = dirPath, inLocations = c("Arville", "Redon"))
#' allLocIntersected<-createIntersect(allLocConcatenated, columns = rep("lcz_primary", 4),
#' workflowNames = c("osm","bdt","wudapt"))
#' testSankey<-prepareSankeyLCZ(intersectedDf = allLocIntersected
#'  , wf1 = "wudapt", wf2 = "osm")
prepareSankeyLCZ <- function(intersectedDf, wf1, wf2) {
  if ("data.table" %in% class(intersectedDf)) { setDF(intersectedDf) }
  if ("sf" %in% class(intersectedDf)) {
    intersectedDf <- st_drop_geometry(intersectedDf)
    intersectedDf <- as.data.frame(intersectedDf)
  }
  intersectedDf <- intersectedDf[, c(wf1, wf2, "area")]
  uniqueLevels<-unique(c(intersectedDf[[wf1]], intersectedDf[[wf2]]))

  if ( prod(uniqueLevels %in% .lczenv$typeLevelsDefault ) == 1){
    internRecode <- function(LCZvect) {
      case_when(
        nchar(as.character(LCZvect)) == 1 ~ paste0("00", LCZvect),
        nchar(as.character(LCZvect)) == 2 ~ paste0("0", LCZvect),
        .default = as.character(LCZvect))
    }

    intersectedDf[[wf1]] <- internRecode(intersectedDf[[wf1]]) %>%
      ordered( levels = rev(
        c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010",
          "101", "102", "103", "104", "105", "106", "107", "Unclassified")))
    intersectedDf[[wf2]] <- internRecode(intersectedDf[[wf2]]) %>%
      ordered( levels = rev(
        c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010",
          "101", "102", "103", "104", "105", "106", "107", "Unclassified")))
  } else {
    intersectedDf[[wf1]] <- factor(intersectedDf[[wf1]], levels = uniqueLevels)
    intersectedDf[[wf2]] <- factor(intersectedDf[[wf2]], levels = uniqueLevels)
  }

  #   intersectedDf <- aggregate(
  #   area ~ get(wf1) + get(wf2),
  #   data = intersectedDf,
  #   FUN = sum
  # )
  # restore column names after aggregate renames them
  names(intersectedDf)[1:2] <- c(wf1, wf2)

  print(names(intersectedDf))

  sankeyfied <- ggsankeyfier::pivot_stages_longer(
    data = st_drop_geometry(intersectedDf),
    stages_from = c(wf1, wf2),
    values_from = "area"
  )
  sankeyfied$node <- ordered(
    sankeyfied$node,
    levels = rev(c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010",
                   "101", "102", "103", "104", "105", "106", "107", "Unclassified"))
  )
  sankeyfied <- sankeyfied[order(sankeyfied$node),]
  return(sankeyfied)
}