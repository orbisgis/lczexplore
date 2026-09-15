#' Draws how LCZ types from several workflows break up into LCZ types of one another when some grouping is precised in the arguments
#'
#'
#' @param weightedFluxIn is typically the output of the function createWeightedFlux,
#' and is expected to contain the following columns orig, dest and weightedFlux,
#' whose names are quite self explanatory : orig is the origin LCZ type, dest is the destination LCZ type
#' and weightedFlux is the percentage of area transfered from orig to dest
#' @param colorMapIn is a named vector whose names are the unique values of orig and dest columns of weightedFluxIn
#' and values are the associated colors. In cas of grouping, these values are overwritten by the groupCols argument,
#' see the ... parameters
#' @param labelMatch allow to match a level of dest or orig from weightedFluxIn and a label.
#' Remember to match labels with the grouping names you chose in the named grouping vectors passed to ...
#' When using grouping vectors, they are visualized in the order they are specified.
#' @param ... allows the user to do on-the-fly grouping. These must be passed as groupName = groupValues
#' where groupName is the name of a resulting group and groupValues a vector of the initial values
#' it will regroup.
#' @return a vector of booleans indicting if the elements of x define a color in R (TRUE) or don't (FALSE)
#' @importFrom circlize circos.clear circos.track circos.text chordDiagram get.cell.meta.data
#' @importFrom collapse unlist2d fselect
#' @importFrom graphics par
#' @export
#' @examples
#' twoLocsDir<-paste0(system.file("extdata", package = "lczexplore"),"/multipleWfs")
#' twoLocsSfList<-loadMultipleLocsSfs(dirPath = twoLocsDir, workflowNames = c("osm","bdt","wudapt"),
#' inLocation = c("Arville", "Redon"))
#' twoLocsSfIntersected <- createIntersect(sfList = twoLocsSfList, columns = rep("lcz_primary", 4),
#' refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.001)
#' twoLocsWeightedFlux<-createWeightedFlux(twoLocsSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))
#' # Subsetting and grouping allow further exploration. Playing on grouping names and alphabetical order allows
#' # to choose the order of the sectors
#' #' # Subsetting and grouping allow further exploration. Playing on grouping names and alphabetical order allows
#' # to choose the order of the sectors
#' twoLocsWeightedFluxNo104no101<-subset(twoLocsWeightedFlux,
#'      !grepl("101", twoLocsWeightedFlux$orig) &
#'      !grepl("104", twoLocsWeightedFlux$orig) &
#'      !grepl("101", twoLocsWeightedFlux$dest) &
#'      !grepl("104", twoLocsWeightedFlux$dest))
#' aggregMatch<-c("acompact"="Compact", "blessCompact" = "Less Compact",
#' "cfewToNoBuild" = "Few to No Buildings at All",
#'     "dunclass" = "Unclassified")
#' drawChordDiagram(twoLocsWeightedFluxNo104no101, labelMatch = aggregMatch,
#'     acompact = c("1", "2", "3"),
#'     blessCompact = c("4", "5", "6", "7", "8", "10"),
#'     cfewToNoBuild = c("101", "102", "103", "104", "105", "106", "107", "9"),
#'     dunclass = "Unclassified",
#'     groupColors = c(
#'       "acompact" = "#8b0101",
#'       "blessCompact" = "#ff9856",
#'        "cfewToNoBuild" = "#bbdb7a","dunclass" = "grey"))
drawChordDiagramWithGrouping<-function(weightedFluxIn,
                                       colorMapIn = NULL,
                                       labelMatch = NULL,
                                        ...){
  args <- list(...)

  # Case when grouping is specified
  colorMapIn <- unlist(args[names(args) == "groupColors"]$groupColors)
  groupArgs<-args[names(args) != "groupColors"]
  uniqueOrigDest<-unique(unlist(weightedFluxIn[,c("orig", "dest")]))
  uniqueOrigDest <- gsub(x = uniqueOrigDest, pattern =  "(.*)(_)(.*)", replacement = "\\3")
  if (prod(!unlist(groupArgs)%in%uniqueOrigDest)){
    message("None of the levels to group are present in the data, please check your grouping vectors
    and retry")
  stop()}

  groupNames<-paste0(
      letters[seq_along(names(groupArgs))],
      names(groupArgs), sep ="")

    if (is.null(labelMatch)){
      labelMatch<-names(groupArgs)
      names(labelMatch)<-groupNames
    } else {names(labelMatch)<-groupNames}


  names(groupArgs)<-groupNames
  names(colorMapIn)<-groupNames
    weightedFluxIn <- do.call(
      groupLCZsuffix,
      c(list(weightedFluxIn = weightedFluxIn), groupArgs)
    )

  sectorsAndGroups <- makeSectorsAndGroups(weightedFluxIn, labelMatch)
  sectors<-sectorsAndGroups$sectors
  sector_ids <- strsplit(sectors, "_") %>%
    unlist2d() %>%
    fselect("V2") %>%
    as.vector %>%
    unlist %>%
    unique

  df.groups <- sectorsAndGroups$df.groups

  colorsCircle <- colorMapIn[
    gsub(
      x = sectors,
      pattern = "(.*)(_)(.*)",
      replacement = "\\3")
  ]
  names(colorsCircle) <- sectors

  circos.clear()
  diagramme <- chordDiagram(
    weightedFluxIn, grid.col = colorsCircle,
    # col =col.mat,
    big.gap = 5, small.gap = 2,
    order = sectors, group = df.groups,
    annotationTrack = NULL,
    preAllocateTracks = list(
      list(track.height = 0.06),
      list(track.height = 0.06)
    ),
    transparency = 0.25,
    symmetric = TRUE,
    directional = 1,
    direction.type = c("arrows", "diffHeight"),
    link.arr.type = "big.arrow",
    link.largest.ontop = TRUE)
  par(font = 2, cex = 1.2)

  circos.track(track.index = 2,
               panel.fun = function(x, y) {
                 sector.name <- get.cell.meta.data("sector.index")
                 xlim <- get.cell.meta.data("xlim")
                 xplot <- get.cell.meta.data("xplot")
                 ylim <- get.cell.meta.data("ylim")

                 # if(abs(xplot[2] - xplot[1]) < 4) {
                 circos.text(
                   mean(xlim), ylim[1], substr(sector.name, 1, 3),
                   facing = "clockwise",
                   niceFacing = TRUE, adj = c(0.01, 0.5))
                 # } else {
                 #   circos.text(mean(xlim), ylim[1], substr(sector.name,1,3), facing = "inside",
                 #               niceFacing = TRUE, adj = c(0.5, 0))
                 # }
               },
               bg.border = NA) # here set bg.border to NA is important
  par(cex = 1.5)
  sectorsIn<-unique(c(diagramme$rn, diagramme$cn))

  if( max(nchar(sector_ids))> 3){sectorFacing <- "bending"} else
  {sectorFacing <- "clockwise"}
  lapply(sector_ids, drawSectors, sectorsIn = sectorsIn,
         colorMapIn = colorMapIn, textMatch = labelMatch)
}

