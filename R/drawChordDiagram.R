#' Draws how LCZ types from several workflows break up into LCZ types of one another
#'
#' @param weightedFluxIn is typically the output of the function createWeightedFlux,
#' and is expected to contain the following columns orig, dest and weightedFlux,
#' whose names are quite self explanatory : orig is the origin LCZ type, dest is the destination LCZ type
#' and weightedFlux is the percentage of area transfered from orig to dest
#' @param colorMapIn is a named vector whose names are the unique values of orig and dest columns of weightedFluxIn
#' and values are the associated colors. In cas of grouping, these values are overwritten by the groupCols argument,
#' see the ... parameters
#' @param labelMatch allox to match a level of dest or orig from weightedFluxIn and a label.
#' It also allows to change the order in which they are visualized (default is alphabetical order).
#' It is then possible to prefixe values of Levels by a letter to force this order and use LabelMatch to
#' use original names.
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
#' twoLocsWeightedFluxNo104no101<-subset(twoLocsWeightedFlux,
#'      !grepl("101", twoLocsWeightedFlux$orig) &
#'      !grepl("104", twoLocsWeightedFlux$orig) &
#'      !grepl("101", twoLocsWeightedFlux$dest) &
#'      !grepl("104", twoLocsWeightedFlux$dest))
#' aggregMatch<-c("acompact"="Compact", "blessCompact" = "Less Compact", "cfewToNoBuild" = "Few to No Buildings",
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
drawChordDiagram <- function(weightedFluxIn, colorMapIn = NULL,
                             labelMatch = NULL, ...) {
  if(!is.null(weightedFluxIn$percArea1)){weightedFluxIn$percArea1<-NULL}
  args <- list(...)

   if (length(args)>0){

     message( " Additional arguments to weightedFluxIn, colorMapIn and labelMatch
      were detected and will be treated as grouping arguments of type key = value with
      a possible groupColors named vector. If this was not intended, it will lead to errors")
     drawChordDiagramWithGrouping(
       weightedFluxIn = weightedFluxIn, colorMapIn = colorMapIn,
       labelMatch = labelMatch,
       ...)
   } else {

   standardSuffix <- c( 1:10, 101:107, "Unclassified")
   uniqueOrigDest<-unique(unlist(weightedFluxIn[,c("orig", "dest")]))
   uniqueOrigDest <- gsub(x = uniqueOrigDest, pattern =  "(.*)(_)(.*)", replacement = "\\3")

   if (prod(uniqueOrigDest %in% standardSuffix) == 1){
             message("Only standard LCZ levels detected, standard colors and labels used")
             labelMatch <- c(
               "001" = "1", "002" = "2", "003" = "3", "004" = "4", "005" = "5", "006" = "6",
               "007" = "7", "008" = "8", "009" = "9", "010" = "10",
               "101" = "A", "102" = "B", "103" = "C", "104" = "D", "105" = "E", "106" = "F",
               "107" = "G","Unclassified" = "Unclass."
             )
             weightedFluxIn$dest<-LCZlevelToOrderedString(weightedFluxIn$dest)
             weightedFluxIn$orig<-LCZlevelToOrderedString(weightedFluxIn$orig)
            # Color Map management.
            # Regular Case
             if(is.null(colorMapIn)){ colorMapIn <- .lczenv$colorMapDefault }
               names(colorMapIn) <- lczexplore::LCZlevelToOrderedString(names(colorMapIn))
     } else {
       message("Non-standard LabelMatch")
         if (is.null(labelMatch)){
           message(" No labels specified, levels of the data will be taken as is")
           labelMatch <- uniqueOrigDest
           names(labelMatch) <- labelMatch
         }

        if (is.null(colorMapIn)) {
          message("No Color map specified, random palette will be chosen")
          colorMapIn <- randomcoloR::randomColor(count = length(uniqueOrigDest))
          names(colorMapIn)<-labelMatch
        }

     }

  # Labels, sectors and groups management
  sectors <- makeSectorsAndGroups(weightedFluxIn)$sectors

  sector_ids <- strsplit(sectors, "_") %>%
    unlist2d() %>%
    fselect("V2") %>%
    as.vector %>%
    unlist %>%
    unique

  df.groups <- makeSectorsAndGroups(weightedFluxIn)$df.groups

  if (is.null(labelMatch)) {
      labelMatch<-sector_ids
      names(labelMatch <- labelMatch)
  }


  # something to adapt when we will hightlight bigger flux
  # if(!is.null(drawpBigger)){
  #   colorMatrix<-colorMapIn[multMatConfLongIn$orig]
  #  multMatConfLongIn<-arrange(multMatConfLongIn, weightedFlux)
  #   threshold<-multMatConfLongIn[drawpBigger, weightedFlux]
  #   transparencyVec<-rep(0.3, nrow(multMatConfLongIn))
  #   transparencyVec[multMatConfLongIn$weightedFlux<threshold]<-0.001
  #   col.mat <- colorRamp2(breaks=colorMatrix[multMatConfLongIn$orig], colors = colorMatrix, transparency = transparencyVec, space = "LAB",
  #              hcl_palette = NULL, reverse = FALSE)
  # }

  colorsCircle <- colorMapIn[
    gsub(
      x = sectors,
      pattern = "(.*)(_)(.*)",
      replacement = "\\3")
  ]
  names(colorsCircle) <- sectors

 if (max(nchar(names(colorsCircle))) > 3){ facing <- "bending"} else { facing <- "clockwise"}

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

     # Prepare workflow inner circle
  circos.track(track.index = 2,
               panel.fun = function(x, y) {
                 sector.name <- get.cell.meta.data("sector.index")
                 xlim <- get.cell.meta.data("xlim")
                 xplot <- get.cell.meta.data("xplot")
                 ylim <- get.cell.meta.data("ylim")

                 # if(abs(xplot[2] - xplot[1]) < 4) {
                 circos.text(
                   mean(xlim), ylim[1], substr(sector.name, 1, 3),
                   facing = facing,
                   niceFacing = TRUE, adj = c(0.1, 0.01))
                 # } else {
                 #   circos.text(mean(xlim), ylim[1], substr(sector.name,1,3), facing = "inside",
                 #               niceFacing = TRUE, adj = c(0.5, 0))
                 # }
               },
               bg.border = NA)
 # Prepare outer circle of LCZ types
     circos.track(track.index = 2,
                  panel.fun = function(x, y) {
                    sector.name <- get.cell.meta.data("sector.index")
                    xlim <- get.cell.meta.data("xlim")
                    xplot <- get.cell.meta.data("xplot")
                    ylim <- get.cell.meta.data("ylim")

                    # if(abs(xplot[2] - xplot[1]) < 4) {

                  },
                  bg.border = NA)


     # here set bg.border to NA is important
  par(cex = 1.5)
  sectorsIn<-unique(c(diagramme$rn, diagramme$cn))
  lapply(sector_ids, drawSectors, sectorsIn = sectorsIn,
         colorMapIn = colorMapIn, textMatch = labelMatch)
  }
}