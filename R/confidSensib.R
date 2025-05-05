#' Computes the agreement between geographical (LCZ) classifications on a range of values of
#' an indicator of confidence granted to each LCZ classification. 
#' The input file or dataset must have been produced by compareLCZ function, 
#' or at least the columns must be in the same order
#'
#' @param inputDf is an R file with geom IDs, (LCZ) classifications and
#' a confidence value granted for the (LCZ) classifications values of each geom. Ignored if filePath is not empty.
#' @param filePath is the path to a csv file containing geom IDs, LCZ classifications and
#' a confidence value granted for the LCZ value of each geom. Set it to "" if inputDf is specified (recommanded)
#' @param nPoints is the number of points (quantiles) of confidence for which
#' the average agreement between classifications will be computed
#' @param wf1 is the name of the workflow used to produce the first LCZ
#' @param wf2 is the name of the workflow used to produce the second LCZ
#' @param geomID1 is the name of the column that contains the geom ID associated to the first workflow
#' @param column1 is the name of the column storing the first LCZ classification values
#' @param confid1 is the name of the column storing the first LCZ classification confidence value
#' @param geomID2 is the name of the column that contains the geom ID associated to the second workflow
#' @param column2 is the name of the column storing the second LCZ classification values
#' @param confid2 is the name of the column storing the second LCZ classification confidence value
#' @param sep the separator used if filePath is not empty
#' @param repr is standard when original LCZ values are expected, alter otherwise
#' @param plot if TRUE the graph is plotted
#' @param saveG if not an empty string, specifies where to save graphs
#' @import dplyr ggplot2
#' @return returns an object called output, which contains the values of the thresholds
#' for the confidence value and the agreement between classifications for the LCZ levels presents in the dataset
#' @export
#'
#' @examples
#' # creation of the comparison data on which to perform the analysis
#' redonCompare<-compareLCZ(sf1=redonBDT, wf1="bdt", geomID1 = "ID_RSU", column1 ="LCZ_PRIMARY",
#'                         confid1 = "LCZ_UNIQUENESS_VALUE",
#'                         sf2=redonOSM, wf2="osm",geomID2 = "ID_RSU", column2="LCZ_PRIMARY",
#'                         confid2 ="LCZ_UNIQUENESS_VALUE", exwrite=FALSE, plot=FALSE, saveG="")
#' confidSensib(inputDf=redonCompare$data,
#' nPoints=5, wf1="bdtopo_2_2", wf2="osm",
#' geomID1="ID_RSU", column1="LCZ_PRIMARY", confid1="LCZ_UNIQUENESS_VALUE",
#' geomID2="ID_RSU.1",column2="LCZ_PRIMARY.1", confid2="LCZ_UNIQUENESS_VALUE.1",
#' sep=";", repr="standard", plot=TRUE, saveG="")
confidSensib <- function(inputDf = "", filePath = "", nPoints = 5,
                         wf1 = "bdtopo_2_2", wf2 = "osm",
                         geomID1 = "ID_RSU", column1 = "LCZ_PRIMARY", confid1 = "LCZ_UNIQUENESS_VALUE",
                         geomID2 = "ID_RSU.1", column2 = "LCZ_PRIMARY.1", confid2 = "LCZ_UNIQUENESS_VALUE.1",
                         sep = ";", repr = "standard",
                         plot = TRUE, saveG = "") {

  colonnes <- c(geomID1, column1, confid1, geomID2, column2, confid2)
  colonnes <- colonnes[sapply(colonnes, nchar) != 0] %>% c("agree", "area", "location")

  # Import the data if they are in a csv file or in a R object
  if (filePath != "") {
    echInt <- dplyr::distinct(read.csv(filePath, sep, header = T, stringsAsFactors = T))
    names(echInt) <- colonnes
  } else {
    if (!is.null(inputDf)) { echInt <- dplyr::distinct(inputDf[, colonnes]) }
    else { stop("You must specifiy a file path or the name of the object storing confidence and agreement") }
  }


  echInt$confidMin <- pmin(echInt[, confid1], echInt[, confid2])


  # What is the agreement between LCZ classifications when no confidence value is available on any of them ?
  echIntNoconf <- subset(echInt, is.na(echInt$confidMin))
  nbOutCasted <- nrow(echIntNoconf)
  # print("Number of geoms without any confidence value : ")
  # print(nbOutCasted)

  NAPercAgr <- matConfLCZGlob(inputDf = echIntNoconf, wf1 = wf1, wf2 = wf2,
                              geomID1 = geomID1, column1 = column1, confid1 = confid1,
                              geomID2 = geomID2, column2 = column2, confid2 = confid2,
                              sep = ";", repr = "standard", typeLevels = "", plot = F)$pourcAcc

  #How does the max of the confidence value of the LCZ classifs influences the agreement
  # between LCZ classifications

  echIntConf <- subset(echInt, !is.na(echInt$confidMin))


  #############################################################################################
  # All LCZ levels treated together
  #############################################################################################
  internFunction <- function(echIntConf, nPoints) {
    confSeq <- quantile(echIntConf$confidMin, probs = seq(0, 1, length.out = nPoints), na.rm = T)
    print("confSeq in internFunction "); print(confSeq)

    percAgrKeep <- NULL
    nbKeep <- NULL
    percAgrDrop <- NULL
    nbDrop <- NULL

    for (i in confSeq) {
         
      echIntKeep <- subset(echIntConf, confidMin >= i)
      if (nrow(echIntKeep) > 0) { #print(nrow(echIntKeep))
        percAgrKeep <- c(percAgrKeep,
                         matConfLCZGlob(inputDf = echIntKeep, wf1 = wf1, wf2 = wf2,
                                        geomID1 = geomID1, column1 = column1, confid1 = confid1,
                                        geomID2 = geomID2, column2 = column2, confid2 = confid2,
                                        sep = ";", repr = "standard", typeLevels = "", plot = F)$pourcAcc)
        nbKeep <- c(nbKeep, nrow(echIntKeep))

      }else {
        percAgrKeep <- c(percAgrKeep, NA)
        nbKeep <- c(nbKeep, 0)
      }

      echIntDrop <- subset(echIntConf, confidMin < i)
      if (nrow(echIntDrop) > 0) { #print(nrow(echIntDrop))
        percAgrDrop <- c(percAgrDrop,
                         matConfLCZGlob(inputDf = echIntDrop, wf1 = wf1, wf2 = wf2,
                                        geomID1 = geomID1, column1 = column1, confid1 = confid1,
                                        geomID2 = geomID2, column2 = column2, confid2 = confid2,
                                        sep = ";", repr = "standard", typeLevels = "", plot = F)$pourcAcc)
        nbDrop <- c(nbDrop, nrow(echIntDrop))
      }else {
        percAgrDrop <- c(percAgrDrop, NA)
        nbDrop <- c(nbDrop, 0) }

    }
    #   summary(echInt)

    data <- data.frame(Confidence = c(confSeq, confSeq),
                       Agreement = c(percAgrKeep, percAgrDrop),
                       Kept = rep(c("confidence >= threshold", "confidence < threshold"), each = nPoints),
                       nbGeoms = c(nbKeep, nbDrop))
    data$Kept <- factor(data$Kept, levels = c("confidence >= threshold", "confidence < threshold"))
    # graphics

    etiquette <- paste0("average agreement percentage for \n LCZ with no confidence value : ",
                        NAPercAgr,
                        " \n (these ",
                        nbOutCasted, " geoms are excluded from computing other points)")
    pointSize <- 20 / sqrt(nPoints)
    confThreshPlot <- ggplot(data = data, aes(x = Confidence, y = Agreement, color = Kept, shape = Kept)) +
      labs(color = "Kept Geometries", shape = "Kept Geometries") +
      #scale_fill_discrete(breaks=c("confidence >= threshold","confidence < threshold"),)+
      scale_color_manual(values =
                           c("confidence >= threshold" = "#00BFC4", "confidence < threshold" = "#F8766D")) +
      geom_point(size = rel(pointSize)) +
      geom_text(aes(x = Confidence, y = Agreement, label = nbGeoms), nudge_y = -2, size = rel(pointSize), show.legend = FALSE) +
      geom_hline(yintercept = NAPercAgr, linetype = 'dashed', color = 'grey') +
      geom_text(aes(x = 0.50, y = NAPercAgr, label = etiquette, vjust = 1.5,),
                inherit.aes = F, color = 'darkgrey', size = rel(6)) +
      ggtitle(label = "Agreement according to the minimum confidence granted to LCZ level",
              subtitle = "Number of geoms used to compute agreement written under each point") +
      theme(axis.title.x = element_text(size = rel(1.8)),
            axis.title.y = element_text(size = rel(1.8)),
            axis.text = element_text(size = rel(2)),
            plot.title = element_text(size = rel(2)),
            plot.subtitle = element_text(size = rel(2)),
            legend.title = element_text(size = rel(1.8)),
            legend.text = element_text(size = rel(1.5)))

    ctOut <- list(ctPlot = confThreshPlot, ctData = data)
    return(ctOut)
  }

  allLCZ <- internFunction(echIntConf = echIntConf, nPoints = nPoints)
  if (plot == TRUE) {
    plot(allLCZ$ctPlot)
  }

  if (saveG != "") {
    plotName <- paste0(saveG, "/GeneralUniquenessSensib.png")
    png(filename = plotName, width = 1200, height = 900)
    print(allLCZ$ctPlot)
    dev.off()
  }

  #############################################################################################
  # Per LCZ levels of the first classification
  #############################################################################################
  typeLevels <- unique(echIntConf[, column1]) %>% as.vector
  #  print("typeLevels")
  #  print(typeLevels)
  # print("echinConf avant boucle LCZ") ; print(head(echIntConf))
  byLCZ <- data.frame(Confidence = numeric(), Agreement = numeric(),
                      Kept = character(), nbGeoms = numeric(), LCZ = character())

  echIntConfSplit <- split(x = echIntConf, f = echIntConf[[column1]], drop = T)

  internFunction2 <- function(echIntConf, nPoints) { internFunction(echIntConf, nPoints)$ctData }
  # sortieParLCZ<-aggregate(echIntConf,by=echIntConf[[column1]],internFunction2,nPoints=nPoints)
  sortieParLCZ <- lapply(echIntConfSplit, internFunction2, nPoints = nPoints)
  nivList <- names(sortieParLCZ)
  output <- data.frame(Confidence = numeric(0), Agreement = numeric(0), Kept = character(0),
                       nbGeom = numeric(0), LCZ = character(0))
  for (i in names(sortieParLCZ)) {
    output <- rbind(output, cbind(sortieParLCZ[[i]], LCZ = rep(i, nrow(sortieParLCZ[[i]]))))
  }

  pointSize <- 13 / sqrt(nPoints)
  byLCZPLot <- ggplot(data = output, aes(x = Confidence, y = Agreement, color = Kept, shape = Kept)) +
    labs(x = "Confidence threshold", color = "Geom set", shape = "Geom set", size = rel(1.3)) +
    scale_fill_discrete(breaks = c("confidence >= threshold", "confidence < threshold"),) +
    scale_color_manual(values =
                         c("confidence >= threshold" = "#00BFC4", "confidence < threshold" = "#F8766D")) +
    geom_point(size = rel(pointSize)) +
    geom_text(aes(x = Confidence, y = Agreement, label = nbGeoms), nudge_y = -6.3, show.legend = FALSE, size = rel(pointSize)) +
    ggtitle(label = "Agreement by minimum confidence within LCZ level",
            subtitle = "Number of geoms used to compute agreement written under each point") +
    facet_wrap(~LCZ, drop = TRUE) +
    theme(axis.title.x = element_text(size = rel(1.8)),
          axis.title.y = element_text(size = rel(1.8)),
          axis.text = element_text(size = rel(1.6)),
          plot.title = element_text(size = rel(2)),
          plot.subtitle = element_text(size = rel(1.8)),
          legend.title = element_text(size = rel(1.8)),
          legend.text = element_text(size = rel(1.5)))


  if (plot == TRUE) {
    plot(byLCZPLot)
  }

  if (saveG != "") {
    plotName <- paste0(saveG, "/byLCZUniquenessSensib.png")
    png(filename = plotName, width = 1200, height = 900)
    print(byLCZPLot)
    dev.off()
  }


  return(output)

}