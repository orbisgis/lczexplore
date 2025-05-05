#' Plots a tile graph of a confusion matrix between two sets of lcz on the same area
#' 
#' @details Most of the time this function will not be called directly
#' by the user but by the compareLCZ function
#' @param matConfLong is a dataframe for the matrice of confusion/agreement, in the longform : 
#' first column is the LCZ level of the first workflow, second the level of the second workflow and 
#' the third column the percentage of surface of the level of the first classification classified in the level of
#' the second classification
#' @param column1 is the name of the first classification
#' @param column2 is the name of the second classification
#' @param agreeColumn is the name where the agreement between classification is stored
#' @param wf1 is the name of the first workflow (for labels)
#' @param wf2 is the name of the second workflow (for labels)
#' @param marginAreas are the percentage of areas for LCZ types for both workflows (an output of matConfLCZ)
#' @return returns a plot of a matrice of confusion
#' @import sf ggnewscale ggplot2 dplyr cowplot forcats units tidyr RColorBrewer rlang
#' @export
#'
#' @examples
#'matConfRedonBDTOSM <- matConfLCZ(sf1 = redonBDT, column1 = 'LCZ_PRIMARY',
#'                                 sf2 = redonOSM, column2 = 'LCZ_PRIMARY', plot = TRUE)
#'matConfLong <- matConfRedonBDTOSM$matConf
#'marginAreas <- matConfRedonBDTOSM$areas
#'matConfPlot(matConfLong,
#'            column1 = "LCZ_PRIMARY", column2 = "LCZ_PRIMARY.1", agreeColumn = "agreePercArea",
#'            wf1 = "reference", wf2 = "alternative", marginAreas = marginAreas)
matConfPlot <- function(matConfLong,
                        column1 = "lcz_primary", column2 = "lcz_primary.1", marginAreas = NULL, agreeColumn = "agreePercArea",
                        wf1 = "reference", wf2 = "alternative") {

  condition <- (
    prod(unique(matConfLong[[column1]]) %in% .lczenv$typeLevelsDefault)
      *
      prod(unique(matConfLong[[column2]]) %in% .lczenv$typeLevelsDefault)
  ) == 1
  print(condition)
  if (condition) {
    matConfLong[[column1]] <- fct_recode(matConfLong[[column1]],
                                         "Compact high" = "1",
                                         "Compact mid" = "2",
                                         "Compact low" = "3",
                                         "Open High" = "4",
                                         "Open mid" = "5",
                                         "Open low" = "6",
                                         "Lightweight low" = "7",
                                         "Large low" = "8",
                                         "Sparsely Built" = "9",
                                         "Heavy industry" = "10",
                                         "Dense trees" = "101",
                                         "Scattered trees" = "102",
                                         "Bush scrub" = "103",
                                         "Low plants" = "104",
                                         "Bare rock paved" = "105",
                                         "Bare soil sand" = "106",
                                         "Water" = "107",
                                         "Unclassified" = "Unclassified") %>% as.ordered
    matConfLong[[column2]] <- fct_recode(matConfLong[[column2]],
                                         "Compact high" = "1",
                                         "Compact mid" = "2",
                                         "Compact low" = "3",
                                         "Open High" = "4",
                                         "Open mid" = "5",
                                         "Open low" = "6",
                                         "Lightweight low" = "7",
                                         "Large low" = "8",
                                         "Sparsely Built" = "9",
                                         "Heavy industry" = "10",
                                         "Dense trees" = "101",
                                         "Scattered trees" = "102",
                                         "Bush scrub" = "103",
                                         "Low plants" = "104",
                                         "Bare rock paved" = "105",
                                         "Bare soil sand" = "106",
                                         "Water" = "107",
                                         "Unclassified" = "Unclassified") %>% as.ordered
    marginAreas$marginLevels <- fct_recode(marginAreas$marginLevels, "Compact high" = "1",
                                           "Compact mid" = "2",
                                           "Compact low" = "3",
                                           "Open High" = "4",
                                           "Open mid" = "5",
                                           "Open low" = "6",
                                           "Lightweight low" = "7",
                                           "Large low" = "8",
                                           "Sparsely Built" = "9",
                                           "Heavy industry" = "10",
                                           "Dense trees" = "101",
                                           "Scattered trees" = "102",
                                           "Bush scrub" = "103",
                                           "Low plants" = "104",
                                           "Bare rock paved" = "105",
                                           "Bare soil sand" = "106",
                                           "Water" = "107",
                                           "Unclassified" = "Unclassified") %>% as.ordered

    ticklabels <- c("Compact high",
                    "Compact mid",
                    "Compact low",
                    "Open High",
                    "Open mid",
                    "Open low",
                    "Lightweight low",
                    "Large low",
                    "Sparsely Built",
                    "Heavy industry",
                    "Dense trees",
                    "Scattered trees",
                    "Bush scrub",
                    "Low plants",
                    "Bare rock paved",
                    "Bare soil sand",
                    "Water",
                    "Unclassified", "(Margins->)")
  } else { ticklabels <- c(unique(c(levels(matConfLong[[column1]]), levels(matConfLong[[column2]]))), "(Margins->)") }

  levels(marginAreas$marginLevels) <- c(levels(marginAreas$marginLevels), "(Margins->)")
  marginAreas <- rbind(marginAreas, list("(Margins->)", 0, 0))
  print(marginAreas)


  coordRef <- length(unique(marginAreas$marginLevels))

  outPlot <- ggplot() +
    geom_tile(data = matConfLong, aes(x = .data[[column1]], y = .data[[column2]], fill = .data[[agreeColumn]]),
              color = "white", lwd = 1.4, linetype = 1) +
    scale_fill_gradient2(low = "grey97", mid = "cyan", high = "blue",
                         midpoint = 50, limit = c(0, 100), space = "Lab",
                         name = "% area", na.value =) +
    geom_text(data = matConfLong[matConfLong[[agreeColumn]] != 0,],
              aes(x = .data[[column1]], y = .data[[column2]], label = round(.data[[agreeColumn]], digits = 0), fontface = "bold"),
              color = "black") +
    coord_fixed() +
    labs(x = wf1, y = wf2) +
    geom_tile(
      data = matConfLong[matConfLong[[column1]] == matConfLong[[column2]],],
      aes(x = .data[[column1]], y = .data[[column2]]),
      color = alpha("orangered", 0.5), lwd = 1.2, linetype = 1, fill = NA) +
    ggnewscale::new_scale_fill() +
    geom_tile(marginAreas, mapping = aes(x = marginLevels, y = coordRef, fill = percArea1), height = 0.8, width = 0.8) +
    theme(panel.background = element_rect(fill = "ghostwhite")) +
    geom_text(data = marginAreas[round(marginAreas$percArea1, 0) != 0,],
              aes(x = marginLevels, y = coordRef, label = round(percArea1, digits = 0)),
              color = "gray37") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    geom_tile(marginAreas, mapping = aes(x = coordRef, y = marginLevels, fill = percArea2), height = 0.8, width = 0.8) +
    geom_text(data = marginAreas[round(marginAreas$percArea2, 0) != 0,],
              aes(x = coordRef, y = marginLevels, label = round(percArea2, digits = 0)),
              color = "gray37") +
    scale_fill_gradient2(low = "white", mid = "cyan", high = "blue",
                         midpoint = 50, limit = c(0, 100), space = "Lab",
                         name = "% area") +
    guides(fill = "none") +
    scale_x_discrete(breaks = ticklabels, labels = ticklabels, position = "bottom", expand = c(0, 0)) +
    scale_y_discrete(breaks = ticklabels, labels = ticklabels, position = "left", expand = c(0, 0)) +
    theme(
      panel.background = element_rect(fill = "ghostwhite")) +
    ggtitle(paste0("Repartition of ", wf1, " classes into ", wf2, " classes"))


  # if (!is.null(saveG) && length(saveG) ==1){
  #   plotPath<-paste0(saveG, "/", wf1,"_",wf2,"_matConfPlot.png")
  #   print(plotPath)
  #   ggsave(plotPath, outPlot)
  # }
  # print(outPlot)
  return(outPlot)
}
