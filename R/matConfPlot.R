#' Plots a tile graph of a confusion matrix between two sets of lcz on the same area
#' 
#' @details Most of the time this function will not be called directly
#' by the user but by the compareLCZ function
#' @param matConf is a dataframe for the matrice of confusion/agreement, in the longform : 
#' first column is the LCZ level of the first workflow, second the level of the second workflow and 
#' the third column the percentage of surface of the level of the first classification classified in the level of
#' the second classification
#' @param column1 is the name of the first classification
#' @param column2 is the name of the second classification
#' @param agreeColumn is the name where the agreement between classification is stored
#' @param wf1 is the name of the first workflow (for labels)
#' @param wf2 is the name of the second workflow (for labels)
#' @param plotNow if TRUE the plot of the matrix is returned
#' @param saveG if not NULL will be the path in which the plot will be saved
#' @param ... a set of unspecified arguments, for instance when the produceAnalysis function calls other functions
#'
#' @return returns a plot of a matrice of confusion
#' @import sf ggplot2 dplyr cowplot forcats units tidyr RColorBrewer rlang
#' @export
#'
#' @examples
#'
matConfPlot <- function(matConf,
                        column1 = "lcz_primary", column2 = "lcz_primary.1", agreeColumn = "agree",
                        wf1 = "reference", wf2 = "alternative", plotNow = TRUE, saveG = NULL) {
  outPlot <- ggplot(matConfLong$matConf) +
    geom_tile(aes(x = .data[[column1]], y = .data[[column2]], fill = .data[[agreeColumn]]),
              color = "white", lwd = 1.2, linetype = 1) +
    scale_fill_gradient2(low = "lightgrey", mid = "cyan", high = "blue",
                         midpoint = 50, limit = c(0, 100), space = "Lab",
                         name = "% area") +
    geom_text(data = matConfLong[matConfLong[[agreeColumn]] != 0,],
              aes(x = .data[[column1]], y = .data[[column2]], label = round(.data[[agreeColumn]], digits = 0)),
              color = "black") +
    coord_fixed() +
    labs(x = wf1, y = wf2) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1),
          panel.background = element_rect(fill = "grey"))
  if (!is.null(saveG) && length(saveG) ==1){
    plotPath<-paste0(saveG, "/", wf1,"_",wf2,"_matConfPlot.png")
    print(plotPath)
    ggsave(plotPath, outPlot)
  }
  print(outPlot)
  return(outPlot)
}
