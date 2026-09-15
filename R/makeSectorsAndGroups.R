#' Creates sectors and groups for the drawChordDiagram function
#'
#' @param weightedFluxIn is typically the output of the function createWeightedFlux,
#' and is expected to contain the following columns orig, dest and weightedFlux,
#' whose names are quite self explanatory : orig is the origin LCZ type, dest is the destination LCZ type
#' and weightedFlux is the percentage of area transfered from orig to dest
#' @param groupOrder allows to specify in which order groups are visualized, usually
#' the ones specified in labelMAtch argument of drawChordDiagram function
#' @return a list containing vectors and groups for a chord diagram
#' @importFrom dplyr case_when
#' @export
makeSectorsAndGroups <- function(weightedFluxIn, groupOrder = NULL) {

  sectors <- unique(c(weightedFluxIn$orig, weightedFluxIn$dest))

  # Extract group names and labels
  sect_lev <- gsub(
    x = sectors,
    pattern = "(.*)(_)(.*)",
    replacement = "\\3")

  sect_wf <- gsub(
    x = sectors,
    pattern = "(.*)(_)(.*)",
    replacement = "\\1")

  # If groupOrder is provided, use it for ordering
  if (!is.null(groupOrder)) {
    # Get the order based on groupOrder
    order_index <- match(sect_lev, groupOrder)
    # Sort by: first by workflow (sect_wf), then by group order
    sectors <- sectors[order(order_index, sect_wf)]
  }

  # Re-extract after reordering
  sect_lev <- gsub(
    x = sectors,
    pattern = "(.*)(_)(.*)",
    replacement = "\\3")

  df_groups_val <- case_when(
    nchar(sect_lev) == 1 ~ paste0("00", sect_lev),
    nchar(sect_lev) == 2 ~ paste0("0", sect_lev),
    .default = sect_lev
  )

  df.groups <- structure(df_groups_val, names = sectors)
  output <- list(sectors = sectors, df.groups = df.groups)
  return(output)
}