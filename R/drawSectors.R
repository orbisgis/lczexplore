#' Draws sectors and groups inside the drawChordDiagram function
#'
#' @param sector_id ar the ideitnfiers of sectors as returned by the makeSectorsAndGroups function
#' @param sectorsIn are the values of a sectors as returned by the makeSectorsAndGroups function
#' @param colorMapIn is a named vector whose names must contain the values of sectorsIn and
#' whose values are the desired colors
#' see drawChorDiagram function for details
#' @param textMatch is fed by drawChordDiagram to provide labels for sectors
#' @return a list containing vectors and groups for a chord diagram
#' @importFrom shades complement
#' @importFrom circlize highlight.sector
#'
#' @export
drawSectors <- function(sector_id, sectorsIn = sectors,
                        colorMapIn = colorMap, textMatch){

  sectorsToHighlight <- grep(x = sectorsIn, pattern = sector_id, value = T) %>% unique
  textOut <- textMatch[sector_id]
   if (max(nchar(textOut)>3)) { facing <- "bending"} else { facing <- "clockwise"}
  if (length(sectorsToHighlight)>0){
  highlight.sector(sectorsToHighlight,
                   track.index = 1, col = colorMapIn[sector_id],
                   text = textOut, text.col = shades::complement(colorMapIn[sector_id]),
                   cex = 0.9, niceFacing = TRUE, facing = facing)
  }
}