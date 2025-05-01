#' Manages the levels and the colors of the LCZ columns
#'
#' @param sf is the input sf file
#' @param column is the column that contains the data
#' @param drop is set to TRUE if one wants to drop unused levels, in case column is a factor with unused levels
#' @param useStandCol is set to TRUE if one wants recognized standard levels to be associated with standard colors. Default is FALSE
#' @param ... other parameters specified, expected a vector of levels and a vector of colors
#' whose name must begin with colors. Other cases are handled to enhance usability.
#' @import dplyr sf
#' @importFrom grDevices palette.colors
#'
#' @return output is a list containing levelsColors, a named vector, which names are the levels
#' present in the data and which values are the associated colors,
#' and case, a string specifying what case was encountered when producing the levels and colors.
#' @export
#'
#' @examples
#' # levCol2 is not to be used directly by user. It deals with levels and colors provided by the user
levCol <- function(sf, column, drop = FALSE, useStandCol = FALSE, ...) {
  # Getting ALL the arguments in (...) and computing the values needed for conditions statements
  args <- list(...)

  # Stop if there are more than 37 arguments.   
  if (length(args) >= 37) { stop("This function can not deal with more than 36 arguments.
  You can use the function groupLCZ to group some levels.") }

  # get the natural levels from the data : uniqueData
  uniqueData <- unique(sf[[column]])
  # Attention, unique outputs a list of length 1

  # Sometimes, the input data can be a factor, with levels not present in the actual data.
  # One may want to drop these levels

  if (drop == TRUE) { uniqueData <- droplevels(uniqueData) }

  uniqueData <- levels(uniqueData) %>%
    as.character() %>%
    as.vector()

  # Stop if there are more than 36 levels : really impossible to read such a map
  # or confusion matrix

  if (length(uniqueData) > 36) { stop(
    "This package is not suited for qualitative variables/classifications with more than 36 levels/types.
  You can use the function groupLCZ to group some levels.") }

  # Getting the levels and separating level vector(s) and colors vector if present.   

  argNames <- names(args)
  indCol <- grep(x = argNames, pattern = "colors")
  if (length(indCol) != 0) {
    if (length(indCol) > 1) {
      stop(
        "Only one argument can start with colors, and it must contain the colors,
      please rename your arguments and retry.") } else {
      argCol <- args[indCol][[1]]
      argLev <- args[-indCol]
      # In case the color vector is made of empty strings
      if (prod(argCol == "") == 1) {
        args <- args[-indCol]
        argCol <- NULL
        # In case the vector of levels is simultaneously made of empty strings
        if (prod(unlist(argLev) == "") == 1) { args <- NULL }
      }
    } } else
  {
    argCol <- NULL
    argLev <- args
  }

  if (length(argLev) > 36) { stop("This package is not suited for classification with more than 36 levels or types.
  You can use the function groupLCZ to group some levels.") }

  #############################################################################################
  ## Simplest Case no arguments passed : levels and colors deduced from the Data
  #############################################################################################

  if (length(args) == 0 ||
    (is.null(args)) ||
    (prod(unlist(args) == "") == 1)) {
    case <- "1: No level vector and no color vector, less than 36 levels,
        levels will be deduced from the data
        and colors will be chosen from a standard palette."

    typeLevels <- standLevCol(levels = uniqueData,
                              colors = palette.colors(n = length(uniqueData), palette = "Polychrome 36"),
                              useStandCol = useStandCol)
  }

  #############################################################################################
  ## Case when only colors are passed
  #############################################################################################
  if (length(args) == 1 &&
    length(indCol) == 1 &&
    prod(argCol != "") == 1)
  {
    if (length(argCol) == length(uniqueData)) {

      case <- "2: No level vector, but a color vector which size covers the number of levels in the data."
      typeLevels <- argCol
      names(typeLevels) <- uniqueData
    }
    else if (length(argCol) < length(uniqueData)) {
      case <- "3: No levels but a color vector which size does not cover the number of levels in the data, missing colors will be picked from a standard palette. "
      lengthDiff <- length(uniqueData) - length(argCol)
      typeLevels <- c(argCol, palette.colors(n = lengthDiff, palette = "Polychrome 36"))
      names(typeLevels) <- uniqueData
    }
    else if (length(argCol) > length(uniqueData)) {
      case <- "3.1 : No levels but a color vector which size is greater than the number of levels in the data, unused colors were dropped. "
      typeLevels <- argCol[1:length(uniqueData)]
      names(typeLevels) <- uniqueData
    }
  }

  #############################################################################################
  ## Case when only levels are passed in a single vector (elements of the vector are not named)
  ############################################################################################# 
  if (is.null(argCol) &&
    length(args) == 1 &&
    is.null(names(argLev[[1]]))) {

    ########### Case where the levels cover the levels of unique Data
    if (prod(uniqueData %in% argLev[[1]]) == 1) {

      case <- "7: No color vector but a level vector whose names cover the levels in the data (even if some levels may not be present in the data)"
      typeLevels <- palette.colors(n = length(argLev[[1]]), palette = "Polychrome 36")
      names(typeLevels) <- argLev[[1]]
    } else if (prod(uniqueData %in% argLev[[1]]) == 0) {
      ########### Case where the levels do not cover the levels of unique Data
      case <- "8: No color vector but a level vector whose names don't cover the levels in the data.
         Missing levels will be deduced from the data and colors will be chosen from a standard palette."
      temp <- unique(c(uniqueData, argLev[[1]]))
      typeLevels <- palette.colors(n = length(temp), palette = "Polychrome 36")
      names(typeLevels) <- temp
    }
  }

  #############################################################################################
  ## Case when only one vector is passed, but the elements of the vector are named. 
  ## Name is expected to be a level, value is expected to be a color.
  #############################################################################################   
  if (is.null(argCol) &&
    length(args) == 1 &&
    !is.null(names(argLev[[1]]))) {
    ########### Case where the level names do not cover the levels of unique Data
    if (prod(uniqueData %in% names(argLev[[1]])) == 1) {
      case <- "4: & 5: A single vector was provided, whose names cover the levels in the data (values are expected to be colors)."
      typeLevels <- argLev[[1]]
    } else if (prod(uniqueData %in% names(argLev[[1]])) == 0) {
      case <- "6: A single vector was provided, whose names don't cover the levels in the data, 
      missing levels were assigned random colors."

      indMiss <- !uniqueData %in% names(argLev[[1]])
      nMiss <- sum(indMiss)
      miss <- palette.colors(n = nMiss, palette = "Polychrome 36")
      names(miss) <- uniqueData[indMiss]
      typeLevels <- c(argLev[[1]], miss)


    }
  }

  #############################################################################################
  ## Case when two vectors are passed. One is expected to be levels, the other to be colors
  #############################################################################################   

  ########### Case where no vector of color is passed, two vectors of levels 

  if (length(args) == 2 &&
    prod(unlist(args) == "") == 0 &&
    length(indCol) == 0) {

    case <- "13: No color vector is specified and there seems to be two ambiguous level vectors,
        they will pasted and fed to the function again, and reduced to the following case : "
    recall <- levCol(sf = sf, column = column, drop = drop, levels = c(argLev[[1]], argLev[[2]]))
    typeLevels <- recall$levelsColors
    case <- c(case, recall$case)
  }

  ########### Case where one vector of color is passed, one vector of levels 

  if (length(args) == 2 &&
    !(prod(unlist(args) == "") == 1) &&
    length(indCol == 1)) {

    ########### Case where the vectors are of the same size or if the levels vector 
    # is shorter than the color vector
    if (length(argLev[[1]]) <= length(argCol)) {

      typeLevels <- argCol
      names(typeLevels) <- argLev[[1]]
      recall <- levCol(sf = sf, column = column, drop = drop, levels = typeLevels)
      typeLevels <- recall$levelsColors
      typeLevels <- typeLevels[!is.na(names(typeLevels))]
      case <- paste(
        "One vector of levels, one vector of colors, either the same length (case 9: and 10:), or colors longer (case 12:, unused colors were dropped),
         reduced to ",
        recall$case)
    } else  if (length(argLev[[1]]) > length(argCol)) {

      ######## case when vectors not of the same size and more levels than colors
      complement <- length(argLev[[1]]) - length(argCol)
      typeLevels <- c(argCol, palette.colors(n = complement, palette = "Polychrome 36"))
      names(typeLevels) <- argLev[[1]]
      recall <- levCol(sf = sf, column = column, drop = drop, levels = typeLevels)
      typeLevels <- recall$levelsColors
      case <- paste("Case one vector of levels, one of colors, the vector of colors being shorter, some colors were picked, case 11:, reduced to ",
                    recall$case, "your may want to check your vector of colors")

    }


  }

  #############################################################################################
  ## Case when more than two vectors are passed. 
  ## Hopefully an argument for each level and zero or one vector of colors
  #############################################################################################

  if (length(args) > 2 && length(indCol) == 1) {
    ######## Case when several vectors of levels and a vector of colors
    LCZlevels <- unique(c(names(argLev), uniqueData))

    recall <- levCol(sf = sf, column = column, drop = drop, levels = LCZlevels, colors = argCol)


    case <- paste("Case 14: with at least a color or 15:, or 17: or 18:, then reduced to ",
                  recall$case)
    typeLevels <- recall$levelsColors
  }

  if (length(args) > 2 && length(indCol) == 0) {
    ######## Case when several vectors of levels and a level of colors
    LCZlevels <- names(argLev)

    recall <- levCol(sf = sf, column = column, drop = drop, levels = LCZlevels)
    case <- paste("Case 16: then reduced to ",
                  recall$case)
    typeLevels <- recall$levelsColors

  }


  #############################################################################################
  ## Check if colors value are recognized, if not, assign a color
  #############################################################################################
  if (prod(areColors(typeLevels)) != 1) {
    case <- paste(
      case,
      " Some of the specified colors are unknown to R and were replaced by colors picked from a Polychrome Palette")
    colFalse <- !areColors(typeLevels)
    typeLevels[colFalse] <- palette.colors(
      n = sum(as.numeric(colFalse)), palette = "Polychrome 36")
  }

  #############################################################################################
  ## If drop=TRUE check if some levels were specified and not present in the data, then drop them
  #############################################################################################


  if (drop == TRUE) {
    indKeep <- names(typeLevels) %in% uniqueData
    if (prod(names(typeLevels) %in% uniqueData) == 0) {
      case <- paste(
        case,
        "Drop=TRUE, some of the specified levels were not found in the data and were dropped")
      typeLevels <- typeLevels[indKeep]
    }
  }
  output <- list(levelsColors = typeLevels, case = case)

}