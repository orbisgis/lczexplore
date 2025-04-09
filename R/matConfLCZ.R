#' Produces a confusion matrix between two sets of lcz on the same area
#' (geoms do not need to be the same)
#'
#' @details Most of the time this function will not be called directly
#' by the user but by the compareLCZ function
#' @param sf1 is the sf dataset containing the first lcz classification
#' @param column1 is the column of the first data set containing the lcz to be compared
#' @param sf2 is the sf dataset containing the second lcz classification
#' @param wf1 the name of the workflow used to produce data from sf1
#' @param wf2 the name of the workflow used to produce data from sf2
#' @param column2 is the column of the second data set containing the lcz to be compared
#' @param typeLevels by default the levels of lcz incoded from 1 to 10 and 101 to 107.
#' When comparing othe variable, like grouped LCZ, the grouped levels have to be specified.
#' @param plot if TRUE the plot of the matrix
#' @param ... a set of unspecified arguments, for instance when the produceAnalysis function calls other functions
#'
#' @return returns an object called matConfOut which contains
#' matConfLong, a confusion matrix in a longer form, which can be written in a file by the compareLCZ function
#' and is used by the geom_tile function of the ggplot2 package.
#' matConfPlot is a ggplot2 object showing the confusion matrix. If plot=TRUE, it is also directly plotted
#' marginAreas contains the sums of each LCZ area
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' @import sf ggplot2 dplyr cowplot forcats units tidyr RColorBrewer rlang

#' @export
#'
#' @examples
#' matConfRedonBDTOSM<-matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
#' sf2=redonOSM,column2='LCZ_PRIMARY',plot=TRUE)
matConfLCZ <- function(sf1, column1, sf2, column2, typeLevels = unique(names(.lczenv$typeLevelsDefault)),
                       plotNow = FALSE, wf1 = "Reference", wf2 = "Alternative", ...) {
  # coerce the crs of sf2 to the crs of sf1
  
  levelsCheck<-prod(unique(c(levels(sf1[[column1]]), levels(sf2[[column2]]))) %in% typeLevels)==0
  if (levelsCheck) {typeLevels<-unique(c(levels(sf1[[column1]]), levels(sf2[[column2]]))) }
  
  
  if (st_crs(sf1) != st_crs(sf2)) { sf2 <- sf2 %>% st_transform(crs = st_crs(sf1)) }

  if (column1 == column2) {
    column2 <- paste0(column1, ".1")
    sf2 <- sf2 %>% mutate(!!column2 := subset(sf2, select = column1, drop = T))
    sf2 <- sf2 %>% mutate(!!column1 := NULL)
  }

  sf1 <- sf1 %>%
    mutate(!!column1 := factor(subset(sf1, select = column1, drop = T), levels = typeLevels)) %>%
    drop_na(column1)
  sf2 <- sf2 %>%
    mutate(!!column2 := factor(subset(sf2, select = column2, drop = T), levels = typeLevels)) %>%
    drop_na(column2)
  print(head(sf1))
  print(head(sf1))  
  # creation of the data set with intersected geoms and the values of both lcz class in these geoms
  echInt <- st_intersection(x = sf1[,column1], y = sf2[,column2])
  # checks if the two LCZ classifications agree
  echInt$agree <- echInt[[column1]] == echInt[[column2]]



  ######################################################
  ###
  ### Confusion matrix, weights being the area of the intersecting geoms
  ###
  ######################################################

  # compute the area of geoms (used later as wieght of agreement between classifcations)
  echInt <- echInt %>%
    mutate(area = st_area(geometry)) %>%
    drop_units

  # the writing/appending will happen in compareLCZ function

  # marginal areas (grouped by levels of LCZ for each input dataset) rounded at the unit
  # marginal for first LCZ
  areaLCZ1 <- echInt %>%
    st_drop_geometry %>%
    group_by_at(.vars = column1) %>%
    summarize(area = sum(area, na.rm = F)) %>%
    drop_units %>%
    ungroup()
  areaLCZ1$area <- round(areaLCZ1$area / sum(areaLCZ1$area, na.rm = F) * 100, digits = 2)

  # marginal for second LCZ
  areaLCZ2 <- echInt %>%
    st_drop_geometry %>%
    group_by_at(.vars = column2) %>%
    summarize(area = sum(area, na.rm = F)) %>%
    drop_units %>%
    ungroup()
  areaLCZ2$area <- round(areaLCZ2$area / sum(areaLCZ2$area, na.rm = F) * 100, digits = 2)

  # Problem : some of the input files do not exhibit all possible LCZ values :
  # pasting the area to the labels would return an error
  # Here is an ugly solution to overcome this (and see later to include the potentially missing combination of levels)

  marginAreas <- data.frame(typeLevels = typeLevels, area1 = 0, area2 = 0)


  for (i in areaLCZ1[[column1]]) {
    if (!is.na(i)) {
      marginAreas[marginAreas$typeLevels == i, "area1"] <- areaLCZ1[areaLCZ1[[column1]] == i, 'area']
    }
  }

  for (i in areaLCZ2[[column2]]) {
    if (!is.na(i)) {
      marginAreas[marginAreas$typeLevels == i, "area2"] <- areaLCZ2[areaLCZ2[[column2]] == i, 'area']
    }
  }

  # Get the general agreement between both input files
  percAgg <- (((echInt %>%
    st_drop_geometry() %>%
    filter(agree == T) %>%
    select(area) %>%
    sum) /
    (echInt %>%
      st_drop_geometry() %>%
      select(area) %>%
      sum)) * 100) %>% round(digits = 2)

  # the way to "feed" group_by is through .dots, to be checked, as it seems to be deprecated :
  # fixed with group_by_at


  matConf <- echInt %>%
    st_drop_geometry %>%
    group_by_at(.vars = c(column1, column2)) %>%
    summarize(area = sum(area)) %>%
    drop_units %>%
    ungroup %>%
    ungroup

  # print("matConf")
  # print(head(matConf))

  # Wider format to compute area based confusion

  # This "confusion matrix" contains the area of intersected geom who have the i LCZ
  # for the first dataset and the j LCZ for the second dataset
  matConfLarge <- pivot_wider(data = matConf, names_from = column2, values_from = area)
  readable <- matConfLarge[, -1] / rowSums(matConfLarge[, -1], na.rm = T) * 100
  matConfLarge <- cbind(matConfLarge[, 1], round(x = readable, digits = 2))

  # print("matConfLarge")
  # print(head(matConfLarge))


  # Longer format to feet the geom_tile aes in ggplot2

  matConfLong <- pivot_longer(matConfLarge, cols = -1, names_to = column2)
  # print("matConfLong avant reorder factor")
  names(matConfLong) <- c(column1, column2, "agree")

  # Reordering of factors (as they were sorted in the order of showing in the file)

  matConfLong <- matConfLong %>% mutate(across(where(is.character), as_factor))
  matConfLong <- matConfLong %>%
    mutate(!!column1 := ordered(subset(matConfLong, select = column1, drop = T), levels = typeLevels))
  matConfLong <- matConfLong %>%
    mutate(!!column2 := ordered(subset(matConfLong, select = column2, drop = T), levels = typeLevels))


  ##############################################################################################################
  # Some values of LCZ may not appear in all datasets, we want to force them to appear to make all heat map easy to compare
  #################################################################################################################

  complement <- cbind(crossing(typeLevels, typeLevels),
                      data.frame(
                        indice = apply(
                          crossing(typeLevels, typeLevels), 1, paste, collapse = "."),
                        area = 0
                      )
  )

  names(complement) <- c("LCZ1", "LCZ2", "uselessIndex", "tempArea")
  # print("complement")
  # complement %>% head %>% print

  completed <- merge(x = matConfLong, y = complement, 
                     by.x = c(column1, column2), by.y = c("LCZ1", "LCZ2"), 
                     all = T)
  completed$agree[is.na(completed$agree)] <- completed$tempArea[is.na(completed$agree)]


  matConfLong <- completed[, c(column1, column2, "agree")]
  matConfLong <- matConfLong %>%
    mutate(!!column1 := addNA(subset(matConfLong, select = column1, drop = T), ifany = T),
           !!column2 := addNA(subset(matConfLong, select = column2, drop = T), ifany = T),) %>%
    mutate(!!column1 := factor(subset(matConfLong, select = column1, drop = T), levels = typeLevels),
           !!column2 := factor(subset(matConfLong, select = column2, drop = T), levels = typeLevels))


  matConfLong <- matConfLong %>% arrange(column1, column2)
  #Include all the lcz levels, even if they are not present in the datasets

  # print("matConfLongaprès reorder factor")
  # print(matConfLong)
  marginAreas <- data.frame(marginLevels = factor(typeLevels), percArea1 = marginAreas$area1, percArea2 = marginAreas$area2)
  ############
  # Plot

  matConfPlot <- matConfPlot(matConf = matConfLong, marginAreas = marginAreas,
                          column1 = column1, column2 = column2 , agreeColumn = "agree",
                          wf1 = wf1, wf2 = wf2)
 # 
 #  coordRef <- length(typeLevels) + 1
 #  print(coordRef)
 #  matConfPlot <- ggplot_build(
 #    ggplot() +
 #      geom_tile(data = matConfLong, aes(x = get(column1), y = get(column2), fill = agree), color = "white", lwd = 1.2, linetype = 1) +
 #      labs(x = wf1, y = wf2) +
 #      scale_fill_gradient2(low = "grey97", mid = "cyan", high = "blue",
 #                           midpoint = 50, limit = c(0, 100), space = "Lab",
 #                           name = "% area") +
 #      geom_text(data=matConfLong[matConfLong$agree!=0,], aes(x = .data[[column1]], y = .data[[column2]], label=round(agree,digits=0)),
 #                color="black") + 
 #        coord_fixed()+
 #      theme(axis.text.x = element_text(angle = 70, hjust = 1),
 #            panel.background = element_rect(fill = "grey97")) +
 #      geom_tile(marginAreas, mapping = aes(x = marginLevels, y = coordRef, fill = percArea1, height = 0.8, width = 0.8)) +
 #      theme(panel.background = element_rect(fill = "grey97")) +
 #      geom_text(data = marginAreas[round(marginAreas$percArea1, 0) != 0,], 
 #                aes(x = marginLevels, y = coordRef, label = round(percArea1, digits = 0)),
 #                color = "gray37") +
 #      coord_fixed() +
 #      geom_tile(marginAreas, mapping = aes(x = coordRef, y = marginLevels, fill = percArea2, height = 0.8, width = 0.8)) +
 #      geom_text(data = marginAreas[round(marginAreas$percArea2, 0) != 0,], 
 #                aes(x = coordRef, y = marginLevels, label = round(percArea2, digits = 0)),
 #                color = "gray37") +
 #      coord_fixed() +
 #      ggtitle(paste0("Repartition of ", wf1, " classes into ", wf2, " classes"))
 #  )

  if (plotNow) { print(matConfPlot) }

  matConfOut <- list(matConf = matConfLong, matConfPlot = matConfPlot, marginAreas = marginAreas, percAgg = percAgg)
  return(matConfOut)

}