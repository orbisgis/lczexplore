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
#' @param sfInt allows to produce the confusion matrix directly on the intersected sf object if it was produced before
#' @param plotNow shows the graphical representation of the confusion matrix
#' @param typeLevels by default the levels of lcz incoded from 1 to 10 and 101 to 107.
#' @param drop if TRUE the empty LCZ types are dropped
#' When comparing othe variable, like grouped LCZ, the grouped levels have to be specified.
#' @param ... a set of unspecified arguments, for instance when the produceAnalysis function calls other functions
#'
#' @return returns an object called matConfOut which contains
#' matConfLong, a confusion matrix in a longer form, which can be written in a file by the compareLCZ function
#' and is used by the geom_tile function of the ggplot2 package.
#' matConfPlot is a ggplot2 object showing the confusion matrix. If plot=TRUE, it is also directly plotted
#' marginAreas contains the sums of each LCZ area
#' percAgg is the general agreement between the two sets of LCZ, expressed as a percentage of the total area of the study zone
#' @import sf ggplot2 dplyr cowplot forcats units tidyr RColorBrewer rlang data.table

#' @export
#'
#' @examples
#' matConfRedonBDTOSM<-matConfLCZ(sf1=redonBDT,column1='LCZ_PRIMARY',
#' sf2=redonOSM,column2='LCZ_PRIMARY',plot=TRUE)
matConfLCZ <- function(sf1, column1, sf2, column2, typeLevels = .lczenv$typeLevelsDefault,
                       plotNow = FALSE, wf1 = "Reference", wf2 = "Alternative", sfInt = NULL, drop = FALSE, ...) {
if (is.null(sfInt)){
  # coerce the crs of sf2 to the crs of sf1
  allLevels<-unique(
    c(
      levels(sf1[[column1]]), 
      levels(sf2[[column2]])
    )
  ) 
  allCheck<- allLevels %in% typeLevels
  levelsUnMAtch<-prod(allCheck)==0
  if (levelsUnMAtch) {typeLevels<-unique(c(levels(sf1[[column1]]), levels(sf2[[column2]]))) }
  
  
  if (st_crs(sf1) != st_crs(sf2)) { sf2 <- sf2 %>% st_transform(crs = st_crs(sf1)) }

  if (column1 == column2) {
    column2 <- paste0(column1, ".1")
    sf2[[column2]] <- sf2[[column1]]
    sf2[[column1]] <- NULL
  }

  sf1[[column1]] <- ordered(sf1[[column1]], levels = typeLevels)
  sf1[[column1]][is.na(sf1[[column1]])]<-"Unclassified"
  sf2[[column2]] <- ordered(sf2[[column2]], levels = typeLevels)
  sf2[[column2]][is.na(sf2[[column2]])]<-"Unclassified"

  # creation of the data set with intersected geoms and the values of both lcz class in these geoms
  sfInt <- st_intersection(x = sf1[,column1], y = sf2[,column2])
  # checks if the two LCZ classifications agree
  sfInt$agree <- sfInt[[column1]] == sfInt[[column2]]
} else {
  allLevels<-unique(
    c(
      levels(sfInt[[column1]]), 
      levels(sfInt[[column2]])
    )
  ) 
  allCheck<- allLevels %in% typeLevels
  levelsUnMatch<-prod(allCheck)==0
  if (levelsUnMatch ) {
    typeLevels<-unique(c(sfInt[[column1]], 
    sfInt[[column2]])) } else if (drop) { 
      uniqueLevels<-unique(c(sfInt[[column1]], 
        sfInt[[column2]]))
      typeLevels<- typeLevels [typeLevels %in% uniqueLevels] }
  
  sfInt[[column1]]<-ordered(sfInt[[column1]], levels = typeLevels)
  sfInt[[column2]]<-ordered(sfInt[[column2]], levels = typeLevels)
  sfInt$agree<-sfInt[[column1]] == sfInt[[column2]]
}




  ######################################################
  ###
  ### Confusion matrix, weights being the area of the intersecting geoms
  ###
  ######################################################

  # compute the area of geoms (used later as wieght of agreement between classifcations)
  sfInt <- sfInt %>%
    mutate(area = st_area(geometry)) %>%
    drop_units

  # the writing/appending will happen in compareLCZ function
  sfInt<-setDT(sfInt)
  # marginal areas (grouped by levels of LCZ for each input dataset) rounded at the unit
  # marginal for first LCZ

col1<-eval(substitute(column1), envir = parent.frame())
col2<-eval(substitute(column2), envir = parent.frame())

areaLCZ1<-sfInt[,.(sumArea = sum(area, na.rm = TRUE)), keyby=col1, env = list(col1 = substitute(col1))][
  , .(col1, percArea1 = 100*sumArea / sum(sumArea)), env = list(col1 = substitute(col1))]

  # marginal for second LCZ
areaLCZ2<-sfInt[,.(sumArea = sum(area, na.rm = TRUE)), keyby=col2, env = list(col2 = substitute(col2))][
    , .(col2, percArea2 = 100*sumArea / sum(sumArea)), env = list(col2 = substitute(col2))]

  marginAreas<-merge(areaLCZ1, areaLCZ2, by.x = column1, by.y = column2, all.x = TRUE, all.y = TRUE)
# print(marginAreas)
  allLevelsDT<-data.table(lcz = typeLevels)
  marginAreas<- merge(marginAreas, allLevelsDT, by.x = column1, by.y = "lcz", all.y = TRUE) 
  marginAreas[["percArea1"]][is.na(marginAreas[["percArea1"]])]<-0
  marginAreas[["percArea2"]][is.na(marginAreas[["percArea2"]])]<-0

  names(marginAreas)[1]<-"marginLevels"


  # print(marginAreas)

  percAgg<-sfInt[,.(agree, percArea = area/sum(area)),][agree==TRUE, .(percAgg = round(sum(percArea)*100,2)),]
  # print(percAgg)

col1<-eval(substitute(column1), envir = parent.frame())
col2<-eval(substitute(column2), envir = parent.frame())
byvars<-c(col1,col2)

matConfLong<-sfInt[,.(col1, col2, area = sum(area, na.rm = TRUE)), 
                     keyby = byvars, 
                     env = list(col1 = substitute(col1), col2 = substitute(col2))][  
  ,.(col2, agreePercArea = 100 * area /sum(area)), keyby = col1, env = list(col1 = substitute(col1), col2 = substitute(col2))]

matConfLong<-completeDT(matConfLong, cols = c(col1, col2), defs = c(agreePercArea = 0))
  
matConfLong[[column1]]<-ordered(matConfLong[[column1]], levels = typeLevels)
matConfLong[[column2]]<-ordered(matConfLong[[column2]], levels = typeLevels)

# matConfLong<-merge(matConfLong, toCompleteCases, by.x = c(column1, column2), by.y = c("lcz1","lcz2"), all.y = TRUE)
# matConfLong[["agreePercArea"]][is.na(matConfLong[["agreePercArea"]])]<-0  



  ############
  # Plot

  matConfPlot <- matConfPlot(matConf = matConfLong, marginAreas = marginAreas,
                          column1 = column1, column2 = column2 , agreeColumn = "agreePercArea",
                          wf1 = wf1, wf2 = wf2)


  if (plotNow) { print(matConfPlot) }
matConfOut<-list(matConf=matConfLong, 
                 matConfPlot=matConfPlot,
                 areas=marginAreas, percAgg=percAgg)
return(matConfOut)

}


# doesn't need to be documented. 
completeDT <- function(DT, cols, defs = NULL){

  make_vals <- function(col) {
    if(is.factor(col)) levels(col)
    else unique(col)
  }

  mDT <- do.call(CJ, c(lapply(DT[, ..cols], make_vals), list(unique=TRUE)))
  res <- DT[mDT, on=names(mDT)]
  if (length(defs))
    res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)]
  res[]
} 