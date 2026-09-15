#' Intersects multiple sf files in which Local Climate Zones are set for polygon geometries
#' @param sfList a list which contains the classifications to compare, as sf objects
#' @param columns a vector which contains, for each sf of sfList,
#' the name of the columns of the classification to compare
#' @param keepAllColumns if TRUE all columns are kept, not only the ones containing LCZ types
#' @param refCrs a number which indicates which sf object from sfList will provide
#' the CRS in which all the sf objects will be projected before comparison
#' By defautl the first sf object CRs is applied to all sf objects.
#' @param workflowNames a vector of strings which contains the names of the workflows used to produce the sf objects
#' @param minZeroArea all geometries smaller than this value are discarded (avoids numeric precision problems)
#' @details The input SfList can contain two levels, the first level being the locations,
#' the second being the workflow. In this case, the output will still be a single sf object,
#' concatenating the workflows and locations as columns
#' @importFrom dplyr mutate
#' @import sf utils
#' @importFrom magrittr "%>%"
#' @return a single sf file with values of LCZ from all the input
#' are assigned to geometries resulting from intersection of all input geometries
#' @export
#' @examples
#' sfList<-loadMultipleSfs(
#' dirPath = paste0(
#' system.file("extdata", package = "lczexplore"),
#' "/multipleWfs/Arville"),
#' workflowNames = c("osm","bdt","wudapt"), inLocation = "Arville")
#' ArvilleIntersect <- createIntersect(
#'  sfList = sfList, columns = rep("lcz_primary", 4),  
#'  workflowNames = c("osm","bdt","wudapt"))
#' # Two Locations
#' sfList2<-loadMultipleLocsSfs(
#'  dirPath = paste0(
#'      system.file("extdata", package = "lczexplore"),
#'      "/multipleWfs"),
#'  workflowNames = c("osm","bdt","wudapt"))
#'
#' TwoLocsIntersect <- createIntersect(
#'  sfList = sfList2, columns = rep("lcz_primary", 3),
#'  workflowNames = c("osm","bdt","wudapt"))
createIntersect <- function(sfList, columns, refCrs = NULL,
                            workflowNames = NULL, minZeroArea = 0.000,
                                keepAllColumns = TRUE) {

  if (is.null(columns) | prod(!is.na(columns) == 0)) {
    message("You didn't specify the name of the LCZ columns, an attempt with lcz_primary is tried")
    columns <- rep("lcz_primary", length(workflowNames))
  }

  if (length(columns) == 1) {
    message("You only specified one column name,
    it is considered as being the same for all sf of the input sf list.")
    columns <- rep(columns, length(workflowNames)) }

  # Igoominous recursive trick in case there are more than one location
  if (collapse::ldepth(sfList) > 1) {

    intersectedList <- lapply(sfList, createIntersect,
                              columns = columns, refCrs = refCrs, workflowNames = workflowNames,
                              minZeroArea = minZeroArea)
    for (i in seq_along(intersectedList)) {
      intersectedList[[i]]$location <- names(sfList)[i]
    }

    sfInt <- do.call(rbind, intersectedList)

    return(sfInt)
  } else {

    # Locations management:keep the value the column will be suppressed and recreated after intersection
    if (!is.null(sfList[[1]][["location"]])) {
      locationRef <- sfList[[1]][["location"]][1]
    } else {
      locationRef <- "No specified Location" }

    # LCZ column name for each sf will be replaced by wf name, easier to read as output.
    if ( is.null(workflowNames) | prod(!is.na(workflowNames)) == 0 | length(workflowNames)!=length(sfList)) {
      message("One or all workflow names are missing")
      stop()
    } else {sfList<-lapply(seq_along(sfList), function(i){
      sfObj<-sfList[[i]]
      if (!is.null(sfObj$location)) { sfObj$location <- NULL }
      names(sfObj)[names(sfObj)==columns[i]]<-workflowNames[i]
      if (!is.null(sfObj$wf)) { sfObj$wf <- NULL }
      return(sfObj)
    })}



    # CRS management

    if (is.null(refCrs)) { refCrs <- st_crs(sfList[[1]]) } else { refCrs <- st_crs(sfList[[refCrs]]) }
    sfListCRSed <- lapply(seq_along(sfList), function(i) {
      # if(!keepAllColumns){sfObj <- sfList[[i]][, workflowNames[i], drop = FALSE]}
      # else{
        sfObj<-sfList[[i]]
      # }
      if (st_crs(sfObj) != refCrs) { sfObj <- st_transform(sfObj, crs = refCrs) }
      return(sfObj)
    })

    # Intersecting

    sfInt <- Reduce(st_intersection, sfListCRSed)
    # sfInt$location<-locationRef
    print(summary(sfInt))
 }

  # Areas management
    sfInt <- dplyr::mutate(sfInt, area = units::drop_units(st_area(sfInt$geometry)),
                           .before = geometry)
    nbDiscardedUnits<-nrow(sfInt[sfInt$area < minZeroArea,])
    if (nbDiscardedUnits >0){message(paste0(nbDiscardedUnits, " spatial units had an area inferior to the
    specified minimum area (", minZeroArea,") and were discarded"))}
    sfInt <- sfInt[sfInt$area > minZeroArea,]
    sfInt <- dplyr::mutate(sfInt, location = locationRef, .before = geometry)

  return(sfInt)
}

