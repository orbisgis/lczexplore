#' Takes an sf object with several workflow values,
#' and creates all pairwise confusion matrices, then conatenates them in long form,
#' usable to plot chord diagrams
#'
#' @param intersectSfWide is an sf object, containing the different lcz classification on
#' already intersected geometries in wide format
#' with a column wf for the workflow names, a column lcz_primary for the LCZ types.
#' @param columns a vector containing the names of the LCZ type columns
#' @param wfNamesIn the name of the input compared workflows. They will be the output columns for the LCZ types
#' @param typeLevelsDefaultIn is a named vector of strings containing the LCZ levels. By default inherited from lczexplore
#' @importFrom dplyr mutate select filter
#' @return a dataframe with columns orig, dest and weightedFlux, weighted flux the percentage of area from a given
#' LCZ type of a given workflow (orig) to another LCZ type of another workflow (dest).
#' @examples
#' twoLocsDir<-paste0(
#'  system.file("extdata", package = "lczexplore"),"/multipleWfs")
#' twoLocsSfList<-importMultipleLocsLCZvect(
#'  dirPath = twoLocsDir, workflowNames = c("osm","bdt","wudapt"),
#'  location = c("Arville", "Redon"))
#'
#' twoLocsSfIntersected <- createIntersect(
#'  sfList = twoLocsSfList, columns = rep("lcz_primary", 4),
#'  refCrs=NULL, workflowNames=c("osm", "bdt", "wudapt"), minZeroArea=0.001)
#'
#' twoLocsWeightedFlux<-createWeightedFlux(
#' twoLocsSfIntersected, wfNamesIn = c("osm","bdt","wudapt"))
#'
#' @export
createWeightedFlux <- function(intersectSfWide, columns = NULL, wfNamesIn = NULL,
                               typeLevelsDefaultIn = .lczenv$typeLevelsDefault) {
  if (nrow(intersectSfWide) > 100) { message("This function computes how any LCZ type from any workflow
  breaks into the LCZ types of all other workflows: it can take a while") }
 data.table::setDF(intersectSfWide)
  intersectSfWide <- st_as_sf(intersectSfWide)
  # allowing wfNamesIn or columns to be NULL
  print(columns)
  print(wfNamesIn)
      if (
        (is.null(wfNamesIn) | prod(!is.na(wfNamesIn))) &
          (!is.null(columns) & prod(!is.na(columns)))
      ) {wfNamesIn<-columns}

      if (
        (is.null(columns) | prod(!is.na(columns))) &
          (!is.null(wfNamesIn) & !prod(is.na(wfNamesIn)))
      ) {columns <- wfNamesIn}

  print(columns)

  if (is.null(typeLevelsDefaultIn)){
    typeLevelsDefaultIn <- unique(
      unlist(st_drop_geometry(intersectSfWide)[columns])
    )
    names(typeLevelsDefaultIn) <- typeLevelsDefaultIn
  }

  for (i in 1:(length(columns) - 1)) {
    for (j in (i + 1):length(columns)) {
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareName,
             matConfLCZ(
               sfInt = intersectSfWide[, c(columns[i], columns[j])],
                column1 = columns[i],
                column2 = columns[j],
               typeLevels = unique(names(typeLevelsDefaultIn)),
               plotNow = FALSE, wf1 = wfNamesIn[i], wf2 = wfNamesIn[j])
      )
    }
  }


  for (i in 1:(length(wfNamesIn) - 1)) {
    for (j in (i + 1):length(wfNamesIn)) {
      compareNameToBind <- paste0(wfNamesIn[i], "_", wfNamesIn[j], "_to_bind")
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      #ugly trick to add perc area
      temp1<-get(compareName)
      temp1$matConf$colTemp1<-temp1$matConf[[1]]
      temp2<-temp1$matConf
      temp2$wf_pair <-paste0(
        wfNamesIn[i], "_", temp2[[columns[i]]], "_", wfNamesIn[j], "_", temp2[[columns[j]]])
      temp2<- base::merge(temp2, temp1$areas, by.x = "colTemp1", by.y =  "marginLevels")
      temp2<-temp2[, c("wf_pair","agreePercArea", "percArea1") ]
      assign(compareNameToBind, temp2)
    }
  }

  ############################################
  ##
  ## symetrical
  ##
  ############################################

  for (i in (length(columns):2)) {
    for (j in 1:(i - 1)) {
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareName,
             matConfLCZ(
               sfInt = intersectSfWide[, c(columns[i], columns[j])],
               column1 = columns[i],
               column2 = columns[j],
               typeLevels = unique(names(typeLevelsDefaultIn)),
               plotNow = FALSE, wf1 = wfNamesIn[i], wf2 = wfNamesIn[j]))

    }
  }

  for (i in (length(wfNamesIn):2)) {
    for (j in 1:(i - 1)) {
      compareNameToBind <- paste0(wfNamesIn[i], "_", wfNamesIn[j], "_to_bind")
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      #ugly trick to add perc area
      temp1<-get(compareName)
      temp1$matConf$colTemp1<-temp1$matConf[[1]]
      temp2<-temp1$matConf
      temp2$wf_pair <-paste0(
        wfNamesIn[i], "_", temp2[[columns[i]]], "_", wfNamesIn[j], "_", temp2[[columns[j]]])
      temp2<- base::merge(temp2, temp1$areas, by.x = "colTemp1", by.y =  "marginLevels")
      temp2<-temp2[, c("wf_pair","agreePercArea", "percArea1") ]
      assign(compareNameToBind, temp2)
    }
  }

  matConfNames <- grep("_to_bind", ls(), value = TRUE)

  if (exists("allMAtConfLong")) { rm(allMatConfLong) }

  allMatConfLong <- do.call(rbind, mget(matConfNames))

  allMatConfLong$orig <- gsub(
    x = allMatConfLong$wf_pair,
    pattern = "(.*)(_)(.*)(_)(.*)(_)(.*)",
    replacement = "\\1\\2\\3"
  )

  allMatConfLong$orig <- gsub(
    x = allMatConfLong$orig,
    pattern = "(wudapt)(_)(.*)",
    replacement = "wud\\2\\3"
  )


  allMatConfLong$dest <- gsub(
    x = allMatConfLong$wf_pair,
    pattern = "(.*)(_)(.*)(_)(.*)(_)(.*)",
    replacement = "\\5\\6\\7"
  )

  allMatConfLong$dest <- gsub(
    x = allMatConfLong$dest,
    pattern = "(wudapt)(_)(.*)",
    replacement = "wud\\2\\3"
  )


  allMatConfLong$weightedFlux <- allMatConfLong$agreePercArea * allMatConfLong$percArea1 / 10000


  # gsub(x = (allMatConfLong$wf_pair %>% unique),
  #      pattern = "(.*)(_)(.*)(_)(.*)(_)(.*)",
  #      replacement = "\\1_\\5") %>% unique

  allMatConfLong <- allMatConfLong[, c("orig", "dest", "weightedFlux")]

  return(allMatConfLong)
}