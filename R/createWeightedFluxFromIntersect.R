#' for several workflows, from the intersected sf object, creates all pairwise confusion matrices and conatenates
#' them in long form usable to plot chord diagrams
#'
#' @param sfIn is an sf object, containing the intersected different lcz classification
#' @param columns the names of the columns containing the LCZ types
#' @param wfNamesIn the name of the workflows
#' @param typeLevelsDefaultIn is a named vector of strings containing the LCZ levels. By default inherited from lczexplore
#' @importFrom dplyr mutate select filter
#' @return a dataframe with columns orig, dest and weightedFlux, weighted flux the percentage of area from a given
#' LCZ type of a given workflow (orig) to another LCZ type of another workflow (dest).
#' @export
createWeightedFluxFromIntersect <- function(sfIn, wfNamesIn, columns = NULL, typeLevelsDefaultIn = .lczenv$typeLevelsDefault) {
  if (nrow(sfIn) > 100) { message("This function computes all the pairwise confusion matrices and can take some time") }
  for (i in 1:(length(wfNamesIn) - 1)) {
    for (j in (i + 1):length(wfNamesIn)) {
      if (is.null(columns)) { columns <- rep("lcz_primary", length(wfNamesIn)) }
      sf1 <- importLCZvect(sfIn = dplyr::filter(sfIn, wf == wfNamesIn[i]), column = columns[i])
      sf2 <- importLCZvect(sfIn = dplyr::filter(sfIn, wf == wfNamesIn[j]), column = columns[j])
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareName,
             matConfLCZ(
               sf1 = sf1, column1 = columns[i],
               sf2 = sf2, column2 = columns[j],
               typeLevels = unique(names(typeLevelsDefaultIn)),
               plotNow = FALSE, wf1 = wfNamesIn[i], wf2 = wfNamesIn[j])
      )
    }
  }


  for (i in 1:(length(wfNamesIn) - 1)) {
    for (j in (i + 1):length(wfNamesIn)) {
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      compareNameToBind <- paste0(wfNamesIn[i], "_", wfNamesIn[j], "_to_bind")
      assign(compareNameToBind,
             get(compareName)$matConf %>%
               dplyr::mutate(wf_pair = paste0(
                 wfNamesIn[i], "_", lcz_primary, "_",
                 wfNamesIn[j], "_", lcz_primary.1)) %>%
               dplyr::select(.data$wf_pair, .data$agreePercArea) %>%
               mutate(percArea = rep(
                 get(compareName)$areas$percArea1,
                 each = length(get(compareName)$areas$percArea2)))
      )
    }
  }

  ############################################
  ##
  ## symetrical
  ##
  ############################################

  for (i in (length(wfNamesIn):2)) {
    for (j in 1:(i - 1)) {
      # for (i in 1 : 2) {
      #   for (j in i + 1 : 3){
      sf1 <- importLCZvect(sfIn = filter(sfIn, wf == wfNamesIn[i]), column = "lcz_primary")
      sf2 <- importLCZvect(sfIn = filter(sfIn, wf == wfNamesIn[j]), column = "lcz_primary")
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareName,
             matConfLCZ(
               sf1 = sf1, column1 = "lcz_primary",
               sf2 = sf2, column2 = "lcz_primary",
               typeLevels = unique(names(typeLevelsDefaultIn)),
               plotNow = FALSE, wf1 = wfNamesIn[i], wf2 = wfNamesIn[j])
      )
    }
  }

  for (i in (length(wfNamesIn):2)) {
    for (j in 1:(i - 1)) {
      compareNameToBind <- paste0(wfNamesIn[i], "_", wfNamesIn[j], "_to_bind")
      compareName <- paste0(wfNamesIn[i], "_", wfNamesIn[j])
      assign(compareNameToBind,
             get(compareName)$matConf %>%
               dplyr::mutate(wf_pair = paste0(wfNamesIn[i], "_", lcz_primary, "_", wfNamesIn[j], "_", lcz_primary.1)) %>%
               dplyr::select(.data$wf_pair, .data$agreePercArea) %>%
               mutate(percArea = rep(get(compareName)$areas$percArea1, each = length(get(compareName)$areas$percArea2)))
      )
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


  allMatConfLong$weightedFlux <- allMatConfLong$agreePercArea * allMatConfLong$percArea / 10000


  # gsub(x = (allMatConfLong$wf_pair %>% unique),
  #      pattern = "(.*)(_)(.*)(_)(.*)(_)(.*)",
  #      replacement = "\\1_\\5") %>% unique

  allMatConfLong$dest <- LCZlevelToOrderedString(allMatConfLong$dest)
  allMatConfLong$orig <- LCZlevelToOrderedString(allMatConfLong$orig)

  allMatConfLong <- allMatConfLong[, c("orig", "dest", "weightedFlux")]

  return(allMatConfLong)
}