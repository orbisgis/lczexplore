#' Regroups levels of origin and destination of a multiple confusion matrix in long form.
#'
#' @param weightedFluxIn is typically the output of the function createWeightedFlux,
#' and is expected to contain the following columns orig, dest and weightedFlux,
#' whose names are quite self explanatory : orig is the origin LCZ type, dest is the destination LCZ type
#' and weightedFlux is the percentage of area transfered from orig to dest
#' @param ... a set of argument passed as groupName = groupValues
#' where groupName is the name of a resulting group and groupValues a vector of the initial values
#' it will regroup.
#' @return a list containing vectors and groups for a chord diagram
#' @export
groupLCZsuffix <- function(weightedFluxIn, ...) {
  #require(forcats)
  origPref <- sub("(.*)(_)(.*)", "\\1\\2", weightedFluxIn$orig)
  destPref <- sub("(.*)(_)(.*)", "\\1\\2", weightedFluxIn$dest)
  origSuff <- gsub("(.*)(_)(.*)", "\\3", weightedFluxIn$orig)
  destSuff <- gsub("(.*)(_)(.*)", "\\3", weightedFluxIn$dest)


  # get the grouping levels as passed by ..., but without keeping arguments about colours
  args <- list(...) #[names(list(...)) != "groupColors"]
  print(args)
  indSep <- names(args)
  # print(names(args))

  args <- append(list(origSuff), args)
  # temp<-do.call(fct_collapse,args)
  origSuffOut <-
    tryCatch(expr = do.call(fct_collapse, args),
             warning = function(w) {
               message("One of the specified levels to group doesn't exist in the data, if it is a mispelled level of the data,
             this level will be kept as ungrouped ", w)
               return(
                 do.call(fct_collapse, args)
               )
             })

  weightedFluxIn$orig <- paste0(origPref, origSuffOut)

  args <- append(list(destSuff), args)
  # temp<-do.call(fct_collapse,args)
  destSuffOut <-
    tryCatch(expr = do.call(fct_collapse, args),
             warning = function(w) {
               message("One of the specified levels to group doesn't exist in the data, if it is a mispelled level of the data,
             this level will be kept as ungrouped ", w)
               return(
                 do.call(fct_collapse, args)
               )
             })


  weightedFluxIn$dest <- paste0(destPref, destSuffOut)

  return(weightedFluxIn)
}