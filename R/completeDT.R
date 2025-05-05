#' creates a complete data.table from the cross cases of the specified columns
#'
#' @param DT is the data table you want to complete
#' @param cols are the columns whose values will be crossed to complete the data.table
#' @param defs  a function name to apply to the crossed value
#' @import data.table
#' @return the completed data table
#' @export
#' @examples 
#' # No example as this function is not to be called by the user, only needed for the package. 
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