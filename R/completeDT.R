completeDT <- function(DT, cols, defs = NULL){

  make_vals <- function(col) {
    if(is.factor(col)) factor(levels(col))
    else unique(col)
  }

  mDT <- do.call(CJ, c(lapply(DT[, ..cols], make_vals), list(unique=TRUE)))
  res <- DT[mDT, on=names(mDT)]
  if (length(defs))
    res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)]
  res[]
} 