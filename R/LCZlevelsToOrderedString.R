#' To make LCZ types appear in the proper order, it can be useful ton convert them into strings with added zeros
#' @param LCZlevels a vector containing LCZlevels from 1 to 10 and 101 to 107
#' @importFrom dplyr case_when
#' @export
LCZlevelToOrderedString <- function(LCZlevels) {

  isWfLCZ <- prod(grepl(x = LCZlevels,
                        pattern = "(.*)(_)(.*)"))
  if (isWfLCZ) {
    wfPrefix <- gsub(
      x = LCZlevels,
      pattern = "(.*)(_)(.*)",
      replacement = "\\1\\2")

    LCZlevels <- gsub(
      x = LCZlevels,
      pattern = "(.*)(_)(.*)",
      replacement = "\\3")
  }

  LCZlevels <- case_when(
    nchar(as.character(LCZlevels)) == 1 ~ paste0("00", as.character(LCZlevels)),
    nchar(as.character(LCZlevels)) == 2 ~ paste0("0", as.character(LCZlevels)),
    .default = as.character(LCZlevels)
  )

  if (isWfLCZ) {
    LCZlevels <- paste0(wfPrefix, LCZlevels) }

  return(LCZlevels)
}
