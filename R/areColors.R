#' Checks if a string or a vector of strings define colors in R
#'
#' @param x is the input string
#' @return a vector of booleans indicting if the elements of x define a color in R (TRUE) or don't (FALSE)
#' @export
#'
#' @examples 
#' areColors(c(NA, "black", "blackk", "1", "#00", "#000000"))
areColors <- function(x) {
  numInd<-grepl('^-?[0-9.]+$', x)
  if(sum(numInd)>0){message("please do not use numeric to specify colors, replace with a string with a color name,
  for instance, replace 1 with the string black.")}
  x[numInd]<-"number not seen as color"
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(x) FALSE)
  })
}
