#' Check if a string or a vector of string define colors in R
#'
#' @param x is the input string
#' @return a vector of booleans indicting if the elements of x define a color in R (TRUE) or don't (FALSE)
#' @export
#'
#' @examples areColors(c(NA, "black", "blackk", "1", "#00", "#000000"))
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(x) FALSE)
  })
}
