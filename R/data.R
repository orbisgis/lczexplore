#' RedonBDT
#'
#' Full baby name data provided by the SSA. This includes all names with at
#' least 5 uses.
#'
#' @format An sf object with 3 variables and a geometry :
#' \describe{
#' \item{\code{ID_RSU}}{index identifying the Reference spatial unit}
#' \item{\code{LCZ_PRIMARY}}{Local Climate Zone encoded according to Stewad and Oke approach, 1 to 10 then 101 to 107}
#' \item{\code{LCZ_UNIQUENESS_VALUE}}{ indicates how the LCZ_PRIMARY value was likely to be the only one suitable for this RSU, similar to a confidence value}
#' \item{\code{geometry}}{containes the geometry of the RSU for all the spatial operations allowed by the sf class}
#' }
#'
#' @docType data
#' @name redonBDT
#' @source "https://github.com/orbisgis/geoclimate"
"redonBDT"

#' RedonOSM
#'
#' Full baby name data provided by the SSA. This includes all names with at
#' least 5 uses.
#'
#' @format An sf object with 3 variables and a geometry :
#' \describe{
#' \item{\code{ID_RSU}}{index identifying the Reference spatial unit}
#' \item{\code{LCZ_PRIMARY}}{Local Climate Zone encoded according to Stewad and Oke approach, 1 to 10 then 101 to 107}
#' \item{\code{LCZ_UNIQUENESS_VALUE}}{ indicates how the LCZ_PRIMARY value was likely to be the only one suitable for this RSU, similar to a confidence value}
#' \item{\code{geometry}}{containes the geometry of the RSU for all the spatial operations allowed by the sf class}
#' }
#'
#' @docType data
#' @name redonOSM
#' @source "https://github.com/orbisgis/geoclimate"
"redonOSM"
