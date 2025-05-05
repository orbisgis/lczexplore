#' RedonBDT
#'
#' LCZ for Redon as produced by GeoClimate on BDTOPO data
#' #'
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
#'
#'
#' @format LCZ for Redon as produced by GeoClimate on OpenStreetMap data
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


#' rasterMD.tif
#'
#'
#'
#' @format A .tif raster used to illustrate the import of several bands from a raster
#' \describe{
#' \item{\code{lcz}}{a band with an lcz value for each pixel}
#' \item{\code{lczFilter}}{a band with a filtered lcz value for each pixel}
#' \item{\code{classProbability}}{a band which provides the probability associated to the lcz value, from 0 to 100}
#' }
#'
#' @docType tif file
#' @name rasterMD.tif
#' @source "https://doi.org/10.3389/fenvs.2021.637455"

