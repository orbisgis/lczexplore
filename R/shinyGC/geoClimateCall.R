#' Calls GeoClimate and feeds it a configuration file by building a command an using system (only tested on linux)
#' @param jarFilePath tells where the geoclimate jar file is, default points to the embedded jar file,
#' i.e. the latest snapshot version when the package was built.
#' Versions can be downloaded from https://github.com/orbisgis/geoclimate/wiki/Download
#' @param configFilePath points to the configuration JSON file for GeoClimate, typically a file created with the
#' geoClimateConfigFile function
#' @param wf is the workflow to use with GeoClimate, the default is OSM for OpenStreetMap.
#' The other possible value is "BDTOPO_V2". Other values will be added (e.g. for BDTOPO_V3) when tested.
#' @return returns nothing but files will be created by GeoClimate in the folder specified in the
#' JSON configuration file.
#' @export
#' @examples
#' test<-geoClimateConfigFile(outFile="", wf="osm",outFolder="/tmp",locations="Redon",
#' rsuIndics = c("LCZ","TEB","UTRF"),
#' gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT","WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION",
#' "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF"))
#' geoClimateCall(
#' jarFilePath= "/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoClimateDefaultCaseV2/geoclimate-0.0.2-SNAPSHOT.jar",
#' configFilePath="/tmp/Redonosm.json",w="osm")
#' test
geoClimateCall<-function(jarFilePath, configFilePath, wf="OSM") {
  command<-paste0(
    "java -jar ", jarFilePath, " -f ", configFilePath, " -w ", toupper(wf))
  print(command)
  system(command)
}

# test<-geoClimateConfigFile(outFile="", wf="osm",outFolder="/tmp",locations="Allaire",
#                            rsuIndics = c("LCZ","TEB","UTRF"),
#                            gridIndics = c("BUILDING_FRACTION",
#                                           "BUILDING_HEIGHT",
#                                           "WATER_FRACTION",
#                                           "VEGETATION_FRACTION",
#                                           "ROAD_FRACTION",
#                                           "IMPERVIOUS_FRACTION",
#                                           "LCZ_PRIMARY",
#                                           "LCZ_FRACTION",
#                                           "UTRF"))
#
#  geoClimateCall(
#  jarFilePath= "/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoClimateDefaultCaseV2/geoclimate-0.0.2-SNAPSHOT.jar",
#  configFilePath="/tmp/Redonosm.json", wf="osm")
#  test
