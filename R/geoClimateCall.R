geoClimateCall<-function(jarFilePath,configFilePath,wf="OSM") {
 command<-paste0(
   "java -jar ", jarFilePath, " -f ", configFilePath, " -w ", wf)
 print(command)
  system(command)
 
}

test<-geoClimateConfigFile(outFile="", wf="osm",outFolder="/tmp",locations="Redon",
                           rsuIndics = c("LCZ","TEB","UTRF"),
                           gridIndics = c("BUILDING_FRACTION",
                                          "BUILDING_HEIGHT",
                                          "WATER_FRACTION",
                                          "VEGETATION_FRACTION",
                                          "ROAD_FRACTION",
                                          "IMPERVIOUS_FRACTION",
                                          "LCZ_PRIMARY",
                                          "LCZ_FRACTION",
                                          "UTRF"))
geoClimateCall(
  jarFilePath=
    "/home/gousseff/Documents/2_CodesSources/GeoClimate/GeoClimateDefaultCaseV2/geoclimate-0.0.2-SNAPSHOT.jar",
  configFilePath="/tmp/Redonosm.json",w="osm")