#' Builds a JSON configuration file for GeoClimate workflow.
#' @param locations is either the town or the coordinates of the bounding box of the area on which GeoClimate will run.
#' If a town name, it will be fed to the Nominoe API, through the overpass API of OpenStreetMap.
#' @param wf is the workflow used by GeoClimate. For now, only OSM" is available, for OpenStreetMap, but "BDT" for
#' BDTOPO of IGN should be added when an online database is available.
#' @param date is the date of the data we extract from OpenStreetMap. The format is "yyyy:mm:ddThh:mm:ssZ"
#' (for BDTOPO the version depends from the input data folder)
#' @param BDTinFolder the path of the folder where are the BDTOPO needed by geoclimate
#' @param outFolder indicates where the results of GeoClimate will be put
#' @param rsuIndics is a vector with the indicators one wants to compute at the RSU scale. The default is c("LCZ","TEB","UTRF"),
#' @param svfSimplified uses the simplified method to calculate skyview factor, default = TRUE
#' @param estimatedHeight uses an algorithm to estimate the missing building height, default = TRUE
#' @param grid_x_size is the x size for the grid if some grid indicators are to be computed, default=100
#' @param grid_y_size is the x size for the grid if some grid indicators are to be computed, default=100
#' @param rowCol if grid_x_size and grid_y_size are not set, one cal set the number of rows and cols throug rowCol,
#' but the recommended and default is FALSE
#' @param outputType is the format of GeoClimate outputs, default="geojson"
#' @param gridIndics is a vector containing the indicators to compute at the grid scale. Default is
#' c("BUILDING_FRACTION",
#' "BUILDING_HEIGHT",
#' "WATER_FRACTION",
#' "VEGETATION_FRACTION",
#' "ROAD_FRACTION",
#' "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF")
#' @param outConfigDir is the folder were the resulting JSON file will be put,
#' the folder where GeoClimate will read it from
#' (different from out outFolder, where GeoClimates will put its geoJSON ouputs), default is "/tmp"
#' @param outConfigFile is the name of your configuration file,
#' if and empty string, a name will be created from location
#' and workflow parameters.
#' @param forceSRID some BD TOPO input file may not have an srid, this forces srid to be 2154
#' @param writeNow if TRUE, the resulting config file is written using the outConfigDir and outConfigFile parameters
#' @returns Exports a json configuration file to be read by GeoClimate software
#' @details GeoClimate is not included in this package. The json configuration call can be fed to
#' GeoClimate with the geoClimateCall function, provided that user has downloaded a version of the software
#' from the official repository https://github.com/orbisgis/geoclimate
#' @importFrom jsonlite unbox toJSON
#' @return returns a JSON configuration file to be fed to GeoClimate
#' @export
#' @examples
#' # not run as they write files
#' # test <- geoClimateConfigFile(
#' #     outConfigFile = "",
#' #     wf = "OSM", outFolder = "", locations = "Redon",
#' #     rsuIndics = c("LCZ", "TEB", "UTRF"),
#' #     gridIndics = c(
#' #         "BUILDING_FRACTION", "BUILDING_HEIGHT", "WATER_FRACTION",
#' #         "VEGETATION_FRACTION", "ROAD_FRACTION",
#' #         "IMPERVIOUS_FRACTION", "LCZ_PRIMARY", "LCZ_FRACTION", "UTRF"),
#' #        writeNow = FALSE)
#' # not run as geoclimate jar source needed
#' # geoClimateCall(
#' # jarFilePath = "path/to/Geoclimate/geoclimate-0.0.2-SNAPSHOT.jar",
#' # configFilePath = "/tmp/RedonOSM2022.json", wf = "OSM")
#' # rsuIndics = c("LCZ","TEB","UTRF"),
#' # gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT",
#' # "WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION",
#' # "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF"))
geoClimateConfigFile<-function(wf, locations, forceSRID=FALSE,
                               outFolder = "/tmp",
                               date = "2022-01-01T12:00:00Z",
                               rsuIndics = c("LCZ","TEB","UTRF"),
                               svfSimplified = TRUE,
                               estimatedHeight = TRUE,
                               grid_x_size = 0,
                               grid_y_size = 0,
                               rowCol = FALSE,
                               outputType = "geojson",
                               gridIndics = "",
                               outConfigDir = "/tmp",
                               outConfigFile = "configFile",
                               BDTinFolder = "", 
                               writeNow = FALSE) {
  # description<-"Test de description unique"

  if (wf=="OSM"){description<-"Processing OSM data"} else {
    if (wf=="BDTOPO_V2") {description<-"Processing BDTopo v2 data"} else {
      if (wf=="BDTOPO_V3") {
        description<-"Processing BDTopo v3 data"}
      else { description<-paste0("Processing on an unrecognized workflow: ", wf)
      }
    }
  }

  outFolder<-tryCatch(
    {
      list(folder=jsonlite::unbox(outFolder)) },
    error=function(e){
      list(folder="\tmp")
    }
  )


  if (wf == "OSM"){
    input<-list(locations=locations, date=jsonlite::unbox(date))
  } else { if (grepl("BDT", x = wf)) {
    if (forceSRID==FALSE){
      input<-
        list(
          folder= tryCatch(
            jsonlite::unbox(BDTinFolder),
            error=function(e){
              list(folder="\tmp")
            }),
          locations=locations
        )
    } else { if (forceSRID==TRUE) {
      input<-
        list(
          folder= tryCatch(
            jsonlite::unbox(BDTinFolder),
            error=function(e){
              list(folder="\tmp")
            })
          ,
          locations=locations,
          srid=jsonlite::unbox(2154))
    }}
  }}


if(grid_x_size!=0 & grid_y_size!=0){
  parameters<-list(
    rsu_indicators = list(
      indicatorUse = rsuIndics,
      svSimplified = jsonlite::unbox(svfSimplified),
      estimatedHeight = jsonlite::unbox(estimatedHeight)),
    grid_indicators = list(
      x_size = jsonlite::unbox(grid_x_size), y_size = jsonlite::unbox(grid_y_size),
      rowCol = jsonlite::unbox(rowCol),
      output = jsonlite::unbox(outputType),
      indicators = gridIndics
    )
  )
} else {
  parameters<-list(
    rsu_indicators = list(
      indicatorUse = rsuIndics,
      svSimplified = jsonlite::unbox(svfSimplified),
      estimatedHeight = jsonlite::unbox(estimatedHeight)))
}

  listJSON <- list(description=jsonlite::unbox(description), input=input, output=outFolder, parameters=parameters)

  output<-toJSON(x=listJSON,
    pretty=TRUE)
  print(output)



if (outConfigFile=="") { outConfigFile<-paste0(locations,wf) }

if (writeNow){
write(output,
file = gsub(
  pattern = "\\s | ,", replacement = "_", 
  x = normalizePath(paste0(outConfigDir,"/",outConfigFile,substr(date,1,4),".json"), mustWork = FALSE)
)
)
}

return(output)

}
