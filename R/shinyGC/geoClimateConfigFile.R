#' Builds a JSON configuration file for GeoClimate workflow
#' @param locations is either the town or the coordinates of the bounding box of the area on which GeoClimate will run.
#' If a town name, it will be fed to the Nominoe API, through the overpass API of OpenStreetMap.
#' @param wf is the workflow used by GeoClimate. For now, only OSM" is available, for OpenStreetMap, but "BDT" for
#' BDTOPO of IGN should be added when an online database is available.
#' @param date is the date of the data we extract from OpenStreetMap. The format is "yyyy:mm:ddThh:mm:ssZ"
#' (for BDTOPO the version depends from the input data folder)
#' @param outFolder indicates where the results of GeoClimate will be put
#' @param rsuIndics is a vector with the indicators one wants to compute at the RSU scale. The default is c("LCZ","TEB","UTRF"),
#' @param svfSimplified uses the simplified method to calculate skyview factor, default = TRUE
#' @param estimatedHeight uses an algorithm to estimate the missing building height, default = TRUE
#' @param grid_x_size is the x size for the grid if some grid indicators are to be computed, default=100
#' @param grid_y_size is the x size for the grid if some grid indicators are to be computed, default=100
#' @param rowCol if grid_x_size and grid_y_size are not set, one cal set the number of rows and cols throug rowCol,
#' but the recommended and default is FALSE
#' @param outputType is the format of GeoClimate outputs, default="geojson",
#' @param gridIndics is a vector containing the indicators to compute at the grid scale. Default is
#' c("BUILDING_FRACTION",
#' "BUILDING_HEIGHT",
#' "WATER_FRACTION",
#' "VEGETATION_FRACTION",
#' "ROAD_FRACTION",
#' "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF")
#' @param outConfigDir is the folder were the resulting JSON file will be put, the folder where GeoClimate will read it from
#' (different from out outFolder, where GeoClimates will put its geoJSON ouputs), default is "/tmp"
#' @param outConfigFile is the name of your configuration file, if and empty string, a name will be created from location
#' and workflow parameters.
#' @param forceSRID some BD TOPO input file may not have an srid, this forces srid to be 2154
#' @importFrom jsonlite unbox toJSON
#' @return returns a JSON configuration file to be fed to GeoClimate
#' @export
#' @examples
#' test<-geoClimateConfigFile(outConfigFile="", wf="osm",outFolder="/tmp",locations="Redon",
#' rsuIndics = c("LCZ","TEB","UTRF"),
#' gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT","WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION",
#' "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF"))
#' test
geoClimateConfigFile<-function(wf, locations, forceSRID=FALSE,
                               outFolder="/tmp",
                               date,
                               rsuIndics=c("LCZ","TEB","UTRF"),
                               svfSimplified = TRUE,
                               estimatedHeight = TRUE,
                               grid_x_size=0,
                               grid_y_size=0,
                               rowCol = FALSE,
                               outputType = "geojson",
                               gridIndics="",
                               outConfigDir = "/tmp",
                               outConfigFile = "configFile",
                               BDTinFolder = "", BDTinseeCode=29301,
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

  outFolder<-tryCatch( # This hideous tryCatch deals with weird shinyDirChoose behavior
    {
      list(folder=jsonlite::unbox(paste0(jsonlite::unbox(outFolder),
                               "/",wf,"/", substr(date,1,4)))) },
    error=function(e){
      list(folder="\tmp")
    }
  )


  if (wf == "OSM"){
    input<-list(locations=locations, date=jsonlite::unbox(date))
  } else { if (grep("BDT",wf)==1) {
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



  if (outConfigFile=="") { outConfigFile<-paste0(locations,wf) }

  if (writeNow == TRUE){
    write(output,file=paste0(outConfigDir,"/",outConfigFile,substr(date,1,4),".json"))
  }

  return(output)

}
#
# library(jsonlite)
# test<-geoClimateConfigFile(outConfigFile="", wf="BDTOPO_V2.2",outFolder=list(folder="/tmp",srid=2154),locations="Allaire",
#  rsuIndics = c("LCZ","TEB","UTRF"),
#  gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT","WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION",
#  "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF"))
#  test
