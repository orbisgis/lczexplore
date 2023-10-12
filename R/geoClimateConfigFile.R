#' Builds a JSON configuration file for GeoClimate workflow
#' @param locations is either the town or the coordinates of the bounding box of the area on which GeoClimate will run.
#' If a town name, it will be fed to the Nominoe API, through the overpass API of OpenStreetMap. 
#' @param wf is the workflow used by GeoClimate. For now, only osm is available, for OpenStreetMap, but bdt for 
#' BD TOPO of IGN should be added when an online database is available. 
#' @param outFolder indicates where the results of GeoClimate will be put
#' @param rsuIndics is a vector with the indicators one wants to compute at the RSU scale. The default is c("LCZ","TEB","UTRF"),
#' @param svSimplified uses the simplified method to calculate skyview factor, default = TRUE
#' @param estimatedHeight uses an algorithm to esitmate the missing building height, default = TRUE
#' @param grid_x_size is the x size for the grid if some grid indicators are to be computed, default=100
#' @param grid_y_size is the x size for the grid if some grid indicators are to be computed, default=100
#' @param rowCol if grid_x_size and grid_y_size are not set, one cal set the number of rows and cols throug rowCol, 
#' but the recommended and defualt is FALSE
#' @param outputType is the format of GeoClimate outputs, default="geojson",
#' @param gridIndics is a vector containing the indicators to compute at the grid scale. Default is 
#' c("BUILDING_FRACTION",
#' "BUILDING_HEIGHT",
#' "WATER_FRACTION",
#' "VEGETATION_FRACTION",
#' "ROAD_FRACTION",
#' "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF")
#' @param outDir is the folder were the resulting JSON file will be put, the folder where GeoClimate will read it from
#' (different from out outFolder, where GeoClimates will put its geoJSON ouputs), default is "/tmp"
#' @param outFile is the name of your configuration file, if anb empty string, a name woill be created from location
#' and workflow parameters. 
#' @importFrom jsonlite unbox toJSON
#' @return returns a JSON configuration file to be fed to GeoClimate
#' @export
#' @examples
#' test<-geoClimateConfigFile(outFile="", wf="osm",outFolder="/tmp",locations="Redon",
#' rsuIndics = c("LCZ","TEB","UTRF"),
#' gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT","WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION",
#' "IMPERVIOUS_FRACTION","LCZ_PRIMARY","LCZ_FRACTION","UTRF"))
#' test
geoClimateConfigFile<-function(locations, wf,
                               outFolder="/tmp",
                               rsuIndics=c("LCZ","TEB","UTRF"),
                               svSimplified = TRUE,
                               estimatedHeight=TRUE,
                               grid_x_size=100,
                               grid_y_size=100,
                               rowCol=FALSE,
                               outputType="geojson",
                               gridIndics=c("BUILDING_FRACTION",
                                            "BUILDING_HEIGHT",
                                            "WATER_FRACTION",
                                            "VEGETATION_FRACTION",
                                            "ROAD_FRACTION",
                                            "IMPERVIOUS_FRACTION",
                                            "LCZ_PRIMARY",
                                            "LCZ_FRACTION",
                                            "UTRF"),
                               outDir = "/tmp",
                               outFile="",
                               BDTinFolder="",BDTinseeCode=29301) {
  print(wf)

  description<-"Test de description unique"
  
    # if (wf=="OSM"){description<-"Processing OSM data"} else {
    # if (wf=="BDTOPO_V2.2") {description<-"Processing BDTopo v2 data"} else {
    # if (wf=="BDTOPO_V3") {
    #   description<-"Processing BDTopo v2 data"}
    #  else { description<-paste0("Processing on an unrecognized workflow: ", wf)
    # }
    # }
    # }


  
# description<-list(description = description)
# input<-list(input=list(locations=locations))

outFolder<-list(folder=unbox(outFolder))
  
      
# if (wf == "osm"){
  input=list(locations=locations)
#   } else {if (grep("BDT",wf)) {
#   input=list(folder=BDTinFolder,locations=BDTinseeCode)
# }
# }



  parameters<-list(
  rsu_indicators = list(
    indicatorUse = rsuIndics,
    svSimplified = unbox(svSimplified),
    estimatedHeight = unbox(estimatedHeight)),
  grid_indicators = list(
    x_size = unbox(grid_x_size), y_size = unbox(grid_y_size),
    rowCol = unbox(rowCol),
    output = unbox(outputType),
    indicators = gridIndics
  )
)    

    
listJSON <- list(input=input,output=outFolder,parameters=parameters) #description=unbox(description),    
    
output<-toJSON(x=listJSON,
               pretty=TRUE)

  
if (outFile=="") { outFile=paste0(locations,wf) }

write(output,file=paste0(outDir,"/",outFile,".json"))  
 
return(output)
  
}

