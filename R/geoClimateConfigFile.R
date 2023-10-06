#library(jsonlite)
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
                               outFile="") {
if (wf=="osm"){description<-"Processing OSM data"} else {
    if (wf=="bdt2") {description<-"Processing BDTopo v2 data"}
}

# description<-list(description = description)
# input<-list(input=list(locations=locations))
  
outFolder<-list(folder=unbox(outFolder))
  
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

    
listJSON = list(description=description,input=list(locations=locations),output=outFolder,parameters=parameters)    
    
output<-toJSON(x=listJSON,
               pretty=TRUE)

  
if (outFile=="") { outFile=paste0(locations,wf) }

write(output,file=paste0(outDir,"/",outFile,".json"))  
 
return(output)
  
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
test
write("test",file=paste0("/tmp","/","test.txt"))  