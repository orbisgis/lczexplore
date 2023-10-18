library(shiny)
library(shinyFiles)
library(ggplot2)
library(lczexplore)
library(magrittr)
library(osmdata)
library(sf)
#devtools::install_github("elipousson/getdata")
library(getdata)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  h1(" This application helps build a configuration file to feed to geoclimate"),
  h2(" The OSM workflow allows to run GeoClimate on any given city."), 
  h2("For BD TOPO workflows, the user has to provide a path to the input data"),
  h2(" It is not possible to build a configuration files using bounding box coordinates yet."),
  
  tabsetPanel(
  ############################################
  ##
  ## Tab to create the config file
  ##
  ############################################
    tabPanel("Create your  GeoClimate configuration JSON file",

  # Sidebar layout with input and output definitions ----
      sidebarLayout(
    
    # Sidebar panel for inputs ----
        sidebarPanel(
          shinyDirButton(id = "outFolder", 
                    label = "Output  folder for GeoClimates results", 
                    title = "Select the folder where GeoClimates will output its results"),
      
          selectInput(inputId="wf", label="Workflow", 
                  choices = list(OpenStreetMap="OSM","BD TOPO V2"="BDTOPO_V2","BD TOPO V3"="BDTOPO_V3"),
                  selected=list(OpenStreetMap="OSM")),
      
          conditionalPanel(
            condition='input.wf!="OSM"',
          shinyDirButton("BDTinFolder",
                       label = "BD_TOPO folder",
                       title = "Choose in which folder are the BD_TOPO files"),
            checkboxInput("forceSRID",label="Force SRID of BD TOPO inputs to 2154",value=FALSE),
            textInput(inputId="inseeCode", label="Enter Insee code of your location (town)", value = "29162")
          ),
      
          conditionalPanel(
            condition='input.wf=="OSM"',
            textInput(inputId="location",label="Enter your locations here",
                  value="Allaire",placeholder="A town name or some coordinates")
          ),

        checkboxGroupInput(inputId="rsuIndics",label = "Choose the indicators to compute at RSU scale",
             choices=c("LCZ","TEB","UTRF"),selected=c("LCZ")),
        fluidRow(
          column( width = 4, checkboxInput(inputId = "svfSimple", 
                         label = "Use simplified algorithm for sky view factor",
                         value = TRUE)),
          column(width = 4, checkboxInput(inputId = "EstimateHeight", label = "Estimate missing building heights", value = TRUE))
        ),
        
        checkboxGroupInput(inputId="gridIndics",label = "Choose the indicators to compute at grid scale",
             choices=c("BUILDING_FRACTION",
                       "BUILDING_HEIGHT",
                       "WATER_FRACTION",
                       "VEGETATION_FRACTION",
                       "ROAD_FRACTION",
                       "IMPERVIOUS_FRACTION",
                       "LCZ_FRACTION"),
             selected=c("BUILDING_FRACTION"
                        )),
        numericInput(inputId="xGridSize", label="Choose the x size for the grid", value = 100, min = 10, max = 1000, step = 10),
        numericInput(inputId="yGridSize", label="Choose the y size for the grid", value = 100, min = 10, max = 1000, step = 10),
        shinyDirButton("configDirOut",
                         label = "Folder to export config File",
                         title = "Choose in which folder to export the config file"),
          textInput(inputId="configOutFile",
                    label="Name your configuration file (without extension)",
                    value=""),
          actionButton(inputId="writeConfigFile",label="Export your parameters to JSON config File")
       )
      
    
      ,

      # Main panel for displaying config file ----
      mainPanel(
        titlePanel(title="Here is the content of the JSON configuration file you are building"),
      
        verbatimTextOutput("configJSON")
        
        
      )
      )
    ),
    
  ############################################
  ##
  ## tab to call geoclimate and show results
  ##
  ############################################
    tabPanel("Call the system to launch Geoclimate with your configuration file and parameters",
             sidebarLayout(
               sidebarPanel(
             shinyFilesButton(id="jarFile",title="Path to geoclimate jarfile", 
                              label="Path to geoclimate jarfile",
                              multiple=FALSE,filters=list("jar files"=c("jar"))),
             
             actionButton(inputId = "runGC",label = "Run GeoClimate with these parameters"),
             verbatimTextOutput("outMessage", placeholder = TRUE),
             actionButton(inputId = "showPlot", label = "View the outputs once GeoClimate executed successfully "),
             actionButton(inputId = "showSourceData", label = "View the source data used to compute the LCZ ")),
               
             mainPanel(
               verbatimTextOutput("folderImport"),
               plotOutput("LCZplot"),
               plotOutput("sourceDataPlot")
             )
             
    )
  )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output,session) {
# Choose the folder where results of geoclimate will be put

  shinyFiles::shinyDirChoose(input, 'outFolder', roots=getVolumes()(),
                                 defaultPath = "", allowDirCreate = TRUE )
  outFolder<-reactive({
    gsub(
      "//","/",
        parseDirPath(roots=getVolumes()(), selection=input$outFolder)) })
  
  shinyFiles::shinyDirChoose(input, id="BDTinFolder", roots=getVolumes()(), defaultPath = "" )
  
  BDTinFolder<-reactive({
       BDTinFolder<-gsub(
        "//","/", parseDirPath(roots=getVolumes()(),
                                                     selection = input$BDTinFolder))
     print(BDTinFolder)
  })
  
  # prepare export of configuration file 
  
  shinyFiles::shinyDirChoose(input, 'configDirOut', roots=getVolumes()(),
                             defaultPath = "", allowDirCreate = TRUE )
  configOutFolder<-reactive({
    gsub(
      "//","/",
    parseDirPath(roots=getVolumes()(), selection=input$configDirOut)
    )
  })
  
  
  #output$outMessage<-renderText({ BDTinFolder()  })
  
  output$configJSON<-renderText({
          geoClimateConfigFile(
            wf = input$wf,
            outFolder = gsub("//","/",outFolder()) ,
            locations = input$location,
            forceSRID=input$forceSRID,
            svfSimplified = input$svfSimple,
            estimatedHeight = input$EstimateHeight,
            grid_x_size = input$xGridSize,
            grid_y_size = input$yGridSize,
            rsuIndics = input$rsuIndics,
            gridIndics = input$gridIndics,
            BDTinseeCode = input$inseeCode, 
            BDTinFolder= BDTinFolder(),
            outConfigDir=configOutFolder(),
            outConfigFile = input$configOutFile,
            writeNow=FALSE)
        })

observeEvent(
  input$writeConfigFile, {
    geoClimateConfigFile(
    wf = input$wf,
    outFolder = gsub("//","/",outFolder()) ,
    locations = input$location,
    forceSRID=input$forceSRID,
    rsuIndics = input$rsuIndics,
    grid_x_size = input$xGridSize,
    grid_y_size = input$yGridSize,
    gridIndics = input$gridIndics,
    BDTinseeCode = input$inseeCode,
    BDTinFolder= BDTinFolder(),
    outConfigDir=configOutFolder(),
    outConfigFile = input$configOutFile,
    writeNow=TRUE) 
  }
)


shinyFiles::shinyFileChoose(input, 'jarFile', roots=getVolumes()(),
                           defaultPath = "/" )
jarFilePath<-reactive({
  if (!is.null(input$jarFile)) {
    gsub(
      "//","/",
      parseFilePaths(roots=getVolumes()(), input$jarFile)$datapath)
      }
  })

output$outMessage<-renderText({jarFilePath()})  
  
observeEvent(
    input$runGC, {
    geoClimateCall(jarFilePath=jarFilePath(),
                   configFilePath=paste0(configOutFolder(),"/",input$configOutFile,".json"),
                   wf=input$wf)
  }
)

  ############################################
  ##
  ## Visualize GC outputs
  ##
  ############################################

  wf<- reactive({input$wf})
  
  LCZpath<-reactive({if ( wf() == "OSM"){ paste0(outFolder(),"/osm_",input$location,"/") }  else
    if (wf() == "BDTOPO_V2") {paste0(outFolder(),"/bdtopo_2_",input$inseeCode,"/") }})
  
  output$folderImport<-renderText({
    LCZpath()
  })

observeEvent(
  input$showPlot,{
  sf1<-importLCZvect(dirPath=LCZpath())
  print(summary(sf1))
  LCZplot<-showLCZ(sf1)
  output$LCZplot<-renderPlot({
    LCZplot
  })
})

observeEvent(
  input$showSourceData,{
  zone<-read_sf(paste0(LCZpath(),"zone.geojson"))
  buildings<-read_sf(paste0(LCZpath(),"/building.geojson"))  %>% st_intersection(zone)
  roads<-read_sf(paste0(LCZpath(),"/road.geojson")) %>% st_intersection(zone)
  vegetation<-read_sf(paste0(LCZpath(),"/vegetation.geojson")) %>% st_intersection(zone)
  water<-read_sf(paste0(LCZpath(),"/water.geojson")) %>% st_intersection(zone)
  
  

  sourceDataPlot<-ggplot()+
    geom_sf(data=vegetation,aes(),fill="#bbdb7a")+
    geom_sf(data=water,aes(),fill="blue")+
    geom_sf(data=roads,aes(),fill="black")+
     geom_sf(data=buildings,aes(),fill="grey")
       
  
  output$sourceDataPlot<-renderPlot({
    sourceDataPlot
  })
  

  })
 
  
}

shinyApp(ui = ui, server = server)