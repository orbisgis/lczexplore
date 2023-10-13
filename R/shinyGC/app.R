library(shiny)
library(shinyFiles)
library(lczexplore)
library(magrittr)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tabsetPanel(
  
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
                       title = "Choose in which folder are the BD_TOPO files",FALSE),
            checkboxInput("forceSRID",label="Force SRID of BD TOPO inputs to 2154",value=FALSE),
            textInput(inputId="inseeCode", label="Enter Insee code of your location (town)", value = "29031")
          ),
      
          conditionalPanel(
            condition='input.wf=="OSM"',
            textInput(inputId="location",label="Enter your locations here",
                  value="Allaire",placeholder="A town name or some coordinates")
          ),

        checkboxGroupInput(inputId="rsuIndics",label = "Choose the indicators to compute at RSU scale",
             choices=c("LCZ","TEB","UTRF"),selected=c("LCZ")),
        
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
        shinyDirButton("configDirOut",
                         label = "Folder to export config File",
                         title = "Choose in which folder to export the config file",FALSE),
          textInput(inputId="configOutFile",
                    label="Name your configuration file (without extension)",
                    value=""),
          actionButton(inputId="writeConfigFile",label="Export your parameters to JSON config File")
       )
      
    
      ,

      # Main panel for displaying outputs ----
      mainPanel(
        titlePanel(title="Here is the content of the JSON configuration file you are building"),
      
        verbatimTextOutput("configJSON"),
        
      )
      )
    ),
    
    tabPanel("Call the system to launch Geoclimate with your configuration file and parameters",
             
             shinyFilesButton(id="jarFile",title="Path to geoclimate jarfile", 
                              label="Path to geoclimate jarfile",
                              multiple=FALSE,filters=list("jar files"=c("jar"))),
             
             actionButton(inputId="runGC",label="run GeoClimate with these parameters"),
             verbatimTextOutput("outMessage", placeholder = TRUE)
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output,session) {
# Choose the folder where results of geoclimate will be put

  shinyFiles::shinyDirChoose(input, 'outFolder', roots=getVolumes()(),
                                 defaultPath = "", allowDirCreate = TRUE )
  outFolder<-reactive({
        parseDirPath(roots=getVolumes()(), selection=input$outFolder) })
  
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
  
  
}

shinyApp(ui = ui, server = server)