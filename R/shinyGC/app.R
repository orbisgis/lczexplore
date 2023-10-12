library(shiny)
library(shinyFiles)
library(lczexplore)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Run Geoclimate on OSM Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      shinyDirButton(id = "outFolder", 
                    label = "Output  folder for GeoClimates results", 
                    title = "Select the folder where GeoClimates will output its results", FALSE),
      selectInput(inputId="wf", label="Workflow", 
                  choices = list(OpenStreetMap="OSM","BD TOPO V2"="BDTOPO_V2.2","BD TOPO V3"="BDTOPO_V3"),
                  selected=list(OpenStreetMap="OSM")),
      conditionalPanel(
        condition='input.wf!="OSM"',
        shinyDirButton("inBDTfolder",
                       label = "BD_TOPO folder",
                       title = "Choose in which folder are the BD_TOPO files"),
        numericInput(inputId="inseeCode", label="Enter Insee code of your location (town)", value = "29031")
      ),
      textInput(inputId="location",label="Enter your locations here",
                value="Redon",placeholder="A town name or some coordinates")
      
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      titlePanel(title="Here is the content of the JSON configuration file you are building"),
      
      verbatimTextOutput("configJSON"),
      verbatimTextOutput("outFolder", placeholder = TRUE)
    )

  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {


  # Choose the folder where results of geoclimate will be put
    # set the path to the button in the interface (input outfolder)
  shinyFiles::shinyDirChoose(input, 'outFolder', roots=getVolumes()(), 
                             defaultPath = "/" )
  # set a default value
  global<-reactiveValues(datapath = "/tmp")
  # get what the interface outputs (nothing before user action) and put it in a reactive value
  outFolder<-reactive(input$outFolder)
  
  output$outFolder<-renderText({
    global$datapath
  })
  # Now when the user sets the value in the interface : 
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$outFolder
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(outFolder()$path[-1]), collapse = .Platform$file.sep))
               })
  
  # output$outFolder<-renderText({gsub("//","/",parseDirPath(roots=getVolumes()(), selection=outFolder()))})



  shinyFiles::shinyDirChoose(input, id="inBDTfolder", roots=c(getVolumes()(),"~"), defaultPath = "/tmp" )
  
  output$configJSON<-renderText({    
    geoClimateConfigFile(
    outFile = "", wf = "OSM",
    outFolder = gsub("//","/", parseDirPath(roots=getVolumes()(), selection = outFolder())),
    locations = input$location,
    rsuIndics = c("LCZ","TEB","UTRF"),gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT",
                                                     "WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION","IMPERVIOUS_FRACTION","LCZ_PRIMARY",
                                                     "LCZ_FRACTION","UTRF"))
  })



}

shinyApp(ui = ui, server = server)