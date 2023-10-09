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
      textInput(inputId="location",label="Enter your locations here",
                value="Redon",placeholder="A town name or some coordinates"),
      shinyDirButton('outFolder', 'Output  folder for GeoClimates results', 'Select the folder where GeoClimates will output its results', FALSE)
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      titlePanel(title="Here is the content of the JSON configuration file you are building"),
      
      verbatimTextOutput("configJSON", ),
      verbatimTextOutput("outFolder", placeholder = TRUE)
    )

  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # test<-geoClimateConfigFile(
  #   outFile="", wf="osm",outFolder="/tmp",locations=input$location, 
  #   rsuIndics = c("LCZ","TEB","UTRF"),gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT",
  #              "WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION","IMPERVIOUS_FRACTION","LCZ_PRIMARY",
  #              "LCZ_FRACTION","UTRF"))

  shinyFiles::shinyDirChoose(input, 'outFolder', roots=getVolumes()(), 
                             filetypes=c('', 'txt'),defaultPath = "" )

  # observeEvent(input$outFolder, {
  #   if (!is.null(input$dir)) {
  #     dir_path <- input$dir
  #     if (!file.exists(dir_path)) {
  #       dir.create(dir_path)
  #       showModal(modalDialog(
  #         title = "Folder Created",
  #         "The folder has been created.",
  #         easyClose = TRUE
  #       ))
  #     }
  #   }
  # }
  # )
  
  outFolder<-reactive(input$outFolder)
  output$outFolder<-renderText({gsub("//","/",parseDirPath(roots=getVolumes()(), selection=outFolder()))})
  
  output$configJSON<-renderText({geoClimateConfigFile(
    outFile="", wf="osm",
    outFolder=gsub("//","/",parseDirPath(roots=getVolumes()(), selection=outFolder())),
    locations=input$location,
    rsuIndics = c("LCZ","TEB","UTRF"),gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT",
                                                     "WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION","IMPERVIOUS_FRACTION","LCZ_PRIMARY",
                                                     "LCZ_FRACTION","UTRF"))})



}

shinyApp(ui = ui, server = server)