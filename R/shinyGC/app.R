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
                    title = "Select the folder where GeoClimates will output its results"),
      
      selectInput(inputId="wf", label="Workflow", 
                  choices = list(OpenStreetMap="OSM","BD TOPO V2"="BDTOPO_V2.2","BD TOPO V3"="BDTOPO_V3"),
                  selected=list(OpenStreetMap="OSM")),
      
      conditionalPanel(
        condition='input.wf!="OSM"',
        shinyDirButton("BDTinFolder",
                       label = "BD_TOPO folder",
                       title = "Choose in which folder are the BD_TOPO files",FALSE),
        numericInput(inputId="inseeCode", label="Enter Insee code of your location (town)", value = "29031")
      ),
      
      conditionalPanel(
        condition='input.wf=="OSM"',
        textInput(inputId="location",label="Enter your locations here",
                  value="Redon",placeholder="A town name or some coordinates")
      )      
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
  
  # set a default value
  # global<-reactiveValues(datapath = "/tmp")
  # get what the interface outputs (nothing before user action) and put it in a reactive value
  # outFolder<-reactive(input$outFolder)
  
  # output$outFolder<-renderText({
  #   global$datapath
  # })

  shinyFiles::shinyDirChoose(input, 'outFolder', roots=getVolumes()(),
                             defaultPath = "", allowDirCreate = TRUE )
  # observe({
  #   cat("\ninput$outFolder value:\n\n")
  # print(input$outFolder)})
  # Now when the user sets the value in the interface : (I'm a script kiddie, got this on SO and do not fully understand it now, shame to be corrected
  # observeEvent(ignoreNULL = TRUE,
  #              eventExpr = {
  #                input$outFolder
  #              },
  #              handlerExpr = {
  #                if (!"path" %in% names(dir())) return()
  #                home <- normalizePath("~")
  #                global$datapath <-
  #                  file.path(home, paste(unlist(outFolder()$path[-1]), collapse = .Platform$file.sep))
  #              })
  
   outFolder<-reactive({
     parseDirPath(roots=getVolumes()(), selection=input$outFolder) })

  # output$outFolder<- renderText({ gsub("//","/", outFolder()) })

  observe({
    cat("outFolder value:\n\n")
  print( gsub("//","/", outFolder()))})
  
  # 
  # shinyFiles::shinyDirChoose(input, id="BDTinFolder", roots=c(getVolumes()(),"~"), defaultPath = "/" )
  # BDTinFolder<-reactive(input$BDTinFolder)
  # global2<-reactiveValues(datapath = "/tmp")
  # 
  # output$BDTinFolder<-renderText({
  #   global2$datapath
  # })
  # observeEvent(ignoreNULL = TRUE,
  #              eventExpr = {
  #                input$BDTinFolder
  #              },
  #              handlerExpr = {
  #                if (!"path" %in% names(dir())) return()
  #                home <- normalizePath("~")
  #                global2$datapath <-
  #                  file.path(home, paste(unlist(BDTinFolder()$path[-1]), collapse = .Platform$file.sep))
  #              })
  # 
  # 
  # output$BDTinFolder<-renderText({gsub("//","/",parseDirPath(roots=getVolumes()(), selection=BDTinFolder()))})
  # 
  output$configJSON<-renderText({    
    geoClimateConfigFile(
    outFile = "", wf = input$wf,
    outFolder = gsub("//","/", outFolder()),
    locations = input$location,
    rsuIndics = c("LCZ","TEB","UTRF"),gridIndics = c("BUILDING_FRACTION","BUILDING_HEIGHT",
                                                     "WATER_FRACTION","VEGETATION_FRACTION","ROAD_FRACTION","IMPERVIOUS_FRACTION","LCZ_PRIMARY",
                                                     "LCZ_FRACTION","UTRF"))
    #,
    # BDTinseeCode = input$inseeCode, 
    # BDTinFolder= gsub("//","/", parseDirPath(roots=getVolumes()(), selection = BDTinFolder())))
  })



}

shinyApp(ui = ui, server = server)