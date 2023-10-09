library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Run Geoclimate on OSM Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput(inputId="location",label="Enter your locations here",
                value="Redon",placeholder="A town name or some coordinates")
     

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      renderText(
         
        
      )

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
 
  output$configFile<-geoClimateConfigFile(outFile="", wf="osm",outFolder="/tmp",locations=input$location,
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
 

}

shinyApp(ui = ui, server = server)