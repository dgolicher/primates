library(shiny)
library(leaflet)
library(mapedit)
library(sf)

map <- leaflet() %>%
  addTiles()



ui <- fluidPage(
  
  # Application title
  titlePanel("Minimal Shiny Leaflet Mapedit"),
  
  # Sidebar with a ui for grabbing mapedit data
  sidebarLayout(
    sidebarPanel(
      actionButton('save', 'Save from Map')
    ),
    
    # add map
    mainPanel(
      editModUI("map")
    )
  )
)


server <- function(input, output) {
  
  edits <- callModule(
    editMod,
    leafmap = map,
    id = "map"
  )
  
  observeEvent(input$save, {
    
    geom <- edits()$finished
    
    if (!is.null(geom)) {
      assign('new_geom', geom, envir = .GlobalEnv)
      sf::write_sf(geom, 'new_geom.geojson', delete_layer = TRUE, delete_dsn = TRUE)
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)