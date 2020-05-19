

library(DT)
library(shiny)
library(RColorBrewer)

library(leaflet)
library(leaflet.extras)
library(mapview)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
sp_list<- read.csv("CovidSpeciesList.csv")
sp_list$Genus <- gsub("Piliolocbus", "Piliocolobus", sp_list$Genus)
binoms<-paste(sp_list$Genus, sp_list$Species)
load("ranges.rda")
load("gbif_primates.rda")



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Choose a species"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      selectizeInput ("Species", label="Choose species:",choices = binoms, selected = binoms[1])
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  observe({
    
    sp <- input$Species
    gbif %>% filter(species == sp) -> GBIF_points
    ranges %>% filter(binomial == sp) -> IUCN_range
    st_crs(GBIF_points)<-st_crs(IUCN_range)
    GBIF_points <- st_filter(GBIF_points, IUCN_range)
    
   
    GBIF_buffer<-st_transform(st_union(st_buffer(st_transform(GBIF_points,3857), dist=100000)),4326)
    
    
    map<- mapview(GBIF_points) + mapview(IUCN_range) + mapview( GBIF_buffer) 
    
    output$map <- renderLeaflet(
      {map@map %>% addWMSTiles(group="Population",
                                "http://r.bournemouth.ac.uk:8083/AG/wms",
                                layers = "AG:GHS_POB",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE)) })
    
    
  })
   

}

# Run the application 
shinyApp(ui = ui, server = server)

