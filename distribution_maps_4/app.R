

library(DT)
library(aqm)
library(shiny)
library(RColorBrewer)
library(raster)
library(rasterVis)
library(classInt)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)

tryObserve <- function(x) {
  x <- substitute(x)
  env <- parent.frame()
  observe({
    tryCatch(
      eval(x, env),
      error = function(e) {
        showNotification(paste("Error: ", e$message), type = "error")
      }
    )
  })
}

load("/home/rstudio/shiny/primates/results.rda")
results_table<-d
results_table %>% arrange(-tot_pop) -> results_table
rm(d)

load("/home/rstudio/shiny/primates/primate_ranges.rda")
load("/home/rstudio/shiny/primates/gbif_primates.rda")
load("/home/rstudio/shiny/primates/primates_maps.rda")

primate_ranges$binomial<-primate_ranges$acc_name
gbif$species<-gbif$acc_name
primates_maps$binomial<-primates_maps$acc_name

binoms<-unique(primate_ranges$binomial)
binoms2<-unique(gbif$species)
binoms3<-unique(as.character(primates_maps$binomial))
binoms<-binoms[binoms%in% binoms2]
binoms<-binoms[binoms%in% binoms3]
binoms <- sort(binoms)
binoms

library(giscourse)
con<-sconnect()

eoos<-read_sf(con,query="select * from primate_eoos")
#save(eoos,file="eoos.rda")
# load("eoos.rda")
binoms<-sort(binoms)
library(RColorBrewer)
pal1<-brewer.pal(8, "YlOrRd")
pal2<-brewer.pal(8, "YlGnBu")[8:1]
pal3<-brewer.pal(8, "YlOrRd")[8:1]
eoos_map<-mapview(eoos, zcol="population", at =c(0,10,20,100,200,500,1000,2000),legend=TRUE,col.regions = pal1)



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Choose a species"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      selectizeInput ("Species", label="Choose species:",choices = binoms, selected = "Rhinopithecus roxelana"),
      imageOutput("myImage"),
      htmlOutput("selURL") 
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("Map",  leafletOutput("map")),
          tabPanel("Population density histogram and map",  
                   plotOutput("fig1"),
                   plotOutput("fig2"),
                  verbatimTextOutput  ("results"),
                   h4("More to be added to illustrate results for each species selected")),
          tabPanel("Table of population densities",  dataTableOutput("pop_dense")),
          tabPanel("Table for all species",  dataTableOutput("results_table"))
          # tabPanel("All species map",  leafletOutput("map2"))
          
          )
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
output$results_table<-renderDataTable(server = FALSE,dt(results_table))
  
tryObserve({
    run<-TRUE
    
    sp <- input$Species
    
###### Images
   
    
    
    output$myImage <- renderImage(deleteFile=FALSE, {
     
      outfile <- sprintf("/home/rstudio/shiny/primates/figs/thumbs2/%s.jpg",sp)
      
    
      
      # Return a list containing the filename
      list(src = outfile,
           alt = "No image for this species")
    })
      
    
    
    
    # sp <-  binoms[1]
    
    ## Get eoa
    # query<-sprintf("select * from mammals where binomial = '%s' ", sp)
    # IUCN_range<-st_read(con, query=query)
    IUCN_range <-filter(primate_ranges,binomial==sp)
    if(!dim(eoos)[1]>1) run<-FALSE
    ## Get gbif
    # query<-sprintf("select * from gbif_primates where species = '%s' ", sp)
    # GBIF_points   <-st_read(con, query=query)
    
    GBIF_points<-filter(gbif,species == sp)
    if(!dim(GBIF_points)[1]>0) run<-FALSE
    
  if(run){
    
    st_crs(GBIF_points)<-st_crs(IUCN_range)
    GBIF_points <- st_filter(GBIF_points, IUCN_range)
    
   
    GBIF_buffer<-st_transform(st_union(st_buffer(st_transform(GBIF_points,3857), dist=100000)),4326)
    
    
    #query1<-sprintf("select * from mammals where binomial = '%s' ", sp)
    #query<-sprintf("select p.name,p.pop_max, p.geom from populated_places p, (%s) s where st_intersects(s.geom, p.geom) ", query1)
    #places<-st_read(con, query=query)
    
   
    
    
    #### Get population data
    pop <- raster("/home/rstudio/geoserver/data_dir/rasters/ghs/gdal_ghs2.tif")
    pop<-raster::crop(pop,IUCN_range)
    pop<-raster::mask(pop,IUCN_range)
    
    d1<-data.frame(x=na.omit(as.vector(pop)))
    
    
    area<-st_area(st_union(IUCN_range))
    h<-hist(pop, breaks=c(0,1,10,100,1000,10000,1000000), plot=FALSE)$counts
    classes<-c("0-1","1-10","10-100","100-1000","1k-10k","10k+")
    d<-data.frame(classes,area=round(as.numeric(area*h/sum(h))/1000000,0))
    d$percent<-round(100*d$area/sum(d$area),1)
    
    
    output$pop_dense<-renderDataTable(dt(d))
    output$results<-renderPrint({
    d
    })
    
    output$fig1<-renderPlot( ggplot(d1,aes(x=x)) + 
                               geom_histogram(fill = 'grey', color = 'grey30') + scale_x_log10() +
                               ylab("Count of number of pixels") +
                               xlab("Population density per square km on log10 scale"))
    
    output$fig2<-renderPlot(
      {
      pal <- brewer.pal(n=11, name = "RdYlGn")[11:1]
     plot(log10(pop+1), col=pal, main="Log10 population density")
        
      })
      
      
    
    ##############################
    
    primates_map <-filter(primates_maps, binomial== sp)
    map<- mapview(GBIF_points) + mapview(IUCN_range) + mapview( GBIF_buffer) + mapview(primates_map, col.regions= "red")
    
    output$map <- renderLeaflet(
      {map@map %>% addWMSTiles(group="Population",
                                "http://r.bournemouth.ac.uk:8083/AG/wms",
                                layers = "AG:GHS_POB",
                                options = WMSTileOptions(format = "image/png", transparent = TRUE)) })
    
    
  }})
   
output$map2 <- renderLeaflet(eoos_map@map)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

