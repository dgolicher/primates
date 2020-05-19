

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


#library(readxl)
#new_list<- read_excel("~/shiny/primates/distribution_maps_3/IUCN_Apr2020_SelectedGeneraCovid.xlsx")$binomial
# save(new_list,file="new_list.rda")
load("new_list.rda")
sp_list<- read.csv("CovidSpeciesList.csv")

sp_list$Genus <- gsub("Piliolocbus", "Piliocolobus", sp_list$Genus)
binoms<-paste(sp_list$Genus, sp_list$Species)
binoms<-sort(binoms)[-c(2,4,8)]

binoms<-new_list


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
      selectizeInput ("Species", label="Choose species:",choices = binoms, selected = "Rhinopithecus roxelana")
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
          tabPanel("All species map",  leafletOutput("map2"))
          
          )
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  observe({
    run<-TRUE
    sp <- input$Species
 
    
    ## Get eoa
    query<-sprintf("select * from mammals where binomial = '%s' ", sp)
    IUCN_range<-st_read(con, query=query)
    if(!dim(eoos)[1]>1) run<-FALSE
    ## Get gbif
    query<-sprintf("select * from gbif_primates where species = '%s' ", sp)
    GBIF_points   <-st_read(con, query=query)
    if(!dim(GBIF_points)[1]>1) run<-FALSE
    
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
    
    
    area<-st_area(IUCN_range)
    h<-hist(pop, breaks=c(0,1,10,100,1000,10000,100000,1000000))$counts
    classes<-c("0-1","1-10","10-100","100-1000","1k-1k","10k-100k", "100k+")
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
    map<- mapview(GBIF_points) + mapview(IUCN_range) + mapview( GBIF_buffer) 
    
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

