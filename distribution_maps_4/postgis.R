# load("gbif_primates.rda")
# library(sf)
# primates_maps<-st_read("/home/rstudio/shiny/primates/Primates_maps_2014_07_03/Primates_2014_07_03.shp")

str(d)
library(giscourse)
con<-sconnect()
library(sf)

sp <- "Pongo abelii"

query<-sprintf("select * from mammals where binomial in ('%s') ", sp)
query
# query<-"select * from mammals where order_ like 'PRIMATES' "
# primate_ranges<-st_read(con, query=query)
# save(primate_ranges,file="primate_ranges.rda")
mapview(eoa)

query<-sprintf("select * from gbif_primates where species = '%s' ", sp)

gbif<-st_read(con, query=query)

query1<-sprintf("select * from mammals where binomial = '%s' ", sp)
query<-sprintf("select p.name,p.pop_max, p.geom from populated_places p, (%s) s where st_intersects(s.geom, p.geom) ", query1)

places<-st_read(con, query=query)

pop <- giscourse::pop_density()
pop<-raster::crop(pop,eoa)
pop<-raster::mask(pop,eoa)
plot(pop)
hist(log10(pop))

cellStats(pop, 'sum')/1000000
sum(places$pop_max)/1000000

plot(log10(pop+1))

d<-data.frame(x=na.omit(as.vector(pop)))

ggplot(d,aes(x=x)) + geom_histogram(fill = 'grey', color = 'grey30') + scale_y_log10() 




