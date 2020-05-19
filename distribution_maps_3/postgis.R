#load("gbif_primates.rda")

library(giscourse)
con<-sconnect()
library(sf)

sp <- "Pongo abelii"

query<-sprintf("select * from mammals where binomial in ('%s') ", sp)
query
eoa<-st_read(con, query=query)
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




