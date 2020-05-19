library(giscourse)
con<-sconnect()
library(sf)
load("primate_ranges.rda")

binoms<-unique(primate_ranges$binomial)
binoms<-sort(binoms)


pop <- raster("/home/rstudio/geoserver/data_dir/rasters/ghs/gdal_ghs2.tif")

f<-function(i=1)
  i=1
IUCN_range = primate_ranges[i]
pop<-raster::crop(pop,IUCN_range)
pop<-raster::mask(pop,IUCN_range)
d1<-data.frame(x=na.omit(as.vector(pop)))
area<-st_area(IUCN_range)
h<-hist(pop, breaks=c(0,1,10,100,1000,10000,100000,1000000), plot=FALSE)$counts
classes<-c("0-1","1-10","10-100","100-1000","1k-1k","10k-100k", "100k+")
d<-data.frame(classes,area=round(as.numeric(area*h/sum(h))/1000000,0))
d$percent<-round(100*d$area/sum(d$area),1)

