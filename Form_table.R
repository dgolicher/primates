library(giscourse)
con<-sconnect()
library(sf)
load("primate_ranges.rda")

binoms<-unique(primate_ranges$binomial)
binoms<-sort(binoms)


pop <- raster("/home/rstudio/geoserver/data_dir/rasters/ghs/gdal_ghs2.tif")

f<-function(i=1){
sp_name<-binoms[i]
primate_ranges %>% filter(binomial==sp_name) -> IUCN_range
pop1<-raster::crop(pop,IUCN_range)
pop1<-raster::mask(pop1,IUCN_range)

area<-st_area(st_union(IUCN_range))

h<-hist(pop1, breaks=c(0,1,10,100,1000,10000,1000000), plot=FALSE)$counts
classes<-c("0-1","1-10","10-100","100-1000","1k-10k","10k+")
d<-data.frame(sp_name,classes,area=round(as.numeric(area*h/sum(h))/1000000,0))
d$percent<-round(100*d$area/sum(d$area),1)
d$tot_pop <- round(cellStats(pop1, 'sum')/1000000,3)
d
}

d<-f(1)

for (i in 2:length(binoms))
{
  try(d1<-f(i))
  d<-try(rbind(d,d1))
}

save(d, file="results.rda")

