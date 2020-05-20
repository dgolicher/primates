
library(giscourse)
con<-sconnect()
library(sf)


load("new_list.rda")
binoms<-new_list
spl<-paste(binoms, collapse="','")
spl

query<-sprintf("select * from mammals where binomial in ('%s') ", spl)
eoas<-st_read(con, query=query)

mapview(eoas)
pop <- giscourse::pop_density()


sps<-unique(eoas$binomial)
sp<-sps[1]

###### Test function out with one species

x<-filter(eoas,binomial==sp)
area<-st_area(x)
pop1<-raster::crop(pop,x)
pop1<-raster::mask(pop1,x)
h<-hist(pop1, breaks=c(0,1,10,100,1000,10000,100000,1000000))$counts

classes<-c("0-1","1-10","10-100","100-1000","1k-1k","10k-100k", "100k+")

d<-data.frame(classes,area=round(as.numeric(area*h/sum(h))/1000000,0))
d$percent<-round(100*d$area/sum(d$area))


get_pop<-function(sp=sp[1])
{
  
  x<-filter(eoas,binomial==sp)
  pop1<-raster::crop(pop,x)
  pop1<-raster::mask(pop1,x)
  try(hist(log10(pop1)))
  cellStats(pop1, 'sum')/1000000
}
d<-data.frame(binomial=sps[1],population=get_pop(sps[1]))


for(sp in sps[-1]){
  d1<-data.frame(binomial=sp,population=get_pop(sp))
  d<-rbind(d,d1)
}


eoos<-left_join(eoas,d)[,c(1,3,29,30)]
names(eoos)



library(RColorBrewer)
pal1<-brewer.pal(8, "YlOrRd")
pal2<-brewer.pal(8, "YlGnBu")[8:1]
pal3<-brewer.pal(8, "YlOrRd")[8:1]
mapview(eoos, zcol="population", at =c(0,10,20,100,200,500,1000,2000),legend=TRUE,col.regions = pal1)
write_sf(eoos,con,"primate_eoos")

eoos<-read_sf(con,query="select * from primate_eoos")

library(RColorBrewer)
pal1<-brewer.pal(8, "YlOrRd")
pal2<-brewer.pal(8, "YlGnBu")[8:1]
pal3<-brewer.pal(8, "YlOrRd")[8:1]
mapview(eoos, zcol="population", at =c(0,10,20,100,200,500,1000,2000),legend=TRUE,col.regions = pal1)
