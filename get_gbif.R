
library(dismo)
library(mapview)
library(dplyr)
library(sf)

load("primate_ranges.rda")

library(tidyr)

primate_ranges %>% separate(binomial, into = c("genus","species")) -> primate_ranges

genera<-unique(primate_ranges$genus)
genera

get_gbif<-function(genus="Pongo")
{
  d<-gbif(genus=genus, species="*", geo=TRUE) # get all records
  d<-d[!is.na(d$lon),] # Take out blanks 
  
  d<-st_as_sf(d,coords=c("lon","lat"),crs=4326) # make sf
  names(d)<-tolower(names(d))
  useful<-c("genus", "species", "coordinateuncertaintyinmeters", "country","eventdate", "basisofrecord")
  d %>% select(useful) -> d
  d
}

d1<- get_gbif(genera[1])
genera2<- genera[-1]
for( i in genera2){
  try(d<-get_gbif(i))
  try(d1<- rbind(d,d1))
}

gbif<-d1
save(gbif, file="gbif_primates.rda")



```{r}

d<-do.call("rbind", data)
save(d, file="gbif.rda")
d<-st_transform(d,3857)
dd<-st_union(st_buffer(d, dist=100000))
```


```{r}
mapview(dd)
```

