
load("/home/rstudio/shiny/primates/primate_ranges.rda")
load("/home/rstudio/shiny/primates/gbif_primates.rda")
load("/home/rstudio/shiny/primates/primates_maps.rda")

# library(tidyr)
# primates_maps %>% separate(NAME,into =c("Genus","Species","Sub")) -> primates_maps 
# primates_maps$binomial<- paste(primates_maps$Genus,primates_maps$Species)

# save(primates_maps,file="/home/rstudio/shiny/primates/primates_maps.rda")

#library(readxl)
#new_list<- read_excel("~/shiny/primates/distribution_maps_3/IUCN_Apr2020_SelectedGeneraCovid.xlsx")$binomial
# save(new_list,file="new_list.rda")


binoms1<-unique(primate_ranges$binomial)
binoms2<-unique(gbif$species)
binoms3<-unique(as.character(primates_maps$binomial))
binoms3
binoms<-binoms1[binoms1%in% binoms2]
binoms<-binoms[binoms%in% binoms3]

binoms<-binoms3[!binoms3%in% binoms1]


library(taxize)
d<-synonyms(sort(na.omit(binoms3)), "itis", rows=1)

syns_iucn<-synonyms_df(d)
filter(syns_iucn, .id != acc_name) -> d3
save(d3,file="synonyms2.rda")





