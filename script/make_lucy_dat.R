install.packages("furrr")
library(tidyverse)

library(furrr)
library(parallel)
library(vegan)

mrdat<-read.csv("data/2016_male_bee_dataset.csv")

fbees<-(mrdat %>% 
         filter(bee_sex=="F") 
        %>% group_by(site, sampling_round)) 

pollenvis<-fbees %>% group_by(site, sampling_round, bee) %>% 
    sample_n(10, replace=T)



nc<-detectCores()-1
plan(strategy=multiprocess, workers=nc)

lucydat<-future_map_dfr(1:nrow(fbees), function(x){
    thissite<-fbees[[x, "site"]]
    thisround<-fbees[[x, "sampling_round"]]
    thisspecies<-fbees[[x, "bee"]]
    out<-fbees %>% 
        filter(site==thissite & sampling_round== thisround & bee == thisspecies) %>% 
        sample_n(rpois(1,2), replace=T)  %>%
        group_by(site, sampling_round, bee, sociality, plant_genus, plant_species) %>%
        summarize(pollen_units=n(), uID=fbees[[x,"uniqueID"]])
    return(out)
} )

lucydat<-lucydat%>%
  unite("plant",c("plant_genus", "plant_species"))
write.csv(lucydat, "lucydat1_pois.csv")
lucydat1<-read_csv("lucydat1.csv")
head(lucydat1)
#shannon and richness of individual loads at each site and round
h<-c(2,4,6)
h/sum(h)
simpson<-function(h){sum((h/sum(h))^2)}
simpson(h)
diversities<-lucydat1%>%
    group_by(uID, site, sampling_round)%>%
    summarise(richness=n(), shannon=exp(diversity(pollen_units, index="shannon")), 
              simpson= 1/simpson(pollen_units))


sample(filter(lucydat1, site=="Baldpate" &round==$uID,1, 50,



unique(r$site)

hist(filter(r, site=="Baldpate")$richness)
hist(filter(r, site=="Baldpate")$shannon)
hist(filter(r, site=="Baldpate")$simpson)
plot(filter(r, site=="Baldpate")$richness,filter(r, site=="Baldpate")$shannon)
plot(filter(r, site=="Baldpate")$richness,filter(r, site=="Baldpate")$simpson)



ggplot
ggplot(filter(lucydat, site=="Baldpate"), aes=(x==bee, y==) )
hist(r$shannon)

lucydat<-lucydat%>%
    unite("plant",c("plant_genus", "plant_species"))



