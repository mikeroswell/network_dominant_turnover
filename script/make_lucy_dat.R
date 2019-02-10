library(tidyverse)
library(furrr)
library(parallel)

mrdat<-read.csv("data/2016_male_bee_dataset.csv")

fbees<-(mrdat %>% 
         filter(bee_sex=="F") 
        %>% group_by(site, sampling_round)) 

pollenvis<-fbees %>% group_by(site, sampling_round, bee) %>% 
    sample_n(10, replace=T)



# options(future.plan="multiprocess", mc.cores = parallel::detectCores() - 1L)
plan(multiprocess)

lucydat<-future_map_dfr(1:nrow(fbees), function(x){
    thissite<-fbees[[x, "site"]]
    thisround<-fbees[[x, "sampling_round"]]
    thisspecies<-fbees[[x, "bee"]]
    out<-fbees %>% 
        filter(site==thissite & sampling_round== thisround & bee == thisspecies) %>% 
        sample_n(10, replace=T)  %>%
        group_by(site, sampling_round, bee, sociality, plant_genus, plant_species) %>%
        summarize(pollen_units=n(), uID=fbees[[x,"uniqueID"]])
    return(out)
} )

