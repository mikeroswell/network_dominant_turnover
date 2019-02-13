install.packages("furrr")
library(tidyverse)

library(furrr)
library(parallel)
library(vegan)
library(bipartite)

#michael's data
mrdat<-read.csv("data/2016_male_bee_dataset.csv")

fbees<-(mrdat %>% 
         filter(bee_sex=="F") 
        %>% group_by(site, sampling_round)) 

#plant use at species level in visit data, for Baldpate sampling round 2

Bald_visits<-filter(mrdat, site =="Baldpate", sampling_round=="2")
Bald_visits<- unite(Bald_visits, "plant", c("plant_genus", "plant_species"))
Bvisits<-Bald_visits%>%
  group_by(bee, plant )%>%
  summarize(visits=n())

B2visits<-spread(Bvisits, plant, visits)
B2visits<-B2visits[,order(colSums(-B2visits,na.rm=TRUE))]
B2visits<-B2visits[order(rowSums(-B2visits, na.rm=T)),]
B2visits<-B2visits%>%
  select(bee, everything())
write.csv(B2visits, "B2visits.csv")


#########POLLEN STUFF

#first try at making the pollen data that doesn't really make sense
pollenvis<-fbees %>% group_by(site, sampling_round, bee) %>% 
  sample_n(10, replace=T)

#####SIMULATING POLLEN DATA; each individual of each species samples plants in proportion to 
#that's species' plant visits; each individual can visit n flowers in a foraging bout 
###(lucydat1 is 10 visits; the code below draws # of viists from a poisson dist.)
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
lucydat1<-lucydat1%>%
  unite("plant",c("plant_genus", "plant_species"))
###########INDIVIDUAL-LEVEL pollen stuff
#shannon and richness of individual loads at each site and round
h<-c(2,4,6)
h/sum(h)
simpson<-function(h){sum((h/sum(h))^2)}
simpson(h)
diversities<-lucydat1%>%
  group_by(bee,uID, site, sampling_round)%>%
  summarise(richness=n(), shannon=exp(diversity(pollen_units, index="shannon")), 
            simpson= 1/simpson(pollen_units))

#i don't know what this is, might have been a first stab at an individual accumualtion curve
n<-sample(filter(lucydat1, site=="Baldpate" &sampling_round== "1")$uID,50, replace=T)

#looking at individual richness, simpson and shannon of lucydat1 at Baldpate
hist(filter(r, site=="Baldpate")$richness)
hist(filter(r, site=="Baldpate")$shannon)
hist(filter(r, site=="Baldpate")$simpson)
plot(filter(r, site=="Baldpate")$richness,filter(r, site=="Baldpate")$shannon)
plot(filter(r, site=="Baldpate")$richness,filter(r, site=="Baldpate")$simpson)

#



#individual load diversity by bee species at Baldpate
Bald<-filter(diversities, site=="Baldpate")
Bald1<-filter(Bald, sampling_round=="1")
Bald1<-arrange(Bald1, bee)
Bald1
Bald1
ggplot(Bald1, aes(x=bee, y=richness) )+
  geom_jitter()
#######FOR EACH BEE SPECIES, ACCUMULATION OF POLLEN TAXA ACROSS INDIVIDUAL LOADS.
#each individual collected ten grains of pollen. 

#pollen accumulation data (across loads) for all species at Baldpate

species<-unique(Baldpate2$bee)
rich<-list()

eachspecies<-list()
for (k in 3:3){
  sp<-filter(Baldpate2, bee==species[k])
  IDs<-unique(sp$uID)
  it<-list()
for (j in 1:length(IDs)){
  rich<-list()
for(i in 1:50){
sampleIDs<-sp[sample(length(IDs), j, replace=T),]
sample<-filter(sp, uID%in%sampleIDs$uID)
rich[i]<-length(unique(sample$plant))
it[[j]]<-rich

}
  ##this is my attempt to "unlist" within the loop, so that "eachspecies"
  #would be a list of dataframes, one for each species. Does not work.
  #accumulation<-data.frame(matrix(unlist(it), nrow=50, byrow=T))
  #n<-1:length(IDs)
  #p<-rep("'", length(IDs))
  #names(accumulation)<-cbind(p,n)[,2]
  #acc<-gather(accumulation, individuals, names(accumulation))
  #names(acc)<-c("individuals", "richness")
  #acc$individuals<-as.numeric(acc$individuals)
  #acc<-arrange(acc, individuals)
  #eachspecies[[k]]<-acc 
  
  eachspecies[[k]]<-it
  
}
}
##^this works, but don't know how to deal with the stuff in eachspecies to make curves. 

#stuff
accumulation<-data.frame(matrix(unlist(eachspecies[[3]]), nrow=50, byrow=T))

ggplot(acc, aes(x=as.factor(individuals), y=richness))+
  geom_boxplot()
eachspecies[[1]]
acc<-acc%>%
  group_by(individuals)%>%
  summarize(meanrich=mean(richness))
ggplot(acc, aes(x=as.factor(individuals), y=meanrich))+
  geom_point()
#############BEE-SPECIES LEVEL pollen stuff
#bee species level pollen composition; the number of "grains" of each plant collected by each bee species 

lucydat1<-lucydat1%>%
  group_by(site, sampling_round, bee, plant)%>%
  mutate(grains=sum(pollen_units))

pollencomp<-select(lucydat1, c("site", "sampling_round", "bee", "plant", "grains"))
pollencomp<-unique(pollencomp)
pollencomp<-arrange(pollencomp, bee)

#just Baldpate sampling round 2
B2comp<-filter(pollencomp, site =="Baldpate" & sampling_round=="2")

B2comp_wide<-spread(B2comp, plant, grains)
B2comp_wide<-B2comp_wide[,4:24]
B2comp_wide<-B2comp_wide[,order(colSums(-B2comp_wide,na.rm=TRUE))]
B2comp_wide$rows<-rowSums(B2comp_wide, na.rm=T)
B2comp_wide$bee<-spread(B2comp, plant, grains)$bee

B2comp_wide<-B2comp_wide%>%
  arrange(desc(rows))

B2comp_wide<-B2comp_wide%>%
  select(bee, everything())

rowSums(B2comp_wide, na.rm=T)





