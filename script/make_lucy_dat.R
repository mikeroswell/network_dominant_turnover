install.packages("furrr")
library(tidyverse)

library(furrr)
library(parallel)
library(vegan)
library(bipartite)
#colleen's data
pollen<-read.csv("PollenData_formatted_4feb.csv")
colleen<-read.csv("forest_pollen_spec.csv")
pollens<-right_join(select(colleen, uniqueID,genus, species, site, date), select(pollen, -beeID))
pollens<-unite(pollens, bee, genus, species)
pollens<-pollens %>%
  mutate(pollen=replace(pollen, pollen=="na", "")) %>%
  as.data.frame()
pollens<-pollens%>%
  group_by(site, date, bee, uniqueID, pollen)%>%
  summarize(counts=n())

#individual data in wide format
pollens_wide<-spread(pollens, pollen, counts)


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
colleen<-read.csv("")
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
hist(filter(diversities, site=="Baldpate")$richness)
hist(filter(diversities, site=="Baldpate")$shannon)
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
Baldpate2<-filter(lucydat1, site=="Baldpate"&sampling_round=="2")
species<-unique(Baldpate2$bee)

blah<-c("richness","iteration","individuals","species")
for (k in 1:length(species)){
  sp<-filter(Baldpate2, bee==species[k])
  IDs<-unique(sp$uID)
for (j in 1:length(IDs)){
for(i in 1:50){
sampleIDs<-sp[sample(length(IDs), j, replace=F),]
sample<-filter(sp, uID%in%sampleIDs$uID)

blah<-rbind(blah,c(length(unique(sample$plant)),i,j,k)) 

}}
}

blah<-as.data.frame(blah)
names(blah)<-c("richness","iteration","individuals","species")
blah<-blah[2:length(blah$richness),]

onebee<-filter(blah, species=="5")
onebee$individuals<-as.numeric(onebee$individuals)
onebee<-arrange(onebee, individuals)

ggplot(onebee, aes(x=individuals, y=as.numeric(richness)))+
  geom_boxplot(aes(group=individuals))


#############BEE-SPECIES LEVEL pollen stuff
#bee species level pollen composition; the number of "grains" of each plant collected by each bee species 

speciespollen<-lucydat1%>%
  group_by(site, sampling_round, bee, plant)%>%
  mutate(grains=sum(pollen_units))

pollencomp<-select(speciespollen, c("site", "sampling_round", "bee", "plant", "grains"))
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



#select pollens that meet 80% threshold for each species. subset full species by pollen taxa data frame
#for each bee's vector of pollen abundances, randomly select a pollen. if it's grains
#meet or exceed 0.8*sum(vector), return a 1 for that species and don't add any other species. if it's 
#grains don't meet or exceed that, return a 1 and then add another species. if the total # grains between the
#two species meets or exceeds the threshold, then done. if not, continue adding species etc. 

###OR randomly sample 80% of each species pollen in each iteration. the pollen taxa that 
#are in the 80% in that iteration are the dominant species for that bee species in that iteration.
#function to take an abundance vector and subsample to size

#Michael's functions to do the second thing:
subsam<-function(ab_vec, size=0.8*sum(ab_vec)){
  ab<-as.numeric(ab_vec)
  spp<-length(ab)
  l<-1:spp
  inds<-unlist(lapply(l, function(x){
    rep(x, ab[x])
  }))
  sam<-sample(inds, size=size, replace=F)
  ss<-unlist(lapply(1:spp, function(y){
    length(which(sam==y))
  }))
  return(ss)
}
?apply
#take a subset of a bunch of community vectors, each subset of equal size 
subcom<-function(com){
  t(apply(com, 1,subsam #function(y){
    #subsam(y)}
  ))
}

#then need to create accumulation of the dominant pollens identitified in the last step across species
#convert to tidy format and use approach from individual accumulation?
noID<-as.data.frame(B2comp_wide[,2:23])
subsam(noID[1,])
t<-length(noID[1,])
t
1:t
as.vector(noID[1,1:t])
lapply(1:t, function(x){
  rep(x, noID[1,x])
})
rep(1,B2comp_wide[1,1])
B2comp_wide[1,1]
domcomp<-subcom(noID)

first<-noID[1,]
as.numeric(first[1])
first[1]
