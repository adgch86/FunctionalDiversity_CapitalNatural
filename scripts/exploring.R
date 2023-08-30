##Automatically setting git in Rstudio
library(usethis)
use_git_config(user.name="adgch86", user.email="adgonzalez86@gmail.com")
create_github_token()
library(gitcreds)
gitcreds_set()

##########Aqui comienza
require(here)
f<-read.csv(here("data","flora.csv"), sep=",")
names(f)

b<-read.csv(here("data", "bees.csv"), sep=",")
head(b)
names(b)
a<-read.csv(here("data","birds.csv"), sep=",")
names(a)
butter<-read.csv(here("data" ,"buterflies.csv"), sep=",")
names(butter)
###Which are the location IDs, are the the same amount for each taxonomic group?
table(f$samplingLocationID) ## 14 stations
table(b$samplingLocationID) ##6 stations

###Lets apply a filter to Animal_data set to analyze only the plot for which we have forest data:
require(dplyr)
aaa<- b %>% filter(samplingLocationID %in%  f$samplingLocationID)
nrow(aaa)
table(aaa$samplingLocationID) ##What are the final locations

birds<- a %>% filter(samplingLocationID %in%  f$samplingLocationID)
nrow(birds)
table(birds$samplingLocationID) ##What are the final locations

bfs<- butter %>% filter(samplingLocationID %in%  f$samplingLocationID)
nrow(bfs)
table(bfs$samplingLocationID) ##What are the final locations

##Lets calculate total taxonomic richness for animals
##1st Preparing a table showing species abundance per site:
richness<-table(aaa$scientificName, aaa$samplingLocationID)
rich_birds<-table(birds$scientificName, birds$samplingLocationID)
rich_but<-table(butter$scientificName, butter$samplingLocationID)
##Transforming table output into a dataframe:
r<-as.data.frame.matrix(richness)
rbds<-as.data.frame.matrix(rich_birds)
rbuts<-as.data.frame.matrix(rich_but)

#Calculating richness and abundance per group:
##Bees (richness = sum of rows without 0):
rich_of_bees<-c(length(r[r$CN02!=0,1]),length(r[r$CN03!=0,2]),length(r[r$CN05!=0,3]),length(r[r$CN06!=0,4]),length(r[r$CN10!=0,5]),length(r[r$CN11!=0,6])) 
abund_of_bees<-c(sum(r[r$CN02!=0,1]), sum(r[r$CN03!=0,2]),sum(r[r$CN05!=0,3]), sum(r[r$CN06!=0,4]), sum(r[r$CN10!=0,5]),sum(r[r$CN11!=0,6]))
samplingLocationID<-c("CN02","CN03","CN05","CN06","CN10","CN11")

##Birds
rich_of_birds<-c(length(rbds[rbds$CN02!=0,1]),length(rbds[rbds$CN03!=0,2]),length(rbds[rbds$CN05!=0,3]),length(rbds[rbds$CN06!=0,4]),length(rbds[rbds$CN10!=0,5]),length(rbds[rbds$CN11!=0,6])) 
abund_of_birds<-c(sum(rbds[rbds$CN02!=0,1]), sum(rbds[rbds$CN03!=0,2]),sum(rbds[rbds$CN05!=0,3]), sum(rbds[rbds$CN06!=0,4]), sum(rbds[rbds$CN10!=0,5]),sum(rbds[rbds$CN11!=0,6]))
##Buterflies
rich_of_buterflies<-c(length(rbuts[rbuts$CN02!=0,1]),length(rbuts[rbuts$CN03!=0,2]),length(rbuts[rbuts$CN05!=0,3]),length(rbuts[rbuts$CN06!=0,4]),length(rbuts[rbuts$CN10!=0,5]),length(rbuts[rbuts$CN11!=0,6])) 
abund_of_buterflies<-c(sum(rbuts[rbuts$CN02!=0,1]), sum(rbuts[rbuts$CN03!=0,2]),sum(rbuts[rbuts$CN05!=0,3]), sum(rbuts[rbuts$CN06!=0,4]), sum(rbuts[rbuts$CN10!=0,5]),sum(rbuts[rbuts$CN11!=0,6]))
###Joining richness (and abundace) data:
animais<-data.frame(rich_of_bees, abund_of_bees,rich_of_birds, abund_of_birds,rich_of_buterflies, abund_of_buterflies, samplingLocationID)
head(animais)

##Calculating richness and abundance for Plants:
#1st) reorganizing data:
richnessF<-as.data.frame.matrix(table(f$scientificName, f$samplingLocationID))
#2nd) computing richness and abudance
rich_flora<-c(length(richnessF[richnessF$CN02!=0,1]),length(richnessF[richnessF$CN03!=0,2]),length(richnessF[richnessF$CN05!=0,3]),length(richnessF[richnessF$CN06!=0,4]),length(richnessF[richnessF$CN10!=0,5]),length(richnessF[richnessF$CN11!=0,6])) 
abund_flora<-c(sum(richnessF[richnessF$CN02!=0,1]), sum(richnessF[richnessF$CN03!=0,2]),sum(richnessF[richnessF$CN05!=0,3]), sum(richnessF[richnessF$CN06!=0,4]), sum(richnessF[richnessF$CN10!=0,5]),sum(richnessF[richnessF$CN11!=0,6]))
#Joining data with plot ID:
flora<-data.frame(rich_flora, abund_flora, samplingLocationID)
###Relating flora richness with forest with bee, bird and butterfly richness:
plot(flora$rich_flora, animais$rich_of_bees)
plot(flora$rich_flora, animais$rich_of_birds)
plot(animais$rich_flora, animais$rich_of_buterflies)
##Joining all data together
all<-data.frame(rich_flora, abund_flora,rich_of_bees, abund_of_bees,rich_of_birds, abund_of_birds,rich_of_buterflies, abund_of_buterflies, samplingLocationID)

###Calculating Richness per Functional trait Category of the Plants:
#Remembering the variables available for plant data:
names(f)
####Working with plantMelithophily 
#1st) Select only those species with value 1 == pollinated by bees
ff<-f %>% filter(plantMelithophily == 1)
#2nd) we create a table with the list of species and abundance per sampling site:
jj<-table(ff$scientificName,  ff$samplingLocationID)
#transforming into a data.frame to calculate species richness and abundance:
beePol<-as.data.frame.matrix(jj)
all$Fbeepol<-c(length(beePol[beePol$CN02!=0,1]),length(beePol[beePol$CN03!=0,2]),length(beePol[beePol$CN05!=0,3]),length(beePol[beePol$CN06!=0,4]),length(beePol[beePol$CN10!=0,5]),length(beePol[beePol$CN11!=0,6])) 
all$Fbeepol_abund<-c(sum(beePol[beePol$CN02!=0,1]), sum(beePol[beePol$CN03!=0,2]),sum(beePol[beePol$CN05!=0,3]), sum(beePol[beePol$CN06!=0,4]), sum(beePol[beePol$CN10!=0,5]),sum(beePol[beePol$CN11!=0,6]))

####Working with plantOrnitophily
#1st) Select only those species with value 1 == pollinated by birds
oo<-f %>% filter(plantOrnitophily == 1)
#2nd) we create a table with the list of species and abundance per sampling site:
jj<-table(oo$scientificName,  oo$samplingLocationID)
#transforming into a data.frame to calculate species richness and abundance:
birdPol<-as.data.frame.matrix(jj)
all$Fbirdpol<-c(length(birdPol[birdPol$CN02!=0,1]),length(birdPol[birdPol$CN03!=0,2]),length(birdPol[birdPol$CN05!=0,3]),length(birdPol[birdPol$CN06!=0,4]),length(birdPol[birdPol$CN10!=0,5]),length(birdPol[birdPol$CN11!=0,6])) 
all$Fbirdpol_abund<-c(sum(birdPol[birdPol$CN02!=0,1]), sum(birdPol[birdPol$CN03!=0,2]),sum(birdPol[birdPol$CN05!=0,3]), sum(birdPol[birdPol$CN06!=0,4]), sum(birdPol[birdPol$CN10!=0,5]),sum(birdPol[birdPol$CN11!=0,6]))

###Working with plantOrnitochory -> Plants dispersed by birds
#1st) Select only those species with value 1 == with seeds dispersed by birds
op<-f %>% filter(plantOrnitophily == 1)
#2nd) we create a table with the list of species and abundance per sampling site:
jj<-table(op$scientificName,  op$samplingLocationID)
#transforming into a data.frame to calculate species richness and abundance:
birdSD<-as.data.frame.matrix(jj)
all$Fbirdsd<-c(length(birdSD[birdSD$CN02!=0,1]),length(birdSD[birdSD$CN03!=0,2]),length(birdSD[birdSD$CN05!=0,3]),length(birdSD[birdSD$CN06!=0,4]),length(birdSD[birdSD$CN10!=0,5]),length(birdSD[birdSD$CN11!=0,6])) 
all$Fbirdsd_abund<-c(sum(birdSD[birdSD$CN02!=0,1]), sum(birdSD[birdSD$CN03!=0,2]),sum(birdSD[birdSD$CN05!=0,3]), sum(birdSD[birdSD$CN06!=0,4]), sum(birdSD[birdSD$CN10!=0,5]),sum(birdSD[birdSD$CN11!=0,6]))


####Working with plantPhalaenophily
#1st) Select only those species with value 1 == pollinated by butterflies
pp<-f %>% filter(plantPhalaenophily == 1)
#2nd) we create a table with the list of species and abundance per sampling site:
jj<-table(pp$scientificName,  pp$samplingLocationID)
#transforming into a data.frame to calculate species richness and abundance of 
#plantas pollinated by butterflies
butPol<-as.data.frame.matrix(jj)
all$Fbutpol<-c(length(butPol[butPol$CN02!=0,1]),length(butPol[butPol$CN03!=0,2]),length(butPol[butPol$CN05!=0,3]),length(butPol[butPol$CN06!=0,4]),length(butPol[butPol$CN10!=0,5]),length(butPol[butPol$CN11!=0,6])) 
all$Fbutpol_abund<-c(sum(butPol[butPol$CN02!=0,1]), sum(butPol[butPol$CN03!=0,2]),sum(butPol[butPol$CN05!=0,3]), sum(butPol[butPol$CN06!=0,4]), sum(butPol[butPol$CN10!=0,5]),sum(butPol[butPol$CN11!=0,6]))

####Calculating continuos funtional trais of plants:
##DBH: Diameter at Breast Height -> mean, sd and max values calculated
DBH<-aggregate(plantDBH~samplingLocationID, data=f, FUN=function(x) c(mean=mean(x), sd=sd(x), max=max(x)))
all$plantDBH.mean<-DBH$plantDBH[,1]
all$plantDBH.sd<-DBH$plantDBH[,2]
all$plantDBH.max<-DBH$plantDBH[,3]

##DBH of pollinator dependent plants
#1st) Select only those species with value 1 == pollinated by bees
ff<-f %>% filter(plantMelithophily == 1)
DBH_pol<-aggregate(plantDBH~samplingLocationID, data=ff, FUN=function(x) c(mean=mean(x)))
all$plantDBH_pol<-DBH_pol[,2]
#Looking at DBH of plants pollinated by Birds:
oo<-f %>% filter(plantOrnitophily == 1)
DBH_polB<-aggregate(plantDBH~samplingLocationID, data=oo, FUN=function(x) c(mean=mean(x)))
all$plantDBH_polB<-DBH_polB[,2]
#Looking at DBH of plants pollinated by Butterflies:
pp<-f %>% filter(plantPhalaenophily == 1)
DBH_polM<-aggregate(plantDBH~samplingLocationID, data=pp, FUN=function(x) c(mean=mean(x)))
all$plantDBH_polM<-DBH_polM[,2]
#Looking at DBH of plants with seeds dispersed by Birds:
op<-f %>% filter(plantOrnitochory == 1)
DBH_seeD_birds<-aggregate(plantDBH~samplingLocationID, data=op, FUN=function(x) c(mean=mean(x)))
all$plantDBH_seeD_birds<-DBH_seeD_birds[,2]


###Wood Density 
wooDensity<-aggregate(plantWoodDensity~samplingLocationID+plantLifeForm, data=f, FUN=function(x) c(mean=mean(x)))
woo<- wooDensity %>% filter(plantLifeForm=="tree")
all$plantWoodDensity<-round(woo$plantWoodDensity,2)
#Wood density of bee pollinator dependent plants
woodDensity_pol<-aggregate(plantWoodDensity~samplingLocationID, data=ff, FUN=function(x) c(mean=mean(x)))
all$plantWoodDensity_pol<-round(woodDensity_pol[,2],2)
#Wood density of bird pollinator dependent plants
woodDensity_polB<-aggregate(plantWoodDensity~samplingLocationID, data=oo, FUN=function(x) c(mean=mean(x)))
all$plantWoodDensity_polB<-round(woodDensity_polB[,2],2)
#Wood density of butterflies pollinator dependent plants
woodDensity_polM<-aggregate(plantWoodDensity~samplingLocationID, data=pp, FUN=function(x) c(mean=mean(x)))
all$plantWoodDensity_polM<-round(woodDensity_polM[,2],2)
#Wood density plants with seeds dispersed by birds:
woodDensity_seeDbirds<-aggregate(plantWoodDensity~samplingLocationID, data=op, FUN=function(x) c(mean=mean(x)))
all$plantWoodDensity_seeDbirds<-round(woodDensity_seeDbirds[,2],2)
all


###List of threatened species:
############ Aggregating categories: 
## NoData= DD, data deficient (9) + NE, not evaluated (659)
## NotEndangared= LC, least concern (1544) + NT, near threathened (11)
## Threat= EN, endangered (16) + VU, vulnerable (39) + NT, near threathened (11)

#1st) Calculating speciess richness and abundance of the threat category:
threattt<-f %>% filter(categoryThreatIUCN == "EN" |categoryThreatIUCN == "VU" |categoryThreatIUCN == "NT" )
nrow(threattt)
##Tansforming species data on table with species abudance per site:
amea<-as.data.frame.matrix(table(threattt$scientificName, threattt$samplingLocationID))
##Calculating richnnes and abudance of threathened species (EN+VU+NT)
all$Fthreat<-c(length(amea[amea$CN02!=0,1]),length(amea[amea$CN03!=0,2]),length(amea[amea$CN05!=0,3]),length(amea[amea$CN06!=0,4]),length(amea[amea$CN10!=0,5]),length(amea[amea$CN11!=0,6])) 
all$Fthreat_abund<-c(sum(amea[amea$CN02!=0,1]), sum(amea[amea$CN03!=0,2]),sum(amea[amea$CN05!=0,3]), sum(amea[amea$CN06!=0,4]), sum(amea[amea$CN10!=0,5]),sum(amea[amea$CN11!=0,6]))
head(all)


####Calculate other traits per animal species to use as response variable:
#a) For bees: ITD , sociality, Nest type (renters)
ITD<-aggregate(beeITDist~samplingLocationID, data=aaa, FUN=function(x) c(mean=mean(x)))
all$ITD<-round(ITD[,2],2)

###Instead of looking to how the richness of bee that nest in cavities respond ("renters")
table(aaa$beeNesting); table(aaa$beeNestLoc)
##We select only bee species classified as renter
renters<-aaa %>% filter(beeNestLoc=="renter")
nrow(renters)
rentersB<-as.data.frame.matrix(table(renters$scientificName, renters$samplingLocationID)) ##calculate adundance per species
##transform to data.frame to calculate richness and abundance
all$rentersB<-c(length(rentersB[rentersB$CN02!=0,1]),length(rentersB[rentersB$CN03!=0,2]),length(rentersB[rentersB$CN05!=0,3]),length(rentersB[rentersB$CN06!=0,4]),length(rentersB[rentersB$CN10!=0,5]),length(rentersB[rentersB$CN11!=0,6])) 
all$rentersB_abund<-c(sum(rentersB[rentersB$CN02!=0,1]), sum(rentersB[rentersB$CN03!=0,2]),sum(rentersB[rentersB$CN05!=0,3]), sum(rentersB[rentersB$CN06!=0,4]), sum(rentersB[rentersB$CN10!=0,5]),sum(rentersB[rentersB$CN11!=0,6]))
###Calculating ITD dos renters:
ITD_renters<-aggregate(beeITDist~samplingLocationID, data=renters, FUN=function(x) c(mean=mean(x)))
all$ITD_renters<-round(ITD_renters[,2],2)
plot(all$ITD, all$ITD_renters)

###Sociality (richness of eusocial and solitary bees):
table(aaa$beeSociality)
social<-aaa %>% filter(beeSociality=="Social"); nrow(social) #458
solitary<-aaa %>% filter(beeSociality=="solitary" |beeSociality=="Solitary"); nrow(solitary) #69
solitary2<-aaa %>% 
  filter(beeSociality=="solitary" |beeSociality=="Solitary"|beeSociality=="semi_social"); nrow(solitary2) #274
##for Eusocial species (stingless bees!)
sociality<-as.data.frame.matrix(table(social$scientificName, social$samplingLocationID))
all$sociality<-c(length(sociality[sociality$CN02!=0,1]),length(sociality[sociality$CN03!=0,2]),length(sociality[sociality$CN05!=0,3]),length(sociality[sociality$CN06!=0,4]),length(sociality[sociality$CN10!=0,5]),length(sociality[sociality$CN11!=0,6])) 
solitarity<-as.data.frame.matrix(table(solitary$scientificName, solitary$samplingLocationID))
solitarity2<-as.data.frame.matrix(table(solitary2$scientificName, solitary2$samplingLocationID))
all$solitarity<-c(length(solitarity[solitarity$CN02!=0,1]),length(solitarity[solitarity$CN03!=0,2]),length(solitarity[solitarity$CN05!=0,3]),length(solitarity[solitarity$CN06!=0,4]),length(solitarity[solitarity$CN10!=0,5]),length(solitarity[solitarity$CN11!=0,6])) 
all$solitarity2<-c(length(solitarity2[solitarity2$CN02!=0,1]),length(solitarity2[solitarity2$CN03!=0,2]),length(solitarity2[solitarity2$CN05!=0,3]),length(solitarity2[solitarity2$CN06!=0,4]),length(solitarity2[solitarity2$CN10!=0,5]),length(solitarity2[solitarity2$CN11!=0,6])) 
###Bee size for each sociality category:
#Eusocial:
ITD_social<-aggregate(beeITDist~samplingLocationID, data=social, FUN=function(x) c(mean=mean(x)))
all$ITD_social<-round(ITD_social[,2],2)
#Only solitary species:
ITD_solitary<-aggregate(beeITDist~samplingLocationID, data=solitary, FUN=function(x) c(mean=mean(x)))
all$ITD_solitary<-round(ITD_solitary[,2],2)
##Solitary and semi_social (solitary2):
ITD_solitary2<-aggregate(beeITDist~samplingLocationID, data=solitary2, FUN=function(x) c(mean=mean(x)))
all$ITD_solitary2<-round(ITD_solitary2[,2],2)
names(all)

#b) Bird size:
names(birds)
#Categorical:
 table(birds$categoryThreatIUCN); table(birds$birdCanopyForStrat)
ameaçadas<-birds %>% filter(categoryThreatIUCN=="EN"|categoryThreatIUCN=="NT"|categoryThreatIUCN=="VU")
  nrow(ameaçadas)
threat_birds<-as.data.frame.matrix(table(ameaçadas$scientificName, ameaçadas$samplingLocationID))  
all$rich_of_birds_threat<- c(length(threat_birds[threat_birds$CN02!=0,1]),length(threat_birds[threat_birds$CN03!=0,2]),length(threat_birds[threat_birds$CN05!=0,3]),length(threat_birds[threat_birds$CN06!=0,4]),length(threat_birds[threat_birds$CN10!=0,5]),length(threat_birds[threat_birds$CN11!=0,6])) 
  #Continuous:
##Hand Wing Index
summary(birds$handWingIndex) ##434 NAs
BHWI<-aggregate(handWingIndex~samplingLocationID, data=birds, FUN=function(x) c(mean=mean(x)))
all$Bird_handWingIndex<-round(BHWI[,2],2)  
##Bird Body Mass (log)
summary(birds$birdBodyMassLog) #434 NAs
BBM<-aggregate(birdBodyMassLog~samplingLocationID, data=birds, FUN=function(x) c(mean=mean(x)))
all$birdBodyMassLog<-round(BBM[,2],2)  
##Body mass of threathened species 
Birdhreat<-aggregate(birdBodyMassLog~samplingLocationID, data=ameaçadas, FUN=function(x) c(mean=mean(x)))
all$birdBodyMassLog_threat<-round(Birdhreat[,2],2)
##Richness of species with Fruit Diet:
table(birds$birdFruitDiet)
##We select only bird species classified as having fruit diet:
Fruties<-birds %>% filter(birdFruitDiet!= 0); nrow(Fruties)
FrutiesB<-as.data.frame.matrix(table(Fruties$scientificName, Fruties$samplingLocationID)) ##calculate adundance per species
 # .... and transform to data.frame to calculate richness and abundance
all$FrutiesB<-c(length(FrutiesB[FrutiesB$CN02!=0,1]),length(FrutiesB[FrutiesB$CN03!=0,2]),length(FrutiesB[FrutiesB$CN05!=0,3]),length(FrutiesB[FrutiesB$CN06!=0,4]),length(FrutiesB[FrutiesB$CN10!=0,5]),length(FrutiesB[FrutiesB$CN11!=0,6])) 


#c) Butterfly size 



##Building model selection:
require(MASS)
require(MuMIn)
ojo<-glm(rich_of_bees ~ rich_flora,family="poisson", data=all)
summary(ojo)
require(visreg)

A<-list()
A[[1]]<-glm(rich_of_bees ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
A[[2]]<-glm(rich_of_bees ~ rich_flora,family="poisson", data=all)
A[[3]]<-glm(rich_of_bees ~ Fbeepol, family="poisson",data=all)
A[[4]]<-glm(rich_of_bees ~ Fbeepol_abund, family="poisson", data=all)
A[[5]]<-glm(rich_of_bees ~ Fbirdpol, family="poisson", data=all)
A[[6]]<-glm(rich_of_bees ~ Fbirdpol_abund, family="poisson", data=all)
A[[7]]<-glm(rich_of_bees ~ Fbutpol, family="poisson", data=all)
A[[8]]<-glm(rich_of_bees ~ Fbutpol_abund, family="poisson", data=all)
A[[9]]<-glm(rich_of_bees ~ Fbirdsd, family="poisson", data=all)
A[[10]]<-glm(rich_of_bees ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
A[[11]]<-glm(rich_of_bees ~ plantDBH.mean, family="poisson", data=all)
A[[12]]<-glm(rich_of_bees ~ plantDBH.max, family="poisson", data=all)
A[[13]]<-glm(rich_of_bees ~ plantDBH_pol, family="poisson", data=all)
A[[14]]<-glm(rich_of_bees ~ plantDBH_polB, family="poisson", data=all)
A[[15]]<-glm(rich_of_bees ~ plantDBH_polM, family="poisson", data=all)
A[[16]]<-glm(rich_of_bees ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
A[[17]]<-glm(rich_of_bees ~ plantWoodDensity, family="poisson", data=all)
A[[18]]<-glm(rich_of_bees ~ plantWoodDensity_pol, family="poisson", data=all)
A[[19]]<-glm(rich_of_bees ~ plantWoodDensity_polB, family="poisson", data=all)
A[[20]]<-glm(rich_of_bees ~ plantWoodDensity_polM, family="poisson", data=all)
A[[21]]<-glm(rich_of_bees ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
A[[22]]<-glm(rich_of_bees ~ Fthreat, family="poisson", data=all)
A[[23]]<-glm(rich_of_bees ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(A, rank="AICc"),7) #Best: average DBH of the plant community 
#################  

Ar<-list()
Ar[[1]]<-glm(rentersB ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
Ar[[2]]<-glm(rentersB ~ rich_flora,family="poisson", data=all)
Ar[[3]]<-glm(rentersB ~ Fbeepol, family="poisson",data=all)
Ar[[4]]<-glm(rentersB ~ Fbeepol_abund, family="poisson", data=all)
Ar[[5]]<-glm(rentersB ~ Fbirdpol, family="poisson", data=all)
Ar[[6]]<-glm(rentersB ~ Fbirdpol_abund, family="poisson", data=all)
Ar[[7]]<-glm(rentersB ~ Fbutpol, family="poisson", data=all)
Ar[[8]]<-glm(rentersB ~ Fbutpol_abund, family="poisson", data=all)
Ar[[9]]<-glm(rentersB ~ Fbirdsd, family="poisson", data=all)
Ar[[10]]<-glm(rentersB ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
Ar[[11]]<-glm(rentersB ~ plantDBH.mean, family="poisson", data=all)
Ar[[12]]<-glm(rentersB ~ plantDBH.max, family="poisson", data=all)
Ar[[13]]<-glm(rentersB ~ plantDBH_pol, family="poisson", data=all)
Ar[[14]]<-glm(rentersB ~ plantDBH_polB, family="poisson", data=all)
Ar[[15]]<-glm(rentersB ~ plantDBH_polM, family="poisson", data=all)
Ar[[16]]<-glm(rentersB ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
Ar[[17]]<-glm(rentersB ~ plantWoodDensity, family="poisson", data=all)
Ar[[18]]<-glm(rentersB ~ plantWoodDensity_pol, family="poisson", data=all)
Ar[[19]]<-glm(rentersB ~ plantWoodDensity_polB, family="poisson", data=all)
Ar[[20]]<-glm(rentersB ~ plantWoodDensity_polM, family="poisson", data=all)
Ar[[21]]<-glm(rentersB ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
Ar[[22]]<-glm(rentersB ~ Fthreat, family="poisson", data=all)
Ar[[23]]<-glm(rentersB ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(Ar, rank="AICc"),7) #Best: average DBH of the plant community, wood density and richnes of threathened species 
#################  

Euso<-list()
Euso[[1]]<-glm(sociality ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
Euso[[2]]<-glm(sociality ~ rich_flora,family="poisson", data=all)
Euso[[3]]<-glm(sociality ~ Fbeepol, family="poisson",data=all)
Euso[[4]]<-glm(sociality ~ Fbeepol_abund, family="poisson", data=all)
Euso[[5]]<-glm(sociality ~ Fbirdpol, family="poisson", data=all)
Euso[[6]]<-glm(sociality ~ Fbirdpol_abund, family="poisson", data=all)
Euso[[7]]<-glm(sociality ~ Fbutpol, family="poisson", data=all)
Euso[[8]]<-glm(sociality ~ Fbutpol_abund, family="poisson", data=all)
Euso[[9]]<-glm(sociality ~ Fbirdsd, family="poisson", data=all)
Euso[[10]]<-glm(sociality ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
Euso[[11]]<-glm(sociality ~ plantDBH.mean, family="poisson", data=all)
Euso[[12]]<-glm(sociality ~ plantDBH.max, family="poisson", data=all)
Euso[[13]]<-glm(sociality ~ plantDBH_pol, family="poisson", data=all)
Euso[[14]]<-glm(sociality ~ plantDBH_polB, family="poisson", data=all)
Euso[[15]]<-glm(sociality ~ plantDBH_polM, family="poisson", data=all)
Euso[[16]]<-glm(sociality ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
Euso[[17]]<-glm(sociality ~ plantWoodDensity, family="poisson", data=all)
Euso[[18]]<-glm(sociality ~ plantWoodDensity_pol, family="poisson", data=all)
Euso[[19]]<-glm(sociality ~ plantWoodDensity_polB, family="poisson", data=all)
Euso[[20]]<-glm(sociality ~ plantWoodDensity_polM, family="poisson", data=all)
Euso[[21]]<-glm(sociality ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
Euso[[22]]<-glm(sociality ~ Fthreat, family="poisson", data=all)
Euso[[23]]<-glm(sociality ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(Euso, rank="AICc"),7) #Best: average DBH of the plant community, wood density and richnes of threathened species 
#################  


soli<-list()
soli[[1]]<-glm(solitarity ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
soli[[2]]<-glm(solitarity ~ rich_flora,family="poisson", data=all)
soli[[3]]<-glm(solitarity ~ Fbeepol, family="poisson",data=all)
soli[[4]]<-glm(solitarity ~ Fbeepol_abund, family="poisson", data=all)
soli[[5]]<-glm(solitarity ~ Fbirdpol, family="poisson", data=all)
soli[[6]]<-glm(solitarity ~ Fbirdpol_abund, family="poisson", data=all)
soli[[7]]<-glm(solitarity ~ Fbutpol, family="poisson", data=all)
soli[[8]]<-glm(solitarity ~ Fbutpol_abund, family="poisson", data=all)
soli[[9]]<-glm(solitarity ~ Fbirdsd, family="poisson", data=all)
soli[[10]]<-glm(solitarity ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
soli[[11]]<-glm(solitarity ~ plantDBH.mean, family="poisson", data=all)
soli[[12]]<-glm(solitarity ~ plantDBH.max, family="poisson", data=all)
soli[[13]]<-glm(solitarity ~ plantDBH_pol, family="poisson", data=all)
soli[[14]]<-glm(solitarity ~ plantDBH_polB, family="poisson", data=all)
soli[[15]]<-glm(solitarity ~ plantDBH_polM, family="poisson", data=all)
soli[[16]]<-glm(solitarity ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
soli[[17]]<-glm(solitarity ~ plantWoodDensity, family="poisson", data=all)
soli[[18]]<-glm(solitarity ~ plantWoodDensity_pol, family="poisson", data=all)
soli[[19]]<-glm(solitarity ~ plantWoodDensity_polB, family="poisson", data=all)
soli[[20]]<-glm(solitarity ~ plantWoodDensity_polM, family="poisson", data=all)
soli[[21]]<-glm(solitarity ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
soli[[22]]<-glm(solitarity ~ Fthreat, family="poisson", data=all)
soli[[23]]<-glm(solitarity ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(soli, rank="AICc"),7) #Best: average DBH of the plant community, wood density and richnes of threathened species 
#################  

soli2<-list()
soli2[[1]]<-glm(solitarity2 ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
soli2[[2]]<-glm(solitarity2 ~ rich_flora,family="poisson", data=all)
soli2[[3]]<-glm(solitarity2 ~ Fbeepol, family="poisson",data=all)
soli2[[4]]<-glm(solitarity2 ~ Fbeepol_abund, family="poisson", data=all)
soli2[[5]]<-glm(solitarity2 ~ Fbirdpol, family="poisson", data=all)
soli2[[6]]<-glm(solitarity2 ~ Fbirdpol_abund, family="poisson", data=all)
soli2[[7]]<-glm(solitarity2 ~ Fbutpol, family="poisson", data=all)
soli2[[8]]<-glm(solitarity2 ~ Fbutpol_abund, family="poisson", data=all)
soli2[[9]]<-glm(solitarity2 ~ Fbirdsd, family="poisson", data=all)
soli2[[10]]<-glm(solitarity2 ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
soli2[[11]]<-glm(solitarity2 ~ plantDBH.mean, family="poisson", data=all)
soli2[[12]]<-glm(solitarity2 ~ plantDBH.max, family="poisson", data=all)
soli2[[13]]<-glm(solitarity2 ~ plantDBH_pol, family="poisson", data=all)
soli2[[14]]<-glm(solitarity2 ~ plantDBH_polB, family="poisson", data=all)
soli2[[15]]<-glm(solitarity2 ~ plantDBH_polM, family="poisson", data=all)
soli2[[16]]<-glm(solitarity2 ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
soli2[[17]]<-glm(solitarity2 ~ plantWoodDensity, family="poisson", data=all)
soli2[[18]]<-glm(solitarity2 ~ plantWoodDensity_pol, family="poisson", data=all)
soli2[[19]]<-glm(solitarity2 ~ plantWoodDensity_polB, family="poisson", data=all)
soli2[[20]]<-glm(solitarity2 ~ plantWoodDensity_polM, family="poisson", data=all)
soli2[[21]]<-glm(solitarity2 ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
soli2[[22]]<-glm(solitarity2 ~ Fthreat, family="poisson", data=all)
soli2[[23]]<-glm(solitarity2 ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(soli2, rank="AICc"),7) #Best: average DBH of the plant community, wood density and richnes of threathened species 
#############

b_itd<-list()
b_itd[[1]]<-lm(ITD_solitary2 ~ 1,  data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
b_itd[[2]]<-lm(ITD_solitary2 ~ rich_flora, data=all)
b_itd[[3]]<-lm(ITD_solitary2 ~ Fbeepol, data=all)
b_itd[[4]]<-lm(ITD_solitary2 ~ Fbeepol_abund, data=all)
b_itd[[5]]<-lm(ITD_solitary2 ~ Fbirdpol, data=all)
b_itd[[6]]<-lm(ITD_solitary2 ~ Fbirdpol_abund, data=all)
b_itd[[7]]<-lm(ITD_solitary2 ~ Fbutpol, data=all)
b_itd[[8]]<-lm(ITD_solitary2 ~ Fbutpol_abund, data=all)
b_itd[[9]]<-lm(ITD_solitary2 ~ Fbirdsd, data=all)
b_itd[[10]]<-lm(ITD_solitary2 ~ Fbirdsd_abund, data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
b_itd[[11]]<-lm(ITD_solitary2 ~ plantDBH.mean, data=all)
b_itd[[12]]<-lm(ITD_solitary2 ~ plantDBH.max, data=all)
b_itd[[13]]<-lm(ITD_solitary2 ~ plantDBH_pol, data=all)
b_itd[[14]]<-lm(ITD_solitary2 ~ plantDBH_polB, data=all)
b_itd[[15]]<-lm(ITD_solitary2 ~ plantDBH_polM, data=all)
b_itd[[16]]<-lm(ITD_solitary2 ~ plantDBH_seeD_birds, data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
b_itd[[17]]<-lm(ITD_solitary2 ~ plantWoodDensity, data=all)
b_itd[[18]]<-lm(ITD_solitary2 ~ plantWoodDensity_pol, data=all)
b_itd[[19]]<-lm(ITD_solitary2 ~ plantWoodDensity_polB, data=all)
b_itd[[20]]<-lm(ITD_solitary2 ~ plantWoodDensity_polM, data=all)
b_itd[[21]]<-lm(ITD_solitary2 ~ plantWoodDensity_seeDbirds, data=all)
### Richness and abundance of Threathened tree species:
b_itd[[22]]<-lm(ITD_solitary2 ~ Fthreat, data=all)
b_itd[[23]]<-lm(ITD_solitary2 ~ Fthreat_abund, data=all)
head(model.sel(b_itd, rank="AICc"),7) #Best: snull model
#null model: is the best
hist(all$ITD_)




A<-list()
A[[1]]<-glm(rich_of_bees ~ 1, family="poisson", data=all)
A[[2]]<-glm(rich_of_bees ~ rich_flora,family="poisson", data=all)
A[[3]]<-glm(rich_of_bees ~ Fbeepol, family="poisson",data=all)
A[[4]]<-glm(rich_of_bees ~ Fbeepol_abund, family="poisson", data=all)
A[[5]]<-glm(rich_of_bees ~ plantDBH.mean, family="poisson", data=all)
A[[6]]<-glm(rich_of_bees ~ plantDBH.max, family="poisson", data=all)
A[[7]]<-glm(rich_of_bees ~ plantDBH_pol, family="poisson", data=all)
A[[8]]<-glm(rich_of_bees ~ plantWoodDensity, family="poisson", data=all)
A[[9]]<-glm(rich_of_bees ~ plantWoodDensity_pol, family="poisson", data=all)
A[[10]]<-glm(rich_of_bees ~ FRic, family="poisson", data=all)#o calculo esta abaixo
A[[11]]<-glm(rich_of_bees ~ FRic_all, family="poisson", data=all)#o calculo esta abaixo
head(model.sel(A, rank="AICc"),7)

visreg(A[[7]], "plantDBH_pol", xlab="Mean plant DBH of pollinator dependent plants",
       ylab="Bees species richness",scale="response", rug=T, line=list(col="red"))+
theme_bw()+geom_point(size=3,alpha=0.1,shape=16)

visreg(A[[9]], "plantWoodDensity_pol", xlab="Mean plant wood density of pollinator dependent plants",
       ylab="Bees species richness",scale="response", rug=T, line=list(col="red"))

visreg(A[[4]], "Fbeepol_abund", xlab="Abudance of plants pollinated by bees",
       ylab="Bees species richness",scale="response", rug=T, line=list(col="red"))
visreg(A[[8]], "plantWoodDensity", xlab="Mean plant wood density",
       ylab="Bees species richness",scale="response", rug=T, line=list(col="red"))



Anb<-list()
Anb[[1]]<-glm.nb(rich_of_bees ~ 1,  data=all)
Anb[[2]]<-glm.nb(rich_of_bees ~ rich_flora, data=all)
Anb[[3]]<-glm.nb(rich_of_bees ~ Fbeepol, data=all)
Anb[[4]]<-glm.nb(rich_of_bees ~ Fbeepol_abund, data=all)
Anb[[5]]<-glm.nb(rich_of_bees ~ plantDBH.mean, data=all)
Anb[[6]]<-glm.nb(rich_of_bees ~ plantDBH.sd, data=all)
Anb[[7]]<-glm.nb(rich_of_bees ~ plantDBH.max,  data=all)
Anb[[8]]<-glm.nb(rich_of_bees ~ plantWoodDensity,  data=all)
model.sel(Anb, rank="AICc")


###Instead of looking to how the richness of bee that nest in cavities respond ("renters")
levels(as.factor(aaa$samplingLocationID))
table(aaa$beeNesting)
table(aaa$beeNestLoc)
##We select only bee species classified as renter
renters<-aaa %>% filter(beeNestLoc=="renter")
nrow(renters)
jj<-table(renters$scientificName, renters$samplingLocationID) ##calculate adundance per species
rentersB<-as.data.frame.matrix(jj) #transform to data.frame to calculate richness and abundance
all$rentersB<-c(length(rentersB[rentersB$CN02!=0,1]),length(rentersB[rentersB$CN03!=0,2]),length(rentersB[rentersB$CN05!=0,3]),length(rentersB[rentersB$CN06!=0,4]),length(rentersB[rentersB$CN10!=0,5]),length(rentersB[rentersB$CN11!=0,6])) 
all$rentersB_abund<-c(sum(rentersB[rentersB$CN02!=0,1]), sum(rentersB[rentersB$CN03!=0,2]),sum(rentersB[rentersB$CN05!=0,3]), sum(rentersB[rentersB$CN06!=0,4]), sum(rentersB[rentersB$CN10!=0,5]),sum(rentersB[rentersB$CN11!=0,6]))


R<-list()
R[[1]]<-glm(rentersB ~ 1, family="poisson", data=all)
R[[2]]<-glm(rentersB ~ rich_flora,family="poisson", data=all)
R[[3]]<-glm(rentersB ~ Fbeepol, family="poisson",data=all)
R[[4]]<-glm(rentersB ~ Fbeepol_abund, family="poisson", data=all)
R[[5]]<-glm(rentersB ~ plantDBH.mean, family="poisson", data=all)
R[[6]]<-glm(rentersB ~ plantDBH.sd, family="poisson", data=all)
R[[7]]<-glm(rentersB ~ plantDBH.max, family="poisson", data=all)
R[[8]]<-glm(rentersB ~ plantWoodDensity, family="poisson", data=all)
head(model.sel(R, rank="AICc"),2)

visreg(R[[8]], "plantWoodDensity", xlab="Mean plant wood density", scale="response", 
       ylab="Cavity renters nesting bee richness", rug=T, line=list(col="red"))

plot(all$rentersB ~ all$plantWoodDensity)


#############Comparing functional traits:
#For the bees:
nestLoc<-table(aaa$beeNestLoc, aaa$samplingLocationID)
barplot(nestLoc, legend.text = T, xlab="location ID", ylab="Frequency")
##For the Flora:
names(f)
##PLants pollinated by bees:
polSyn<-table(f$plantMelithophily, f$samplingLocationID)
barplot(polSyn, legend.text = T, xlab="location ID", ylab="Frequency of plants sps. pollinated by bees")
##PLant live forms : treee , palm and vines
plantLF<-table(f$plantLifeForm, f$samplingLocationID)
barplot(plantLF, legend.text = T, xlab="location ID", ylab="Frequency plants sps. per life form")

### 1)Consider translocation these tables (polSyn and PlantLF) to add them to the kk data.frame
### 2) Calculate richness of trees that depend on bees for pollination
### 3) Same can be done for richness of trees that depend on seed dispersal and the richness of birds
### 4) consider calculating abudance of trees that benefit form pollination and seed dispersals, 
### eventually dominance coudl be calculated as well

B<-list()
B[[1]]<-glm(rich_of_birds ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
B[[2]]<-glm(rich_of_birds ~ rich_flora,family="poisson", data=all)
B[[3]]<-glm(rich_of_birds ~ Fbeepol, family="poisson",data=all)
B[[4]]<-glm(rich_of_birds ~ Fbeepol_abund, family="poisson", data=all)
B[[5]]<-glm(rich_of_birds ~ Fbirdpol, family="poisson", data=all)
B[[6]]<-glm(rich_of_birds ~ Fbirdpol_abund, family="poisson", data=all)
B[[7]]<-glm(rich_of_birds ~ Fbutpol, family="poisson", data=all)
B[[8]]<-glm(rich_of_birds ~ Fbutpol_abund, family="poisson", data=all)
B[[9]]<-glm(rich_of_birds ~ Fbirdsd, family="poisson", data=all)
B[[10]]<-glm(rich_of_birds ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
B[[11]]<-glm(rich_of_birds ~ plantDBH.mean, family="poisson", data=all)
B[[12]]<-glm(rich_of_birds ~ plantDBH.max, family="poisson", data=all)
B[[13]]<-glm(rich_of_birds ~ plantDBH_pol, family="poisson", data=all)
B[[14]]<-glm(rich_of_birds ~ plantDBH_polB, family="poisson", data=all)
B[[15]]<-glm(rich_of_birds ~ plantDBH_polM, family="poisson", data=all)
B[[16]]<-glm(rich_of_birds ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
B[[17]]<-glm(rich_of_birds ~ plantWoodDensity, family="poisson", data=all)
B[[18]]<-glm(rich_of_birds ~ plantWoodDensity_pol, family="poisson", data=all)
B[[19]]<-glm(rich_of_birds ~ plantWoodDensity_polB, family="poisson", data=all)
B[[20]]<-glm(rich_of_birds ~ plantWoodDensity_polM, family="poisson", data=all)
B[[21]]<-glm(rich_of_birds ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
B[[22]]<-glm(rich_of_birds ~ Fthreat, family="poisson", data=all)
B[[23]]<-glm(rich_of_birds ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(B, rank="AICc"),7) #Best: average DBH of the plant community 
#################                                 that is pollinated by butterflies  B[[15]]

visreg(B[[15]], "plantDBH_polM" , scale="response")
visreg(B[[11]], "plantDBH.mean" , scale="response")
visreg(B[[13]], "plantDBH_pol" , scale="response")
plot(all$plantDBH.mean, all$rich_of_birds)

##Bird functional traits as response:
#1st) Richness of birds considered as threathened (NT, VU and EN)
Bt<-list()
Bt[[1]]<-glm(rich_of_birds_threat ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
Bt[[2]]<-glm(rich_of_birds_threat ~ rich_flora,family="poisson", data=all)
Bt[[3]]<-glm(rich_of_birds_threat ~ Fbeepol, family="poisson",data=all)
Bt[[4]]<-glm(rich_of_birds_threat ~ Fbeepol_abund, family="poisson", data=all)
Bt[[5]]<-glm(rich_of_birds_threat ~ Fbirdpol, family="poisson", data=all)
Bt[[6]]<-glm(rich_of_birds_threat ~ Fbirdpol_abund, family="poisson", data=all)
Bt[[7]]<-glm(rich_of_birds_threat ~ Fbutpol, family="poisson", data=all)
Bt[[8]]<-glm(rich_of_birds_threat ~ Fbutpol_abund, family="poisson", data=all)
Bt[[9]]<-glm(rich_of_birds_threat ~ Fbirdsd, family="poisson", data=all)
Bt[[10]]<-glm(rich_of_birds_threat ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
Bt[[11]]<-glm(rich_of_birds_threat ~ plantDBH.mean, family="poisson", data=all)
Bt[[12]]<-glm(rich_of_birds_threat ~ plantDBH.max, family="poisson", data=all)
Bt[[13]]<-glm(rich_of_birds_threat ~ plantDBH_pol, family="poisson", data=all)
Bt[[14]]<-glm(rich_of_birds_threat ~ plantDBH_polB, family="poisson", data=all)
Bt[[15]]<-glm(rich_of_birds_threat ~ plantDBH_polM, family="poisson", data=all)
Bt[[16]]<-glm(rich_of_birds_threat ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
Bt[[17]]<-glm(rich_of_birds_threat ~ plantWoodDensity, family="poisson", data=all)
Bt[[18]]<-glm(rich_of_birds_threat ~ plantWoodDensity_pol, family="poisson", data=all)
Bt[[19]]<-glm(rich_of_birds_threat ~ plantWoodDensity_polB, family="poisson", data=all)
Bt[[20]]<-glm(rich_of_birds_threat ~ plantWoodDensity_polM, family="poisson", data=all)
Bt[[21]]<-glm(rich_of_birds_threat ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
Bt[[22]]<-glm(rich_of_birds_threat ~ Fthreat, family="poisson", data=all)
Bt[[23]]<-glm(rich_of_birds_threat ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(Bt, rank="AICc"),7) #Best: average DBH of the plant community 
#################       
#2nd) Birds with Fruit Diet:
FB<-list()
FB[[1]]<-glm(FrutiesB ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
FB[[2]]<-glm(FrutiesB ~ rich_flora,family="poisson", data=all)
FB[[3]]<-glm(FrutiesB ~ Fbeepol, family="poisson",data=all)
FB[[4]]<-glm(FrutiesB ~ Fbeepol_abund, family="poisson", data=all)
FB[[5]]<-glm(FrutiesB ~ Fbirdpol, family="poisson", data=all)
FB[[6]]<-glm(FrutiesB ~ Fbirdpol_abund, family="poisson", data=all)
FB[[7]]<-glm(FrutiesB ~ Fbutpol, family="poisson", data=all)
FB[[8]]<-glm(FrutiesB ~ Fbutpol_abund, family="poisson", data=all)
FB[[9]]<-glm(FrutiesB ~ Fbirdsd, family="poisson", data=all)
FB[[10]]<-glm(FrutiesB ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
FB[[11]]<-glm(FrutiesB ~ plantDBH.mean, family="poisson", data=all)
FB[[12]]<-glm(FrutiesB ~ plantDBH.max, family="poisson", data=all)
FB[[13]]<-glm(FrutiesB ~ plantDBH_pol, family="poisson", data=all)
FB[[14]]<-glm(FrutiesB ~ plantDBH_polB, family="poisson", data=all)
FB[[15]]<-glm(FrutiesB ~ plantDBH_polM, family="poisson", data=all)
FB[[16]]<-glm(FrutiesB ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
FB[[17]]<-glm(FrutiesB ~ plantWoodDensity, family="poisson", data=all)
FB[[18]]<-glm(FrutiesB ~ plantWoodDensity_pol, family="poisson", data=all)
FB[[19]]<-glm(FrutiesB ~ plantWoodDensity_polB, family="poisson", data=all)
FB[[20]]<-glm(FrutiesB ~ plantWoodDensity_polM, family="poisson", data=all)
FB[[21]]<-glm(FrutiesB ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
### Richness and abundance of Threathened tree species:
FB[[22]]<-glm(FrutiesB ~ Fthreat, family="poisson", data=all)
FB[[23]]<-glm(FrutiesB ~ Fthreat_abund, family="poisson", data=all)
head(model.sel(FB, rank="AICc"),7) #Best: similar to overall richness
#################       
#3rd) Birds Hand Wing Index:
BHW<-list()
BHW[[1]]<-lm(Bird_handWingIndex ~ 1,  data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
BHW[[2]]<-lm(Bird_handWingIndex ~ rich_flora, data=all)
BHW[[3]]<-lm(Bird_handWingIndex ~ Fbeepol, data=all)
BHW[[4]]<-lm(Bird_handWingIndex ~ Fbeepol_abund, data=all)
BHW[[5]]<-lm(Bird_handWingIndex ~ Fbirdpol, data=all)
BHW[[6]]<-lm(Bird_handWingIndex ~ Fbirdpol_abund, data=all)
BHW[[7]]<-lm(Bird_handWingIndex ~ Fbutpol, data=all)
BHW[[8]]<-lm(Bird_handWingIndex ~ Fbutpol_abund, data=all)
BHW[[9]]<-lm(Bird_handWingIndex ~ Fbirdsd, data=all)
BHW[[10]]<-lm(Bird_handWingIndex ~ Fbirdsd_abund, data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
BHW[[11]]<-lm(Bird_handWingIndex ~ plantDBH.mean, data=all)
BHW[[12]]<-lm(Bird_handWingIndex ~ plantDBH.max, data=all)
BHW[[13]]<-lm(Bird_handWingIndex ~ plantDBH_pol, data=all)
BHW[[14]]<-lm(Bird_handWingIndex ~ plantDBH_polB, data=all)
BHW[[15]]<-lm(Bird_handWingIndex ~ plantDBH_polM, data=all)
BHW[[16]]<-lm(Bird_handWingIndex ~ plantDBH_seeD_birds, data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
BHW[[17]]<-lm(Bird_handWingIndex ~ plantWoodDensity, data=all)
BHW[[18]]<-lm(Bird_handWingIndex ~ plantWoodDensity_pol, data=all)
BHW[[19]]<-lm(Bird_handWingIndex ~ plantWoodDensity_polB, data=all)
BHW[[20]]<-lm(Bird_handWingIndex ~ plantWoodDensity_polM, data=all)
BHW[[21]]<-lm(Bird_handWingIndex ~ plantWoodDensity_seeDbirds, data=all)
### Richness and abundance of Threathened tree species:
BHW[[22]]<-lm(Bird_handWingIndex ~ Fthreat, data=all)
BHW[[23]]<-lm(Bird_handWingIndex ~ Fthreat_abund, data=all)
head(model.sel(BHW, rank="AICc"),7) #Best: snull model
hist(all$Bird_handWingIndex)
#################       
#4th) Birds Body Mass:
BBM<-list()
BBM[[1]]<-lm(birdBodyMassLog ~ 1,  data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
BBM[[2]]<-lm(birdBodyMassLog ~ rich_flora, data=all)
BBM[[3]]<-lm(birdBodyMassLog ~ Fbeepol, data=all)
BBM[[4]]<-lm(birdBodyMassLog ~ Fbeepol_abund, data=all)
BBM[[5]]<-lm(birdBodyMassLog ~ Fbirdpol, data=all)
BBM[[6]]<-lm(birdBodyMassLog ~ Fbirdpol_abund, data=all)
BBM[[7]]<-lm(birdBodyMassLog ~ Fbutpol, data=all)
BBM[[8]]<-lm(birdBodyMassLog ~ Fbutpol_abund, data=all)
BBM[[9]]<-lm(birdBodyMassLog ~ Fbirdsd, data=all)
BBM[[10]]<-lm(birdBodyMassLog ~ Fbirdsd_abund, data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
BBM[[11]]<-lm(birdBodyMassLog ~ plantDBH.mean, data=all)
BBM[[12]]<-lm(birdBodyMassLog ~ plantDBH.max, data=all)
BBM[[13]]<-lm(birdBodyMassLog ~ plantDBH_pol, data=all)
BBM[[14]]<-lm(birdBodyMassLog ~ plantDBH_polB, data=all)
BBM[[15]]<-lm(birdBodyMassLog ~ plantDBH_polM, data=all)
BBM[[16]]<-lm(birdBodyMassLog ~ plantDBH_seeD_birds, data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
BBM[[17]]<-lm(birdBodyMassLog ~ plantWoodDensity, data=all)
BBM[[18]]<-lm(birdBodyMassLog ~ plantWoodDensity_pol, data=all)
BBM[[19]]<-lm(birdBodyMassLog ~ plantWoodDensity_polB, data=all)
BBM[[20]]<-lm(birdBodyMassLog ~ plantWoodDensity_polM, data=all)
BBM[[21]]<-lm(birdBodyMassLog ~ plantWoodDensity_seeDbirds, data=all)
### Richness and abundance of Threathened tree species:
BBM[[22]]<-lm(birdBodyMassLog ~ Fthreat, data=all)
BBM[[23]]<-lm(birdBodyMassLog ~ Fthreat_abund, data=all)
head(model.sel(BBM, rank="AICc"),7) #Best: null model
#################       
#4th) Birds Body Mass of threatened species:
BBMt<-list()
BBMt[[1]]<-lm(birdBodyMassLog_threat ~ 1,  data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
BBMt[[2]]<-lm(birdBodyMassLog_threat ~ rich_flora, data=all)
BBMt[[3]]<-lm(birdBodyMassLog_threat ~ Fbeepol, data=all)
BBMt[[4]]<-lm(birdBodyMassLog_threat ~ Fbeepol_abund, data=all)
BBMt[[5]]<-lm(birdBodyMassLog_threat ~ Fbirdpol, data=all)
BBMt[[6]]<-lm(birdBodyMassLog_threat ~ Fbirdpol_abund, data=all)
BBMt[[7]]<-lm(birdBodyMassLog_threat ~ Fbutpol, data=all)
BBMt[[8]]<-lm(birdBodyMassLog_threat ~ Fbutpol_abund, data=all)
BBMt[[9]]<-lm(birdBodyMassLog_threat ~ Fbirdsd, data=all)
BBMt[[10]]<-lm(birdBodyMassLog_threat ~ Fbirdsd_abund, data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
BBMt[[11]]<-lm(birdBodyMassLog_threat ~ plantDBH.mean, data=all)
BBMt[[12]]<-lm(birdBodyMassLog_threat ~ plantDBH.max, data=all)
BBMt[[13]]<-lm(birdBodyMassLog_threat ~ plantDBH_pol, data=all)
BBMt[[14]]<-lm(birdBodyMassLog_threat ~ plantDBH_polB, data=all)
BBMt[[15]]<-lm(birdBodyMassLog_threat ~ plantDBH_polM, data=all)
BBMt[[16]]<-lm(birdBodyMassLog_threat ~ plantDBH_seeD_birds, data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
BBMt[[17]]<-lm(birdBodyMassLog_threat ~ plantWoodDensity, data=all)
BBMt[[18]]<-lm(birdBodyMassLog_threat ~ plantWoodDensity_pol, data=all)
BBMt[[19]]<-lm(birdBodyMassLog_threat ~ plantWoodDensity_polB, data=all)
BBMt[[20]]<-lm(birdBodyMassLog_threat ~ plantWoodDensity_polM, data=all)
BBMt[[21]]<-lm(birdBodyMassLog_threat ~ plantWoodDensity_seeDbirds, data=all)
### Richness and abundance of Threathened tree species:
BBMt[[22]]<-lm(birdBodyMassLog_threat ~ Fthreat, data=all)
BBMt[[23]]<-lm(birdBodyMassLog_threat ~ Fthreat_abund, data=all)
head(model.sel(BBMt, rank="AICc"),7) #Best: null model






Bd<-list()
Bd[[1]]<-glm(rich_of_buterflies ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
Bd[[2]]<-glm(rich_of_buterflies ~ rich_flora,family="poisson", data=all)
Bd[[3]]<-glm(rich_of_buterflies ~ Fbeepol, family="poisson",data=all)
Bd[[4]]<-glm(rich_of_buterflies ~ Fbeepol_abund, family="poisson", data=all)
Bd[[5]]<-glm(rich_of_buterflies ~ Fbirdpol, family="poisson", data=all)
Bd[[6]]<-glm(rich_of_buterflies ~ Fbirdpol_abund, family="poisson", data=all)
Bd[[7]]<-glm(rich_of_buterflies ~ Fbutpol, family="poisson", data=all)
Bd[[8]]<-glm(rich_of_buterflies ~ Fbutpol_abund, family="poisson", data=all)
Bd[[9]]<-glm(rich_of_buterflies ~ Fbirdsd, family="poisson", data=all)
Bd[[10]]<-glm(rich_of_buterflies ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
Bd[[11]]<-glm(rich_of_buterflies ~ plantDBH.mean, family="poisson", data=all)
Bd[[12]]<-glm(rich_of_buterflies ~ plantDBH.max, family="poisson", data=all)
Bd[[13]]<-glm(rich_of_buterflies ~ plantDBH_pol, family="poisson", data=all)
Bd[[14]]<-glm(rich_of_buterflies ~ plantDBH_polB, family="poisson", data=all)
Bd[[15]]<-glm(rich_of_buterflies ~ plantDBH_polM, family="poisson", data=all)
Bd[[16]]<-glm(rich_of_buterflies ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
Bd[[17]]<-glm(rich_of_buterflies ~ plantWoodDensity, family="poisson", data=all)
Bd[[18]]<-glm(rich_of_buterflies ~ plantWoodDensity_pol, family="poisson", data=all)
Bd[[19]]<-glm(rich_of_buterflies ~ plantWoodDensity_polB, family="poisson", data=all)
Bd[[20]]<-glm(rich_of_buterflies ~ plantWoodDensity_polM, family="poisson", data=all)
Bd[[21]]<-glm(rich_of_buterflies ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
head(model.sel(Bd, rank="AICc"),7) #Best: null  model, nothing explains  Butterfly richness

visreg(B[[15]], "plantDBH_polM" , scale="response")

###Instead of looking to how the richness of birds that are seed dispersers? ("renters")
levels(as.factor(birds$samplingLocationID))
table(birds$birdSeedDiet)
table(birds$birdFruitDiet)
##We select only bird species classified as having fruit diet:
Fruties<-birds %>% filter(birdFruitDiet!= 0)
nrow(Fruties)
jj<-table(Fruties$scientificName, Fruties$samplingLocationID) ##calculate adundance per species
FrutiesB<-as.data.frame.matrix(jj) #transform to data.frame to calculate richness and abundance
all$FrutiesB<-c(length(FrutiesB[FrutiesB$CN02!=0,1]),length(FrutiesB[FrutiesB$CN03!=0,2]),length(FrutiesB[FrutiesB$CN05!=0,3]),length(FrutiesB[FrutiesB$CN06!=0,4]),length(FrutiesB[FrutiesB$CN10!=0,5]),length(FrutiesB[FrutiesB$CN11!=0,6])) 

FB<-list()
FB[[1]]<-glm(FrutiesB ~ 1, family="poisson", data=all)
##Richness and abundance of all flora, those pollinated or seed dispersed by animals
FB[[2]]<-glm(FrutiesB ~ rich_flora,family="poisson", data=all)
FB[[3]]<-glm(FrutiesB ~ Fbeepol, family="poisson",data=all)
FB[[4]]<-glm(FrutiesB ~ Fbeepol_abund, family="poisson", data=all)
FB[[5]]<-glm(FrutiesB ~ Fbirdpol, family="poisson", data=all)
FB[[6]]<-glm(FrutiesB ~ Fbirdpol_abund, family="poisson", data=all)
FB[[7]]<-glm(FrutiesB ~ Fbutpol, family="poisson", data=all)
FB[[8]]<-glm(FrutiesB ~ Fbutpol_abund, family="poisson", data=all)
FB[[9]]<-glm(FrutiesB ~ Fbirdsd, family="poisson", data=all)
FB[[10]]<-glm(FrutiesB ~ Fbirdsd_abund, family="poisson", data=all)
###DBH for all plants, and for those pollinated or dispersed by animals:
FB[[11]]<-glm(FrutiesB ~ plantDBH.mean, family="poisson", data=all)
FB[[12]]<-glm(FrutiesB ~ plantDBH.max, family="poisson", data=all)
FB[[13]]<-glm(FrutiesB ~ plantDBH_pol, family="poisson", data=all)
FB[[14]]<-glm(FrutiesB ~ plantDBH_polB, family="poisson", data=all)
FB[[15]]<-glm(FrutiesB ~ plantDBH_polM, family="poisson", data=all)
FB[[16]]<-glm(FrutiesB ~ plantDBH_seeD_birds, family="poisson", data=all)
###Wood Density for all plants, and for those pollinated or dispersed by animals:
FB[[17]]<-glm(FrutiesB ~ plantWoodDensity, family="poisson", data=all)
FB[[18]]<-glm(FrutiesB ~ plantWoodDensity_pol, family="poisson", data=all)
FB[[19]]<-glm(FrutiesB ~ plantWoodDensity_polB, family="poisson", data=all)
FB[[20]]<-glm(FrutiesB ~ plantWoodDensity_polM, family="poisson", data=all)
FB[[21]]<-glm(FrutiesB ~ plantWoodDensity_seeDbirds, family="poisson", data=all)
head(model.sel(FB, rank="AICc"),7) #Best: null  model, nothing explains  Butterfly richness

####Calculate other traits per animal species to use as response variable:
#a) For bees: ITD , sociality, Nest type (renters)
ITD<-aggregate(beeITDist~samplingLocationID, data=aaa, FUN=function(x) c(mean=mean(x)))


#b) Bird size
#c) Butterfly size 

require(ggplot2)
###Calculatng mean values
DBH<-aggregate(plantDBH~samplingLocationID, data=f, FUN=function(x) c(mean=mean(x), sd=sd(x), max=max(x)))
DBH<-aggregate(plantDBH~samplingLocationID, data=f, FUN=function(x) max=max(x))
ggplot(DBH,                                   # Draw barchart of table
       aes(x = samplingLocationID,
           y = plantDBH)) + 
  geom_bar(stat = "identity")
wooDensity<-aggregate(plantWoodDensity~samplingLocationID, data=f, FUN=function(x) c(mean=mean(x), count=length(x)))
wooDensity<-aggregate(plantWoodDensity~samplingLocationID+plantLifeForm, data=f, FUN=function(x) c(mean=mean(x), count=length(x)))

ITD<-aggregate(beeITDist~samplingLocationID, data=aaa, FUN=function(x) c(mean=mean(x)))
ggplot(ITD,                                   # Draw barchart of table
          aes(x = samplingLocationID,
              y = beeITDist)) + 
       geom_bar(stat = "identity")


###Joining aggregated data:

kk$plantDBH<-DBH$plantDBH
kk$samplingID<-DBH$samplingLocationID
kk$ITD<-ITD$beeITDist
require(MuMIn)
ojo<-glm(bee_Rich ~ Flora_rich, family="poisson", data=kk)
ojo2<-glm(bee_Rich ~ 1, family="poisson", data=kk)
ojo3<-glm(bee_Rich ~ plantDBH, family="poisson", data=kk)
model.sel(ojo, ojo2,ojo3, rank="AICc") ##Flora richness explain bee richness
r.squaredLR(ojo)

o<-lm(ITD ~ Flora_rich, data=kk)
o2<-lm(ITD ~ 1, data=kk)
o3<-lm(ITD ~ plantDBH,  data=kk)
model.sel(o, o2,o3, rank="AICc") ##Null model


summary(ojo)
visreg(ojo3, "plantDBH")



###Lets CAlculate som indixes!!!!!!!!
##https://github.com/funecology/fundiversity
library("fundiversity")

##Tow types of data.frames need to be incorporated:
#1) sites(Y-axis) vs. species (X-axis)
flora<-t(as.data.frame.matrix(table(f$scientificName, f$samplingLocationID)))
#2) species (Y-axis) vs. Traits (X-axis)
#a) 1st funtional trait: DAP / DBH mean and max
DBHsps<-aggregate(plantDBH~scientificName, data=f, FUN=function(x) c(mean=mean(x), max=max(x)))
DBHsps1 <- data.frame(DBHsps[,-1], row.names = DBHsps[,1])
#b) 2nd funtional trait: wood density (mean)
wooDensity<-aggregate(plantWoodDensity~scientificName, data=f, FUN=function(x) c(mean=mean(x)))
head(wooDensity_sps)
flora_traits<-left_join(DBHsps, wooDensity)
head(flora_traits)
head(wooDensity_sps)
##Plant Height
plantHeight<-aggregate(plantHeight~scientificName, data=f, FUN=function(x) c(mean=mean(x)))
##When comparing small trees vs. the old large trees, is there a function frequency change? 
summary(plantHeight)

#c) 3rd Functional trait: Pollination indicating the abundance of trees 
##                which are pollinated by c1=bees; c2=birds; c3=butterflies:
#c1=Bees
melitho<-as.data.frame.matrix(table(f$scientificName, f$plantMelithophily))
melitho$scientificName<-row.names(melitho)
colnames(melitho)[2] ="plantMelithophily"
head(melitho[,2:3])
flora_traits1<-left_join(flora_traits,melitho[,2:3])
summary(flora_traits1$plantMelithophily)
head(flora_traits1)
#c2=Insects (birds or butterflies have too many NAs)
Entomo<-as.data.frame.matrix(table(f$scientificName, f$plantEntomophily))
Entomo$scientificName<-row.names(Entomo)
colnames(Entomo)[2] ="plantEntomophily"
head(Entomo[,2:3])
flora_traits2<-left_join(flora_traits1,Entomo[,2:3])
summary(flora_traits2$plantEntomophily)
head(flora_traits2)

#c2=Zoochory and Zoophily (seed dispersed by animals or pollinated by animals)
Zoof<-as.data.frame.matrix(table(f$scientificName, f$plantZoophily))
Zoof$scientificName<-row.names(Zoof)
colnames(Zoof)[2] ="plantZoophily"
head(Zoof[,2:3])
summary(Zoof) ##Are 0   & 1 always "No" & "Yes", respectively??? 

Zooc<-as.data.frame.matrix(table(f$scientificName, f$plantZoochory))
Zooc$scientificName<-row.names(Zooc)
colnames(Zooc)[2] ="plantZoochory"
head(Zooc[,2:3])
summary(Zooc)
zoo<-left_join(Zoof[,2:3], Zooc[,2:3])

##Adding all categorical (pollination or seed dispersion) to all continuous data:
flora_traits3<-left_join(flora_traits2, zoo)
head(flora_traits3)
summary(flora_traits3)

#Other continues trait of plants: 
#plantLeafArea	plantLeafNitrogen	plantLeafPhosphorus

#d) 4th Functional trait: plant leaf area:
pla<-aggregate(plantLeafArea~scientificName, data=f, FUN=function(x) c(mean=mean(x)))
FT<-left_join(flora_traits3, pla, by="scientificName")
plp<-aggregate(plantLeafNitrogen~scientificName, data=f, FUN=function(x) c(mean=mean(x)))
FT2<-left_join(FT, plp)
pln<-aggregate(plantLeafPhosphorus~scientificName, data=f, FUN=function(x) c(mean=mean(x)))
FT3<-left_join(FT2, pln)
summary(FT3)
##transforming 1st row into name column (species names):
flora_traits4<-data.frame(FT3[,-1], row.names=FT3[,1])
summary(flora_traits4)

###CAlculating funcitonal diversity of the forest:
FRDic<-fd_fric( flora_traits4, flora) ##Ojo, si se agregan traits sin valores para algunas especies esas especies son eliminadas
colnames(FRDic)[1]<-"samplingLocationID"
all<-left_join(all[,1:19], FRDic)
FRic<-fd_fric( flora_traits4[, c(1,3:6)], flora) ##Ojo, si se agregan traits sin valores para algunas especies esas especies son eliminadas
colnames(FRic)<-c("samplingLocationID","FRic_all")
all<-left_join(all, FRic)


##Considering only two functinoal traits: 
##  abudance of tree pollinated by bees and wood density
FRic_imp<-fd_fric( flora_traits4[, 2:3], flora) ##Ojo, si se agregan traits sin valores para algunas especies esas especies son eliminadas
colnames(FRic_imp)<-c("samplingLocationID","FRic_two")
all<-left_join(all, FRic_imp)


###Model selection for bee richness as response:
A<-list()
A[[1]]<-glm(rich_of_bees ~ 1, family="poisson", data=all)
A[[2]]<-glm(rich_of_bees ~ rich_flora,family="poisson", data=all)
A[[3]]<-glm(rich_of_bees ~ Fbeepol, family="poisson",data=all)
A[[4]]<-glm(rich_of_bees ~ Fbeepol_abund, family="poisson", data=all)
A[[5]]<-glm(rich_of_bees ~ plantDBH.mean, family="poisson", data=all)
A[[6]]<-glm(rich_of_bees ~ plantDBH.sd, family="poisson", data=all)
A[[7]]<-glm(rich_of_bees ~ plantDBH.max, family="poisson", data=all)
A[[8]]<-glm(rich_of_bees ~ plantWoodDensity, family="poisson", data=all)
A[[9]]<-glm(rich_of_bees ~ FRic, family="poisson", data=all)#o calculo esta abaixo
A[[10]]<-glm(rich_of_bees ~ FRic_all, family="poisson", data=all)#o calculo esta abaixo
A[[11]]<-glm(rich_of_bees ~ FRic_two, family="poisson", data=all)#o calculo esta abaixo
model.sel(A, rank="AICc")

###Models richness of bees with renter as nesting trade
R<-list()
R[[1]]<-glm(rentersB ~ 1, family="poisson", data=all)
R[[2]]<-glm(rentersB ~ rich_flora,family="poisson", data=all)
R[[3]]<-glm(rentersB ~ Fbeepol, family="poisson",data=all)
R[[4]]<-glm(rentersB ~ Fbeepol_abund, family="poisson", data=all)
R[[5]]<-glm(rentersB ~ plantDBH.mean, family="poisson", data=all)
R[[6]]<-glm(rentersB ~ plantDBH.sd, family="poisson", data=all)
R[[7]]<-glm(rentersB ~ plantDBH.max, family="poisson", data=all)
R[[8]]<-glm(rentersB ~ plantWoodDensity, family="poisson", data=all) ##remember wood density only for trees
R[[9]]<-glm(rentersB ~ FRic, family="poisson", data=all)
R[[10]]<-glm(rentersB ~ FRic_all, family="poisson", data=all)
R[[11]]<-glm(rentersB ~ FRic_two, family="poisson", data=all)
model.sel(R, rank="AICc")





fd_fric( DBHsps1, flora)

