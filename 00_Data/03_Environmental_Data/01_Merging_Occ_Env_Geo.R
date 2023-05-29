################################################################################
#
# Script for merging the Climate, Fire, Soil, Topographic and Geophytic datasets  
# #
# by E. Gagnon, part of Geophyte publication 2023
################################################################################

rm(list = ls())


#Libraries to use
library(stringr)
library("dplyr")

##################################################
# Uploading the datasets
####################################################

setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/03_Environmental_Data/")

#Read climate dataset
climate.data<-read.csv("./Climate/DF_results_climate_50379.csv",header=TRUE,sep=";")

dim(climate.data)#50379

#Read fire dataset
fire.data<-read.csv("./Fire/DF_results_Firedata_50379.csv",header=TRUE,sep=";")
dim(fire.data)#50379

#read the SoilGrid dataset
soil.data<-read.csv("./Soil/SoilGrids_50379.csv",header=TRUE)
dim(soil.data)#50379

#read the topographic heterogeneity dataset
topo.data<-read.csv("./Topography_Elevation/topo_data_50379.csv")
dim(topo.data)

#Read trait dataset
trait.data<-read.csv("../01_Trait_Data/SolTrGeo.csv",header=TRUE)
dim(trait.data)#1232


names(trait.data)
names(fire.data)
names(climate.data)
names(soil.data)
names(topo.data)


#this allows to check that both dataframes are oredered in the same way.

head(fire.data$BRAHMS)
head(climate.data$BRAHMS)
head(topo.data$BRAHMS)
head(soil.data$BRAHMS)#THis is order in a different way, used SORT on the dataset...

#merging fire, climate and topography datasets
#climate.data$FRI<-fire.data$FRI
climate.data$q95size<-fire.data$q95size
#climate.data$q95FRP<-fire.data$q95FRP
climate.data$vrm_med<-topo.data$vrm_med
climate.data$elv_sd<-topo.data$elv_sd

#Sorting the climate dataset so that I can easily merge it with the soildataset

climate.data.1 <- climate.data[order(climate.data$BRAHMS),]

#Check if they are in the same order
head(climate.data.1$BRAHMS)
head(soil.data$BRAHMS)

tail(climate.data.1$BRAHMS)
tail(soil.data$BRAHMS)

#They are! so now merging together

climate.data.1$sand_05_mean<-soil.data$sand_05_mean
#climate.data.1$clay_05_mean<-soil.data$clay_05_mean
#climate.data.1$bdod_05_mean<-soil.data$bdod_05_mean
#climate.data.1$cvfo_05_mean<-soil.data$cvfo_05_mean

names(climate.data.1)

#Fixing some issues with the names in the file

# Fixing additional issues:
#[1] "Solanum_tweedianum" "Solanum_lianoides" 
#S. tweedianum is mispelled, should be S. tweedieanum
#S. lianoides is a synonym of Solanum schefferi


climate.data.2 <- climate.data.1 %>% 
  mutate(genus.sp = str_replace(genus.sp, "Solanum_tweedianum", "Solanum_tweedieanum")) %>%
  mutate(genus.sp = str_replace(genus.sp, "Solanum_lianoides", "Solanum_schefferi"))

#write.csv(climate.data.2,"climate_data_2_50379.csv")

########## Checking problems with synonyms


#This next section, we will be checking for species that are not in the accepted list of names for the genus Solanum, according to the SolanaceaeSource database
#Retrieving list of accepted names.
#

name.list<-read.csv("../00_Taxonomy_Accepted_Names/solanum_clades_accepted_mod.csv", header=TRUE)
head(name.list)
dim(name.list)#1232

species<-word(name.list$X...FULLNAME,2)
genus.sp<-paste("Solanum_",species,sep="")

#length(setdiff(unique(climate.data.1$genus.sp),genus.sp))
#20 problematic names, not in the list, checking what are the problems

## This is a script that finds synonyms and replace them with accepted names

#1) Code the recoderFunc in R

recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

#2) Open the tab-delimited file that contains a column with oldnames, and a column with new names
name.list2<-read.table("../00_Taxonomy_Accepted_Names/master_Solanaceae_names.txt", sep="\t", header=TRUE)
head(name.list2)
dim(name.list2)

#3) Use the recoderFunc command to create a new list, that will be based on searching and replacing the oldnames with the newnames
climate.data.2$genus.sp->toto2
#df$genus.sp->toto2
newnames<-recoderFunc(toto2, name.list2$tip.name, name.list2$current.accepted.name)
length(newnames)#50379

#4) Check if new names still has problematic names
length(setdiff(unique(newnames),genus.sp))#20

setdiff(unique(newnames),genus.sp)#39

#5) Add the updated names to the supermatrix
climate.data.2$genus.sp<-newnames
#df$genus.sp<-newnames
length(unique(climate.data.2$genus.sp))#1179 names
#length(unique(df$genus.sp))#1179 names

1179/1232 #94%

#library(dplyr)


###This is the match command that works much better
#Merging the trait dataset with the environmental dataset

toto<-climate.data.2
#toto<-df
names(trait.data)
head(trait.data)

toto$CLADE1 <- trait.data$CLADE1[match(toto$genus.sp, trait.data$genus.sp)]
dim(toto)

toto$roots <- trait.data$roots[match(toto$genus.sp, trait.data$genus.sp)]
dim(toto)

toto$roots1 <- trait.data$roots1[match(toto$genus.sp, trait.data$genus.sp)]
dim(toto)

toto$roots2 <- trait.data$roots2[match(toto$genus.sp, trait.data$genus.sp)]
dim(toto)


#Double-checking for missing data

na.toto<-toto[which(is.na(toto$roots)), ]
unique(na.toto$genus.sp)


#removing names to drop + I fixed a huge bunch of synonyms...)
remove<-c("Solanum_celebense","Solanum_gracile","Solanum_haematocarpon","Solanum_ligulatum", "Solanum_micranthum","Solanum_mirum","Solanum_paucispinum","Solanum_postremum","Solanum_rufistellatum")

dim(toto[toto$genus.sp %in% remove,])#14

dim(toto[!toto$genus.sp %in% remove,])#50365

toto<-toto[!toto$genus.sp %in% remove,]



#removing specimes based on their brahms ID to drop from the tomato clade, which are cultivated
remove<-c("2280","1532","4482","11274", "79734","4730","4482","5076","5070","4613","4611","4602","4601","65008","4610")

dim(toto[toto$BRAHMS %in% remove,])#13

dim(toto[!toto$BRAHMS %in% remove,])#50352

toto<-toto[!toto$BRAHMS %in% remove,]

head(toto)

write.table(toto,"DF_merged_data_50352a_V3.csv",sep=";")
write.csv(toto,"DF_merged_data_50352_V3.csv")

write.table(toto,"../../01_Maps/DF_merged_data_50352a_V3.csv",sep=";")
write.table(toto,"../../02_PCA_Kernel_Density/DF_merged_data_50352a_V3.csv",sep=";")


