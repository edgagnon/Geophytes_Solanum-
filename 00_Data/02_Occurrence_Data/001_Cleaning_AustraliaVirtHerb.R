##########################################
#
# Script for cleaning specimen occurrence dataset from 
# the Australian Virtual herbarium, for a download of Solanum data from 2019
# doi:  
#
# by E. Gagnon, part of Geophyte publication 2023
#########################################

rm(list = ls())

#Libraries to use
library(maptools)
data(wrld_simpl)
library(stringr)
library(dismo)
library(raster)
library(dplyr)
library(ggplot2)
library(rgbif)
library(CoordinateCleaner)


#####################
#Step 0
#Set the working directory. Alternatively, put file.choose() whenever you see a file name.
getwd()
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/02_Occurrence_Data/01_AustraliaVirtHerb/")
dir()

#####################
#Step 1: import data to be used

df<-read.csv("AVH_Solanum_records-2019-06-05_mod.csv",header=TRUE,sep=";")

dim(df)#29393 40
names(df)
head(df)

#create a column whose name is a mixture of genus and sp1, or alternative just search based on sp1

df$genus.sp <- paste(df$genus,"_",df$sp1, sep="")


#####################
#Step 2: Create a list of dataframes with occurence records for each potato species
#####################
dir.create("Solanum_occurence_australia")

length(unique(df$genus.sp))#251
write.csv(table(df$genus.sp),"./Solanum_occurence_australia/Solanum_species_in_Australia.csv")

occurence.records<-list()
#number.records.potato<-matrix(,nrow=185,ncol=2)
number.records<-matrix(,nrow=length(unique(df$genus.sp)),ncol=2)

number.records
rownames(number.records)<-unique(df$genus.sp)
head(number.records)

sol.names<-unique(df$genus.sp)

#script for producing the data.
for (i in sol.names)
{
    df[grepl(i,df$genus.sp),]->occurence.records[[i]]
  
  #This writes a table of occurence data for each species
  name<-paste("./Solanum_occurence_australia/",i,"_occurence_data.txt",sep="_")
  write.csv(df[grepl(i,df$genus.sp),], file=name)
  
  #This produces a map for each species
  df[grepl(i,df$genus.sp),]->toto
  name.map<-paste("./Solanum_occurence_australia/",i,"occurence_data.pdf",sep="_")
  pdf(file=name.map)
  plot(wrld_simpl)
  points(toto$LONGDEC,toto$LATDEC,col="red",pch=20,cex=0.75)
  name.title<-paste(i,"occurence","data")
  title(name.title)
  dev.off()
  #class(df[grepl(i,df$genus.sp),])  
  print(dim(occurence.records[[i]]))
  
  dim(occurence.records[[i]])->number.records[i,]
}
dev.off()

#This indicates the number of recrods per species, from smallest number of records to largest.
number.records
number.records<-number.records[order(number.records[,1]),]

write.table(number.records,file="./Solanum_occurence_australia/summary_nb_records.txt",sep="\t")


#Untag png lines if you to save
#pdf("map_data.pdf")
plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,10), axes=TRUE, col="light yellow")
points(df$LONGDEC, df$LATDEC, col='red', pch=20, cex=0.75)
#dev.off()


######################################################
#Step 3: Remove species that are cultivated + other bits and bobs
######################################################

sp.to.remove<-c(
"Solanum_cheesmaniae x galapagense",
"Solanum_cheesmaniae x pimpinellif",
"Solanum_cheesmaniae/lycopersicum",
"Solanum_sp.",
"Solanum_sp. (Basarthrum)",
"Solanum_sp. (Brevantherum)",
"Solanum_sp. (Geminata)",
"Solanum_sp. (Morelloid)",
"Solanum_sp. (Petota)",
"Aureliana_fasciculata",
"Jaltomata_antillana",
"Lycianthes_biflora",
"Lycianthes_fugax",
"Lycianthes_laevis",
"Lycianthes_stellata",
"Lycianthes_testacea",
"Lycianthes_virgata",
"Solanum_'hunzikeri'",
"Solanum_'hydroides'",
"Solanum_'pseudonitidi'",
"Solanum_'tiinae2",
"Solanum_tuberosum",
"Solanum_lycopersicum", 
"Solanum_melongena", 
"Solanum_muricatum",
"Solanum_aethiopicum", 
"Solanum_macrocarpon", 
"Solanum_lasiocarpum", 
"Solanum_betaceum", 
"Solanum_sessiliflorum", 
"Solanum quitoense",
"Solanum_scabrum", 
"Solanum_aviculare", 
"Solanum_crispum", 
"Solanum_laciniatum", 
"Solanum_laxum", 
"Solanum_pseudocapsicum", 
"Solanum_seaforthianum", 
"Solanum_wendlandii", 
"Solanum_mammosum")

data<-df
dim(data)

data <- data[ ! data$genus.sp %in% sp.to.remove, ]

#for (i in 1:length(sp.to.remove))
#{data<-data[!data$genus.sp==sp.to.remove[i],]
#}
dim(df)#29393
dim(data)#27367


#check if there is an issue with missing LATDEC and LONGDEC
table(is.na(data$LONGDEC))
table(is.na(data$LATDEC))
#18 cultivated specimens removed prior to this script

#This writes the data that had missing Latdec and longdec
dim(data[is.na(data$LATDEC)== TRUE,])
write.csv(data[is.na(data$LATDEC)==TRUE,], "flag_missing_LATDEC.csv")

#This removes the data with missing latdec and longdec
data<-data[!is.na(data$LATDEC)==TRUE,]
dim(data)#27349


##############

#remove cultivated species from Solanum
remove.cultivated<-c("Solanum_tuberosum","Solanum_lycopersicum", "Solanum_melongena", "Solanum_muricatum","Solanum_aethiopicum", 
                     "Solanum_macrocarpon", "Solanum_lasiocarpum", "Solanum_betaceum", "Solanum_sessiliflorum", "Solanum quitoense",
                     "Solanum_scabrum", "Solanum_aviculare", "Solanum_crispum", "Solanum_laciniatum", "Solanum_laxum", 
                     "Solanum_pseudocapsicum", "Solanum_seaforthianum", "Solanum_wendlandii", "Solanum_mammosum")
data <- data[ ! data$genus.sp %in% remove.cultivated, ]

dim(data)#27349, so none removed
length(unique(data$genus.sp))#245

##############
#This next section, we will be checking for species that are not in the accepted list of names for the genus Solanum, according to the SolanaceaeSource database

#Retrieving list of accepted names.

name.list<-read.csv("../../00_Taxonomy_Accepted_Names/solanum_clades_accepted.csv", header=TRUE)
head(name.list)
dim(name.list)#1257

species<-word(name.list$FULLNAME,2)
genus.sp<-paste("Solanum_",species,sep="")

length(setdiff(unique(data$genus.sp),genus.sp))
#17 problematic names, not in the list, checking what are the problems



########## This is a script that finds synonyms and replace them with accepted names

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
name.list2<-read.table("../../00_Taxonomy_Accepted_Names/master_Solanaceae_names.txt", sep="\t", header=TRUE)
head(name.list2)

#3) Use the recoderFunc command to create a new list, that will be based on searching and replacing the oldnames with the newnames
toto2<-data$genus.sp
newnames<-recoderFunc(toto2, name.list2$tip.name, name.list2$current.accepted.name)
length(newnames)#27336

#4) Check if new names still has problematic names
length(setdiff(unique(newnames),genus.sp))#8

setdiff(unique(newnames),genus.sp)#8


#5) Add the updated names to the supermatrix
data$genus.sp<-newnames
length(unique(data$genus.sp))#238 names

#removing the Lycianthes
#remove cultivated species from Solanum
remove.sp<-c("Lycianthes_cladotrichota","Lycianthes_oliverianum","Lycianthes_umbonatum","Solanum_acanthocarpum","Solanum_moszkowskii","Solanum_hirsutum","Solanum_stuartianum")    # "Solanm_discolor"
data <- data[ ! data$genus.sp %in% remove.sp, ]
dim(data)#27319
length(unique(data$genus.sp))#231

#Fixing typo
data[ data$genus.sp %in% "Solanm_discolor", ]$genus.sp<-"Solanum_discolor"
dim(data)#27319
length(unique(data$genus.sp))#230


#6) Write new fasta file into your documents.
write.csv(table(data$genus.sp),"freq_names.csv")


######################################################
#Step 4: Check for wrong country
###################################################

# Set working directory
getwd()

table(data$country)

# Method one: use hijmans over function, as show in :
getData("countries")->world

data->data2
dim(data2)
as.numeric(data2$LONGDEC)->data2$LONGDEC
as.numeric(data2$LATDEC)->data2$LATDEC
coordinates(data2)<-~LONGDEC+LATDEC
crs(data2)<-crs(world)
class(data2)

ovr <- over(data2, world)
head(ovr)
colnames(ovr)

cntr<-ovr$NAME_ENGLISH

j <-which((cntr) != as.vector(data2$country))
j

length(j)#gives number of coordinates which don't fall in the right country
#0, so no country is in the wrong country.

cbind(cntr, data2$country)

cbind(cntr, data)[j,]->probs_j
table(probs_j$genus.sp)
table(probs_j$country)

dim(probs_j)
#[1] none, no need for flags


##################
#Step 5: check if occurrence data in the sea 
#
##################

############################################################################################################################
# 1) Load data and examine
############################################################################################################################

data.coord <- data %>%
  dplyr::select(genus.sp, LATDEC, LONGDEC, country, majorarea, localitynotes,minorarea,
                brahms, family, collyy, genus, alt, altmax, llres, llunit,
                typeStatus, vernacularName, collector, number) 

#examine the results
class(data.coord)
dim(data.coord)
head(data.coord)

class(data.coord[,3])
class(data.coord[,2])

data.coord[,2]<-as.numeric(data.coord[,2])

#create an object of class "SpatialPoints" with the geographic coordinates of the specimens
data.coord.spatial <- SpatialPoints(data.coord[,3:2], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
class(data.coord.spatial)


############################################################################################################################
# 2) Read raster mask that indicates the grid cells that have climate data. The raster has a 30 arc secods resolution,
# or 0.008333334 X 0.008333334 degrees.
############################################################################################################################

files <- list.files(path="../../03_Environmental_Data/Climate/", pattern='.tif', full.names=TRUE)
list(files)
#predictors <- stack(files)
#predictors
predictors.bio1<-raster("CHELSA_bio_1.tif") #selects Bio1 file
plot(predictors.bio1)

gc()

############################################################################################################################
# 3) Determine which specimen records (that have geographic coordinates and have been determined to species) fall outside
# the mask for the climate data 
############################################################################################################################

#plot the mask
plot(predictors.bio1, useRaster=T, legend=F)
plot(data.coord.spatial, add=T, pch=19, cex=0.2, col="red")
axis(1)
mtext(side=1, "Longitude (degrees)", cex=1.5, line=3)
axis(2)
mtext(side=2, "Latitude (degrees)", cex=1.5, line=3)

#extract the values of the mask at the coordinates of the specimen records
data.coord.land <- extract(predictors.bio1, data.coord[,3:2], method='simple')

class(data.coord.land)
summary(data.coord.land)
sum(is.na(data.coord.land)) #number of specimens falling outside the mask
#325
sum(!is.na(data.coord.land)) #number of specimens falling inside the mask
#26994

#plot the mask and specimens that fall off the mask
plot(predictors.bio1, col="gray90", useRaster=T, legend=F)
plot(data.coord.spatial[which(is.na(data.coord.land))], add=T, pch=19, cex=0.2, col="blue")
points(data.coord[which(is.na(data.coord.land)),3:2], pch=19, cex=0.5, col="red")
mtext(side=1, "Longitude (degrees)", cex=1.5, line=3)
mtext(side=2, "Latitude (degrees)", cex=1.5, line=3)

class(data.coord.land)
length(data.coord.land) #27319

############################################################################################################################
# 4) Discard the specimen records that fall outside the mask, and write them into a separate file.
############################################################################################################################

#Eliminating 325 localities
getwd()

#specimen.records.to.edit <- data.frame(data.coord[which(is.na(data.coord.land)),], altered.coor)
flags.pushback<- data.frame(data[which(is.na(data.coord.land)),])
length(data.coord.land)
dim(flags.pushback)
head(flags.pushback)
table(flags.pushback$genus.sp)
table(flags.pushback$country)

#setwd("C://Users/egagnon/Documents/OneDrive/Projets_Recherche/2018_Solanum_phylogeny/Solanum_phylogeny")
write.csv(flags.pushback, file="./flags_pushback_solanum_australia_2023.csv")



data.pushback<- data[!data$brahms %in% flags.pushback$brahms,]
data<-data.pushback
dim(data.pushback) #26994
length(unique(data.pushback$genus.sp)) #229 species
#colnames(specimen.records.to.edit)[4] <-  "CollectionNumberNumeric"
#colnames(specimen.records.to.edit)[5] <- "SeniorCollectorPersonID"
plot(wrld_simpl)
points(flags.pushback$LONGDEC,flags.pushback$LATDEC,col="red",pch=20,cex=0.75)



###################################################################################
#Step 6: Coordinate_Cleaner section
###################################################################################

#select columns of interest
data3 <- data %>%
  dplyr::select(genus.sp, LATDEC, LONGDEC, country, majorarea, localitynotes,minorarea,
                brahms, family, collyy, genus, alt, altmax, llres, llunit,
                typeStatus, vernacularName, collector, number) 

dim(data3)#26994

##plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = data3, aes(x = LONGDEC, y = LATDEC),
             colour = "darkred", size = 0.5)+
  theme_bw()

#to avoid specifying it in each function, changing the name of lat long
names(data3)[2:3] <- c("decimallatitude", "decimallongitude")


####points that are not valid
clean_val <- data3%>%
  cc_val()

data[!data3$brahms%in%clean_val$brahms,]->val
dim(val)#0 found


####Equivalent lon & lat
clean_equ <- data3%>%
  cc_equ()#zero

data[!data3$BRAHMS%in%clean_equ$BRAHMS,]->equ
dim(equ)


####Points that fall near the gbif institution
clean_gbif <- data3%>%
  cc_gbif() #ZERO

data[!data3$BRAHMS%in%clean_gbif$BRAHMS,]->gbif
dim(gbif)

####Points that fall near gardens and other institutions
clean_inst <- data3%>%
  cc_inst()#9 records

data[!data3$brahms%in%clean_inst$brahms,]->inst

pdf(file="./Solanum_inst_flag.pdf")
plot(wrld_simpl,  axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(inst$LONGDEC, inst$LATDEC, col="red", cex=0.75)
dev.off()

write.csv(inst,file="./Solanum_inst_flag_australia.csv")


####Points that are zero longitude and zero latitude and a radium around the zero lon and zero lat
clean_zero <- data3%>%
  cc_zero()#zero

data[!data3$brahms%in%clean_zero$brahms,]->zero
dim(zero)


#Final dataset without the values flagged in previous tests

clean_val[clean_val$brahms%in%clean_equ$brahms,]->clean_cc
clean_cc[clean_cc$brahms%in%clean_gbif$brahms,]->clean_cc
clean_cc[clean_cc$brahms%in%clean_inst$brahms,]->clean_cc
clean_cc[clean_cc$brahms%in%clean_zero$brahms,]->clean_cc

dim(clean_cc)#26985
data[data3$brahms%in%clean_cc$brahms,]->clean_cc2
dim(clean_cc2)

data<-clean_cc2
dim(data)#26985


#############

####Points that fall into centroids
clean_cen <- data3%>%
  cc_cen()

data[!data3$brahms%in%clean_cen$brahms,]->cen
dim(cen)#3 records

plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(cen$LONGDEC, cen$LATDEC, col="red", cex=0.75)

write.csv(cen,file="./Solanum_cen_flag_Australia.csv")


####Points that fall into capitals, within a 10 km radium from centroid
clean_cap <- data3%>%
  cc_cap()#42

data[!data3$brahms%in%clean_cap$brahms,]->cap

plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(cap$LONGDEC, cap$LATDEC, col="red", cex=0.75)

getwd()
write.csv(cap,file="./Solanum_cap_flag_australia.csv")

######### removing duplicates
clean_dupl <- data3%>%
  cc_dupl(.,species="genus.sp",additions = c("collector","number"))
#2536 records

data[!data3$brahms%in%clean_dupl$brahms,]->dupl
dim(dupl)

flags.dupl<-dupl

plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(dupl$LONGDEC, dupl$LATDEC, col="red", cex=0.75)

write.csv(dupl,file="./Solanum_dupl_flag_australia.csv")

dim(data)#26985

toto<-data[data$brahms%in%clean_dupl$brahms,]
toto<-toto[toto$brahms%in%clean_cen$brahms,]
toto<-toto[toto$brahms%in%clean_cap$brahms,]

dim(toto)#24416

#data->backup
data<-toto

################################################
# Removing additional specimens which upon closer inspection are problematic
#################################################
#This is a list of identifiers from the brahms column, whose rows need to be removed in the occurence database.
remove.brahms<-c("a11","a12","a13107","a13109","a21388","a25931","a25932","a27707","a27948")
data <- data[ ! data$brahms %in% remove.brahms, ]
dim(data)#24407

##############################################
# Final step: producing map of cleaned data, and saving the table.
#
##################################################

pdf("FINAL_24407_map_SOLANUM.pdf")
plot(wrld_simpl, axes=TRUE, col="light yellow")
points(df$LONGDEC, df$LATDEC, col="black", cex=0.75)
points(data$LONGDEC, data$LATDEC, col="red", cex=0.75)
dev.off()

write.csv(data,file="Australia_24407_FINAL_2021.csv")
write.table(data,file="Australia_24407_FINAL_2021_v2.csv",sep=";")
save.image("Australia_cleaning.R")
