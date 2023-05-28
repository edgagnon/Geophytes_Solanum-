##########################################
#
# Script for cleaning specimen occurrence dataset from 
# the SolanaceaeSource database, for the genus Solanum
# 
# by E. Gagnon, published as part of MANUSCRIPT
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
library(countrycode)

library(CoordinateCleaner)


#If not installed, can be done as so:
##install.packages("maptools")

#####################
#Step 0
#Set the working directory. Alternatively, put file.choose() whenever you see a file name.
getwd()
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/02_Occurrence_Data/02_SolanaceaeSource/")
dir()

#####################
#Step 1: import data to be used

Sys.setlocale("LC_ALL", "C")

#Import csv file with Solanum occurence data. This file has the special characters "×" to "X".
coll.solanum<-read.csv("Solanum_all_COLLEXTRACT_21-04-2021_at_08-39-16_v2.CSV",header=TRUE,na.strings = c("NA",""," "))

dim(coll.solanum)
sort(names(coll.solanum))
df<-coll.solanum

#create a column whose name is a mixture of genus and sp1, or alternative just search based on sp1
df$genus.sp <- paste(df$GENUS,"_",df$SP1, sep="")
df$genus.sp<-gsub("/","_",df$genus.sp)

#This is a list of unique Brahm identifiers whose rows need to be removed in the occurence database.
remove.brahms<-c(9233,9226,123968,123967,75090,75091,95021,72466,103162,123972,123973,34563,6167,4467,118661,88482,121767,138552,123979,36021,36022)
df <- df[ ! df$BRAHMS %in% remove.brahms, ]
dim(df)
solanum.clade.names<-unique(df$genus.sp)
table.original.data<-table(df$genus.sp)

#####################
#Step 2: Create maps and a list of dataframes with occurence records for each Solanum species
#####################

dir.create("output")

occurence.records<-list()
number.records.solanum<-matrix(,nrow=length(unique(df$genus.sp)),ncol=2)

number.records.solanum
rownames(number.records.solanum)<-unique(df$genus.sp)
head(number.records.solanum)

solanum.clade.names<-unique(df$genus.sp)


#script for producing maps of the data by species.
for (i in solanum.clade.names)
{
  print(i)
  df[grepl(i,df$genus.sp),]->occurence.records[[i]]
  
  #This writes a table of occurence data for each species
  name<-paste("output/",i,"occurence_data.txt",sep="")
  write.table(df[grepl(i,df$genus.sp),], file=name)
  
  #This produces a map for each species
  df[grepl(i,df$genus.sp),]->toto
  name.map<-paste("output/",i,"_occurence_data.pdf",sep="")
  pdf(file=name.map)
  plot(wrld_simpl)
  points(toto$LONGDEC,toto$LATDEC,col="red",pch=20,cex=0.75)
  name.title<-paste(i,"occurence","data")
  title(name.title)
  dev.off()
  #class(df[grepl(i,df$genus.sp),])  
  print(dim(occurence.records[[i]]))
  
  dim(occurence.records[[i]])->number.records.solanum[i,]
}


#This indicates the number of recrods per species, from smallest number of records to largest.
number.records.solanum
toto<-number.records.solanum[order(number.records.solanum[,1]),]
toto
write.table(toto,file="output/summary_nb_records_solanum.txt",sep="\t")


# plot all data
data<-df
names(data)

plot(wrld_simpl)
#plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col='red', pch=20, cex=0.75)
#dev.off()



######################################################
#Step 3: Remove species that are cultivated + other bits and bobs
######################################################

sp.to.remove<-c(
  "Solanum_cheesmaniae x galapagense",
  "Solanum_cheesmaniae x pimpinellif",
  "Solanum_cheesmaniae_lycopersicum",
  "Solanum_pimpinellif x lycopersicu",
  "Solanum_sp.",
  "Solanum_sp. (Basarthrum)",
  "Solanum_sp. (Brevantherum)",
  "Solanum_sp. (Dulcamaroid)",
  "Solanum_sp. (Erythrotrichum)",
  "Solanum_sp. (Geminata)",
  "Solanum_sp. (Morelloid)",
  "Solanum_sp. (Petota)",
  "Solanum_spnov",
  "Aureliana_fasciculata",
  "Jaltomata_antillana",
  "Lycianthes_biflora",
  "Lycianthes_fugax",
  "Lycianthes_laevis",
  "Lycianthes_stellata",
  "Lycianthes_testacea",
  "Lycianthes_virgata",
  "Lycianthes_pachypetala",
  "Solanum_'hunzikeri'",
  "Solanum_'hydroides'",
  "Solanum_'pseudonitidi'",
  "Solanum_'tiinae2",
  "Capsicum_lucidulum",
  "Capsicum_lucidum",
  "Lycopersicon_esculentum",
#Adding here species names that are synonym of other genera
 "Solanum_berteroanum",
 "Solanum_blumei",
 "Solanum_cyathocalyx",
 "Solanum_davidsoniae",
 "Solanum_dejectum",
 "Solanum_denticulatum",
 "Solanum_domingense",
 "Solanum_guianense",
 "Solanum_haitense",
 "Solanum_jelskii",
 "Solanum_longepedunculatum",
 "Solanum_macrodon",
 "Solanum_mitratum",
 "Solanum_nocturnum",
 "Solanum_peckii",
 "Solanum_pedunculare",
 "Solanum_symonianum"
)
data<-df
dim(data) #115475

data <- data[ ! data$genus.sp %in% sp.to.remove, ]

#for (i in 1:length(sp.to.remove))
#{data<-data[!data$genus.sp==sp.to.remove[i],]
#}
dim(df)#72306 #115475
dim(data)#72284 #113112


#remove cultivated species from Solanum
remove.cultivated<-c("Solanum_tuberosum","Solanum_lycopersicum", "Solanum_melongena", "Solanum_muricatum","Solanum_aethiopicum", 
              "Solanum_macrocarpon", "Solanum_lasiocarpum", "Solanum_betaceum", "Solanum_sessiliflorum", "Solanum quitoense",
              "Solanum_scabrum", "Solanum_aviculare", "Solanum_crispum", "Solanum_laciniatum", "Solanum_laxum", 
              "Solanum_pseudocapsicum", "Solanum_seaforthianum", "Solanum_wendlandii", "Solanum_mammosum")
data <- data[ ! data$genus.sp %in% remove.cultivated, ]
dim(data)#107205
length(unique(data$genus.sp))#1226


#check if there is an issue with missing LATDEC and LONGDEC

table(is.na(data$LONGDEC))#FALSE
table(is.na(data$LATDEC))#FALSE




#This next section, we will be checking for species that are not in the accepted list of names for the genus Solanum, according to the SolanaceaeSource database

#Retrieving list of accepted names.
#


name.list<-read.csv("../../00_Taxonomy_Accepted_Names/solanum_clades_accepted.csv", header=TRUE)
head(name.list)
dim(name.list)#1257

species<-word(name.list$X...FULLNAME,2)
genus.sp<-paste("Solanum_",species,sep="")

length(setdiff(unique(data$genus.sp),genus.sp))
#114 problematic names, not in the list, checking what are the problems

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
name.list2<-read.table("../../00_Taxonomy_Accepted_Names/master_Solanaceae_names.txt", sep="\t", header=TRUE)
head(name.list2)

#3) Use the recoderFunc command to create a new list, that will be based on searching and replacing the oldnames with the newnames
data$genus.sp->toto2
newnames<-recoderFunc(toto2, name.list2$tip.name, name.list2$current.accepted.name)
length(newnames)#107205

#4) Check if new names still has problematic names
length(setdiff(unique(newnames),genus.sp))#29

setdiff(unique(newnames),genus.sp)#39

#5) Add the updated names to the supermatrix
data$genus.sp<-newnames
length(unique(data$genus.sp))#1146 names

1146/1250 #92%

#6) Write new fasta file into your documents.
write.csv(table(data$genus.sp),"freq_names.csv")



######################################################
#Step 4: Check for wrong country
###################################################
library(dismo)

# Set working directory

getwd()


#In column COUNTRY, rename United States of America, to United States

sub("United States of America","United States",data$COUNTRY)->new
new->data$COUNTRY

sub("Russian Federation","Russia",data$COUNTRY)->new
new->data$COUNTRY

sub("Virgin Islands \\(US\\)","Virgin Islands, U.S.", data$COUNTRY)->new2
new2->data$COUNTRY

sub("Netherlands Antilles","Bonaire, Saint Eustatius and Saba",data$COUNTRY)->new
new->data$COUNTRY

sub("Myanmar \\(Burma\\)","Myanmar",data$COUNTRY)->new
new->data$COUNTRY

sub("Cocos \\(Keeling\\) Islands","Cocos Islands",data$COUNTRY)->new
new->data$COUNTRY

sub("Egypt\\/Sudan","Sudan",data$COUNTRY)->new
new->data$COUNTRY

sub("C?te d'Ivoire","Côte d'Ivoire",data$COUNTRY)->new
new->data$COUNTRY

sub("Brunei Darussalam","Brunei",data$COUNTRY)->new
new->data$COUNTRY

sub("Korea, Republic of","South Korea",data$COUNTRY)->new
new->data$COUNTRY

sub("Marquesas Islands","French Polynesia",data$COUNTRY)->new
new->data$COUNTRY

sub("Syrian Arab Republic","Syria",data$COUNTRY)->new
new->data$COUNTRY

sub("Palestine","Palestina",data$COUNTRY)->new
new->data$COUNTRY

sub("S?o Tome e Principe","Sao Tome and Principe",data$COUNTRY)->new
new->data$COUNTRY



table(new)
table(data$COUNTRY)

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


j <-which((cntr) != as.vector(data2$COUNTRY))
j

length(j)#gives number of coordinaes which don't fall in the right country
#164

cbind(cntr, data2$COUNTRY)

cbind(cntr, data)[j,]->probs_j
table(probs_j$genus.sp)
table(probs_j$COUNTRY)

dim(probs_j)
#[1] 164 174
probs_j->flags.countries

#visualize
plot(wrld_simpl)
points(flags.countries$LONGDEC,flags.countries$LATDEC,col="red",pch=20,cex=0.75)

#write.csv(flags.countries,"./flags_countries_other2round.csv")

#remove this data
data[-j,]->data.country
dim(data.country)#106531
length(unique(data.country$genus.sp))#1244
data.country->data

#Add in Data that is actually okay, upon inspection
extract.brahms<-c(102417, 181, 20675, 66264,66292,68020,102703,102704,102706,30823,30824,31355,31356,31445,31446
                  ,31448,31449,31451,62071,62083
                  ,66413
                  ,66415
                  ,66438
                  ,66540
                  ,66542
                  ,128889
                  ,128938
                  ,128939
                  ,128945
                  ,128961
                  ,128963
                  ,128964
                  ,128965
                  ,128968
                  ,128969
                  ,128970
                  ,101972
                  ,105474
                  ,105475
                  ,105478
                  ,105479
                  ,98350
                  ,98352
                  ,57519
                  ,96156
                  ,96157
                  ,96158
                  ,96228
                  ,66446
                  ,66461
                  ,119108
                  ,60563
                  ,132184
                  ,132185
                  ,132186
                  ,132187
                  ,191
                  ,258
                  ,13596
                  ,102721
                  ,68156
                  ,68158
                  ,68194
                  ,68195
                  ,33848
                  ,33849
                  ,33869
                  ,33950
                  ,33951
                  ,33952
                  ,33971
                  ,33973
                  ,34121
                  ,34123
                  ,54122
                  ,104152
                  ,104153
                  ,104154
                  ,110377,117716,119932,129409,129411,67639,67862,67864,67865,67866,67869,67870,67872,67873,67875,67877,67880,102443
                  ,67623
                  ,68064
                  ,102860
                  ,102868
                  ,102869
                  ,102877
                  ,102878
                  ,102879
                  ,102884
                  ,102885
                  ,102888
                  ,102889
                  ,139330
                  ,39465
                  ,41401
                  ,41421
                  ,41423
                  ,33869
                  ,33950
                  ,33951
                  ,34121
                  ,34123
                  ,34131
                  ,34135
                  ,129411)


data.keep <- data[  data$BRAHMS %in% extract.brahms, ]
dim(data.keep)

data.country2<-rbind(data.country,data.keep)
dim(data.country2)#106510
data.country2->data

dim(df)[1]-dim(data)[1]#8965 removed from country

### cleaning up
remove(world)
remove(toto)
remove(toto2)

#save.image("workingspace_cleaning_occurrence.R")

##################
# Step 5: check if occurrence data in the sea 
# Note: this section takes a lot of memory, and so was done on a HPC server 
##################

############################################################################################################################
# 1) Load packages 
############################################################################################################################

data$genus.sp <- paste(data$GENUS,"_",data$SP1, sep="")

data.coord <- data %>%
  dplyr::select(genus.sp, LATDEC, LONGDEC, COUNTRY, MAJORAREA, LOCNOTES,LATLONG,
                BRAHMS, FAMILY,  YEAR, GENUS, ALT, ALTMAX, ALTUNIT, ALTRES,
                TYPE, ACCEPTED, COLLECTOR, NUMBER) 

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
predictors.bio1<-raster(files) #selects Bio1 file
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
sum(is.na(data.coord.land)) #number of specimens falling outside the mask: 31306
sum(!is.na(data.coord.land)) #number of specimens falling inside the mask : 75227

#plot the mask and specimens that fall off the mask

#Save flags in flag directory
#pdf("Flags_Sea.pdf")
plot(predictors.bio1, col="gray90", useRaster=T, legend=F)
plot(data.coord.spatial[which(is.na(data.coord.land))], add=T, pch=19, cex=0.2, col="blue")
points(data.coord[which(is.na(data.coord.land)),3:2], pch=19, cex=0.5, col="red")
mtext(side=1, "Longitude (degrees)", cex=1.5, line=3)
mtext(side=2, "Latitude (degrees)", cex=1.5, line=3)
#dev.off()

class(data.coord.land)
length(data.coord.land)#106497


############################################################################################################################
# 4)  Discard the specimen records that fall outside the mask, and write them into a separate file.
############################################################################################################################


flags.pushback<- data.frame(data[which(is.na(data.coord.land)),])
length(data.coord.land)
dim(flags.pushback)
head(flags.pushback)
table(flags.pushback$genus.sp)
table(flags.pushback$COUNTRY)


write.csv(flags.pushback, file="./flags_pushback_other.csv")
# examine that latter...

data.pushback<- data[!data$BRAHMS %in% flags.pushback$BRAHMS,]
data<-data.pushback
dim(data.pushback)#75210
length(unique(data.pushback$genus.sp))#1126
plot(wrld_simpl,  axes=TRUE, col="light yellow")
points(flags.pushback$LONGDEC,flags.pushback$LATDEC,col="red",pch=20,cex=0.75)






###################################################################################
#
#Step 6: Coordinate_Cleaner section
#
###################################################################################


#select columns of interest
data3 <- data %>%
  dplyr::select(genus.sp, LATDEC, LONGDEC, COUNTRY, MAJORAREA,
                BRAHMS, FAMILY,  YEAR, GENUS, ALT, ALTMAX, ALTUNIT, ALTRES,
                TYPE, ACCEPTED, COLLECTOR, NUMBER) 


# remove records without coordinates

data3 <- data3%>%
  filter(!is.na(LONGDEC))%>%
  filter(!is.na(LATDEC))
dim(data3)#75182

##plot data to get an overview
wm <- borders("world", colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = data3, aes(x = LONGDEC, y = LATDEC),
             colour = "darkred", size = 0.5)+
  theme_bw()

#convert country code from ISO2c to ISO3c

data3$ISO3C <-  countrycode(data3$COUNTRY, origin =  'country.name', destination = 'iso3c')

data[data3$COUNTRY=="Micronesia",]
data[data3$COUNTRY=="Netherland Antilles",]

#to avoid specifying it in each function

(names(data3))
names(data3)[2]<-"decimallatitude"
names(data3)[3]<-"decimallongitude"
(names(data3))


####points that are not valid
clean_val <- data3%>%  cc_val()

data[!data3$BRAHMS%in%clean_val$BRAHMS,]->val
dim(val)#0!


####Equivalent lon & lat
clean_equ <- data3%>%
  cc_equ()

data[!data3$BRAHMS%in%clean_equ$BRAHMS,]->equ
dim(equ)#4

#pdf(file="./flags/Solanum_equ_flag.pdf")
plot(wrld_simpl, axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(equ$LONGDEC, equ$LATDEC, col="red", cex=0.75)
#dev.off()

write.csv(equ,file="./Flag_equ_solanum.csv")

####Points that fall near the gbif institution
clean_gbif <- data3%>%
  cc_gbif()

data[!data3$BRAHMS%in%clean_gbif$BRAHMS,]->gbif
dim(gbif)#0!

####Points that fall near gardens and other institutions
clean_inst <- data3%>%
  cc_inst()

data[!data3$BRAHMS%in%clean_inst$BRAHMS,]->inst
dim(inst)#128!

#pdf(file="./flags/Solanum_inst_flag.pdf")
plot(wrld_simpl,  axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(inst$LONGDEC, inst$LATDEC, col="red", cex=0.75)
#dev.off()

write.csv(inst,file="./Solanum_inst_flag.csv")

####Points that are zero longitude and zero latitude and a radium around the zero lon and zero lat
clean_zero <- data3%>%
  cc_zero()

data[!data3$BRAHMS%in%clean_zero$BRAHMS,]->zero
dim(zero)#4

#pdf(file="./Solanum_zero_flag.pdf")
plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(zero$LONGDEC, zero$LATDEC, col="red", cex=0.75)
#dev.off()
write.csv(zero,file="./Solanum_zero_flag.csv")

#Final dataset without the values flagged in previous tests
clean_val[clean_val$BRAHMS%in%clean_equ$BRAHMS,]->clean_cc
clean_cc[clean_cc$BRAHMS%in%clean_gbif$BRAHMS,]->clean_cc
clean_cc[clean_cc$BRAHMS%in%clean_inst$BRAHMS,]->clean_cc
clean_cc[clean_cc$BRAHMS%in%clean_zero$BRAHMS,]->clean_cc

dim(clean_cc)
data[data3$BRAHMS%in%clean_cc$BRAHMS,]->clean_cc2
dim(clean_cc2)

data<-clean_cc2 # 76527 occurence records

#############
#Second round of functions that help clean
####Points that fall into centroids
clean_cen <- data3%>%
  cc_cen()

data[!data3$BRAHMS%in%clean_cen$BRAHMS,]->cen
dim(cen) # 237

#pdf(file="./Solanum_cen_flag.pdf")
plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(cen$LONGDEC, cen$LATDEC, col="red", cex=0.75)
#dev.off()

getwd()
write.csv(cen,file="./Solanum_cen_flag.csv")


####Points that fall into capitals, within a 10 km radium from centroid
clean_cap <- data3%>%
  cc_cap()
#1674 records

data[!data3$BRAHMS%in%clean_cap$BRAHMS,]->cap
dim(cap)#1674 records

#pdf(file="./Solanum_cap_flag.pdf")
plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(cap$LONGDEC, cap$LATDEC, col="red", cex=0.75)
#dev.off()

getwd()
write.csv(cap,file="./Solanum_cap_flag.csv")

######### removing duplicates

#Second version, removes less, and herbarium duplicates
clean_dupl <- data3%>%
  cc_dupl(.,species="genus.sp",additions = c("COLLECTOR","NUMBER")) #936 records

data[!data3$BRAHMS%in%clean_dupl$BRAHMS,]->dupl
dim(dupl) #934

#pdf(file="./Solanum_dupl_flag.pdf")
plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE, col="light yellow")
points(data$LONGDEC, data$LATDEC, col="black", cex=0.75)
points(dupl$LONGDEC, dupl$LATDEC, col="red", cex=0.75)
#dev.off()

write.csv(dupl,file="./flags/Solanum_dupl_flag.csv")

dim(data)#75058

toto<-data
toto<-toto[toto$BRAHMS%in%clean_dupl$BRAHMS,]
toto<-toto[toto$BRAHMS%in%clean_cap$BRAHMS,]
toto<-toto[toto$BRAHMS%in%clean_cen$BRAHMS,]

dim(toto)#72485

dim(data)-dim(toto)#2573 removed
data->backup
toto->data.dup
toto->data

data.dup->data
dim(data)#72485

###################################

write.table(data,file="FINAL_72485_Solanum_SolSource.tab",sep = "\t")
write.table(data,file="FINAL_72485_Solanum_SolSource.csv",sep = ";")
#save.image("working_space.R")
#############

#pdf("FINAL_72485_map_SOLANUM.pdf")
plot(wrld_simpl, axes=TRUE, col="light yellow")
points(original.data$LONGDEC, original.data$LATDEC, col="black", cex=0.75)
points(data$LONGDEC, data$LATDEC, col="red", cex=0.75)
#dev.off()

#### Remove additional species that are not accepted
remove_species<-c("Solanum_agglutinatum","Solanum_ferox","Solanum_glandulosum","Solanum_haematocarpon","Solanum_killippii","Solanum_ligulatum",
                 "Solanum_madidum","Solanum_micranthum","Solanum_mirum","Solanum_paucispinum","Solanum_postremum","Solanum_ramizii",
                 "Solanum_rufistellatum","Solanum_serratum","Solanum_rondeletii")

data <- data[ ! data$genus.sp %in% remove_species, ]

dim(data)#72448
save.image("working_space2.R")

#next steps i will modify the synonyms according to Tiina's table, then merge with Australian dataset
############# GO clean Australian specimens
#

############### ADD AUSTRALIAN SPECIMENS.
add_records2<-read.csv(file.choose(),header=TRUE,sep=";")
#Added a file with 24407 records
#

add_records<-add_records2

#need to add 
aus.names<-names(add_records2)
data.dup.names<-names(data)

setdiff(aus.names,data.dup.names)
setdiff(data.dup.names,aus.names)

names(add_records2)[18]<-"COUNTRY"
names(add_records2)[12]<-"USES"
names(add_records2)[16]<-"HABITATTXT"
names(add_records2)[17]<-"NOTES"
names(add_records2)[33]<-"DETBY" 
names(add_records2)[34]<-"DETDAY"
names(add_records2)[35]<-"DETMONTH" 
names(add_records2)[36]<-"DETYEAR"
names(add_records2)[39]<-"SP1"
names(add_records2)[40]<-"AUTHOR1"
names(add_records2)[4]<-"ACCESSION"
names(add_records2)[1]<-"BRAHMS"
names(add_records2)[22]<-"ALT"


setdiff(aus.names,data.dup.names)
setdiff(data.dup.names,aus.names)

length(unique(data$genus.sp))#1123 species.

remove.col<-setdiff(data.dup.names,aus.names)
data.slim<-data[,!names(data) %in% remove.col]
dim(data.slim)#72485 18

remove.col2<-setdiff(aus.names,data.dup.names)
data.slim2<-add_records2[,!names(add_records2) %in% remove.col2]
dim(data.slim2)#24407 18

72485+24407

aus.names
data.dup.names
names(data.slim)
merge.aus<-rbind(data.slim,data.slim2)
dim(merge.aus)#96892

#Ok, merge was successful!
getwd()
write.table(merge.aus,file="FINAL_96892_Solanum_07_2021.csv",sep=";")
