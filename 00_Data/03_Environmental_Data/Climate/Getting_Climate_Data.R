########################################################################
#
# Script for extracting data from climatic layers with specimen occurrence records of Solanum
# 
# by E. Gagnon, part of Solanum Geophyte publication 2023
########################################################################

rm(list=ls())

#Load libraries
library(raster)
library(maptools)
data(wrld_simpl)

#Set working directiry
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/03_Environmental_Data/Climate/")
dir()

# Load occurrence data
occurrence.data<-read.csv("../../02_Occurrence_Data/03_Spatial_filtering/DF_resuls_all_spatial_filtering_50379a.csv",row.names=1)

wd <- getwd()
files <- list.files(path=wd, pattern='.tif', full.names=TRUE)
list(files)

###############################
# Annual Moisture Index (MI)


predictors <- stack(files[[1]]) #MI
predictors

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)

# here make sure you are calling the right column headings
# i think for you it should say $Long and $lat 
dim(occurrence.data)
names(occurrence.data)#12-13

# extract raw values which you'll need for running SDMs (95 equals long, 94 equal lat)
AMI_values <- raster::extract(predictors, occurrence.data[,13:12])
AMI_values1 <- data.frame(AMI_values)
str(AMI_values1)

occurrence.data$mi<-AMI_values1$X04_AnnualMI_world
table(is.na(occurrence.data$mi))#566 occurrence records don't have GDD values

###############################
# Chelsa Bio5

predictors <- stack(files[[2]]) #Bio5, max heat
predictors

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)

# extract raw values which you'll need for running SDMs (95 equals long, 94 equal lat)
bio5_values <- extract(predictors, occurrence.data[,13:12])
bio5_values1 <- data.frame(bio5_values)
str(bio5_values1)

occurrence.data$bio5<-bio5_values1$CHELSA_bio_05
table(is.na(occurrence.data$bio5)) #0 records don't have bio5 values

###############################
# Chelsa Bio6

predictors <- stack(files[[3]]) #Bio6, cold
predictors

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)

# extract raw values which you'll need for running SDMs (95 equals long, 94 equal lat)
bio6_values <- extract(predictors, occurrence.data[,13:12])
bio6_values1 <- data.frame(bio6_values)
str(bio6_values1)

occurrence.data$bio6<-bio6_values1$Chelsa_bio_06
table(is.na(occurrence.data$bio6)) #0 records don't have bio6 values

###############################
# Chelsa Bio7

predictors <- stack(files[[4]]) #Bio7, temperature range
predictors

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)

# extract raw values which you'll need for running SDMs (95 equals long, 94 equal lat)
bio7_values <- extract(predictors, occurrence.data[,13:12])
bio7_values1 <- data.frame(bio7_values)
str(bio7_values1)

occurrence.data$bio7<-bio7_values1$Chelsa_bio_07
table(is.na(occurrence.data$bio7)) #0 records don't have bio7 values


###############################
# Chelsa Bio15
#### Precipitation Seasonality

predictors <- stack(files[[5]]) #Bio15
predictors

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)

# extract raw values which you'll need for running SDMs (95 equals long, 94 equal lat)
BIO15_values <- extract(predictors, occurrence.data[,13:12])
BIO15_values1 <- data.frame(BIO15_values)
str(BIO15_values1)

occurrence.data$bio15<-BIO15_values1$CHELSA_bio_15
table(is.na(occurrence.data$bio15)) #3 records don't have bio15 values



#################################
#This is a little script that will print each environmental layer as a pdf, then as a pdf with missing data.

for (i in 1:length(files))
{
predictors <- stack(files[[i]]) #Length growing season(months)
predictors
layer.name<-names(predictors)

pdf(file=paste(layer.name,".pdf",sep=""))

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)
title(names(predictors))
dev.off()

###Now doing it plotting where missing data is.

values <- extract(predictors, occurrence.data[,13:12])
values1 <- data.frame(values)
table(is.na(values1))[2]
list1<-which(is.na(values1))


pdf(file=paste(layer.name,"_Missing_Data.pdf",sep=""))

plot(predictors, 1)
plot(wrld_simpl, add=TRUE)
points(occurrence.data$LONGDEC[list1], occurrence.data$LATDEC[list1], col='red', pch=20, cex=0.75)
title(paste(names(predictors),"_Missing_Data",sep=""))

dev.off()


}

### For each cell, add rastercell number, based on bio6

predictors <- stack(files[[3]]) #bio6

xy<-cbind(occurrence.data$LONGDEC,occurrence.data$LATDEC)
cell.nb<-extract(predictors, xy)
dim(cell.nb)
length(cell.nb)
head(cell.nb)

occurrence.data$cell<-cell.nb

write.table(occurrence.data,"DF_results_climate_50379.csv",sep=";")
