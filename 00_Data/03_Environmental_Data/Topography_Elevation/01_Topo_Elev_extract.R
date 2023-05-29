################################################################################
#Extraction environmental values for topographic data 
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#
#Data comes from this portal:http://www.earthenv.org/topography
#Download beforehand the tif file you are interested in from the website above.
###############################################################################

#load packages
require(envirem)
library(raster)
require(SpaDES)
library(maptools)  ## For wrld_simpl


#Get occurrence data
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/03_Environmental_Data/Topography_Elevation/")
dir()

#Going to do the climate data before I do the fire data. 

#occurrence.data<-read.table("DF_resuls_all_spatial_filtering.csv",sep=";")
occurrence.data<-read.csv("../../02_Occurrence_Data/03_Spatial_filtering/DF_resuls_all_spatial_filtering_50379a.csv",row.names=1)

toto<-occurrence.data

#Get environmental layers
dir()
elevation_sd<-raster("elevation_1KMsd_SRTM.tif")
elevation_sd #30 arc sec

vrm_med<-raster("vrm_1kmmd_SRTM.tif")
vrm_med #30 arc sec
#########
#Extract values based on environmental layers

vrm_med_ext <- raster::extract(vrm_med, occurrence.data[,13:12])
vrm_med_ext_1 <- data.frame(vrm_med_ext)
str(vrm_med_ext_1)

toto$vrm_med<-vrm_med_ext_1$vrm_med_ext
table(is.na(toto$vrm_med))#13 occurrence records have NA values

#########
elv_sd_ext <- raster::extract(elevation_sd, occurrence.data[,13:12])
elv_sd_ext_1 <- data.frame(elv_sd_ext)
str(elv_sd_ext_1)

toto$elv_sd<-elv_sd_ext_1$elv_sd_ext
table(is.na(toto$elv_sd))#13 occurrence records have NA values

write.csv(toto,"topo_data_50379.csv")

############

#plotting the maps of the data, to see what it will look like.

library(maptools)
data(wrld_simpl)

getwd()

#pdf(file="maptest.pdf",width=8.5, height = 11)
plot(elevation_sd)
plot(wrld_simpl, add=TRUE)

plot(vrm_med)
plot(wrld_simpl, add=TRUE)

#dev.off()

