################################################################################
#Getting Soil grid layers into R, and extracting data.
#
#I really wanted to have the SoilGrids data at 30 arcsec resolution
#I found a github repository that allowed to download all the tiles automatically.
#https://github.com/zecojls/downloadSoilGridsV2
#The script is super long, because it then goes through all the tiles and checks if there are
#any occurrence records that match that tile, and extracts the relevant info.
#I present an example here with the sand layer!
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
################################################################################

rm(list=ls())

#set working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/03_Environmental_Data/Soil/")
dir.create("raster")
dir()


#libraries
library(curl)
library(XML)
library(tidyverse)

library(raster)
library(maptools)
data(wrld_simpl)


## Setup

options(stringsAsFactors = FALSE)

options(pillar.sigfig=3)


## Directories

dir.root <- dirname(getwd()); dir.root
dir.proj <- getwd(); dir.proj

list.files(dir.root)
list.files(dir.proj)

dir.export <- paste0(dir.proj, "/raster")

min.long <- -179
min.lat <- -51
max.long <- 190
max.lat <- 70

seq.long <- seq(min.long, max.long, by = 10)
seq.lat <- seq(min.lat, max.lat, by = 10)

combination.min <- expand.grid(seq.long[-length(seq.long)], seq.lat[-length(seq.lat)])
combination.max <- expand.grid(seq.long[-1], seq.lat[-1])

full.combination <- tibble(min.long = combination.min[,1],
                           max.long = combination.max[,1],
                           min.lat = combination.min[,2],
                           max.lat = combination.max[,2])

full.combination <- full.combination %>%
  mutate(min.long = min.long - 0.01,
         max.long = max.long + 0.01,
         min.lat = min.lat - 0.01,
         max.lat = max.lat + 0.01)

full.combination <- as.data.frame(full.combination)

bbox.coordinates <- full.combination %>%
  mutate(left.coord = paste0(ifelse(min.long < 0, "W", "E"), round(abs(min.long), 0)),
         top.coord = paste0(ifelse(max.lat < 0, "S", "N"), round(abs(max.lat), 0)))

bbox.coordinates

# Download links

# WRB
#"https://maps.isric.org/mapserv?map=/map/wrb.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=MostProbable&FORMAT=image/tiff&SUBSET=long(-54.2280,-52.2280)&SUBSET=lat(-22.0906,-20.0906)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
# pH
#'https://maps.isric.org/mapserv?map=/map/phh2o.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=phh2o_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-51.8169,-49.8169)&SUBSET=lat(-20.9119,-18.9119)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326'
# SOC
#"https://maps.isric.org/mapserv?map=/map/soc.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=soc_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-52.0848,-50.0848)&SUBSET=lat(-17.2684,-15.2684)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
# N
#"https://maps.isric.org/mapserv?map=/map/nitrogen.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=nitrogen_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-49.0307,-47.0307)&SUBSET=lat(-20.4832,-18.4832)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
# CTC
#"https://maps.isric.org/mapserv?map=/map/cec.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=cec_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-49.2986,-47.2986)&SUBSET=lat(-23.7516,-21.7516)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
# Silt
#"https://maps.isric.org/mapserv?map=/map/silt.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=silt_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-51.1739,-49.1739)&SUBSET=lat(-20.1082,-18.1082)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
# Clay
#"https://maps.isric.org/mapserv?map=/map/clay.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=clay_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-52.8950,-50.8950)&SUBSET=lat(-19.4116,-17.4116)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
# Sand
#"https://maps.isric.org/mapserv?map=/map/sand.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=sand_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-48.3342,-46.3342)&SUBSET=lat(-19.1437,-17.1437)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326"
# BD
#'https://maps.isric.org/mapserv?map=/map/bdod.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=bdod_0-5cm_mean&FORMAT=image/tiff&SUBSET=long(-50.9661,-48.9661)&SUBSET=lat(-18.0721,-16.0721)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326'

# Automatic download

bbox.coordinates

attributes <- c("sand.map")
layers <- c("0-5cm_mean")#, "5-15cm_mean")#, "15-30cm_mean", "30-60cm_mean")

#It's possible to do for several values at the same time
#attributes <- c("clay.map","sand.map","bdod.map","cfvo.map")
#layers <- c("0-5cm_mean", "5-15cm_mean", "15-30cm_mean", "30-60cm_mean")



for(a in 1:length(attributes)) {
  
  attribute <- attributes[a]
  
  attribute.prefix <- gsub(".map", "", attribute)
  
  if(attribute == "wrb.map") {
    
    layer <- "MostProbable"
    
    for(t in 1:nrow(bbox.coordinates)) {
      
      min.long = bbox.coordinates[t,"min.long"]
      max.long = bbox.coordinates[t,"max.long"]
      min.lat = bbox.coordinates[t,"min.lat"]
      max.lat = bbox.coordinates[t,"max.lat"]
      left.coord <- bbox.coordinates[t,"left.coord"]
      top.coord <- bbox.coordinates[t,"top.coord"]
      
      wcs <- paste0("https://maps.isric.org/mapserv?map=/map/", attribute, "&",
                    "SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=", layer, "&",
                    "FORMAT=image/tiff&",
                    "SUBSET=long(", min.long, ",", max.long, ")&",
                    "SUBSET=lat(", min.lat, ",", max.lat, ")&",
                    "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
      
      destination.file <- paste0(dir.export, "/SoilGrids_",
                                 paste(attribute.prefix, layer,
                                       left.coord, top.coord, sep = "_"),
                                 ".tif")
      
      if(file.exists(destination.file)) {
        
        next
        
      } else {
        
        cat("Downloading: ", destination.file, "\n")
        download.file(wcs, destfile = destination.file, mode = 'wb')
        
      }
      
    }
    
  } else {
    
    for(l in 1:length(layers)) {
      
      layer <- layers[l]
      
      for(t in 1:nrow(bbox.coordinates)) {
        
        min.long = bbox.coordinates[t, "min.long"]
        max.long = bbox.coordinates[t, "max.long"]
        min.lat = bbox.coordinates[t, "min.lat"]
        max.lat = bbox.coordinates[t, "max.lat"]
        left.coord <- bbox.coordinates[t, "left.coord"]
        top.coord <- bbox.coordinates[t, "top.coord"]
        
        wcs <- paste0("https://maps.isric.org/mapserv?map=/map/", attribute, "&",
                      "SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=", attribute.prefix, "_", layer, "&",
                      "FORMAT=image/tiff&",
                      "SUBSET=long(", min.long, ",", max.long, ")&",
                      "SUBSET=lat(", min.lat, ",", max.lat, ")&",
                      "SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
        
        destination.file <- paste0(dir.export, "/SoilGrids_",
                                   paste(attribute.prefix, layer,
                                         left.coord, top.coord, sep = "_"),
                                   ".tif")
        
        if(file.exists(destination.file)) {
          
          next
          
        } else {
          
          cat("Downloading: ", destination.file, "\n")
          download.file(wcs, destfile = destination.file, mode = 'wb')
          
        }
      }
    }
  }
}


##################################
#Now extracting data
# This step takes up a lot of memory, might be good to empty out temporary directory, ir run this on a HPC.

wd<-getwd()
# Load occurrence data
occurrence.data<-read.csv("../../02_Occurrence_Data/03_Spatial_filtering/DF_resuls_all_spatial_filtering_50379a.csv",row.names=1)

#do sand layer layer

library(raster)

setwd("C:/Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/03_Environmental_Data/Soil/raster/")

tif_layers<-list.files(pattern=".tif", full.names=T)
tmp <- lapply(tif_layers,raster)
tmp$tolerance <- 1

####################
#1-11
tmp2<-tmp[1:14]
tmp2$tolerance<-1
sand_05mean_099_1101<-do.call(merge,tmp2)

toto2<-occurrence.data[which(occurrence.data$LONGDEC>0.99 & occurrence.data$LONGDEC<=11),]
dim(toto2)#332

plot(sand_05mean_099_1101)
plot(wrld_simpl, add=TRUE)
points(toto2$LONGDEC,toto2$LATDEC, col="red")


# extract raw values
sand_0991101 <- raster::extract(sand_05mean_099_1101, toto2[,13:12])
sand_0991101_1 <- data.frame(sand_0991101)
str(sand_0991101_1)

toto2$sand_05_mean<-sand_0991101_1$sand_0991101
table(is.na(toto2$sand_05_mean))#0 occurrence records don't have GDD values

#writeRaster(sand_05mean_099_1101,"sand_05mean_099_1101.tif")

#####101-111

tmp3<-tmp[15:26]
tmp3$tolerance<-1
sand_05mean_101_111<-do.call(merge,tmp3)
sand_05mean_101_111_arc30<-aggregate(sand_05mean_101_111,4)

gc()

toto3<-occurrence.data[which(occurrence.data$LONGDEC>=100.99 & occurrence.data$LONGDEC<=111),]
dim(toto3)#298

plot(sand_05mean_101_111)
plot(wrld_simpl, add=TRUE)
points(toto3$LONGDEC,toto3$LATDEC, col="red")



# extract raw values
sand_101_111 <- raster::extract(sand_05mean_101_111_arc30, toto3[,13:12])
sand_101_111_1 <- data.frame(sand_101_111)
str(sand_101_111_1)

toto3$sand_05_mean<-sand_101_111_1$sand_101_111
table(is.na(toto3$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
final.data<-rbind(toto2,toto3)

############

#####11-21

tmp4<-tmp[27:38]
tmp4$tolerance<-1
sand_05mean_11_21<-do.call(merge,tmp4)
sand_05mean_11_21_arc30<-aggregate(sand_05mean_11_21,4)
gc()

toto4<-occurrence.data[which(occurrence.data$LONGDEC>=10.99 & occurrence.data$LONGDEC<=21),]
dim(toto4)#298

plot(sand_05mean_11_21)
plot(wrld_simpl, add=TRUE)
points(toto4$LONGDEC,toto4$LATDEC, col="red")

# extract raw values
sand_11_21 <- raster::extract(sand_05mean_11_21_arc30, toto4[,13:12])
sand_11_21_1 <- data.frame(sand_11_21)
str(clay_11_21_1)

toto4$sand_05_mean<-sand_11_21_1$sand_11_21
table(is.na(toto4$sand_05_mean))#0 occurrence records don't have GDD values

#add results to table
final.data<-rbind(final.data,toto4)

##########

#####111-121

tmp5<-tmp[39:50]
tmp5$tolerance<-1
sand_05mean_111_121<-do.call(merge,tmp5)
sand_05mean_111_121_arc30<-aggregate(sand_05mean_111_121,4)
gc()

toto5<-occurrence.data[which(occurrence.data$LONGDEC>=110.99 & occurrence.data$LONGDEC<=121),]
dim(toto5)#1913

plot(sand_05mean_111_121)
plot(wrld_simpl, add=TRUE)
points(toto5$LONGDEC,toto5$LATDEC, col="red")

# extract raw values
sand_111_121 <- raster::extract(sand_05mean_111_121_arc30, toto5[,13:12])
sand_111_121_1 <- data.frame(sand_111_121)
str(sand_111_121_1)

toto5$sand_05_mean<-sand_111_121_1$sand_111_121
table(is.na(toto5$sand_05_mean))#0 occurrence records don't have GDD values

#add toto2 and 6 together
dim(final.data)#1196
final.data<-rbind(final.data,toto5)
dim(final.data)#3109


##########

#####121-131

tmp6<-tmp[51:62]
tmp6$tolerance<-1
sand_05mean_121_131<-do.call(merge,tmp6)
sand_05mean_121_131_arc30<-aggregate(sand_05mean_121_131,4)
gc()

toto6<-occurrence.data[which(occurrence.data$LONGDEC>=120.99 & occurrence.data$LONGDEC<=131),]
dim(toto6)#1913

plot(sand_05mean_121_131)
plot(wrld_simpl, add=TRUE)
points(toto6$LONGDEC,toto6$LATDEC, col="red")

# extract raw values
sand_121_131 <- raster::extract(sand_05mean_121_131_arc30, toto6[,13:12])
sand_121_131_1 <- data.frame(sand_121_131)
str(sand_121_131_1)

toto6$sand_05_mean<-sand_121_131_1$sand_121_131
table(is.na(toto6$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#3109
final.data<-rbind(final.data,toto6)
dim(final.data)#5225

#############
#131-141

tmp7<-tmp[63:74]
tmp7$tolerance<-1
sand_05mean_131_141<-do.call(merge,tmp7)
sand_05mean_131_141_arc30<-aggregate(sand_05mean_131_141,4)
gc()

toto7<-occurrence.data[which(occurrence.data$LONGDEC>=130.99 & occurrence.data$LONGDEC<=141),]
dim(toto7)#3780

plot(sand_05mean_131_141)
plot(wrld_simpl, add=TRUE)
points(toto7$LONGDEC,toto7$LATDEC, col="red")

# extract raw values
sand_131_141 <- raster::extract(sand_05mean_131_141_arc30, toto7[,13:12])
sand_131_141_1 <- data.frame(sand_131_141)
str(sand_131_141_1)

toto7$sand_05_mean<-sand_131_141_1$sand_131_141
table(is.na(toto7$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#5225
final.data<-rbind(final.data,toto7)
dim(final.data)#9005


#############
#141-151

tmp8<-tmp[75:86]
tmp8$tolerance<-1
sand_05mean_141_151<-do.call(merge,tmp8)
sand_05mean_141_151_arc30<-aggregate(sand_05mean_141_151,4)
gc()

toto8<-occurrence.data[which(occurrence.data$LONGDEC>=140.99 & occurrence.data$LONGDEC<=151),]
dim(toto8)#4209

plot(sand_05mean_141_151)
plot(wrld_simpl, add=TRUE)
points(toto8$LONGDEC,toto8$LATDEC, col="red")

# extract raw values
sand_141_151 <- raster::extract(sand_05mean_141_151_arc30, toto8[,13:12])
clay_141_151_1 <- data.frame(clay_141_151)
str(clay_141_151_1)

toto8$clay_05_mean<-clay_141_151_1$clay_141_151
table(is.na(toto8$clay_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#9005
final.data<-rbind(final.data,toto8)
dim(final.data)#13214

#############
#151-161

tmp9<-tmp[87:98]
tmp9$tolerance<-1
sand_05mean_151_161<-do.call(merge,tmp9)
sand_05mean_151_161_arc30<-aggregate(sand_05mean_151_161,4)
gc()

toto9<-occurrence.data[which(occurrence.data$LONGDEC>=150.99 & occurrence.data$LONGDEC<=161),]
dim(toto9)#3780

plot(sand_05mean_151_161)
plot(wrld_simpl, add=TRUE)
points(toto9$LONGDEC,toto9$LATDEC, col="red")

# extract raw values
sand_151_161 <- raster::extract(sand_05mean_151_161_arc30, toto9[,13:12])
sand_151_161_1 <- data.frame(sand_151_161)
str(sand_151_161_1)

toto9$sand_05_mean<-sand_151_161_1$sand_151_161
table(is.na(toto9$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#13214
final.data<-rbind(final.data,toto9)
dim(final.data)#14896


#############
#161-171

tmp10<-tmp[99:110]
tmp10$tolerance<-1
sand_05mean_161_171<-do.call(merge,tmp10)
sand_05mean_161_171_arc30<-aggregate(sand_05mean_161_171,4)
gc()

toto10<-occurrence.data[which(occurrence.data$LONGDEC>=160.99 & occurrence.data$LONGDEC<=171),]
dim(toto10)#141

plot(sand_05mean_161_171)
plot(wrld_simpl, add=TRUE)
points(toto10$LONGDEC,toto10$LATDEC, col="red")

# extract raw values
sand_161_171 <- raster::extract(sand_05mean_161_171_arc30, toto10[,13:12])
sand_161_171_1 <- data.frame(sand_161_171)
str(sand_161_171_1)

toto10$sand_05_mean<-sand_161_171_1$sand_161_171
table(is.na(toto10$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#14896
final.data<-rbind(final.data,toto10)
dim(final.data)#15037

write.table(final.data,"final_data.tab",sep="/t")

#############
#171-181

tmp11<-tmp[111:122]
tmp11$tolerance<-1
sand_05mean_171_181<-do.call(merge,tmp11)
sand_05mean_171_181_arc30<-aggregate(sand_05mean_171_181,4)
gc()

toto11<-occurrence.data[which(occurrence.data$LONGDEC>=170.99 & occurrence.data$LONGDEC<=181),]
dim(toto11)#250

plot(sand_05mean_171_181)
plot(wrld_simpl, add=TRUE)
points(toto11$LONGDEC,toto11$LATDEC, col="red")

# extract raw values
sand_171_181 <- raster::extract(sand_05mean_171_181_arc30, toto11[,13:12])
sand_171_181_1 <- data.frame(sand_171_181)
str(sand_171_181_1)

toto11$sand_05_mean<-sand_171_181_1$sand_171_181
table(is.na(toto11$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#15037
final.data<-rbind(final.data,toto11)
dim(final.data)#15287

write.table(final.data,"final_data.tab",sep="/t")



#############
#21-31

tmp12<-tmp[123:134]
tmp12$tolerance<-1
sand_05mean_21_31<-do.call(merge,tmp12)
sand_05mean_21_31_arc30<-aggregate(sand_05mean_21_31,4)
gc()

toto12<-occurrence.data[which(occurrence.data$LONGDEC>=20.99 & occurrence.data$LONGDEC<=31),]
dim(toto12)#991

plot(sand_05mean_21_31)
plot(wrld_simpl, add=TRUE)
points(toto12$LONGDEC,toto12$LATDEC, col="red")

# extract raw values
sand_21_31 <- raster::extract(sand_05mean_21_31_arc30, toto12[,13:12])
sand_21_31_1 <- data.frame(sand_21_31)
str(sand_21_31_1)

toto12$sand_05_mean<-sand_21_31_1$sand_21_31
table(is.na(toto12$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#15287
final.data<-rbind(final.data,toto12)
dim(final.data)#16278

write.table(final.data,"final_data.tab",sep="/t")


#############
#31-41

tmp13<-tmp[135:146]
tmp13$tolerance<-1
sand_05mean_31_41<-do.call(merge,tmp13)
sand_05mean_31_41_arc30<-aggregate(sand_05mean_31_41,4)
gc()

toto13<-occurrence.data[which(occurrence.data$LONGDEC>=30.99 & occurrence.data$LONGDEC<=41),]
dim(toto13)#2637

plot(sand_05mean_31_41)
plot(wrld_simpl, add=TRUE)
points(toto13$LONGDEC,toto13$LATDEC, col="red")

# extract raw values
sand_31_41 <- raster::extract(sand_05mean_31_41_arc30, toto13[,13:12])
sand_31_41_1 <- data.frame(sand_31_41)
str(sand_31_41_1)

toto13$sand_05_mean<-sand_31_41_1$sand_31_41
table(is.na(toto13$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#16278
final.data<-rbind(final.data,toto13)
dim(final.data)#18915

write.table(final.data,"final_data.tab",sep="/t")


#############
#41-51

tmp15<-tmp[159:158]

tmp14<-tmp[147:158]
tmp14$tolerance<-1
sand_05mean_41_51<-do.call(merge,tmp14)
sand_05mean_41_51_arc30<-aggregate(sand_05mean_41_51,4)
gc()

toto14<-occurrence.data[which(occurrence.data$LONGDEC>=40.99 & occurrence.data$LONGDEC<=51),]
dim(toto14)#1067

plot(sand_05mean_41_51)
plot(wrld_simpl, add=TRUE)
points(toto14$LONGDEC,toto14$LATDEC, col="red")

# extract raw values
sand_41_51 <- raster::extract(sand_05mean_41_51_arc30, toto14[,13:12])
sand_41_51_1 <- data.frame(sand_41_51)
str(sand_41_51_1)

toto14$sand_05_mean<-sand_41_51_1$sand_41_51
table(is.na(toto14$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#18915
final.data<-rbind(final.data,toto14)
dim(final.data)#19982


#############
#51-61

tmp15<-tmp[159:170]
tmp15$tolerance<-1
sand_05mean_51_61<-do.call(merge,tmp15)
sand_05mean_51_61_arc30<-aggregate(sand_05mean_51_61,4)
gc()

toto15<-occurrence.data[which(occurrence.data$LONGDEC>=50.99 & occurrence.data$LONGDEC<=61),]
dim(toto15)#75

plot(sand_05mean_51_61)
plot(wrld_simpl, add=TRUE)
points(toto15$LONGDEC,toto15$LATDEC, col="red")

# extract raw values
sand_51_61 <- raster::extract(sand_05mean_51_61_arc30, toto15[,13:12])
sand_51_61_1 <- data.frame(sand_51_61)
str(sand_51_61_1)

toto15$sand_05_mean<-sand_51_61_1$sand_51_61
table(is.na(toto15$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#19982
final.data<-rbind(final.data,toto15)
dim(final.data)#20057


write.table(final.data,"final_data.tab",sep="/t")

#############
#61-71

tmp16<-tmp[171:182]
tmp16$tolerance<-1
sand_05mean_61_71<-do.call(merge,tmp16)
sand_05mean_61_71_arc30<-aggregate(sand_05mean_61_71,4)
gc()

toto16<-occurrence.data[which(occurrence.data$LONGDEC>=60.99 & occurrence.data$LONGDEC<=71),]
dim(toto16)#74

plot(sand_05mean_61_71)
plot(wrld_simpl, add=TRUE)
points(toto16$LONGDEC,toto16$LATDEC, col="red")

# extract raw values
sand_61_71 <- raster::extract(sand_05mean_61_71_arc30, toto16[,13:12])
sand_61_71_1 <- data.frame(sand_61_71)
str(sand_61_71_1)

toto16$sand_05_mean<-sand_61_71_1$sand_61_71
table(is.na(toto16$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#20057
final.data<-rbind(final.data,toto16)
dim(final.data)#20131


write.table(final.data,"final_data.tab",sep="/t")


#############
#71-81

tmp17<-tmp[183:194]
tmp17$tolerance<-1
sand_05mean_71_81<-do.call(merge,tmp17)
sand_05mean_71_81_arc30<-aggregate(sand_05mean_71_81,4)
gc()

toto17<-occurrence.data[which(occurrence.data$LONGDEC>=70.99 & occurrence.data$LONGDEC<=81),]
dim(toto17)#552

plot(sand_05mean_71_81)
plot(wrld_simpl, add=TRUE)
points(toto17$LONGDEC,toto17$LATDEC, col="red")

# extract raw values
sand_71_81 <- raster::extract(sand_05mean_71_81_arc30, toto17[,13:12])
sand_71_81_1 <- data.frame(sand_71_81)
str(sand_71_81_1)

toto17$sand_05_mean<-sand_71_81_1$sand_71_81
table(is.na(toto17$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#20131
final.data<-rbind(final.data,toto17)
dim(final.data)#20683


write.table(final.data,"final_data.tab",sep="/t")



#############
#81-91

tmp19<-tmp[207:218]

tmp18<-tmp[195:206]
tmp18$tolerance<-1
sand_05mean_81_91<-do.call(merge,tmp18)
sand_05mean_81_91_arc30<-aggregate(sand_05mean_81_91,4)
gc()

toto18<-occurrence.data[which(occurrence.data$LONGDEC>=80.99 & occurrence.data$LONGDEC<=91),]
dim(toto18)#149

plot(sand_05mean_81_91)
plot(wrld_simpl, add=TRUE)
points(toto18$LONGDEC,toto18$LATDEC, col="red")

# extract raw values
sand_81_91 <- raster::extract(sand_05mean_81_91_arc30, toto18[,13:12])
sand_81_91_1 <- data.frame(sand_81_91)
str(sand_81_91_1)

toto18$sand_05_mean<-sand_81_91_1$sand_81_91
table(is.na(toto18$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#20683
final.data<-rbind(final.data,toto18)
dim(final.data)#20832

write.table(final.data,"final_data.tab",sep="/t")

#############
#91-101

tmp19<-tmp[207:218]
tmp19$tolerance<-1
sand_05mean_91_101<-do.call(merge,tmp19)
sand_05mean_91_101_arc30<-aggregate(sand_05mean_91_101,4)
gc()

toto19<-occurrence.data[which(occurrence.data$LONGDEC>=90.99 & occurrence.data$LONGDEC<=101),]
dim(toto19)#149

plot(sand_05mean_91_101)
plot(wrld_simpl, add=TRUE)
points(toto19$LONGDEC,toto19$LATDEC, col="red")

# extract raw values
sand_91_101 <- raster::extract(sand_05mean_91_101_arc30, toto19[,13:12])
sand_91_101_1 <- data.frame(sand_91_101)
str(sand_91_101_1)

toto19$sand_05_mean<-sand_91_101_1$sand_91_101
table(is.na(toto19$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#20832
final.data<-rbind(final.data,toto19)
dim(final.data)#21092

write.table(final.data,"final_data.tab",sep="/t")

#############
#-109-99

tmp20<-tmp[219:230]
tmp20$tolerance<-1
sand_05mean_N109_99<-do.call(merge,tmp20)
sand_05mean_N109_99_arc30<-aggregate(sand_05mean_N109_99,4)
gc()

toto20<-occurrence.data[which(occurrence.data$LONGDEC>-109.01 & occurrence.data$LONGDEC<=-98.99),]
dim(toto20)#1697

plot(sand_05mean_N109_99)
plot(wrld_simpl, add=TRUE)
points(toto20$LONGDEC,toto20$LATDEC, col="red")

# extract raw values
sand_N109_99 <- raster::extract(sand_05mean_N109_99_arc30, toto20[,13:12])
sand_N109_99_1 <- data.frame(sand_N109_99)
str(sand_N109_99_1)

toto20$sand_05_mean<-sand_N109_99_1$sand_N109_99
table(is.na(toto20$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#21092
final.data<-rbind(final.data,toto20)
dim(final.data)#22789

write.table(final.data,"final_data.tab",sep="/t")


#############
#-119-109

tmp21<-tmp[231:242]
tmp21$tolerance<-1
sand_05mean_N119_109<-do.call(merge,tmp21)
sand_05mean_N119_109_arc30<-aggregate(sand_05mean_N119_109,4)
gc()

toto21<-occurrence.data[which(occurrence.data$LONGDEC>-119.01 & occurrence.data$LONGDEC<=-108.99),]
dim(toto21)#1697

plot(sand_05mean_N119_109)
plot(wrld_simpl, add=TRUE)
points(toto21$LONGDEC,toto21$LATDEC, col="red")

# extract raw values
sand_N119_109 <- raster::extract(sand_05mean_N119_109_arc30, toto21[,13:12])
sand_N119_109_1 <- data.frame(sand_N119_109)
str(sand_N119_109_1)

toto21$sand_05_mean<-sand_N119_109_1$sand_N119_109
table(is.na(toto21$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#21092
final.data<-rbind(final.data,toto21)
dim(final.data)#22789

write.table(final.data,"final_data.tab",sep="/t")


#############
#-119-109

tmp21<-tmp[231:242]
tmp21$tolerance<-1
sand_05mean_N119_109<-do.call(merge,tmp21)
sand_05mean_N119_109_arc30<-aggregate(sand_05mean_N119_109,4)
gc()

toto21<-occurrence.data[which(occurrence.data$LONGDEC>-119.01 & occurrence.data$LONGDEC<=-108.99),]
dim(toto21)#1697

plot(sand_05mean_N119_109)
plot(wrld_simpl, add=TRUE)
points(toto21$LONGDEC,toto21$LATDEC, col="red")

# extract raw values
sand_N119_109 <- raster::extract(sand_05mean_N119_109_arc30, toto21[,13:12])
sand_N119_109_1 <- data.frame(sand_N119_109)
str(sand_N119_109_1)

toto21$sand_05_mean<-sand_N119_109_1$sand_N119_109
table(is.na(toto21$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#21092
final.data<-rbind(final.data,toto21)
dim(final.data)#23646

write.table(final.data,"final_data.tab",sep="/t")


#############
#-129-119

tmp22<-tmp[243:254]
tmp22$tolerance<-1
sand_05mean_N129_119<-do.call(merge,tmp22)
sand_05mean_N129_119_arc30<-aggregate(sand_05mean_N129_119,4)
gc()

toto22<-occurrence.data[which(occurrence.data$LONGDEC>-129.01 & occurrence.data$LONGDEC<=-118.99),]
dim(toto22)#1697

plot(sand_05mean_N129_119)
plot(wrld_simpl, add=TRUE)
points(toto22$LONGDEC,toto22$LATDEC, col="red")

# extract raw values
sand_N129_119 <- raster::extract(sand_05mean_N129_119_arc30, toto22[,13:12])
sand_N129_119_1 <- data.frame(sand_N129_119)
str(sand_N129_119_1)

toto22$sand_05_mean<-sand_N129_119_1$sand_N129_119
table(is.na(toto22$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#21092
final.data<-rbind(final.data,toto22)
dim(final.data)#24171

write.table(final.data,"final_data.tab",sep="/t")


#############
#-139-129

tmp23<-tmp[255:266]
tmp23$tolerance<-1
sand_05mean_N139_129<-do.call(merge,tmp23)
sand_05mean_N139_129_arc30<-aggregate(sand_05mean_N139_129,4)
gc()

toto23<-occurrence.data[which(occurrence.data$LONGDEC>-139.01 & occurrence.data$LONGDEC<=-128.99),]
dim(toto23)#6

plot(sand_05mean_N139_129)
plot(wrld_simpl, add=TRUE)
points(toto23$LONGDEC,toto23$LATDEC, col="red")

# extract raw values
sand_N139_129 <- raster::extract(sand_05mean_N139_129_arc30, toto23[,13:12])
sand_N139_129_1 <- data.frame(sand_N139_129)
str(sand_N139_129_1)

toto23$sand_05_mean<-sand_N139_129_1$sand_N139_129
table(is.na(toto23$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24171
final.data<-rbind(final.data,toto23)
dim(final.data)#24177

write.table(final.data,"final_data.tab",sep="/t")


#############
#-149-139

tmp24<-tmp[267:278]#
tmp24$tolerance<-1
sand_05mean_N149_139<-do.call(merge,tmp24)
sand_05mean_N149_139_arc30<-aggregate(sand_05mean_N149_139,4)
gc()

toto24<-occurrence.data[which(occurrence.data$LONGDEC>-149.01 & occurrence.data$LONGDEC<=-138.99),]
dim(toto24)#10
plot(sand_05mean_N149_139)
plot(wrld_simpl, add=TRUE)
points(toto24$LONGDEC,toto24$LATDEC, col="red")

# extract raw values
sand_N149_139 <- raster::extract(sand_05mean_N149_139_arc30, toto24[,13:12])
sand_N149_139_1 <- data.frame(sand_N149_139)
str(sand_N149_139_1)

toto24$sand_05_mean<-sand_N149_139_1$sand_N149_139
table(is.na(toto24$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24177
final.data<-rbind(final.data,toto24)
dim(final.data)#24187

write.table(final.data,"final_data.tab",sep="/t")


#############
#-159-149

tmp25<-tmp[279:290]#
tmp25$tolerance<-1
sand_05mean_N159_149<-do.call(merge,tmp25)
sand_05mean_N159_149_arc30<-aggregate(sand_05mean_N159_149,4)
gc()

toto25<-occurrence.data[which(occurrence.data$LONGDEC>-159.01 & occurrence.data$LONGDEC<=-148.99),]
dim(toto25)#60

plot(sand_05mean_N159_149)
plot(wrld_simpl, add=TRUE)
points(toto25$LONGDEC,toto25$LATDEC, col="red")

# extract raw values
sand_N159_149 <- raster::extract(sand_05mean_N159_149_arc30, toto25[,13:12])
sand_N159_149_1 <- data.frame(sand_N159_149)
str(sand_N159_149_1)

toto25$sand_05_mean<-sand_N159_149_1$sand_N159_149
table(is.na(toto25$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24187
final.data<-rbind(final.data,toto25)
dim(final.data)#24247

write.table(final.data,"final_data.tab",sep="/t")


#############
#-169-159

tmp26<-tmp[291:302]#
tmp26$tolerance<-1
sand_05mean_N169_159<-do.call(merge,tmp26)
sand_05mean_N169_159_arc30<-aggregate(sand_05mean_N169_159,4)
gc()

toto26<-occurrence.data[which(occurrence.data$LONGDEC>-169.01 & occurrence.data$LONGDEC<=-158.99),]
dim(toto26)#29

plot(sand_05mean_N169_159)
plot(wrld_simpl, add=TRUE)
points(toto26$LONGDEC,toto26$LATDEC, col="red")

# extract raw values
sand_N169_159 <- raster::extract(sand_05mean_N169_159_arc30, toto26[,13:12])
sand_N169_159_1 <- data.frame(sand_N169_159)
str(sand_N169_159_1)

toto26$sand_05_mean<-sand_N169_159_1$sand_N169_159
table(is.na(toto26$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24247
final.data<-rbind(final.data,toto26)
dim(final.data)#24276

write.table(final.data,"final_data.tab",sep="/t")


#############
#-179-169

tmp27<-tmp[303:314]#
tmp27$tolerance<-1
sand_05mean_N179_169<-do.call(merge,tmp27)
sand_05mean_N179_169_arc30<-aggregate(sand_05mean_N179_169,4)
gc()

toto27<-occurrence.data[which(occurrence.data$LONGDEC>-180.01 & occurrence.data$LONGDEC<=-168.99),]
dim(toto27)#27

plot(sand_05mean_N179_169)
plot(wrld_simpl, add=TRUE)
points(toto27$LONGDEC,toto27$LATDEC, col="red")

# extract raw values
sand_N179_169 <- raster::extract(sand_05mean_N179_169_arc30, toto27[,13:12])
sand_N179_169_1 <- data.frame(sand_N179_169)
str(sand_N179_169_1)

toto27$sand_05_mean<-sand_N179_169_1$sand_N179_169
table(is.na(toto27$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24276
final.data<-rbind(final.data,toto27)
dim(final.data)#

write.table(final.data,"final_data.tab",sep="/t")

#############
#-19-9

tmp28<-tmp[315:326]#
tmp28$tolerance<-1
sand_05mean_N19_9<-do.call(merge,tmp28)
sand_05mean_N19_9_arc30<-aggregate(sand_05mean_N19_9,4)
gc()

toto28<-occurrence.data[which(occurrence.data$LONGDEC>-19.01 & occurrence.data$LONGDEC<=-8.99),]
dim(toto28)#84

plot(sand_05mean_N19_9)
plot(wrld_simpl, add=TRUE)
points(toto28$LONGDEC,toto28$LATDEC, col="red")

# extract raw values
sand_N19_9 <- raster::extract(sand_05mean_N19_9_arc30, toto28[,13:12])
sand_N19_9_1 <- data.frame(sand_N19_9)
str(sand_N19_9_1)

toto28$sand_05_mean<-sand_N19_9_1$sand_N19_9
table(is.na(toto28$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24303
final.data<-rbind(final.data,toto28)
dim(final.data)#24387

write.table(final.data,"final_data.tab",sep="/t")



#############
#-29-19

tmp29<-tmp[327:338]#
tmp29$tolerance<-1
sand_05mean_N29_19<-do.call(merge,tmp29)
sand_05mean_N29_19_arc30<-aggregate(sand_05mean_N29_19,4)
gc()

toto29<-occurrence.data[which(occurrence.data$LONGDEC>-29.01 & occurrence.data$LONGDEC<=-18.99),]
dim(toto29)#21

plot(sand_05mean_N29_19)
plot(wrld_simpl, add=TRUE)
points(toto29$LONGDEC,toto29$LATDEC, col="red")

# extract raw values
sand_N29_19 <- raster::extract(sand_05mean_N29_19_arc30, toto29[,13:12])
sand_N29_19_1 <- data.frame(sand_N29_19)
str(sand_N29_19_1)

toto29$sand_05_mean<-sand_N29_19_1$sand_N29_19
table(is.na(toto29$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24387
final.data<-rbind(final.data,toto29)
dim(final.data)#24408

write.table(final.data,"final_data.tab",sep="/t")


#############
#-39-29

tmp30<-tmp[339:350]#
tmp30$tolerance<-1
sand_05mean_N39_29<-do.call(merge,tmp30)
sand_05mean_N39_29_arc30<-aggregate(sand_05mean_N39_29,4)
gc()

toto30<-occurrence.data[which(occurrence.data$LONGDEC>-39.01 & occurrence.data$LONGDEC<=-28.99),]
dim(toto30)#486

plot(sand_05mean_N39_29)
plot(wrld_simpl, add=TRUE)
points(toto30$LONGDEC,toto30$LATDEC, col="red")

# extract raw values
sand_N39_29 <- raster::extract(sand_05mean_N39_29_arc30, toto30[,13:12])
sand_N39_29_1 <- data.frame(sand_N39_29)
str(sand_N39_29_1)

toto30$sand_05_mean<-sand_N39_29_1$sand_N39_29
table(is.na(toto30$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24408
final.data<-rbind(final.data,toto30)
dim(final.data)#24894

write.table(final.data,"final_data.tab",sep="/t")



#############
#-49-39

tmp31<-tmp[351:362]#
tmp31$tolerance<-1
sand_05mean_N49_39<-do.call(merge,tmp31)
sand_05mean_N49_39_arc30<-aggregate(sand_05mean_N49_39,4)
gc()

toto31<-occurrence.data[which(occurrence.data$LONGDEC>-49.01 & occurrence.data$LONGDEC<=-38.99),]
dim(toto31)#3323

plot(sand_05mean_N49_39)
plot(wrld_simpl, add=TRUE)
points(toto31$LONGDEC,toto31$LATDEC, col="red")

# extract raw values
sand_N49_39 <- raster::extract(sand_05mean_N49_39_arc30, toto31[,13:12])
sand_N49_39_1 <- data.frame(sand_N49_39)
str(sand_N49_39_1)

toto31$sand_05_mean<-sand_N49_39_1$sand_N49_39
table(is.na(toto31$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#24894
final.data<-rbind(final.data,toto31)
dim(final.data)#28217

write.table(final.data,"final_data.tab",sep="/t")

#############
#-59-49

tmp32<-tmp[363:374]#
tmp32$tolerance<-1
sand_05mean_N59_49<-do.call(merge,tmp32)
sand_05mean_N59_49_arc30<-aggregate(sand_05mean_N59_49,4)
gc()

toto32<-occurrence.data[which(occurrence.data$LONGDEC>-59.01 & occurrence.data$LONGDEC<=-48.99),]
dim(toto32)#2945

plot(sand_05mean_N59_49)
plot(wrld_simpl, add=TRUE)
points(toto32$LONGDEC,toto32$LATDEC, col="red")

# extract raw values
sand_N59_49 <- raster::extract(sand_05mean_N59_49_arc30, toto32[,13:12])
sand_N59_49_1 <- data.frame(sand_N59_49)
str(sand_N59_49_1)

toto32$sand_05_mean<-sand_N59_49_1$sand_N59_49
table(is.na(toto32$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#28217
final.data<-rbind(final.data,toto32)
dim(final.data)#31162

write.table(final.data,"final_data.tab",sep="/t")


#############
#-69-59

tmp33<-tmp[375:386]#
tmp33$tolerance<-1
sand_05mean_N69_59<-do.call(merge,tmp33)
sand_05mean_N69_59_arc30<-aggregate(sand_05mean_N69_59,4)
gc()

toto33<-occurrence.data[which(occurrence.data$LONGDEC>-69.01 & occurrence.data$LONGDEC<=-58.99),]
dim(toto33)#680

plot(sand_05mean_N69_59)
plot(wrld_simpl, add=TRUE)
points(toto33$LONGDEC,toto33$LATDEC, col="red")

# extract raw values
sand_N69_59 <- raster::extract(sand_05mean_N69_59_arc30, toto33[,13:12])
sand_N69_59_1 <- data.frame(sand_N69_59)
str(sand_N69_59_1)

toto33$sand_05_mean<-sand_N69_59_1$sand_N69_59
table(is.na(toto33$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#31162
final.data<-rbind(final.data,toto33)
dim(final.data)#37967

write.table(final.data,"final_data.tab",sep="/t")



#############
#-79-69

tmp34<-tmp[387:398]#
tmp34$tolerance<-1
sand_05mean_N79_69<-do.call(merge,tmp34)
sand_05mean_N79_69_arc30<-aggregate(sand_05mean_N79_69,4)
gc()

toto34<-occurrence.data[which(occurrence.data$LONGDEC>-79.01 & occurrence.data$LONGDEC<=-68.99),]
dim(toto34)#8436

plot(sand_05mean_N79_69)
plot(wrld_simpl, add=TRUE)
points(toto34$LONGDEC,toto34$LATDEC, col="red")

# extract raw values
sand_N79_69 <- raster::extract(sand_05mean_N79_69_arc30, toto34[,13:12])
sand_N79_69_1 <- data.frame(sand_N79_69)
str(sand_N79_69_1)

toto34$sand_05_mean<-sand_N79_69_1$sand_N79_69
table(is.na(toto34$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#37967
final.data<-rbind(final.data,toto34)
dim(final.data)#46403

write.table(final.data,"final_data.tab",sep="/t")

#############
#-89-79

tmp35<-tmp[399:410]#
tmp35$tolerance<-1
sand_05mean_N89_79<-do.call(merge,tmp35)
sand_05mean_N89_79_arc30<-aggregate(sand_05mean_N89_79,4)
gc()

toto35<-occurrence.data[which(occurrence.data$LONGDEC>-89.01 & occurrence.data$LONGDEC<=-78.99),]
dim(toto35)#2472

plot(sand_05mean_N89_79)
plot(wrld_simpl, add=TRUE)
points(toto35$LONGDEC,toto35$LATDEC, col="red")

# extract raw values
sand_N89_79 <- raster::extract(sand_05mean_N89_79_arc30, toto35[,13:12])
sand_N89_79_1 <- data.frame(sand_N89_79)
str(sand_N89_79_1)

toto35$sand_05_mean<-sand_N89_79_1$sand_N89_79
table(is.na(toto35$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#46403
final.data<-rbind(final.data,toto35)
dim(final.data)#48875

write.table(final.data,"final_data.tab",sep="/t")


#############
#-9-1

tmp36<-tmp[411:422]#
tmp36$tolerance<-1
sand_05mean_N9_1<-do.call(merge,tmp36)
sand_05mean_N9_1_arc30<-aggregate(sand_05mean_N9_1,4)
gc()

toto36<-occurrence.data[which(occurrence.data$LONGDEC>-9.01 & occurrence.data$LONGDEC<=1.01),]
dim(toto36)#250

plot(sand_05mean_N9_1)
plot(wrld_simpl, add=TRUE)
points(toto36$LONGDEC,toto36$LATDEC, col="red")

# extract raw values
sand_N9_1 <- raster::extract(sand_05mean_N9_1_arc30, toto36[,13:12])
sand_N9_1_1 <- data.frame(sand_N9_1)
str(sand_N9_1_1)

toto36$sand_05_mean<-sand_N9_1_1$sand_N9_1
table(is.na(toto36$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#48875
final.data<-rbind(final.data,toto36)
dim(final.data)#49225

write.table(final.data,"final_data.tab",sep="/t")


#############
#-99-89

tmp37<-tmp[423:434]#
tmp37$tolerance<-1
sand_05mean_N99_89<-do.call(merge,tmp36)
sand_05mean_N99_89_arc30<-aggregate(sand_05mean_N99_89,4)
gc()

toto37<-occurrence.data[which(occurrence.data$LONGDEC>-99.01 & occurrence.data$LONGDEC<=-88.99),]
dim(toto37)#1626

plot(sand_05mean_N99_89)
plot(wrld_simpl, add=TRUE)
points(toto37$LONGDEC,toto37$LATDEC, col="red")

# extract raw values
sand_N99_89 <- raster::extract(sand_05mean_N99_89_arc30, toto37[,13:12])
sand_N99_89_1 <- data.frame(sand_N99_89)
str(sand_N99_89_1)

toto37$sand_05_mean<-sand_N99_89_1$sand_N99_89
table(is.na(toto37$sand_05_mean))#0 occurrence records don't have GDD values

#add results to final table
dim(final.data)#
final.data<-rbind(final.data,toto37)
dim(final.data)

dim(occurrence.data)
dim(final.data[!duplicated(final.data$BRAHMS), ])
final.data.all<-final.data[!duplicated(final.data$BRAHMS), ]
write.csv(final.data.all,"sand_final_data_50379.csv")


library(data.table)
unique(setDT(final.data)[order(Date, -Depth)], by = "Date")


toto2<-occurrence.data[which(occurrence.data$LONGDEC>-9 & occurrence.data$LONGDEC<=1),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>-19 & occurrence.data$LONGDEC<=-9),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-29 & occurrence.data$LONGDEC<=-19),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>-39 & occurrence.data$LONGDEC<=-29),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-49 & occurrence.data$LONGDEC<=-39),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>-59 & occurrence.data$LONGDEC<=-49),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>-69 & occurrence.data$LONGDEC<=-59),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-79 & occurrence.data$LONGDEC<=-69),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-89 & occurrence.data$LONGDEC<=-79),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>-99 & occurrence.data$LONGDEC<=-89),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-109 & occurrence.data$LONGDEC<=-99),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-119 & occurrence.data$LONGDEC<=-109),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-129 & occurrence.data$LONGDEC<=-119),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-139 & occurrence.data$LONGDEC<=-129),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-149 & occurrence.data$LONGDEC<=-139),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-159 & occurrence.data$LONGDEC<=-149),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>-169 & occurrence.data$LONGDEC<=-159),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>-179 & occurrence.data$LONGDEC<=-169),]




toto2<-occurrence.data[which(occurrence.data$LONGDEC>1 & occurrence.data$LONGDEC<=11),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>11 & occurrence.data$LONGDEC<=21),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>21 & occurrence.data$LONGDEC<=31),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>31 & occurrence.data$LONGDEC<=41),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>41 & occurrence.data$LONGDEC<=51),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>51 & occurrence.data$LONGDEC<=61),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>61 & occurrence.data$LONGDEC<=71),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>71 & occurrence.data$LONGDEC<=81),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>81 & occurrence.data$LONGDEC<=91),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>91 & occurrence.data$LONGDEC<=101),]

toto2<-occurrence.data[which(occurrence.data$LONGDEC>101 & occurrence.data$LONGDEC<=111),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>111 & occurrence.data$LONGDEC<=121),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>121 & occurrence.data$LONGDEC<=131),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>131 & occurrence.data$LONGDEC<=141),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>141 & occurrence.data$LONGDEC<=151),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>151 & occurrence.data$LONGDEC<=161),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>161 & occurrence.data$LONGDEC<=171),]
toto2<-occurrence.data[which(occurrence.data$LONGDEC>171 & occurrence.data$LONGDEC<=181),]

plot(sand_05mean_world)

  newdata <- mydata[ which(gender=='F' & age > 65),]
  detach(mydata)  
  
]

