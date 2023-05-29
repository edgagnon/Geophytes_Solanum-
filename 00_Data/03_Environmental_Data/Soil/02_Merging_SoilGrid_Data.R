################################################################################
# Merging data extracted from Soil Grids.  
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#
#Data comes from this portal:http://www.earthenv.org/topography
#Download beforehand the tif file you are interested in from the website above.
###############################################################################

#set working directory
#setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/TUBER_RPROJ/Data_Prep/Environmental_layers/SoilGridData")
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/03_Environmental_Data/Soil/")
dir()

#list the csv files from which I was able to extract the environmental data from Soil Grils
files<-list.files()
files

clay_05<-read.csv("clay_final_data_50379.csv")
head(clay_05)

cvfo_05<-read.csv("cvfo_final_data_50379.csv")
head(cvfo_05)

sand_05<-read.csv("sand_final_data_50379.csv")
head(sand_05)

# sort all datasets so they are in the same order
sand_05a <- sand_05[order(sand_05$BRAHMS),]
clay_05a <- clay_05[order(clay_05$BRAHMS),]
cvfo_05a <- cvfo_05[order(cvfo_05$BRAHMS),]

#Adding columns together
merged.data<-cbind(sand_05a,clay_05a$clay_05_mean,cvfo_05a$clay_05_mean)

names(merged.data)

#change names 
names(merged.data)[20:22]<-c("sand_05_mean","clay_05_mean","cvfo_05_mean")
names(merged.data)

write.csv(merged.data,"SoilGrids_50379.csv")

