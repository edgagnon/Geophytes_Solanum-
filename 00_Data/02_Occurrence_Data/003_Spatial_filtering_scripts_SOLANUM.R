##########################################
#
# Script for spatial filtering of Solanum occurrence data
# 
# by E. Gagnon, 2023, published as part of MANUSCRIPT
#########################################


library(sp)
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/00_Data/02_Occurrence_Data/03_Spatial_filtering/")


#######################
# This is the function itself
filterByProximity <- function(xy, dist, mapUnits = F) {
  #xy can be either a SpatialPoints or SPDF object, or a matrix
  #dist is in km if mapUnits=F, in mapUnits otherwise
  if (!mapUnits) {
    d <- spDists(xy,longlat=T)
  }
  if (mapUnits) {
    d <- spDists(xy,longlat=F)
  }
  diag(d) <- NA
  close <- (d <= dist)
  diag(close) <- NA
  closePts <- which(close,arr.ind=T)
  discard <- matrix(nrow=2,ncol=2)
  if (nrow(closePts) > 0) {
    while (nrow(closePts) > 0) {
      if ((!paste(closePts[1,1],closePts[1,2],sep='_') %in% paste(discard[,1],discard[,2],sep='_')) & (!paste(closePts[1,2],closePts[1,1],sep='_') %in% paste(discard[,1],discard[,2],sep='_'))) {
        discard <- rbind(discard, closePts[1,])
        closePts <- closePts[-union(which(closePts[,1] == closePts[1,1]), which(closePts[,2] == closePts[1,1])),]
      }
    }
    discard <- discard[complete.cases(discard),]
    discard <- as.matrix(discard)
    return(xy[-discard[,1],])
    #return <- discard
  }
  if (nrow(closePts) == 0) {
    return(xy)
    #return <- discard
  }
  return(return)
}

####################################
#Load the appropriate dataset
#df<-read.table(file.choose(),sep="\t")
#df<-read.table("FINAL_96892_Solanum_07_2021.csv",sep=";")
#write.csv(df,"FINAL_96892_Solanum_07_2021_corrected.csv")
df<-read.csv("FINAL_96892_Solanum_07_2021_corrected.csv",row.names=1)

colnames(df)
dim(df)

names.list<-unique((df$genus.sp))
length(names.list)
#

#Prior to filtering, remove duplicate coordinates within each species.
library(sp)
coordinates(df) <- 13:12
splitData <- split(df, df$genus.sp)
newData <- lapply(splitData, remove.duplicates, zero = 0, remove.second = TRUE)

newData2<-do.call("rbind", newData)
toto<-as.data.frame(newData2)
dim(toto)#80553
df<-toto

write.csv(toto,"DF_resuls_noduplicates_80553.csv")

#This next section is when we will start to do spatial filtering
#The next few lines set up the objects needed to run the script.

occurrence.records<-c()
df.results<-c()
number.records<-c()
#as.matrix(number.records)
list1<-c()#contains all the species that have more than 5 occurence points, and that have lost some records, but still more than 5
list2<-c()#contains all species that originally had less than 5 occurence points
list3<-c()#contains all species that originally had more than 5 occurence points, but now have less than five.

sink("./spatial_filtering_results_v5_removal_duplicates.txt")


for (i in names.list)
{
print(names.list[i])
    occurrence.records[[i]]<-df[df$genus.sp %in% i,]
number.records[i]<-dim(occurrence.records[[i]])[1]
  
  #This writes a table of occurence data for each species
  name<-paste(i,"occurence_data.txt",sep="_")
  write.table(df[grepl(i,df$genus.sp),], file=name, sep=";")
  

  toto<-df[df$genus.sp %in% i,]
print(paste(i,"is a matrix of", dim(toto)[1],"occurence records",sep=" "))  
# Set the distance over which to preform Spatial Filtering in km
scale_distance <- 10

# This object should be coordinates for your species/group in decimal degrees
species_data<-(toto[,12:13])
print(dim(species_data))

# This is how you run the function
results<-filterByProximity(data.matrix(species_data), scale_distance, mapUnits=F)
if (inherits(results, "matrix"))
  {print(paste("There is a difference of ", length(species_data[,1])-length(results[,1])," between the two datasets.", sep=""))
  }else
  {print(paste("There is a difference of ", length(species_data[1])-length(results[1])," between the two datasets.", sep=""))
    }

if (length(results)>=10)
  {print("More than five occurence points retained")
  toto2<-df[rownames(results),]
  df.results<-rbind(df.results,toto2)
  print(paste("df.results now has",dim(df.results)[1],"rows, and", dim(df.results)[2],"columns",sep=" "))
  if(length(species_data[,1])-length(results[,1]!=0))
    {list1[i]<-i
    }
  } else

  {

        if (length(species_data)<10)
          {df.results<-rbind(df.results,toto)
          print("Original dataset had less than five occurence points retained")
           print(paste("df.results now has",dim(df.results)[1],"rows, and", dim(df.results)[2],"columns",sep=" "))
           list2[i]<-i
          }else
          {df.results<-rbind(df.results,toto)
          print("Original dataset had at least five occurence points, but less than five retained")
          print(paste("df.results now has",dim(df.results)[1],"rows, and", dim(df.results)[2],"columns",sep=" "))
           list3[i]<-i
          }
       
  }
gc()

}

sink()
#file.show("spatial_filtering_results.txt)

dim(df.results) # From 96892 records, there are now 50379 records

write.table(df.results,"DF_resuls_all_spatial_filtering_50379.csv",sep=";")
write.csv(df.results,"DF_resuls_all_spatial_filtering_50379a.csv")

length(list1)#830 species

length(list2)#187 species

length(list3)#106 species

