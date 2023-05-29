##########################################
#
# Script for calculating niche breadth of Solanum species
# At the end of the file, we also create a table that merges the niche breadth with range sizes of each species
# Script by E. Gagnon, some adaptations from Ludwig Baldaszati, part of Solanum Geophyte publication 2023
#########################################

###############################################################################

rm(list = ls())

#Opening libraries

#Change working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/05_Niche_Breadth/")


#sp1<-read.csv("../Data/DF_merged_data_50352a.csv",header=TRUE)
sp1<-read.csv("../02_PCA_Kernel_Density/DF_merged_data_50352.csv",header=TRUE,row.names=1)


names(sp1)
# selecting columns of interest
sp <- sp1[c(1,18,13,12,23,19,20,22,21,24,25,26,28)]
names(sp)

# [1] "BRAHMS"       "genus.sp"     "LONGDEC"      "LATDEC"       "BIO5"         "bio6"         "bio7"        
#[8] "BIO15"        "AMI"          "q95size"      "vrm_med"      "sand_05_mean"        "roots" 


# Climate range function
climrange <- function(x) {  
  (max(x) - min(x))}

toto<-sp
# ranges of climatic layers (But this is actually the range of )
sp_n <- na.omit(sp)#This removes all the NA lines, this is why so little data is kept!
#Here I will only select the 7 lines needed, + 
dim(sp_n)#44969

sp_n$rangebio5 <- climrange(sp_n$BIO5)
sp_n$rangebio6 <- climrange(sp_n$bio6)
sp_n$rangebio7 <- climrange(sp_n$bio7)
sp_n$rangebio15 <- climrange(sp_n$BIO15)
sp_n$rangeAMI <- climrange(sp_n$AMI)
sp_n$rangeq95size <- climrange(sp_n$q95size)
sp_n$rangeVRM <- climrange(sp_n$vrm_med)
sp_n$rangesand <- climrange(sp_n$sand_05_mean)

#sp_SA_ext <- sp_SA_ext_n

## dplyr

a <- sp_n %>%
  dplyr::group_by(genus.sp) %>%
 dplyr::mutate(Specimens = n())


a <- as.data.frame(a)

dim(a)

b <- as.data.frame(sp %>%
                     group_by(genus.sp) %>%
                     dplyr::summarise(Specimens = n()))
dim(b)


b.c<-b


sp_u10 <-dplyr::filter(a, Specimens < 10)
sp_o10 <-dplyr::filter(a, Specimens >= 10)
spfilt_u10 <-dplyr::filter(a, genus.sp %in% sp_u10$genus.sp)
spfilt_o10 <-dplyr::filter(a, genus.sp %in% sp_o10$genus.sp)

dim(sp)#2114
dim(sp_o10)#44981

prop.climrange <- function(x,y) {  
  (max(x) - min(x))/y
  }


#spfilt_u10$genus.sp[spfilt_u10$genus.sp == "Solanum_?michoacanum"] <-"Solanum_michoacanum"
#spfilt_u10$genus.sp[spfilt_u10$genus.sp == "Solanum_?blanco-galdosii"] <-"Solanum_blancogaldosii"
#spfilt_u10$genus.sp[spfilt_u10$genus.sp == "Solanum_?vallis-mexici"] <-"Solanum_vallismexici"
#spfilt_u10$genus.sp[spfilt_u10$genus.sp == "Solanum_?doddsii"] <-"Solanum_doddsii"
#spfilt_u10$genus.sp[spfilt_u10$genus.sp == "Solanum_?neoweberbaueri"] <-"Solanum_neoweberbaueri"
#spfilt_u10$genus.sp[spfilt_u10$genus.sp == "Solanum_?procurrens"] <-"Solanum_procurrens"

#"Solanum_?vallis-mexici"
#"Solanum_?blanco-galdosii"
#"Solanum_?doddsii"
#"Solanum_?neoweberbaueri"

# prop. niche with PCA chosen layers 
# for species below 10 records
niches_u10 <- as.data.frame(spfilt_u10 %>%
                             group_by(genus.sp) %>%
                             dplyr::summarise(nichebio5m = prop.climrange(bio5,spfilt_u10$rangebio5[1]),
                                       nichebio6m = prop.climrange(bio6,rangebio6[1]),
                                       nichebio7m = prop.climrange(bio7,rangebio7[1]),
                                       nichebio15m = prop.climrange(bio15,rangebio15[1]),
                                       nicheAMIm = prop.climrange(MI,rangeAMI[1]),
                                       nicheq95sizem = prop.climrange(q95size,rangeq95size[1]),
                                       nicheVRMm = prop.climrange(vrm_med,rangeVRM[1]),
                                       nichesandm = prop.climrange(sand_05_mean,rangesand[1]),
                                       ))
  

niches_u10

#sp.test2 <-dplyr::filter(niche_low, genus.sp == "Solanum_huaylasense")

### Not needed because I already subsampled
# subsampling for species with >= 10 records 
list <- list()
for (i in 1:1000) {
  list[[i]] <-as.data.frame(spfilt_o10 %>%
                              group_by(genus.sp) %>% 
                              sample_frac(0.5) %>% 
                              dplyr::summarise(nichebio5m = prop.climrange(bio5,rangebio5[1]),
                                        nichebio6m = prop.climrange(bio6,rangebio6[1]),
                                        nichebio7m = prop.climrange(bio7,rangebio7[1]),
                                        nichebio15m = prop.climrange(bio15,rangebio15[1]),
                            nicheAMIm = prop.climrange(MI,rangeAMI[1]),
                            nicheq95sizem = prop.climrange(q95size,rangeq95size[1]),
                            nicheVRMm = prop.climrange(vrm_med,rangeVRM[1]),
                            nichesandm = prop.climrange(sand_05_mean,rangesand[1]),
  ))
  
  }



data_out <- as.data.frame(do.call(rbind, list))

niches_o10 <-as.data.frame(data_out %>%
                             group_by(genus.sp) %>% 
                             dplyr::summarise(
                                      nichebio5me = mean(nichebio5m),
                                       nichebio6me = mean(nichebio6m),
                                       nichebio7me = mean(nichebio7m),
                                       nichebio15me = mean(nichebio15m),
                                       nicheAMIme = mean(nicheAMIm),
                                       nicheq95sizeme = mean(nicheq95sizem),
                                       nicheVRMme = mean(nicheVRMm),
                                       nichesandme = mean(nichesandm)
                                       ))


niches_u10 <- dplyr::rename(niches_u10, 
                            nichebio5me = nichebio5m,
                            nichebio6me = nichebio6m,
                            nichebio7me = nichebio7m,
                            nichebio15me = nichebio15m,
                            nicheAMIme = nicheAMIm,
                            nicheq95sizeme = nicheq95sizem,
                            nicheVRMme = nicheVRMm,
                            nichesandme = nichesandm)

                             
niches_o10_un <- niches_o10 %>%  distinct()
niches_u10_un <- niches_u10 %>%  distinct()

dim(niches_o10_un)#708
dim(niches_u10_un)#443

names(niches_o10_un)
names(niches_u10_un)


# merging
niches <- rbind(niches_u10, niches_o10)

# merging
niches_un <- rbind(niches_u10_un, niches_o10_un)
dim(niches_un)#1151

getwd()
write.table(niches,file="niche_ranges_Solanum_8vars_v2.csv",sep=";")

###############################
#The next section here is getting some stats and exploring the dataset
### Number of specimens for each species

quantile(b$Specimens)
#0%  25%  50%  75% 100% 
#1    6   15   40 1591

#Less than 10
#Less than 20
#Less than 35
#More than 35
u10 <-dplyr::filter(b, Specimens < 10)
u20 <-dplyr::filter(b, Specimens < 20)
u35 <-dplyr::filter(b, Specimens < 35)
o35 <-dplyr::filter(b, Specimens >= 35)

u10$Specimens<-"9"
u20$Specimens<-"19"
u35$Specimens<-"34"
o35$Specimens<-"35"

cat.ab<-rbind(u10,u20,u35,o35) # This files let's me see how many species are in each category.
dim(cat.ab)#2290, 2
write.csv(cat.ab,"occ_abb_4cat_v2.csv")



############## 
#Write a table that indicates number of specimens per species

b.c #(only for clim data)
#write.table(b.c,"abundance_attr_8vars_1169_v2.csv",sep=";")

b.c.f <- as.data.frame(rbind(spfilt_u10,spfilt_o10) %>%
                     group_by(genus.sp) %>%
                     dplyr::summarise(Specimens = n()))
b.c.f
write.table(b.c.f,"abundance_attr_8vars_1169_v2.csv",sep=";")


##############################
# Next section, I import the range size dataset

per_species<-read.csv("../04_Range_Size_Niche_Breadth/range_sizes_1179.txt")
names(per_species)[4]<-"genus.sp"


# ordering alphabetically to make merging easier
niches <- niches[with(niches, order(niches$genus.sp)), ]

# merging datasets 
niches_perspeciesex <- inner_join(niches, per_species, by = "genus.sp")

# merging with dataset with specimen counts from line 129
niches_perspecies <- inner_join(niches_perspeciesex, b, by = "genus.sp")

# calculating breadth
niches_perspecies$breadth <- NA
for (i in 1:length(niches_perspecies$genus.sp)){
     niches_perspecies$breadth[i] <- sum(niches_perspecies[i,2:9])
}

niches_perspecies_un<-unique(niches_perspecies)

niches_perspecies_un$logbreadth<-log(niches_perspecies_un$breadth+1)# This is where I transform things into logbreadth
niches_perspecies_un$sqrbreadth<-sqrt(niches_perspecies_un$breadth)# This is where I transform things into logbreadth

dim(niches_perspecies_un)#1151
dim(niches_perspecies_un[complete.cases(niches_perspecies_un),])#1062
toto2<-niches_perspecies_un[complete.cases(niches_perspecies_un),]

write.csv(toto2,"Solanum_range_breadth_1062_v2.csv")
