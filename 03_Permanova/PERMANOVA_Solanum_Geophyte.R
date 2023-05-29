---
title: "Permanova analysis"
output: html_notebook
---
By Edeline Gagnon, Solanum geophyte evolutio paper 2023.

Code in this notebook was given by and adapted from Jess Rickenback, PhD student at the University of Edinburgh.We are trying to answer the question about whether Solanum with different growth forms occupy different ecological niche spaces.

```{r}
#Preparing the environment and loading the data

rm(list = ls())

setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/03_Permanova/")

library(vegan)

## my data
## dataframe of bioclim extracted points based on lat longs

occ.data<-read.csv("../02_PCA_Kernel_Density/DF_merged_data_50352.csv",header=TRUE, row.names=1)
dim(occ.data)
remove<-c(2:11,14:17) #Doing this to retain only the relevant variables
dat<-(occ.data[,-remove])
names(dat)


dat$logq95size<-log(dat$q95size+1)
dat$logAMI<-log(dat$MI+1)#
dat$logvrm<-log(dat$vrm_med+1)#
names(dat)


dat2<-dat[,c(9,5,6,8,16,15,17,12,4)] #8 variables, +roots + genus.sp 
names(dat2)

names(dat2)
#names(dat2[,c(1:5,7,9:12,14)])

dat2<-dat2[complete.cases(dat2),]
dim((dat2))#47083
dat3<-na.omit(dat2)
length(unique(dat3$genus.sp))#1151 species

#df[df$id %in% names(which(table(df$id)>=5)), ]
```


```{r}
#Here I transform the data, because I really want to work with a matrix of species, and not a matrix with specimen occurrence records

#This gets me the mean for each species
mean.sp<-aggregate(.~genus.sp, data=dat3, mean)
mean.sp

#Add in traits for each species (I need to redo this.)
#Read trait dataset
trait.data<-read.csv("../00_Data/01_Trait_Data/SolTrGeo.csv",header=TRUE)
dim(trait.data)#1232
head(trait.data)
names(trait.data)

toto<-mean.sp
toto$roots <- trait.data$roots[match(toto$genus.sp, trait.data$genus.sp)]
dim(toto)
head(toto)
str(toto)

mean.sp<-toto
```


```{r}
## Get the dissimilarity matrix (Bray-Curtis distance between bioclimatic communities)

commdis<-vegdist(mean.sp[,c(2:9)],method='bray')#allvars
#Error message: 
#This can be resolved by removing rows that have zeros. df1[rowSums(df1[])>0,]
#This error is appearing because there are negative values in the bio 6 colum. I am fixing this by doing a translation of the data.
range(mean.sp$bio6+100)
mean.sp$bio6<-mean.sp$bio6+100

commdis<-vegdist(mean.sp[,c(2:9)],method='bray')#allvars
#no error message!
range(commdis)

#X[X < 0.0] <- 0.0

commdis <- as.matrix(commdis)
#View(commdis)
write.csv(commdis, file = "commdis_MeanSp_8vars.csv")

# read vegetation types

habit <- mean.sp[,"roots"]#select roots
head(habit)
summary(habit)


```


```{r}

## Permutational Multivariate Analysis Of Variance (PERMANOVA)


## overall effect of vegetation type
adonis_a = adonis2(commdis ~ habit, data=mean.sp)
adonis_a


round(adonis_a$SumOfSqs[1]/adonis_a$SumOfSqs[3],2) # % sum of squares for vegetation type


```


```{r}
## comparison of rhizomatous and non-geophytes
f = which(mean.sp$roots %in% c('rhizomatous','nongeophyte'))
adonis_RNg = adonis2(commdis[f,f] ~ roots, data=mean.sp[f,])
adonis_RNg

# -> very significant (p=0.001) difference between trees and shrubs
round(adonis_RNg$SumOfSqs[1]/adonis_RNg$SumOfSqs[3],2) # % sum of squares for vegetation type


```


```{r}
## comparison of rhizomatous species and USOs
f = which(mean.sp$roots %in% c('rhizomatous','tuberous'))
adonis_RU = adonis2(commdis[f,f] ~ roots, data=mean.sp[f,])
adonis_RU


adonis_RU$SumOfSqs[1]/adonis_RU$SumOfSqs[3] # % sum of squares for vegetation type

```

```{r}

## comparison of USO and non-geophytes
f = which(mean.sp$roots %in% c('nongeophyte','tuberous'))
adonis_NgU = adonis2(commdis[f,f] ~ roots, data=mean.sp[f,])
adonis_NgU

#######
adonis_NgU$SumOfSqs[1]/adonis_NgU$SumOfSqs[3]

```

