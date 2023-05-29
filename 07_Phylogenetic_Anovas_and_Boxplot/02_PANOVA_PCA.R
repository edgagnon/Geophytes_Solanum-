#########################################################################
#
# Script for doing Phylogenetic Anovas on individual environmental axis.
# 
# by E. Gagnon, part of Solanum Geophyte publication 2023
#########################################################################


#*** This script was run on R, v.4.3.0, and the RRPP package (v.1.3.1)

rm(list = ls())

library(ape)
library(geiger)
library(stringi)
library(stringr)
library(ggplot2)
library(RRPP)

#for colors
library(wesanderson)
ng.col<-"navyblue"
tb.col<-wes_palette("Zissou1", n = 5)[c(5)]
rh.col<-wes_palette("Zissou1", n = 5)[c(3)]

#setting working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/07_Phylogenetic_Anovas/")
wd<-getwd()

#Reading in the tree
mytree<-read.tree("Solanum_phylogeny_735_tips.tre")
#This tree needs to get changed. I need to do the ancestral trait reconstruction first.

#Here I am getting the tree that I curated in the Mapping_characters_at_tip R script.
mytree$tip.label

#Read trait dataset
trait.data<-read.csv("../00_Data/01_Trait_Data/SolTrGeo.csv",header=TRUE)
dim(trait.data)
head(trait.data)

table(trait.data$roots)

#Load the occurrence data  
occ.dat<-read.csv("../02_PCA_Kernel_Density/DF_merged_data_50352.csv",header=TRUE,row.names=1)
#This keeping BRAHMS, LATDEC, LONGDEC, genus.sp, and all the environmental variables and traits and clade info.

names(occ.dat)
remove<-c(2:11,14:17) #Keeping only brahms, latdec, long dec, and then everything else from genus.sp
dat<-(occ.dat[,-remove])
names(dat)

dat$logq95size<-log(dat$q95size+1)
dat$logvrm<-log(dat$vrm_med+1)
dat$logAMI<-log(dat$MI+1)

names(dat)
dat2<-dat[,c(1,4,14,9,5,6,8,17,15,16,12)] #BRAHMS + genus.sp + roots +
#8 variables, #Bio5,Bio6,Bio7,BIo15,logAMI,logq95size, logvrm, sand

#Now removing lines with missing data
names(dat2)
dat2<-dat2[complete.cases(dat2),]
unique(dat2$roots)
index.nongeophyte<-dat2$roots== "nongeophyte"
index.rhizomatous<-dat2$roots == "rhizomatous"
index.tuberous<-dat2$roots == "tuberous"

length(index.tuberous)#47083
table(index.nongeophyte)#30691
table(index.rhizomatous)#12243
table(index.tuberous)#4149

#Calculating means for species across the dataset
#DF.sol.mean<-aggregate( dat2[,4:13]~ genus.sp, dat2, mean )

toto<-aggregate( bio5 ~ genus.sp, dat2, mean )
toto2<-aggregate( bio6 ~ genus.sp, dat2, mean )
toto3<-aggregate( bio7 ~ genus.sp, dat2, mean )
toto4<-aggregate( bio15 ~ genus.sp, dat2, mean )
toto5<-aggregate( logAMI ~ genus.sp, dat2, mean )
toto6<-aggregate( logq95size ~ genus.sp, dat2, mean )
toto7<-aggregate( logvrm ~ genus.sp, dat2, mean)
toto8<-aggregate( sand_05_mean ~ genus.sp, dat2, mean )
toto<-cbind(toto,toto2$bio6,toto3$bio7,toto4$bio15,toto5$logAMI,toto6$logq95size, toto7$logvrm, toto8$sand_05_mean)
dim(toto)#1151 species, genus.sp+8vars
names(toto)
#Fix names of toto



#Then, associate the roots trait character with the genus.sp.
names(trait.data)
head(trait.data)

#toto$CLADE1 <- clade.data$CLADE1[match(toto$genus.sp, clade.data$genus.sp)]
#dim(toto)

toto$roots <- trait.data$roots[match(toto$genus.sp, trait.data$genus.sp)]
dim(toto)

names(toto)

names(toto)<- c("genus.sp", "bio5", "bio6", "bio7", "bio15","logAMI", "log95size", "logvrm", "sand", "roots")
names(toto)

#Checking for missing data

na.toto<-toto[which(is.na(toto$roots)), ]
unique(na.toto$genus.sp)#none

#################
#Checking if names in the table match names in the phylogeny

setdiff(toto$genus.sp,mytree$tip.label)
setdiff(mytree$tip.label,toto$genus.sp)

#I realise that I need to drop cultivated species from tip
remove.cultivated<-c("Solanum_tuberosum","Solanum_lycopersicum", "Solanum_melongena", "Solanum_muricatum","Solanum_aethiopicum", 
                     "Solanum_macrocarpon", "Solanum_lasiocarpum", "Solanum_betaceum", "Solanum_sessiliflorum", "Solanum quitoense",
                     "Solanum_scabrum", "Solanum_aviculare", "Solanum_crispum", "Solanum_laciniatum", "Solanum_laxum", 
                     "Solanum_pseudocapsicum", "Solanum_seaforthianum", "Solanum_wendlandii", "Solanum_mammosum")
tree.backup<-mytree

tree.backup->mytree


mytree<-drop.tip(mytree,remove.cultivated)#717 tips
mytree
mytree$tip.label
#185 Leopoldense
#551 Blanco-galdosii
#303 "Solanum_peikouense"
mytree$tip.label[185]<-"Solanum_leopoldensis"
mytree$tip.label[551]<-"Solanum_Wblanco-galdosii"
mytree$tip.label[303]<-"Solanum_peikuoense"

mytree #717 tips

remove.data<-setdiff(toto$genus.sp,mytree$tip.label)#449 species not in tree
remove.tree<-setdiff(mytree$tip.label,toto$genus.sp)#15 additional species in the tree for which we don't have 


####################
#First method doing ANOVA and phyloANOVA
DF.sol.mean<-toto

#And then here i can start 
mydata2<-DF.sol.mean
rownames(mydata2)<-mydata2$genus.sp
table(mydata2$roots)

#nongeophyte rhizomatous    tuberous 
#857         177         117 

# we start by defining factors:

grp2<-as.factor(mydata2$roots)
names(grp2)<-rownames(mydata2)
grp2


############
# RRPP script for PCA axes
#https://cran.r-project.org/web/packages/RRPP/vignettes/Using.RRPP.html
# See examples for lm.rrpp to see how anova.lm.rrpp works in conjunction
# with other functions


#### FOr my dataset

#extracting the covariance matrices of the phylogenies
vcv<-vcv.phylo(mytree)

mt2<-drop.tip(mytree,remove.tree)
mt2#702 trees

vcv.sol2<-vcv.phylo(mt2)

mydata3<-mydata2[!mydata2$genus.sp %in% remove.data,]#make sure data has 702 species
dim(mydata3)#702

prin<-princomp((mydata3[2:9]), cor = TRUE, scores = TRUE)
pc12<-prin$scores[,1:8]

data3.sol.pc<-rrpp.data.frame(dim1=pc12[,1],dim2=pc12[,2],dim3=pc12[,3],dim4=pc12[,4],dim5=pc12[,5],roots=mydata3$roots,y=vcv.sol2)

str(data3.sol.pc)



#Untag if you want to save results to a txt file
#sink("Results_Solanum_PhylANOVA_RRPP_roots_PCA_MAY_2023_V2.txt")
##########
print("dim1")

fitGLS <- lm.rrpp(dim1 ~ roots, 
                  data = data3.sol.pc, 
                  Cov = data3.sol.pc$y,
                  print.progress = FALSE)

print(anova(fitGLS))


PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol.pc$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


###############################
print("dim2")

fitGLS <- lm.rrpp(dim2 ~ roots, 
                  data = data3.sol.pc, 
                  Cov = data3.sol.pc$y,
                  print.progress = FALSE)

print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol.pc$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


###############################
print("dim3")

fitGLS <- lm.rrpp(dim3 ~ roots, 
                  data = data3.sol.pc, 
                  Cov = data3.sol.pc$y,
                  print.progress = FALSE)

print(anova(fitGLS))


PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol.pc$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


###############################
print("dim1+dim2")

fitGLS <- lm.rrpp(dim1+dim2 ~ roots, 
                  data = data3.sol.pc, 
                  Cov = data3.sol.pc$y,
                  print.progress = FALSE)

print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol.pc$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


###############################
print("dim1+dim2+dim3")

fitGLS <- lm.rrpp(dim1+dim2+dim3 ~ roots, 
                  data = data3.sol.pc, 
                  Cov = data3.sol.pc$y,
                  print.progress = FALSE)

print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol.pc$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 

##############################

sink()
