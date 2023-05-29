###############################################################################
# Script that does model fitting for simmap  
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#
# Some relevant blogs that helped build the script
#http://blog.phytools.org/2016/03/comparing-alternative-models-for.html
#http://blog.phytools.org/2015/09/the-difference-between-different.html
#http://www.phytools.org/eqg2015/asr.html
###############################################################################

rm(list = ls())

# untag and install if needed
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#BiocManager::install("ggtree")
#BiocManager::install("ggtreeExtra")

## first load packages
library(phytools)
library(geiger)
library(treeio)
library(stringr)


library(wesanderson)

#Set up colors
ng.col<-"navyblue"
tb.col<-wes_palette("Zissou1", n = 5)[c(5)]
rh.col<-wes_palette("Zissou1", n = 5)[c(3)]


#set working directory and file paths
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/08_Ancestral_trait_reconstruction/")

# Load the trees
Sys.setlocale( 'LC_ALL','C' ) 
sol.tree <- read.tree("../00_Data/04_Phylogeny/Solanum_phylogeny_735_tips.tre")

# Load the corrected matrices of character traits.

PPgeophyte <- read.csv("../00_Data/01_Trait_Data/simmap_binary_v2.csv",row.names = 1)
PPgeophyte1 <- data.matrix(PPgeophyte[,2:4])
rownames(PPgeophyte1) <- PPgeophyte[,1];
PPgeophyte <- PPgeophyte1 ; rm(PPgeophyte1)


#check if the names in PPgeophyte and PPgeophyte2 actually mach

PPgeophyte
setdiff(rownames(PPgeophyte),sol.tree$tip.label)
#There are 4 names that need to be modified to match.
#change a few names in sol.tree so it's easier to work with all the files.

sol.tree$tip.label
sol.tree$tip.label[561]<-"Solanum_blanco-galdosii"
sol.tree$tip.label[523]<-"Solanum_doddsii"

sol.tree$tip.label[186]<-"Solanum_leopoldensis"
sol.tree$tip.label[307]<-"Solanum_peikuoense"

# Check:
name.check(sol.tree, PPgeophyte)
#Ok!

# Reorder the trait data so they match the order of the tip labels of the tree
PPgeophyte <- PPgeophyte[match(sol.tree$tip.label, rownames(PPgeophyte)),]
#rm(tree)

head(PPgeophyte)


## equal-rates model

fitER<-fitMk(sol.tree,PPgeophyte,model="ER")
fitER

## symmetric model
fitSYM<-fitMk(sol.tree,PPgeophyte,model="SYM")
fitSYM

## all-rates-different model
fitARD<-fitMk(sol.tree,PPgeophyte,model="ARD")
fitARD


AIC(fitER) ## for instance

## or over all models
aic<-as.matrix(sapply(list(ER=fitER,SYM=fitSYM,ARD=fitARD),AIC))
aic

#> aic
#[,1]
#ER  459.0749
#SYM 421.6349
#ARD 404.6934

#Lowest aic is selected, as indicates a more parsimonious model;
#Here, ARD is significantly lower than ER.

