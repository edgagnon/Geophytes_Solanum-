##########################################
#
# Script for doing Phylogenetic Anovas on individual environmental axis.
# 
# by E. Gagnon, part of Solanum Geophyte publication 2023
#########################################


#*** This script should be run with an older version of R, v.4.0.3, and the RRPP package (v.1.0.0)

rm(list = ls())

library(ape)
library(geiger)
library(stringi)
library(stringr)
library(ggplot2)

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
# Doing boxplots of the variables
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


#What is going on here...?
x<-names(mydata2)[2:9]
length(x)

plot.list<-list()
#pdf(paste(wd,"/boxplot_output/Boxplots_Solanum_10vars_mean_v2.pdf",sep=""),width=8, height=6)

for(i in 1:length(x))

{
  #i<-1
  print(x[i])
  #  pdf(paste(wd,"/boxplot_output/Boxplots_Solanun_",x[i],"_mean.pdf",sep=""),width=8, height=6)
  
  plot.list[[i]]<-ggplot(mydata2, aes_string(x = "roots", y = x[i], fill = "roots")) +
    geom_boxplot() +
    #    geom_jitter(shape = 15,
    #                color = "steelblue",
    #                position = position_jitter(0.21)) +
    scale_fill_manual(values=c(ng.col, rh.col,tb.col,"white"))+ #fix the colors, ng
    theme_minimal()+
    theme(legend.position="none") +
    ggtitle(x[i])
  print(plot.list[[i]]) 
#dev.off()  
  }
#dev.off()     

library(gridExtra)
library(cowplot)
do.call('grid.arrange',plot.list[]) #, ncol = 3))

legend <- cowplot::get_legend(plot.list[[1]])
plot.list[[length(x)+1]]<-legend

grid.fig<-do.call('grid.arrange',plot.list) #, ncol = 3)))
grid.fig
#ggsave(grid.fig,file="Boxplots_Solanum_10vars_mean_v4.png",width=8,height=10, units="in")

#dev.off()



############
#RRPP tutorial: https://cran.r-project.org/web/packages/RRPP/vignettes/Using.RRPP.html

#### For my dataset

#extracting the covariance matrices of the phylogenies
vcv<-vcv.phylo(mytree)

mt2<-drop.tip(mytree,remove.tree)
mt2#702 trees

#write.tree(mt2,"Solanum_702_tips.tre")

vcv.sol2<-vcv.phylo(mt2)

mydata3<-mydata2[!mydata2$genus.sp %in% remove.data,]#make sure data has 702 species

dim(mydata3)

data3.sol<-rrpp.data.frame(bio5=mydata3$bio5,bio6=mydata3$bio6, bio7=mydata3$bio7,bio15=mydata3$bio15,logAMI=mydata3$logAMI,logq95size=mydata3$log95size,
                           logvrm=mydata3$logvrm,sand=mydata3$sand,roots=mydata3$roots)

data3.sol<-rrpp.data.frame(data3.sol,y=vcv.sol2)

str(data3.sol)


#Untag to save to a text file
#sink("Results_Solanum_PhylANOVA_RRPP_roots_MAY_2023_NEWVERSION_OLD_RRPP.txt")

##########
# Bio5

fitGLS <- lm.rrpp(bio5 ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))


PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 

############
# Bio6

fitGLS <- lm.rrpp(bio6 ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = data3.sol$roots)
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


##########
# Bio7

fitGLS <- lm.rrpp(bio7 ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))


PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))

# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 



############
# bio15

fitGLS <- lm.rrpp(bio15 ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))


PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


############
# log(MI)

fitGLS <- lm.rrpp(logAMI ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))

# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


############
# log(q95size)
fitGLS <- lm.rrpp(logq95size ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))


PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


############
# log(vrm)
fitGLS <- lm.rrpp(logvrm ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


############
# Sand

fitGLS <- lm.rrpp(sand ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)

print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 
#summary(PW1, confidence = 0.95, test.type = "dist", stat.table = FALSE)



################################
## All 8 variables
fitGLS <- lm.rrpp(bio5+bio6+bio7+bio15+logAMI+logq95size+logvrm+sand ~ roots, 
                  data = data3.sol, 
                  Cov = data3.sol$y,
                  print.progress = FALSE)


print(anova(fitGLS))

PW1 <- pairwise(fitGLS, groups = as.factor(data3.sol$roots))
# distances between means
summary(PW1, confidence = 0.95, test.type = "dist") 


sink()

