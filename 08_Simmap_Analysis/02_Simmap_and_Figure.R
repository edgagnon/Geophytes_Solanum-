###############################################################################
# Script to run 
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
PPgeophyte2<- read.csv("../00_Data/01_Trait_Data/simmap_multi_v2.csv",row.names = 1)
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


# Create a dataframe in which the results will be stored
results <- matrix(nrow=200, ncol=1001)
results <- data.frame(results)
colnames(results) <- c("Original", 1:1000)

results2 <- vector(mode = "list", length = 1001)

  # Select the model (one of ER, ARD, or SYM)
model.choice <- "ARD"
#ARD selected based on previous AIC model selection.

# Run the analyses
  tree <- sol.tree
  
  # Run make.simmap. Make sure PPgeo
  gmode<-setNames(PPgeophyte2$roots,PPgeophyte2$genus.sp)
  btrees <- make.simmap(sol.tree,gmode,nsim=2,model=model.choice)
  BB <- describe.simmap(btrees,plot=FALSE)
  
  # Get the average amount of changes
  changes <- sum(BB$count[,1])/length(BB$count[,1])
  results[1] <- changes
  results2[[1]]<-BB$count
  
# Save the results
write.csv(results, "Results.csv", row.names = FALSE)
write.csv(results2, "Results2_BB.csv", row.names = FALSE)

#save.image("Solanum_3tr_ARD.RData")


###############################################################################
#Drawing the results (Figure 4), with ggtree
#source: https://github.com/YuLab-SMU/ggtree/issues/419

library(evobiR)
library(varhandle)

library(ggtree)
library(ggtreeExtra)
library(ggpp)

XX<-describe.simmap(btrees,plot=FALSE)
XX$ace #internal nodes

dat<-as.data.frame(XX$ace)
dat$node<-rownames(dat)
dat2<-data.frame(node=dat$node,NG=dat$nongeophyte,RH=dat$rhizomatous, USO=dat$tuberous)

new.order <- ReorderData(sol.tree, gmode, taxa.names="row names")
dat3<-as.data.frame(new.order)
dat3a<-to.dummy(dat3$new.order,"sp")
dat3a<-as.data.frame(dat3a)
dat3$node<-rownames(dat3)
toto<-data.frame(node=dat3$node,NG=dat3a$sp.nongeophyte,RH=dat3a$sp.rhizomatous, USO=dat3a$sp.tuberous)
dat3<-toto
setdiff(sol.tree$tip.label,dat3$node)

dat4<-rbind(dat3,dat2[1:734,])
dat4$node[1:735]<-c(1:735)
dat4$node<-as.numeric(dat4$node)

pies <- nodepie(dat4, cols=2:4, alpha=1, color=c(ng.col,rh.col,tb.col))
df <- tibble::tibble(node=as.numeric(dat4$node), pies=pies)

p<-ggtree(sol.tree,size = 0.2,ladderize=TRUE,layout="circular")#+
#  geom_tiplab(size=1) 
p %<+% df

#This is so we can plot the ancestral state reconstruction at the nodes.
pdf("test_figure_tipless_v2.pdf")
p2<-p + ggpp::geom_plot(data=td_unnest(), mapping=aes(x=x,y=y, label=pies), vp.width=0.04, vp.height=0.04, hjust=0.5, vjust=0.5)
p2
dev.off()

dat5<-dat3

TraitListToPlot<-c("NG","RH","USO")
for(i in 1:length(TraitListToPlot)){
  dat5[TraitListToPlot[i]] <- factor(ifelse(dat5[TraitListToPlot[i]] >= 1, i, 0 ))
}  

rownames(dat5)<-dat5$node
bs = 2; fs = 3; ofs = 30; cl = c("grey90", "grey20")


traitCoveragePlot <- gheatmap(p2, dat5[TraitListToPlot], colnames = FALSE, width = 0.2, color = "transparent") +
  scale_fill_manual(values = c(ng.col,rh.col,tb.col,"white"),
                    breaks = c("1", "2", "3"),
                    labels = as.factor(TraitListToPlot),
                    expand = c(0,0),
                    na.value="white") + 
  theme(legend.title = element_blank())+
    geom_cladelabel(node = 842, label = "EHS Clade", hjust = 0.5, extend = 0.5,barsize = 1, fontsize = fs, color = "grey20",angle = 57,offset=0.03)+
    geom_cladelabel(node = 1248, label = "Potato Clade", hjust = 0.5, extend = 0.5,barsize = 1, fontsize = fs, color = "grey20",angle = 100,offset=0.04)+
    geom_cladelabel(node = 738, label = "Clade2", hjust = 0.5, extend = 0.5,barsize = 1, fontsize = fs, color = "grey20",angle = -35,offset=0.05)+
    geom_cladelabel(node = 1359, label = "DulMo", hjust = 0.5, extend = 0.5,barsize = 1, fontsize = fs, color = "grey20",angle = -35,offset=0.03)
#    geom_cladelabel(node = 1360, label = "VANans", hjust = 0.5, extend = 0.5,barsize = 1, fontsize = fs, color = "grey20",angle = -35,offset=0.03)


pdf("test_figure4_tipless_v2.pdf")
traitCoveragePlot
dev.off()


#This is for finding out where the different clade labels should go, untag if useful
#MRCA(sol.tree,"Solanum_acroglossum","Solanum_buesii")#1248 #Potato clade
#MRCA(sol.tree,"Solanum_euacanthum","Solanum_rivicola")#842 #EHS clade
#MRCA(sol.tree,"Solanum_anomalostemon","Solanum_rivicola")#738 #Clade 2
#MRCA(sol.tree,"Solanum_aviculare","Solanum_zuloagae")#1359 #Clade 2

#MRCA(sol.tree,"Solanum_laciniatum","Solanum_valdiviense")#1360 #Clade 2#Vanans
