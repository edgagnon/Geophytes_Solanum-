---
title: "PCA Kernel Densities Analyses"
output:
  html_document:
    df_print: paged
---
 Script for plotting a PCA biplot with kernel densities in Diaz et. al
 Original author: Bjorn Reu (bjoern.reu@googlemail.com)
 Modified by Edeline Gagnon (edeline.gagnon@gmail.com), for Solanum geophyte paper (2023).


```{r}


#Load libraries
library(vegan)
library(ks)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(factoextra)
library("corrplot")
require(gridExtra)

#for colors
library(wesanderson)
ng.col<-"navyblue"
tb.col<-wes_palette("Zissou1", n = 5)[c(5)]
rh.col<-wes_palette("Zissou1", n = 5)[c(3)]


#set working directory and load data
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/02_PCA_Kernel_Density/")
occ.data<-read.csv("DF_merged_data_50352.csv",header=TRUE,row.names=1)
dim(occ.data)
table(occ.data$roots)

#create a data.frame with only variables of interest

#This keeping BRAHMS, LATDEC, LONGDEC, genus.sp, and all the environmental variables and traits and clade info.
remove<-c(2:11,14:17) 
dat<-(occ.data[,-remove])
print("names")
names(dat)
```

```{r}
# Histograms on distribution of all traits
#Bio5,Bio6,Bio7,BIo15,AMI,q95size,

hist<-dat[,c(5:12)]%>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() + ggtitle("Histograms of all data solanum")

hist
#ggsave(hist, device="pdf",filename = "histograms_all_vars_solanum.pdf")
```


```{r}
#Log transformation of data
dat$logq95size<-log(dat$q95size+1)
dat$logMI<-log(dat$MI+1)#
dat$logvrm<-log(dat$vrm_med+1)#
names(dat)

hist.1<-dat[,c(5:12,15:17)]%>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() + ggtitle("Histograms of all + transf Solanum")

hist.1
#ggsave(hist.1, device="pdf",filename = "histograms_all_vars_solanum_logs.pdf")

```



```{r}

# Histograms on distribution of eight traits
#Bio5,Bio6,Bio7,Bio15,log(MI),log(q95size),log(vrm_med),sand

#hist2<-dat[,c(13,5,6,12,7,15,16,19,18,20:22)]%>% 
hist2<-dat[,c(9,5,6,8,16,15,17,12)]%>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() + ggtitle("Histograms eight vars solanum")

hist2
#ggsave(hist2, device="pdf",filename = "histograms_12vars_solanum.pdf")

```



```{r}
# Creating a data frame containing only the required data.

#names(dat)
dat2<-dat[,c(9,5,6,8,16,15,17,12,14,4)] #8 variables, +roots + genus.sp 
names(dat2)


dat2<-dat2[complete.cases(dat2),]
unique(dat2$roots)
index.nongeophyte<-dat2$roots== "nongeophyte"
index.rhizomatous<-dat2$roots == "rhizomatous"
index.tuberous<-dat2$roots == "tuberous"

length(index.nongeophyte)#47083
length(index.rhizomatous)#47083
length(index.tuberous)#47083

#dat2 <- dat2[is.finite(rowSums(dat2)),]

dat2<-dat2 %>% 
  filter_all(all_vars(!is.infinite(.)))

dim(dat)#50352
dim(dat2)#47083

which(dat2$FRI==-999)->remove3# none, so don't need to do next two lines.
#dat2<-dat2[-remove3,]
#dim(dat2)
write.csv(dat2,"dat2_47083.csv")
```


```{r}
#################### PCA on climate, fire, soil and topography #####################################
# select the 8 variables
dat3<-dat2[,c(1:8)] #8 variables

names(dat2)
names(dat3)

dim(dat3)#47083
names(dat3)

# use princomp for PCA for being consistent with scaling of scores in Sandra's analysis 
prin<-princomp((dat3), cor = TRUE, scores = TRUE)
pc12<-prin$scores[,c(1,2)]
ll<-prin$loadings

# check for consistent results
### Not sure what this does for me..., not sure what is data [,18]
#pc12[,1]<-pc12[,1]*-1
#plot(pc12[,1]*-1, pc12[,1])
#plot(pc12[,1], dat3[,7])
#cor(pc12[,1], dat3[,7])

unique(dat2$roots)

table(dat2$roots)
index.nongeophyte<-dat2$roots== "nongeophyte"
index.rhizomatous<-dat2$roots == "rhizomatous"
index.tuberous<-dat2$roots == "tuberous"
```


```{r}
################ KERNEL DENSITY ESTIMATION ##############################
H <- Hpi(x=pc12)      # optimal bandwidth estimation
est<- kde(x=pc12, H=H, compute.cont=TRUE)     # kernel density estimation

H1 <- Hpi(x=pc12[index.rhizomatous,])      # optimal bandwidth estimation
est1<- kde(x=pc12[index.rhizomatous,], H=H1, compute.cont=TRUE)     # kernel density estimation

H2 <- Hpi(x=pc12[index.tuberous,])      # optimal bandwidth estimation
est2<- kde(x=pc12[index.tuberous,], H=H2, compute.cont=TRUE)     # kernel density estimation

H3 <- Hpi(x=pc12[index.nongeophyte,])      # optimal bandwidth estimation
est3<- kde(x=pc12[index.nongeophyte,], H=H3, compute.cont=TRUE)     # kernel density estimation


# Should I be actually doing it in clades of crops, rather than the whole dataset...


# set contour probabilities for drawing contour levels
cl<-contourLevels(est, prob=c(0.5, 0.10, 0.05, 0.001), approx=TRUE)
cl1<-contourLevels(est1, prob=c(0.5, 0.10, 0.05, 0.001), approx=TRUE)
cl2<-contourLevels(est2, prob=c(0.5, 0.10, 0.05, 0.001), approx=TRUE)
cl3<-contourLevels(est3, prob=c(0.5, 0.10, 0.05, 0.001), approx=TRUE)

fit<-envfit(pc12, dat3) # use envfit for drawing arrows, can be also done using trait loadings
fit2<-fit$vectors$arrows*-1 # drawing line segments in arrow opposites direction for pretty layout


```


```{r}
########
# PCA Kernel Density estimation for all data

#pdf("./Ecology_Letters_2/PCA_KD_test_8vars_April2023_ALLDATA.pdf")

plot(est, cont=seq(1,100,by=1), col=rev(hcl.colors(101,palette="Grays")),display="filled.contour2", add=FALSE, ylab="", xlab="", cex.axis=0.75, ylim=c(-6, 5), xlim=c(-5, 5),las=1) 
plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est,abs.cont=cl[2], labels=c(0.90),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est,abs.cont=cl[3], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est,abs.cont=cl[4], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")

title("All Growth forms")
abline(h=0,v=0,col="lightgray")
plot(fit, cex=0.90, col=1)
segments(0,0, fit2[,1], fit2[,2], col=1, lty=2, lwd=1)
mtext("PC1", cex=0.75, side=1, line=0.5, adj=1)
mtext("PC2", cex=0.75, side=2, line=0.5, at=4.7) #, las=2)
#dev.off()
```


```{r}
#PCA Kernel Density - USOs
#pdf("./Ecology_Letters_2/PCA_KD_test_8vars_April2023_USO.pdf")

plot(est2, cont=seq(1,100,by=1),col=rev(hcl.colors(101,palette="Reds 3")), display="filled.contour2", add=FALSE, ylab="", xlab="", cex.axis=0.75, ylim=c(-6, 5), xlim=c(-5, 5),las=1) 
plot(est2,abs.cont=cl2[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est2,abs.cont=cl2[2], labels=c(0.90),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est2,abs.cont=cl2[3], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est2,abs.cont=cl2[4], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
#
title("USO")
abline(h=0,v=0,col="lightgray")
plot(fit, cex=0.90, col=1)
segments(0,0, fit2[,1], fit2[,2], col=1, lty=2, lwd=1)
mtext("PC1", cex=0.75, side=1, line=0.5, adj=1)
mtext("PC2", cex=0.75, side=2, line=0.5, at=4.7) #, las=2)

#dev.off()
```


```{r}
########
#PCA Kernel Density - Non-geophytes


#pdf("./Ecology_Letters_2/PCA_KD_test_8vars_April2023_NG.pdf")

plot(est3, cont=seq(1,100,by=1), display="filled.contour2", col=rev(hcl.colors(101,palette="Blues3")),add=FALSE, ylab="", xlab="", cex.axis=0.75, ylim=c(-6, 5), xlim=c(-5, 5),las=1) 
plot(est3,abs.cont=cl3[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est3,abs.cont=cl3[2], labels=c(0.90),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est3,abs.cont=cl3[3], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est3,abs.cont=cl3[4], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
#
title("Non-geophytes")
abline(h=0,v=0,col="lightgray")
plot(fit, cex=0.90, col=1)
segments(0,0, fit2[,1], fit2[,2], col=1, lty=2, lwd=1)
mtext("PC1", cex=0.75, side=1, line=0.5, adj=1)
mtext("PC2", cex=0.75, side=2, line=0.5, at=4.7) #, las=2)
#dev.off()
```


```{r}
########
#PCA Kernel Density - Rhizomes

#pdf("./Ecology_Letters_2/PCA_KD_test_8vars_April2023_RH.pdf")

plot(est1, cont=seq(1,100,by=1), display="filled.contour2", col=rev(hcl.colors(101,palette="YlOrBr")), add=FALSE, ylab="", xlab="", cex.axis=0.75, ylim=c(-6, 5), xlim=c(-5, 5),las=1) 
plot(est1,abs.cont=cl1[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est1,abs.cont=cl1[2], labels=c(0.90),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est1,abs.cont=cl1[3], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est1,abs.cont=cl1[4], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
#
title("Rhizomes")
abline(h=0,v=0,col="lightgray")
plot(fit, cex=0.90, col=1)
segments(0,0, fit2[,1], fit2[,2], col=1, lty=2, lwd=1)
mtext("PC1", cex=0.75, side=1, line=0.5, adj=1)
mtext("PC2", cex=0.75, side=2, line=0.5, at=4.7) #, las=2)
#dev.off()
```


```{r}
########
#PCA Kernel Density - Overlap of growth forms

#pdf("./Ecology_Letters_2/PCA_KD_test_8vars_April2023_Overlap1.pdf")

plot(est3,abs.cont=cl3[1], labels=c(0.5),labcex=0.75, add=FALSE, lwd=3, col=ng.col ,ylab="", xlab="", cex.axis=0.75, ylim=c(-4.5, 4.5), xlim=c(-4.5, 4.5),las=1)
plot(est3,abs.cont=cl3[2], labels=c(0.90),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est3,abs.cont=cl3[3], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col=ng.col)
plot(est3,abs.cont=cl3[4], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")

title("Overlap of growth forms")
abline(h=0,v=0,col="lightgray")
#plot(fits, cex=0.90, col="lightgray")

plot(est2,abs.cont=cl2[1], col=tb.col,labels=c(0.5),add=TRUE,lwd=3)
plot(est2,abs.cont=cl2[4], col=tb.col,labels=c(0.99),add=TRUE)
plot(est1,abs.cont=cl1[1], col=rh.col,labels=c(0.5),add=TRUE,lwd=3)
plot(est1,abs.cont=cl1[4], col="sienna1",labels=c(0.99),add=TRUE)

#dev.off()
```



```{r}
####
#Then I want the PCA circle of contribution of all the variables
#I want to also see the chart of the contribution of each variable.
#I also want ot print out the correlation table

#Doing this for all 8 variables.
cor(dat3)
range(cor(dat3))
#write.table(cor(dat3),file="./Ecology_Letters_2/correlation_table_8vars.txt")

# use princomp for PCA for being consistent with scaling of scores in Sandra's analysis 
prin<-princomp((dat3), cor = TRUE, scores = TRUE)
pc12<-prin$scores[,1:2]
ll<-prin$loadings

eig.val <- get_eigenvalue(prin)
eig.val

p1<-fviz_eig(prin) #shows % of variation for each axis

#Shows circule of contribution
p2<-fviz_pca_var(prin,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#pdf("PCA_contrib_variances_8vars.pdf", width = 8, height = 12) # Open a new pdf file
grid.arrange(p2, p1, nrow=2)
#dev.off() 

#pdf("./Ecology_Letters_2/PCA_circle_contribution_8vars.pdf", width = 8, height = 12) # Open a new pdf file
grid.arrange(p2, p1, nrow=2)

#png("./Ecology_Letters_2/PCA_variation_in_each_axis_black.png")
p1
#dev.off() 

#png("./Ecology_Letters_2/PCA_circle_contribution_8vars_contrib.png")
p2
#dev.off()
```


```{r}
# Results from variables
res.var <- get_pca_var(prin)
res.var$coord          # Coordinates
res.var$contrib        # Contributions aux axes
res.var$cos2           # Quality of representation
```


```{r}
#library(corrplot)
#png("./Ecology_Letters_2/corrplot_8vars_ALLData_contrib_v3.png")
corrplot::corrplot(res.var$cos2, is.corr=FALSE)
#dev.off()
```


