##########################################
#
# Script for doing the Kruskall-Wallis test on range sizes of Solanum species
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#########################################
rm(list = ls())

#Opening libraries
library(tidyverse)
library(ggpubr)
library(rstatix)

#Change working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/04_Range_Size_Niche_Breadth/")


#Opening Range Size file

range.sizes<-read.csv("01_results_1179sp_incAOO_unfiltered.csv")
range.sizes
names(range.sizes)


range.sizes$a2log10<-log10(range.sizes$EOO_a2) ### This is where I do a log transformation.
range.sizes$a2sqr<-sqrt(range.sizes$EOO_a2) ### This is where I do a square root transformation.

#According to Ludwig Baldaszti recommendations.
#For specimens with 3 records or more, Using Alpha Hull 2range.sizes$a2log10
#For specimens with 2 records, use EOO value
#For specimens with 1 record, use AOO value

range.sizes$final.range<-range.sizes$a2log10

length(range.sizes[range.sizes$Specimens==2,]$final.range)# 60 species have two records or less
length(range.sizes[range.sizes$Specimens==1,]$final.range)# 46 species have only a single record

table(is.na(range.sizes[range.sizes$Specimens==2,]$EOO_con))
#58 species don't have range size calculations at all.

which(is.na(range.sizes$final.range))

##########################################

# Adding traits to the range size table.

#Read trait dataset
trait.data<-read.csv("../00_Data/01_Trait_Data/SolTrGeo.csv",header=TRUE)
dim(trait.data)#1232

names(trait.data)
#names(all.data)



range.sizes$roots <- trait.data$roots[match(range.sizes$Species, trait.data$genus.sp)]

na.toto<-range.sizes[which(is.na(range.sizes$roots)), ]
unique(na.toto$Species)
#[1] "Solanum_haematocarpon" "Solanum_ligulatum"     "Solanum_micranthum"    "Solanum_mirum"        
#[5] "Solanum_paucispinum"   "Solanum_postremum"     "Solanum_rufistellatum" "Solanum_gracile" 

#remove rows with NA

dim(range.sizes)#1179 species
dim(range.sizes[complete.cases(range.sizes), ])# 1072 species

range.sizes.1179<-range.sizes

range.sizes<-range.sizes[complete.cases(range.sizes), ]

head(range.sizes)

#Distribution of range sizes as represented by box plots

boxplot(a2log10~roots,data=range.sizes, main="",
        xlab="growth from", ylab="AlphaHulla2")

#Histogram of range sizes.
#pdf("hist_range_sizes.pdf")
hist(range.sizes$a2log10)
#dev.off()


######### Doing the Kruskall-Wallis test, a non-parametric test
#https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/

res.kruskal <- range.sizes %>% rstatix::kruskal_test(a2log10 ~ roots)
res.kruskal
#This is significant...

# A tibble: 1 x 6
#.y.         n statistic    df       p method        
#* <chr>   <int>     <dbl> <int>   <dbl> <chr>         
#  1 a2log10  1072      11.4     2 0.00341 Kruskal-Wallis

range.sizes %>% rstatix::kruskal_effsize(a2log10 ~ roots) #magnitude is small, less than 0.006
# A tibble: 1 x 5
#.y.         n effsize method  magnitude
#* <chr>   <int>   <dbl> <chr>   <ord>    
#  1 a2log10  1072 0.00876 eta2[H] small 

#Pairwise Comparison Dunn test
pwc <- range.sizes %>% 
  dunn_test(a2log10 ~ roots, p.adjust.method = "bonferroni") 
pwc

# A tibble: 3 x 9
#.y.     group1      group2         n1    n2 statistic       p   p.adj p.adj.signif
#* <chr>   <chr>       <chr>       <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
#1 a2log10 nongeophyte rhizomatous   792   170     3.05  0.00227 0.00680 **          
#2 a2log10 nongeophyte tuberous      792   110    -0.965 0.334   1       ns          
#3 a2log10 rhizomatous tuberous      170   110    -2.91  0.00360 0.0108  *

#Pairwise comparison Wilcoxon test
pwc2 <- range.sizes %>% 
  rstatix::wilcox_test(a2log10 ~ roots, p.adjust.method = "bonferroni")
pwc2

# A tibble: 3 x 9
#.y.     group1      group2         n1    n2 statistic     p p.adj p.adj.signif
#* <chr>   <chr>       <chr>       <int> <int>     <dbl> <dbl> <dbl> <chr>       
#1 a2log10 nongeophyte rhizomatous   792   170     57267 0.002 0.007 **          
#2 a2log10 nongeophyte tuberous      792   110     46051 0.331 0.993 ns          
#3 a2log10 rhizomatous tuberous      170   110     11255 0.004 0.012 *  

# Visualization: box plots with p-values

pwc <- pwc %>% add_xy_position(x = "roots")
ggboxplot(range.sizes, x = "roots", y = "a2log10") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#####################

# Permutation test
library(coin)

range.sizes$roots<-as.factor(range.sizes$roots)

independence_test(a2log10 ~ roots,
                  data = range.sizes)


#Asymptotic General Independence Test

#data:  a2log10 by roots (nongeophyte, rhizomatous, tuberous)
#maxT = 2.9407, p-value = 0.008821
#alternative hypothesis: two.sided



library(rcompanion)

range.sizes$roots<-as.factor(range.sizes$roots)

PT = pairwisePermutationTest(a2log10 ~ roots,
                             data = range.sizes,
                             method="fdr")

PT

#Comparison   Stat  p.value p.adjust
#1 nongeophyte - rhizomatous = 0 -2.795 0.005182  0.01555
#2    nongeophyte - tuberous = 0 0.5634   0.5732  0.57320
#3    rhizomatous - tuberous = 0  2.471  0.01347  0.02020


library(rcompanion)

cldList(p.adjust ~ Comparison,
        data = PT,
        threshold  = 0.05)

#Group Letter MonoLetter
#1 nongeophyte      a         a 
#2 rhizomatous      b          b
#3    tuberous      a         a 
