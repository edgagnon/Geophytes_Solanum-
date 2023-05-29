##########################################
#
# Script for doing the Kruskall-Wallis test on niche breadth of Solanum species
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#########################################


rm(list = ls())

#Opening libraries
library(tidyverse)
library(ggpubr)
library(rstatix)


#Change working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/05_Niche_Breadth/")
#Opening file with range sizes and niche breadth
list.files()

data<-read.csv("Solanum_range_breadth_1062_v2.csv")
dim(data)
names(data)

data$roots

#Distribution of range sizes as represented by box plots
# Boxplot of MPG by Car Cylinders
boxplot(logbreadth~roots,data=data, main="",
        xlab="growth from", ylab="logbreadth")

#Histogram of range sizes.
hist(data$logbreadth)

data$roots<-as.factor(data$roots)


######### Doing the Kruskall-Wallis test, a non-parametric test
#https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/

res.kruskal <- data %>% rstatix::kruskal_test(logbreadth ~ roots)
res.kruskal

# A tibble: 1 x 6
#.y.            n statistic    df     p method        
#* <chr>      <int>     <dbl> <int> <dbl> <chr>         
#  1 logbreadth  1062      4.34     2 0.114 Kruskal-Wallis
#This is not significant...

data %>% kruskal_effsize(logbreadth ~ roots) #magnitude is small, less than 0.001
# A tibble: 1 x 5
#.y.            n effsize method  magnitude
#* <chr>      <int>   <dbl> <chr>   <ord>    
#  1 logbreadth  1062 0.00221 eta2[H] small


#Pairwise Comparison Dunn test
pwc <- data %>% 
  dunn_test(logbreadth ~ roots, p.adjust.method = "bonferroni") 
pwc



#Pairwise comparison Wilcoxon test
pwc2 <- data %>% 
  rstatix::wilcox_test(logbreadth ~ roots, p.adjust.method = "bonferroni")
pwc2


pwc <- pwc %>% add_xy_position(x = "roots")
ggboxplot(data, x = "roots", y = "a2log10") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


#####################

# Permutation test
library(coin)
data$roots<-as.factor(data$roots)

independence_test(a2log10 ~ roots,
                  data = data)

###########

library(rcompanion)


PT = pairwisePermutationTest(a2log10 ~ roots,
                             data = data,
                             method="fdr")

PT



cldList(p.adjust ~ Comparison,
        data = PT,
        threshold  = 0.05)

