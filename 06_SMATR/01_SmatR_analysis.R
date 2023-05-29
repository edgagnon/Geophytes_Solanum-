##########################################
#
# Script for doing a Smatr analysis, to test for changes in the relationship between range size and niche breadth
# across different type of growth forms in Solanum
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#########################################

rm(list = ls())

#install.packages("smatr")

library(smatr)
library(ggplot2)

library(wesanderson)
ng.col<-"navyblue"
tb.col<-wes_palette("Zissou1", n = 5)[c(5)]
rh.col<-wes_palette("Zissou1", n = 5)[c(3)]

#Change working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/06_smatR3/")

#Opening file with range sizes and niche breadth
list.files()
data<-read.csv("../05_Niche_Breadth/Solanum_range_breadth_1062_v2.csv")
dim(data)
names(data)


#########
#Carry out smatr analyses
#One Sample analysis + test for significance

# Fit a single MA for log(leaf longevity) vs log(leaf mass per area):
ma(logbreadth ~ a2log10,  data=data)
#Call: sma(formula = ..1, data = ..2, method = "MA") 

#Fit using Major Axis 

#------------------------------------------------------------
#  Coefficients:
#elevation     slope
#estimate    -0.002243123 0.1931877
#lower limit -0.045678803 0.1832905
#upper limit  0.041192558 0.2031215

#H0 : variables uncorrelated
#R-squared : 0.5796718 
#P-value : < 2.22e-16 


# Test if the MA slope is not significantly different from 1 for longevity and leaf mass per area (LMA):
ma_obj <- ma(logbreadth ~ a2log10, slope.test=1, data=data, robust=T) 

summary(ma_obj)

#Call: sma(formula = ..1, data = ..3, method = "MA", slope.test = 1, 
#          robust = ..4) 

#Fit using Major Axis 

#------------------------------------------------------------
#  Coefficients:
#  elevation     slope
#estimate    -0.08676972 0.2116109
#lower limit -0.13406721 0.2009233
#upper limit -0.03947224 0.2223450

#H0 : variables uncorrelated
#R-squared : 0.5796718 
#P-value : < 2.22e-16 

#------------------------------------------------------------
#  H0 : slope not different from 1 
#Test statistic : r= -0.9489 with 1060 degrees of freedom under H0
#P-value : < 2.22e-16 


#pdf("SMA_one_v2.pdf")
plot(ma_obj)
#dev.off()


#Checking assumptions
#png("Assumptions_v2.png",width=8, height=4,units="in",res=300)
par(mfrow=c(1,2))

plot(ma_obj, which="residual") #Fig 1b
plot(ma_obj, which="qq") #Fig 1c
#dev.off()

#########################
#Multiple sample analyses

#Often researchers wish to compare between slopes or intercepts of axes between different groups. 

b_r_obj <- sma(logbreadth ~ a2log10*roots, data=data, robust=T,multcomp = TRUE,multcompmethod = "adjusted")

# Lets plot longevity vs LMA separately for each group first:
#pdf("SMA_3slopes_v2.pdf")
plot(b_r_obj,col=c(ng.col,rh.col,tb.col))
#dev.off()

#By default, the sma() function will first fit a common slope for all groups and then test whether there is a difference in elevation
summary(b_r_obj)

########
#Call: sma(formula = logbreadth ~ a2log10 * roots, data = data, multcomp = TRUE, 
#          multcompmethod = "adjusted", robust = T) 

#Fit using Standardized Major Axis 

#------------------------------------------------------------
#  Results of comparing lines among groups.

#H0 : slopes are equal.
#Likelihood ratio statistic : 11.65 with 2 degrees of freedom
#P-value : 0.0029572 

#------------------------------------------------------------
#  Results of multiple comparisons among groups.

#Test for pair-wise difference in slope :
#  roots_1     roots_2      Pval   TestStat
#1 nongeophyte rhizomatous 0.0019667 11.6099832
#2 nongeophyte    tuberous 0.9687471  0.1645408
#3 rhizomatous    tuberous 0.0584090  5.4240242

#------------------------------------------------------------
#  Coefficients by group in variable "roots"

#Group: nongeophyte 
#elevation     slope
#estimate    -0.2529484 0.2536644
#lower limit -0.3053196 0.2420663
#upper limit -0.2005773 0.2658182

#H0 : variables uncorrelated.
#R-squared : 0.5826516 
#P-value : < 2.22e-16 

#Group: rhizomatous 
#elevation     slope
#estimate    -0.6256038 0.3070134
#lower limit -0.7683302 0.2781395
#upper limit -0.4828775 0.3388847

#H0 : variables uncorrelated.
#R-squared : 0.6221908 
#P-value : < 2.22e-16 

#Group: tuberous 
#elevation     slope
#estimate    -0.2450602 0.2595603
#lower limit -0.3584103 0.2344857
#upper limit -0.1317101 0.2873162

#H0 : variables uncorrelated.
#R-squared : 0.6953704 
#P-value : < 2.22e-16 


############
#Again, using the slope.test will test whether the common slope is significantly different to 1.

# Fit SMAs separately at each of high and low rainfall sites,
# and test if there is a common slope and whether it is equal to 1:

root_slope_obj <- sma(logbreadth ~ a2log10*roots, data=data, slope.test=1,robust=T,multcomp = TRUE,multcompmethod = "adjusted" )
summary(root_slope_obj)

############
#Call: sma(formula = logbreadth ~ a2log10 * roots, data = data, slope.test = 1, 
#          multcomp = TRUE, multcompmethod = "adjusted", robust = T) 

#Fit using Standardized Major Axis 

#------------------------------------------------------------
#  Results of comparing lines among groups.

#H0 : slopes are equal.
#Likelihood ratio statistic : 11.65 with 2 degrees of freedom
#P-value : 0.0029572 
#------------------------------------------------------------
  
#  H0 : common slope not different from 1 
#Likelihood ratio statistic = 2258 with 3 degrees of freedom under H0
#P-value : < 2.22e-16 

#Results of multiple comparisons among groups.

#Test for pair-wise difference in slope :
#  roots_1     roots_2      Pval   TestStat
#1 nongeophyte rhizomatous 0.0019667 11.6099832
#2 nongeophyte    tuberous 0.9687471  0.1645408
#3 rhizomatous    tuberous 0.0584090  5.4240242

#------------------------------------------------------------
#  Coefficients by group in variable "roots"

#Group: nongeophyte 
#elevation     slope
#estimate    -0.2529484 0.2536644
#lower limit -0.3053196 0.2420663
#upper limit -0.2005773 0.2658182

#H0 : variables uncorrelated.
#R-squared : 0.5826516 
#P-value : < 2.22e-16 

#H0 : slope not different from 1 
#Test statistic: r= -0.9511 with 780 degrees of freedom under H0
#P-value : < 2.22e-16 

#Group: rhizomatous 
#elevation     slope
#estimate    -0.6256038 0.3070134
#lower limit -0.7683302 0.2781395
#upper limit -0.4828775 0.3388847

#H0 : variables uncorrelated.
#R-squared : 0.6221908 
#P-value : < 2.22e-16 

#H0 : slope not different from 1 
#Test statistic: r= -0.925 with 168 degrees of freedom under H0
#P-value : < 2.22e-16 

#Group: tuberous 
#elevation     slope
#estimate    -0.2450602 0.2595603
#lower limit -0.3584103 0.2344857
#upper limit -0.1317101 0.2873162

#H0 : variables uncorrelated.
#R-squared : 0.6953704 
#P-value : < 2.22e-16 

#H0 : slope not different from 1 
#Test statistic: r= -0.96 with 108 degrees of freedom under H0
#P-value : < 2.22e-16 

################################
#The type argument allows users to test for a change in elevation

# Fit SMAs separately at each of high and low rainfall sites, and test whether sites differ in the elevation of their SMA
root_elev_obj <- sma(logbreadth ~ a2log10+roots, data=data, type = "elevation",robust=T,multcomp = TRUE, multcompmethod = "adjusted")
summary(root_elev_obj)

#So there is a significant change in elevation amongst the slopes
#########
#Call: sma(formula = logbreadth ~ a2log10 + roots, data = data, type = "elevation", 
#          multcomp = TRUE, multcompmethod = "adjusted", robust = T) 

#Fit using Standardized Major Axis 

#------------------------------------------------------------
#  Results of comparing lines among groups.

#H0 : slopes are equal.
#Likelihood ratio statistic : 11.65 with 2 degrees of freedom
#P-value : 0.0029572 
#------------------------------------------------------------
  
#  H0 : no difference in elevation.
#Wald statistic: 61.19 with 2 degrees of freedom
#P-value : 5.1736e-14 
#------------------------------------------------------------
  
#  Results of multiple comparisons among groups.

#Test for pair-wise difference in elevation :
#  roots_1     roots_2       Pval  TestStat
#1 nongeophyte rhizomatous 1.2443e-12 52.571809
#2 nongeophyte    tuberous     0.1865  3.367615
#3 rhizomatous    tuberous 4.4315e-12 50.078536

#------------------------------------------------------------
#  Coefficients by group in variable "roots"

#Group: nongeophyte 
#elevation     slope
#estimate    -0.2716454 0.2616295
#lower limit -0.3327186 0.2518260
#upper limit -0.2411333 0.2725109

#H0 : variables uncorrelated.
#R-squared : 0.5826516 
#P-value : < 2.22e-16 

#Group: rhizomatous 
#elevation     slope
#estimate    -0.4070004 0.2616295
#lower limit -0.4762545 0.2518260
#upper limit -0.3612687 0.2725109

#H0 : variables uncorrelated.
#R-squared : 0.6221908 
#P-value : < 2.22e-16 

#Group: tuberous 
#elevation     slope
#estimate    -0.2465343 0.2616295
#lower limit -0.3071716 0.2518260
#upper limit -0.1999651 0.2725109

#H0 : variables uncorrelated.
#R-squared : 0.6953704 
#P-value : < 2.22e-16 

root_shift_obj <- sma(logbreadth ~ a2log10+roots, type = "shift",data=data,robust=T,multcomp = T,multcompmethod = "adjusted")
summary(root_shift_obj)

#Call: sma(formula = logbreadth ~ a2log10 + roots, data = data, type = "shift", 
#          multcomp = T, multcompmethod = "adjusted", robust = T) 

#Fit using Standardized Major Axis 

#------------------------------------------------------------
#  Results of comparing lines among groups.

#H0 : slopes are equal.
#Likelihood ratio statistic : 11.65 with 2 degrees of freedom
#P-value : 0.0029572 
#------------------------------------------------------------
  
#  H0 : no shift along common axis.
#Wald statistic: 0.8389 with 2 degrees of freedom
#P-value : 0.65741 
#------------------------------------------------------------
  
#  Results of multiple comparisons among groups.
#
#Test for pair-wise difference in shift :
#  roots_1     roots_2    Pval  TestStat
#1 nongeophyte rhizomatous 0.97042 0.1582907
#2 nongeophyte    tuberous 0.83155 0.5764237
#3 rhizomatous    tuberous 0.68789 0.9821256

#------------------------------------------------------------
#  Coefficients by group in variable "roots"

#Group: nongeophyte 
#elevation     slope
#estimate    -0.2716454 0.2616295
#lower limit -0.3053196 0.2518260
#upper limit -0.2005773 0.2725109

#H0 : variables uncorrelated.
#R-squared : 0.5826516 
#P-value : < 2.22e-16 

#Group: rhizomatous 
#elevation     slope
#estimate    -0.4070004 0.2616295
#lower limit -0.7683302 0.2518260
#upper limit -0.4828775 0.2725109

#H0 : variables uncorrelated.
#R-squared : 0.6221908 
#P-value : < 2.22e-16 

#Group: tuberous 
#elevation     slope
#estimate    -0.2465343 0.2616295
#lower limit -0.3584103 0.2518260
#upper limit -0.1317101 0.2725109

#H0 : variables uncorrelated.
#R-squared : 0.6953704 
#P-value : < 2.22e-16 

############
root_shift_T_obj <- sma(logbreadth ~ a2log10+roots, shift=T,data=data,robust=T,multcomp = T,multcompmethod = "adjusted")
summary(root_shift_T_obj)

  ###########################

#https://stackoverflow.com/questions/33601213/combining-output-from-smatr-with-ggplot2


do.call(rbind, lapply(unique(data$roots), function(x) {
  obj <- sma(logbreadth ~ a2log10*roots, data=subset(data, roots==x),robust=T)
  data.frame(roots=x, 
             intercept=obj$coef[[1]][1, 1],
             slope=obj$coef[[1]][2, 1])
})) -> fits

fits


gg <- ggplot(data) 
gg <- gg + geom_point(aes(x=a2log10, y=logbreadth, color=roots))
gg <- gg + geom_abline(data=fits, aes(slope=slope, intercept=intercept,color=roots))+theme_minimal()
gg
#ggsave("SMA_3groups_slopes_v2.png")

gg2 <- gg + facet_wrap(~roots, ncol=2)
gg2
#ggsave("SMA_3groups_slopes_facets_v2.pdf")

#####################
