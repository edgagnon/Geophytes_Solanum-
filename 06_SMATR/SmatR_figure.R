##########################################
#
# Script for doing plotting the results of a Smatr analysis, including a figure
# with a confidence interval
#https://stackoverflow.com/questions/39453540/how-to-plot-confidence-intervals-for-a-major-axis-fit-using-the-smatr-package-in
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#########################################

rm(list = ls())
gc()
#install.packages("smatr")

# load packages
library(smatr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(wesanderson)

#setting the colors
ng.col<-"navyblue"
tb.col<-wes_palette("Zissou1", n = 5)[c(5)]
rh.col<-wes_palette("Zissou1", n = 5)[c(3)]

#Change working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/06_smatR3/")

#Opening file with range sizes and niche breadth
list.files()
data<-read.csv("Solanum_range_breadth_1062_v2.csv")
dim(data)
names(data)

#graphs
do.call(rbind, lapply(unique(data$roots), function(x) {
  obj <- sma(logbreadth ~ a2log10*roots, data=subset(data, roots==x),robust=T)
  data.frame(roots=x, 
             intercept=obj$coef[[1]][1, 1],
             slope=obj$coef[[1]][2, 1])
})) -> fits

fits


data.uso<-subset(data, roots == "tuberous")
data.rh<-subset(data, roots == "rhizomatous")
data.ng<-subset(data, roots == "nongeophyte")

dim(data.uso)
dim(data.ng)
dim(data.rh)

# make columns log scale
#leaflife <- mutate(leaflife, log_longev = log10(longev),
#                   log_lma = log10(lma))

# fit sma
#mod <- sma(log_longev ~ log_lma, data=leaflife, method="SMA")
mod.uso<-sma(logbreadth ~ a2log10, data=data.uso, robust=T,multcomp = TRUE,multcompmethod = "adjusted")
mod.rh<-sma(logbreadth ~ a2log10, data=data.rh, robust=T,multcomp = TRUE,multcompmethod = "adjusted")
mod.ng<-sma(logbreadth ~ a2log10, data=data.ng, robust=T,multcomp = TRUE,multcompmethod = "adjusted")

# plot model
#plot(mod)

plot(mod.uso)
plot(mod.rh)
plot(mod.ng)

####Done here with data.ng

# create new data set of log_lma at a high resolution (200 points from min to max)
preds.ng <- data.frame(expand.grid(a2log10 = seq(min(data.ng$a2log10, na.rm = T), max(data.ng$a2log10, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
preds.ng <- data.ng %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(logbreadth ~ a2log10, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  dplyr::select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*preds.ng$a2log10, 
                a2log10 = preds.ng$a2log10)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each log_lma value
  group_by(., a2log10) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., logbreadth = coef(mod.ng)[1] + coef(mod.ng)[2]*a2log10)

# plot with ggplot
ggplot(data.ng, aes(a2log10, logbreadth )) +
  geom_point() +
#  geom_line(data = preds) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, preds.ng) +
  theme_bw()


#######################################
####Done here with data.rh

# create new data set of log_lma at a high resolution (200 points from min to max)
preds.rh <- data.frame(expand.grid(a2log10 = seq(min(data.rh$a2log10, na.rm = T), max(data.rh$a2log10, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
preds.rh <- data.rh %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(logbreadth ~ a2log10, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  dplyr::select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*preds.rh$a2log10, 
                a2log10 = preds.rh$a2log10)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each log_lma value
  group_by(., a2log10) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., logbreadth = coef(mod.rh)[1] + coef(mod.rh)[2]*a2log10)

# plot with ggplot
ggplot(data.rh, aes(a2log10, logbreadth )) +
  geom_point() +
#  geom_line(data = preds.rh) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, preds.rh) +
  theme_bw()


#######################################
####Done here with data.uso

# create new data set of log_lma at a high resolution (200 points from min to max)
preds.uso <- data.frame(expand.grid(a2log10 = seq(min(data.uso$a2log10, na.rm = T), max(data.uso$a2log10, na.rm = T), length.out = 200), stringsAsFactors = FALSE))

# bootstrap data and get predictions
preds.uso <- data.uso %>%
  # create new bootstrapped data sets
  modelr::bootstrap(n = 1000, id = 'boot_num') %>%
  # fit sma to every bootstrap
  group_by(boot_num) %>%
  mutate(., fit = map(strap, ~ sma(logbreadth ~ a2log10, data=data.frame(.), method="SMA"))) %>%
  ungroup() %>%
  # extract intercept and slope from each fit
  mutate(., intercept = map_dbl(fit, ~coef(.x)[1]),
         slope = map_dbl(fit, ~coef(.x)[2])) %>%
  dplyr::select(., -fit) %>%
  # get fitted values for each bootstrapped model
  # uses the preds dataframe we made earlier
  group_by(boot_num) %>%
  do(data.frame(fitted = .$intercept + .$slope*preds.uso$a2log10, 
                a2log10 = preds.uso$a2log10)) %>%
  ungroup() %>%
  # calculate the 2.5% and 97.5% quantiles at each log_lma value
  group_by(., a2log10) %>%
  dplyr::summarise(., conf_low = quantile(fitted, 0.025),
                   conf_high = quantile(fitted, 0.975)) %>%
  ungroup() %>%
  # add fitted value of actual unbootstrapped model
  mutate(., logbreadth = coef(mod.uso)[1] + coef(mod.uso)[2]*a2log10)

# plot with ggplot
  smatr.p<-ggplot(data, aes(a2log10, logbreadth ))+
  geom_point(aes(x=a2log10, y=logbreadth, color=roots))+
  #  geom_line(data = preds) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, preds.uso, fill=tb.col)+
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, preds.rh, fill=rh.col)+
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.1, preds.ng, fill=ng.col)+
  geom_abline(data=fits, aes(slope=slope, intercept=intercept,color=roots))+
  scale_color_manual(values=c(ng.col,rh.col,tb.col))+
  theme_minimal()
  smatr.p
#  ggsave(smatr.p,file="smatR_3curves.png",width=102.5, height=124,units="mm")


