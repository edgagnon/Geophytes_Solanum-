##########################################
#
# Script for doing boxplots of niche breadth and range sizes of Solanum species
#
# by E. Gagnon, part of Solanum Geophyte publication 2023
#########################################

rm(list = ls())

#Opening Libraries
library(gridExtra)
library(wesanderson)

#Setting colors
ng.col<-"navyblue"
tb.col<-wes_palette("Zissou1", n = 5)[c(5)]
rh.col<-wes_palette("Zissou1", n = 5)[c(3)]

#Range sizes
#Change working directory
setwd("C://Users/edeli/OneDrive/Projets_Recherche/2019_Tuber_project/new_TUber_coding/Solanum_geophytes/05_Niche_Breadth/")

#Opening file with range sizes and niche breadth
list.files()

data<-read.csv("Solanum_range_breadth_1062_v2.csv")
dim(data)
names(data)

data2<-data[,c(21,24)]


### Do a boxplot for range sizes
bp <- ggplot(data2, aes(x=roots, y=a2log10, fill=roots)) + 
  geom_boxplot()+
  scale_fill_manual(values=c(ng.col, rh.col, tb.col))+
  labs(title="Boxplot of range sizes",x="", y = "range size log a2")+ 
  theme_minimal()+
    theme(legend.position="none",
            axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
   
bp
#ggsave("boxplot_rangesizes.pdf")

bp2 <- ggplot(data2, aes(x=roots, y=a2log10, fill=roots)) + 
  geom_boxplot()+
  scale_fill_manual(values=c(ng.col, rh.col, tb.col))+
  labs(title="Boxplot of range sizes",x="", y = "range size log a2")+ 
  theme_minimal()#+
#  theme(legend.position="none") 
bp2



#I need to add in the log-transformed variables to the table.
data$logbio5<-log(data$nichebio5me+1)
data$logbio6<-log(data$nichebio6me+1)
data$logbio7<-log(data$nichebio7me+1)
data$logbio15<-log(data$nichebio15me+1)
data$logAMI<-log(data$nicheAMIme+1)
data$logq95size<-log(data$nicheq95sizeme+1)
data$logVRM<-log(data$nicheVRMme+1)
data$logsand<-log(data$nichesandme+1)

dat2<-data[,c(27:35,24)]

x<-names(dat2[c(1:9)])



################


plot.list<-list()
#pdf("Boxplots_Solanum_8vars_nichebreadth_v2.pdf",width=8, height=6)

for (i in 1:length(x))
{
  #i<-1
  print(x[i])
  #  pdf(paste(wd,"/boxplot_output/Boxplots_Solanun_",x[i],"_mean.pdf",sep=""),width=8, height=6)
  
  plot.list[[i]]<-ggplot(dat2, aes_string(x = "roots", y = x[i], fill = "roots")) +
    geom_boxplot() +
    #    geom_jitter(shape = 15,
    #                color = "steelblue",
    #                position = position_jitter(0.21)) +
    scale_fill_manual(values=c(ng.col, rh.col,tb.col,"white"))+ #fix the colors, ng
    theme_minimal()+
    theme(legend.position="none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ggtitle(x[i])
  print(plot.list[[i]])  
  
}
#dev.off()     

do.call('grid.arrange',plot.list) #, ncol = 3))

legend <- cowplot::get_legend(bp2)
plot.list[[length(x)+1]]<-bp
plot.list[[length(x)+2]]<-legend

#grid.fig<-do.call('grid.arrange',plot.list[[1:11]]) #, ncol = 3)))
#grid.fig
do.call('grid.arrange',plot.list) #, ncol = 3))

#ggsave(file="Grid_boxplot_8vars_slolanum_niche_breadth_v2.png",width=8, height=8,units="in")
