library(ggplot2)
library(ape)
library(repr)
library("readxl")
library('gridExtra')
#library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library("ggsci")
library(ggalt)
library("Hmisc")
library("scales")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")

# load pacakges
library(ggtree)
library(tidyverse)
library(tidytree)
library(ape)
library(treeio)

# Fig2C and Fig S2###
tree<-read.tree('new_tree/timetree.nwk')
#Switch between these two metadata depending on what you need to plot (ALL or just REDE)
#metadata_df <- read_excel('Brazil_metadata_lineage_clusters.xlsx') ### All Brazil
metadata_df <- read_excel('Brazil_metadata_lineage.xlsx') ### Only REDE Genomes

# transform dates
metadata_df$date<-as.Date(cut(metadata_df$date,
                              breaks = "week",
                              start.on.monday = FALSE))

metadata_df$date2<-as.Date(cut(metadata_df$date,
                              breaks = "2 week",
                              start.on.monday = FALSE))

metadata_df$date3<-as.Date(cut(metadata_df$date,
                               breaks = "1 month",
                               start.on.monday = FALSE))


pangolin_count<-as.data.frame(table(metadata_df$lineage))
pangolin_count<-pangolin_count[order(pangolin_count$Freq),]
pangolin_count_top20<-tail(pangolin_count,n=10)
pangolin_count_top20

pangolin_count_top20<-pangolin_count_top20 %>% 
  rename(
    lineage = Var1,
  )

pangolin_count_top20$pangolin_Brazil<-pangolin_count_top20$lineage

metadata_df <- merge(metadata_df,pangolin_count_top20,by="lineage",all.x = TRUE)
metadata_df = select(metadata_df, -lineage)


p<-ggtree(tree, mrsd="2021-06-28", as.Date=TRUE,color='grey',size=0.1) %<+% metadata_df + theme_tree2() 

p1<-p+scale_colour_manual(values=c("darkseagreen3",'darkorange2','goldenrod2','bisque4','coral3')) +
  scale_fill_manual(values=c('bisque2','antiquewhite4','dodgerblue3','darkseagreen4','hotpink3','purple3','goldenrod2','grey30','darkorange2','coral4'), name='Brazil Genomes',na.value="grey90")+
  geom_tippoint(aes(
    subset=(country=='Brazil' & !is.na(pangolin_Brazil)), fill=pangolin_Brazil),size=3, align=F, color='black',shape=21, stroke=0.1) +
  scale_x_date(date_labels = "%B-%Y",date_breaks = "2 month") +
  theme(axis.text=element_text(size=10)) +
  expand_limits(y = 27000) +
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p1

ggsave('', width = 100, height = 500, units = "cm",limitsize = FALSE)

#### FIG 2B###
myColor <- randomcoloR::distinctColorPalette(k = 12)

data2<-subset(metadata_df,country=='Brazil')

pangolin_count<-as.data.frame(table(data2$lineage))
pangolin_count<-pangolin_count[order(pangolin_count$Freq),]
pangolin_count_top20<-tail(pangolin_count,n=10)
pangolin_count_top20

pangolin_count_top20<-pangolin_count_top20 %>% 
  rename(
    lineage = Var1,
  )

pangolin_count_top20$pangolin_Brazil<-pangolin_count_top20$lineage
#pangolin_count_top20[c("pangolin_Brazil")][is.na(pangolin_count_top20[c("pangolin_Brazil")])] <- 0

data2 <- merge(data2,pangolin_count_top20,by="lineage",all.x = TRUE)

custom2<-c("tan4",'peachpuff3','antiquewhite2','darkseagreen2','darkseagreen4','darkolivegreen',
           #custom2<-c("darkolivegreen",'darkseagreen3','darkseagreen2','antiquewhite2','peachpuff3','tan4',
           'red3','deeppink3','hotpink2','plum2','grey',
           'thistle3','mediumpurple','mediumorchid3','purple4',
           'blue3','dodgerblue1','cadetblue3','skyblue1','slategray1', 'white')



custom1 <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
             "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352", "black","slategrey",
             "burlywood4","thistle4","lightsalmon2","coral2","lightpink3","indianred3","orange3","yellow3","yellowgreen",
             "khaki4","springgreen4","aquamarine3","lightblue2","lightblue4")


#data2 <- merge(data2,pangolin_count_top20,by="lineage",all.x = TRUE)


panelC<-data2 %>%
  mutate(date=as.POSIXct(date2)) %>%
  ggplot(data=subset(data2, !is.na(strain)), mapping = aes(x = date2, fill=pangolin_Brazil))+
  geom_bar(position='fill',width=10,color='black')+
  #geom_bar(width=5)+
  #geom_bar(width=10,color='black')+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  xlab("Sampling Date")+ 
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=15))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  scale_fill_manual(values=c('bisque2','antiquewhite4','dodgerblue3','darkseagreen4','hotpink3','purple3','goldenrod2','grey30','darkorange2','coral4'), name='Brazil Genomes',na.value="grey90")+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  theme(legend.position = "top")+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  xlab('Date')+
  #ylab('Genome Count')
  ylab('Proportion of Genomes')
#ggtitle('')
panelC


#####Figs 2 D-E#####
tree<-read.tree('/Users/Mittenavoig/Desktop/Big_SARS-CoV-2/Figures/Tree/P.1_Import_Export/Tree_Time_P.1/P.1_timetree.nwk')
metadata_df <- read_excel('/Users/Mittenavoig/Desktop/metadata_P1.xlsx') ### All P.1

# transform dates
metadata_df$date<-as.Date(cut(metadata_df$date,
                              breaks = "week",
                              start.on.monday = FALSE))

metadata_df$date2<-as.Date(cut(metadata_df$date,
                               breaks = "2 week",
                               start.on.monday = FALSE))

metadata_df$date3<-as.Date(cut(metadata_df$date,
                               breaks = "1 month",
                               start.on.monday = FALSE))

p<-ggtree(tree, mrsd="2021-06-23", as.Date=TRUE,color='grey',size=0.1) %<+% metadata_df + theme_tree2() 
p
p2<-p +
  scale_fill_manual(values=c('bisque2','antiquewhite4','dodgerblue3','darkseagreen4','hotpink3','purple3','goldenrod2','grey30','darkorange2','coral4','darkgreen','cadetblue3','indianred3'), name='Paraguay Genomes',na.value="grey90")+
  #geom_tippoint(fill='black',size=2, align=F, color='black',shape=21, stroke=0.1) +
  geom_tippoint(aes(fill=region),size=3, color='black',shape=21, stroke=0.1) +
  scale_x_date(date_labels = "%B-%Y",date_breaks = "2 month") +
  theme(axis.text=element_text(size=5)) +
  #expand_limits(y = 11000) +
  ggplot2::ylim(0, 11000)+
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
#geom_text(aes(label=node), hjust=-.3, size=2)
p2


##### Figs 2F; 3CD####

library(ggalluvial)
library(lubridate)
#importexport<-read_excel('RA_Continent.xlsx')
importexport<-read_excel('Within_BRA_P2.xlsx')

importexport$EventTime<-as.numeric(importexport$EventTime)


importexport$date <- as.Date(format(date_decimal(importexport$EventTime), "%Y-%m-%d"))

importexport$date

#import_bots<-subset(importexport, Destination=='Brazil')


importexport$days<-as.Date(cut(importexport$date,
                               breaks = "day",
                               start.on.monday = FALSE))

importexport$date<-as.Date(cut(importexport$date,
                               breaks = "2 week",
                               start.on.monday = FALSE))
importexport$date2<-as.Date(cut(importexport$date,
                                breaks = "1 month",
                                start.on.monday = FALSE))

#importexport$date2 <- as.POSIXct(importexport$date2, format = "%m/%d/%Y %H:%M:%S")
#importexport$date2 <- format(importexport$date2, format="%b-%Y")


panelA<-importexport %>%
  mutate(date=as.POSIXct(date)) %>%
  ggplot()+
  geom_bar(data=subset(importexport, Origin=='Brazil'), mapping = aes(x = date, fill='Exports'),width=10)+
  geom_bar(data=subset(importexport, Destination=='Brazil'), mapping = aes(x = date, fill='Imports'),width=10)+
  
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=16))+
  theme(axis.title.x = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.y = element_text(color="black", size=15, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=15))+
  scale_fill_manual(values=c('grey70','black','goldenrod3','darkseagreen3','darkorange3','red3','deeppink3','chartreuse3','lightpink1','gold2','orchid2'))+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(legend.position = "none")+
  labs(fill=" ")+
  xlab('Date')+
  ylab('Counts')+
  ggtitle("Viral exchanges")
panelA

panel<-importexport %>%
  mutate(date=as.POSIXct(date)) %>%
  ggplot()+
  geom_bar(data=subset(importexport, y=Origin=='Brazil'), mapping = aes(x = Destination),width=0.6)+
  #geom_bar(data=subset(importexport, Destination=='Brazil'), mapping = aes(x = date, fill='Imports'),width=10)+
  
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.x = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8, angle=90))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_fill_manual(values=c('grey70','black','goldenrod3','darkseagreen3','darkorange3','red3','deeppink3','chartreuse3','lightpink1','gold2','orchid2'))+
  #scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(legend.position = "none")+
  labs(fill=" ")+
  xlab('Date')+
  ylab('Counts')+
  ggtitle("Viral Exports from Brazil")
panel

panel2<-importexport %>%
  mutate(date=as.POSIXct(date)) %>%
  ggplot()+
  geom_bar(data=subset(importexport, Destination=='Brazil'), mapping = aes(x = Origin),width=0.6)+
  #geom_bar(data=subset(importexport, Destination=='Brazil'), mapping = aes(x = date, fill='Imports'),width=10)+
  
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=10))+
  theme(axis.title.x = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.x = element_text(color="black", size=8, angle=90))+
  theme(axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.y = element_text(color="black", size=10))+
  scale_fill_manual(values=c('grey70','black','goldenrod3','darkseagreen3','darkorange3','red3','deeppink3','chartreuse3','lightpink1','gold2','orchid2'))+
  #scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  theme(legend.text = element_text(size=10))+
  theme(legend.title = element_text(size=14))+
  #theme(legend.position = c(0.2, 0.8))+
  theme(plot.margin = unit(c(2,2,0,0), "lines"))+
  #theme(legend.position = "none")+
  labs(fill=" ")+
  xlab('Date')+
  ylab('Counts')+
  ggtitle("Viral Imports to Brazil")
panel2

plot_grid(panel2, panel,ncol=1, rel_heights = c(3/7,4/7))


panelD

####Fig3E####
library(xlsx)
library(readxl)
library(ggplot2)

setwd("BRA_P1_P2_Import_export")

P1_data<-read_excel('BRA_Continent_P.1.xlsx')
P1_data<-subset(P1_data, Origin=='Brazil')
P2_data<-read_excel('Continent_P.2.xlsx')

P1_P2_data<-rbind(P1_data, P2_data)

P1_P2_plot<-ggplot(P1_P2_data, aes(x=Destination, fill=Lineage))+
  geom_bar(color='black')+
  theme_classic()+
  ylab('Number of exportation events')+
  scale_fill_manual(values=c('white','black'))+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))

P1_P2_plot


###Figs3A-B####

# 1. Extracting the spatio-temporal information contained in posterior trees

treefile<- "P.1/P1_phylogeoSTEXP100ML_Combined.trees"

localTreesDirectory = "data/P.1_Tree_extractions"
allTrees = scan(file=treefile, what="", sep="\n", quiet=T)
burnIn = 0
randomSampling = FALSE
nberOfTreesToSample = 100
mostRecentSamplingDatum = 2021.38 ##P1 ##2021.
coordinateAttributeName = "Location"

treeExtractions(localTreesDirectory, allTrees, burnIn, randomSampling, nberOfTreesToSample, mostRecentSamplingDatum, coordinateAttributeName)


# 2. Extracting the spatio-temporal information embedded in the MCC tree
treefile<- "P.1/P1_phylogeoSTEXP100ML_Combined.tree"

source("mccExtractions.r")
mcc_tre = readAnnotatedNexus(treefile)
mcc_tab = mccTreeExtractionJL(mcc_tre, mostRecentSamplingDatum)
write.csv(mcc_tab, "P1_cluster_MCC.csv", row.names=F, quote=F)

# 3. Estimating the HPD region for each time slice
nberOfExtractionFiles = nberOfTreesToSample
prob = 0.95; precision = 0.025
startDatum = min(mcc_tab[,"startYear"])

polygons = suppressWarnings(spreadGraphic2(localTreesDirectory, nberOfExtractionFiles, prob, startDatum, precision))


# 4.1 spatial boundaries
my_spdf <- readOGR(
  dsn= paste0("TM_WORLD_BORDERS_SIMPL-0.3") ,
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

which(my_spdf@data$NAME=="Brazil")
BRAZILBD <- my_spdf[my_spdf@data$NAME=="Brazil" , ]
plot(BRAZILBD)

borders=BRAZILBD

template_raster=BRAZILBD

# 4.2 Defining the different colour scales
minYear = 2020.893; maxYear = 2020.996
mcc_tab_this<- mcc_tab %>% data.frame()
mcc_tab_this<- mcc_tab_this %>% dplyr::filter(startYear>=minYear) %>% dplyr::filter(endYear<=maxYear)
endYears_indices = (((mcc_tab_this[,"endYear"]-minYear)/(maxYear-minYear))*100)+1
min(mcc_tab_this[,"startYear"])
max(mcc_tab_this[,"endYear"])

n_number_colours_needed<- max(round(endYears_indices))
n_repeats_discrete<- 10
c1<- rev(brewer.pal(4,"PuRd"))
c2<- (brewer.pal(9,"Blues"))
colours<- rev(rep(c(c1,c2), each=n_repeats_discrete))
colour_scale<- colorRampPalette(colours)(n_number_colours_needed)

endYears_colours = colour_scale[round(endYears_indices)]
polygons_colours = rep(NA, length(polygons))
for (i in 1:length(polygons))
{
  date = as.numeric(names(polygons[[i]]))
  polygon_index = round((((date-minYear)/(maxYear-minYear))*100)+1)
  polygons_colours[i] = paste0(colour_scale[polygon_index],"55")
}

# 5. Generating the dispersal history plot
pdf('map_P1_cluster.pdf',width=6, height=4,bg="white")

ptsize<- 0.4
pitjit<- 0.08
par(mar=c(0,0,0,0), oma=c(1.2,3.5,1,0), mgp=c(0,0.4,0), lwd=0.2, bty="o")
plot(template_raster, col="white", box=F, axes=F, colNA="grey90", legend=F)
plot(borders, add=T, lwd=0.1, border="gray10")
for (i in length(polygons):1)
{
  plot(polygons[[i]], axes=F, col=polygons_colours[i], add=T, border=NA)
}
for (i in 1:dim(mcc_tab_this)[1])
{
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2.5*1.1, lty=1, lcol="grey22", arr.col=NA, arr.pos=FALSE, curve=0.3, dr=NA, endhead=F)
  curvedarrow(cbind(mcc_tab_this[i,"startLon"],mcc_tab_this[i,"startLat"]), cbind(mcc_tab_this[i,"endLon"],mcc_tab_this[i,"endLat"]), arr.length=0,
              arr.width=0, lwd=2*1.1, lty=1, lcol=endYears_colours[i], arr.col=NA, arr.pos=FALSE, curve=0.3, dr=NA, endhead=F)
}
for (i in dim(mcc_tab_this)[1]:1)
{
  xs<- mcc_tab_this[i,"startLon"]
  ys<- mcc_tab_this[i,"startLat"]
  xe<- jitter(mcc_tab_this[i,"endLon"],pitjit)
  ye<- jitter(mcc_tab_this[i,"endLat"],pitjit)
  if (i == 1)
  {
    points(xs, ys, pch=16, col=colour_scale[1], cex=ptsize)
    points(xs, ys, pch=1, col="gray10", cex=ptsize)
  }
  points(xe, ye, pch=16, col=endYears_colours[i], cex=ptsize)
  points(xe, ye, pch=1, col="gray10", cex=ptsize)
}

xrange<- c(xmin(template_raster), xmax(template_raster))
yrange<- c(ymin(template_raster), ymax(template_raster))
rect(xrange[1], yrange[1], xrange[2], yrange[2], xpd=T, lwd=0.2)
axis(1, c(ceiling(xmin(template_raster)), floor(xmax(template_raster))), pos=ymin(template_raster), mgp=c(0,0.2,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=-0.8, tck=-0.01, col.axis="gray30")
axis(2, c(ceiling(ymin(template_raster)), floor(ymax(template_raster))), pos=xmin(template_raster), mgp=c(0,0.5,0), cex.axis=0.5, lwd=0, lwd.tick=0.2, padj=1, tck=-0.01, col.axis="gray30")
rast = raster(matrix(nrow=1, ncol=2)); rast[1] = min(mcc_tab_this[,"startYear"]); rast[2] = max(mcc_tab_this[,"endYear"])
plot(rast, legend.only=T, add=T, col=colour_scale, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.40,0.80,0.14,0.155),
     legend.args=list(text="", cex=0.7, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.6, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))

a<-dev.off()

### Figure 4 ###
tree<-read.tree('timetree.nwk')
metadata_df <- read_excel('whole_metadata.xlsx') ### All Paraguay

# transform dates
metadata_df$date<-as.Date(cut(metadata_df$date,
                              breaks = "week",
                              start.on.monday = FALSE))

metadata_df$date2<-as.Date(cut(metadata_df$date,
                               breaks = "2 week",
                               start.on.monday = FALSE))

metadata_df$date3<-as.Date(cut(metadata_df$date,
                               breaks = "1 month",
                               start.on.monday = FALSE))

p<-ggtree(tree, mrsd="2021-06-28", as.Date=TRUE,color='grey',size=0.1) %<+% metadata_df + theme_tree2() 

p2<-p+scale_colour_manual(values=c("darkseagreen3",'darkorange2','goldenrod2','bisque4','coral3')) +
  scale_fill_manual(values=c('bisque2','antiquewhite4','dodgerblue3','darkseagreen4','hotpink3','purple3','goldenrod2','grey30','darkorange2','coral4','darkgreen','cadetblue3','indianred3'), name='Paraguay Genomes',na.value="grey90")+
  geom_tippoint(aes(
    subset=(country=='Paraguay'), fill=lineage),size=5, align=F, color='black',shape=21, stroke=0.1) +
  geom_tippoint(aes(
    subset=(country=='Paraguay' & virus=="REDE"), fill=lineage),size=5, align=F, color='red3',shape=21, stroke=0.5) +
  scale_x_date(date_labels = "%B-%Y",date_breaks = "2 month") +
  theme(axis.text=element_text(size=10)) +
  expand_limits(y = 27000) +
  theme(axis.text.x = element_text(size=8.5,hjust = 1,vjust=0.5, angle=90))+
  guides(fill = guide_legend(override.aes = list(size=5)))
p2


