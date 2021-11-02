if (!require("ggplot2")){
    install.packages("ggplot2")
}
library("ggplot2") #load the library
#remotes::install_github("wilkelab/cowplot")
library("cowplot")
theme_set(theme_cowplot())
install.packages("cowplot")

library(cowplot)
theme_set(theme_cowplot())

if (! require("lubridate")){
  install.packages("lubridate")
  library(lubridate)
}

#Converting decimal year to date format
library(lubridate)


myFileData <- "/Users/martagiovanetti/Desktop/data_country.txt"
if (!(file.exists(myFileData))){
  stop("Error:directory does not exist, please check if the path is correct.")
}
#DATA
data = read.table(myFileData, header=T) # read from the file

#GGPLOT
ggplot(data, aes(date, distance, location))+
geom_point(aes(fill = location, shape=location), size = 4)+
scale_shape_manual(values = c(21, 21, 21, 21, 21, 21, 21, 21,21, 21))+
geom_smooth(method=lm,se=T, colour="black")+
#theme_bw()+
ylab("Root-to-tip-distances")+
xlab("Time (years)")+
ggtitle("VOC/VOI")+
annotate("text", fontface =2,x=2020.05, y=0.004, label="cofficient correletion=0.92", col='tomato3')+
annotate("text", fontface =2,x=2020.05, y=0.003, label="r2=0.84", col='tomato3')
  

