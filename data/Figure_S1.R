###S1A####
list.files()

library(ggplot2) #need to install 
library(cowplot)
theme_set(theme_cowplot())

ct_cov = read.csv("AllGenomes.txt", header=TRUE, row.names=1,sep = "\t")
colnames(ct_cov)
ct_cov$MeanCT<- as.numeric(ct_cov$MeanCT)
ct_cov$Coverage<- as.numeric(ct_cov$Coverage)

ggplot(ct_cov, aes(x = MeanCT, y = Coverage, fill= Country)) +
  geom_point(shape=21,size = 3) +
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text.x = element_text(size = 8, hjust=0.5, vjust=0.5, colour = "black"),
        axis.text.y = element_text(size = 8, hjust=0.5, vjust=0.5, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("ct vs coverage")

###S1B####

metadata<- read_excel('Brazil_metadata_lineage.xlsx')
data2<-subset(metadata,country=='Brazil')
colnames(data2)
col_we_need <- c("strain", "division", "date")
strain = data2[,colnames(data2) %in% col_we_need]
levels(strain$strain)
levels(strain$division)
levels((strain$date))

strain$date = as.numeric(strain$date)

library(dplyr) #need to be install 
genome_need_count = strain %>% 
  group_by(date, division) %>% 
  count %>% 
  ungroup

ggplot(genome_need_count, aes(x = date, y= n, fill= division)) +
  geom_bar(stat = "identity", position = "stack", alpha=1,width = 0.5)+
  scale_fill_brewer() +
  theme(axis.text.x = element_text(size = 8, hjust=0.5, vjust=0.5, colour = "black"),
        axis.text.y = element_text(size = 8, hjust=0.5, vjust=0.5, colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  ylab("sample number")+
  # ylim(0,18)
  ggtitle("")
ggsave("", width = 6, height = 4)

