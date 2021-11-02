require(tidyverse)
require(ggplot2)
require(EpiWeek)
require(scales)
library(ggnewscale)
require(readxl)
require(wesanderson)
require(lubridate)


##Figure_1
data2<-read_excel('AllGenomes.xlsx')
data2$date<-as.Date(data2$date)
data2$days<-as.Date(cut(data2$date, breaks = "day", start.on.monday = FALSE))
data2$date<-as.Date(cut(data2$date, breaks = "week", start.on.monday = FALSE))
data2<- data2 %>% mutate(Collection_date=as.POSIXct(days))
data2<- subset(data2, !is.na(Strain))


incidence<- read.csv("cases-brazil-states.csv",header=T)
incidence$date<- as.Date(incidence$date, format="%Y-%m-%d")
incidence<- incidence %>% dplyr::select("date","newCases","newDeaths")
incidence<- plyr::ddply(incidence, plyr::.(date), summarize, cases=sum(newCases), deaths=sum(newDeaths))


incidence <- incidence %>% 
  dplyr::mutate(cases_7day = zoo::rollmean(cases, k = 7, fill = NA),
                deaths_7day = zoo::rollmean(deaths, k = 7, fill = NA)) %>% 
                  
  dplyr::ungroup()

g<- ggplot(data=tmp) +  ggstyleTILE +
  geom_hline(yintercept=0, color='black', size=0.3) +
  geom_hline(yintercept=1, color="#009176", linetype=2) +
  geom_segment(data=phases, aes(x=start, y=y, xend=end, yend=y, col=lab), size=5) 
  scale_color_manual("",values=collevels, labels=phases$lab)  +
  new_scale_color()+
  geom_bar(aes(x=date, y=cases), stat ="identity", size=0.6, color="grey44") +
  geom_line(aes(x=date, y=deaths*f), size=0.6, color="red") +
  scale_y_continuous(sec.axis = sec_axis(~./f, name = "deaths")) +
  ggtitle("SARS-CoV-2 epidemic in Brazil") + ylab("cases") + xlab("month") +
  theme(legend.position=c(0.65,0.7),legend.text= element_text(size=12, family="Helvetica")) +
  scale_shape_manual("", values='_', labels="Level 5") +
  scale_x_date(labels = date_format("%b-%y"), date_breaks = "1 month") +
  geom_rug(data= data2, aes(x=days),color='dodgerblue3',alpha=0.2,outside =FALSE,length = unit(0.03, "npc"), show.legend=TRUE)
g


###PanelC####
pacman::p_load(ribge, geobr, sf, ggplot2, dplyr, scales, ggthemes)

brazil <- read.csv("cases-brazil-states.csv", header = TRUE, sep = ",")
munArea <- area_municipios()
pop <- populacao_municipios(2021)

munArea <- subset(munArea, munArea$CD_GCMUN != "4300001" & munArea$CD_GCMUN != "4300002")

munArea$region <- NA
munArea$code_region <- NA
munArea$areaRegion <- NA
munArea$region[which(munArea$CD_GCUF >= 11 & munArea$CD_GCUF <= 17)] <- "North"
munArea$region[which(munArea$CD_GCUF >= 21 & munArea$CD_GCUF <= 29)] <- "Northeast"
munArea$region[which(munArea$CD_GCUF >= 31 & munArea$CD_GCUF <= 35)] <- "Southeast"
munArea$region[which(munArea$CD_GCUF >= 41 & munArea$CD_GCUF <= 43)] <- "South"
munArea$region[which(munArea$CD_GCUF >= 50 & munArea$CD_GCUF <= 53)] <- "Midwest"
munArea$code_region[which(munArea$CD_GCUF >= 11 & munArea$CD_GCUF <= 17)] <- 1
munArea$code_region[which(munArea$CD_GCUF >= 21 & munArea$CD_GCUF <= 29)] <- 2
munArea$code_region[which(munArea$CD_GCUF >= 31 & munArea$CD_GCUF <= 35)] <- 3
munArea$code_region[which(munArea$CD_GCUF >= 41 & munArea$CD_GCUF <= 43)] <- 4
munArea$code_region[which(munArea$CD_GCUF >= 50 & munArea$CD_GCUF <= 53)] <- 5
munArea$areaRegion[which(munArea$code_region == 1)] <- sum(munArea$AR_MUN_2020[which(munArea$code_region == 1)])
munArea$areaRegion[which(munArea$code_region == 2)] <- sum(munArea$AR_MUN_2020[which(munArea$code_region == 2)])
munArea$areaRegion[which(munArea$code_region == 3)] <- sum(munArea$AR_MUN_2020[which(munArea$code_region == 3)])
munArea$areaRegion[which(munArea$code_region == 4)] <- sum(munArea$AR_MUN_2020[which(munArea$code_region == 4)])
munArea$areaRegion[which(munArea$code_region == 5)] <- sum(munArea$AR_MUN_2020[which(munArea$code_region == 5)])

pop$region <- NA
pop$code_region <- NA
pop$popRegion <- NA
pop$code_region[which(pop$codigo_uf >= 11 & pop$codigo_uf <= 17)] <- 1
pop$code_region[which(pop$codigo_uf >= 21 & pop$codigo_uf <= 29)] <- 2
pop$code_region[which(pop$codigo_uf >= 31 & pop$codigo_uf <= 35)] <- 3
pop$code_region[which(pop$codigo_uf >= 41 & pop$codigo_uf <= 43)] <- 4
pop$code_region[which(pop$codigo_uf >= 50 & pop$codigo_uf <= 53)] <- 5
pop$region[which(pop$code_region == 1)] <- "North"
pop$region[which(pop$code_region == 2)] <- "Northeast"
pop$region[which(pop$code_region == 3)] <- "Southeast"
pop$region[which(pop$code_region == 4)] <- "South"
pop$region[which(pop$code_region == 5)] <- "Midwest"
pop$popRegion[which(pop$code_region == 1)] <- sum(pop$populacao[which(pop$code_region == 1)])
pop$popRegion[which(pop$code_region == 2)] <- sum(pop$populacao[which(pop$code_region == 2)])
pop$popRegion[which(pop$code_region == 3)] <- sum(pop$populacao[which(pop$code_region == 3)])
pop$popRegion[which(pop$code_region == 4)] <- sum(pop$populacao[which(pop$code_region == 4)])
pop$popRegion[which(pop$code_region == 5)] <- sum(pop$populacao[which(pop$code_region == 5)])

brazil <- subset(brazil, brazil$ibgeID > 60)
brazil$codigo_uf <- NA
nomeUfs <- names(table(pop$uf))
for (i in 1:length(nomeUfs)){
  brazil$codigo_uf[which(brazil$state == nomeUfs[i])] <- as.numeric(names(table(pop$codigo_uf[pop$uf == nomeUfs[i]])))
}

brazil$region <- NA
brazil$code_region <- NA
brazil$totalCasesRegion <- NA
brazil$code_region[which(brazil$codigo_uf >= 11 & brazil$codigo_uf <= 17)] <- 1
brazil$code_region[which(brazil$codigo_uf >= 21 & brazil$codigo_uf <= 29)] <- 2
brazil$code_region[which(brazil$codigo_uf >= 31 & brazil$codigo_uf <= 35)] <- 3
brazil$code_region[which(brazil$codigo_uf >= 41 & brazil$codigo_uf <= 43)] <- 4
brazil$code_region[which(brazil$codigo_uf >= 50 & brazil$codigo_uf <= 53)] <- 5
brazil$region[which(brazil$code_region == 1)] <- "North"
brazil$region[which(brazil$code_region == 2)] <- "Northeast"
brazil$region[which(brazil$code_region == 3)] <- "Southeast"
brazil$region[which(brazil$code_region == 4)] <- "South"
brazil$region[which(brazil$code_region == 5)] <- "Midwest"
brazil$totalCasesRegion[which(brazil$code_region == 1)] <- sum(brazil$totalCases[which(brazil$code_region == 1)])
brazil$totalCasesRegion[which(brazil$code_region == 2)] <- sum(brazil$totalCases[which(brazil$code_region == 2)])
brazil$totalCasesRegion[which(brazil$code_region == 3)] <- sum(brazil$totalCases[which(brazil$code_region == 3)])
brazil$totalCasesRegion[which(brazil$code_region == 4)] <- sum(brazil$totalCases[which(brazil$code_region == 4)])
brazil$totalCasesRegion[which(brazil$code_region == 5)] <- sum(brazil$totalCases[which(brazil$code_region == 5)])

munArea <- munArea[, c("CD_GCUF","NM_UF_SIGLA", "CD_GCMUN", "AR_MUN_2020", "region", "code_region", "areaRegion")]
names(munArea) <- c("code_state", "abbrev_state", "code_muni", "area_muni", "region", "code_region", "area_region")
munArea$code_state <- as.numeric(munArea$code_state)
munArea$code_muni <- as.numeric(munArea$code_muni)

pop <- pop[, c("codigo_uf", "uf", "cod_municipio", "populacao", "region", "code_region", "popRegion")]
names(pop) <- c("code_state", "abbrev_state", "code_muni", "populacao", "region", "code_region", "pop_region")
pop$code_state <- as.numeric(pop$code_state)
pop$code_muni <- as.numeric(pop$code_muni)

brazil <- brazil[, c("codigo_uf", "state", "ibgeID", "totalCases", "totalCases_per_100k_inhabitants", "region", "code_region", "totalCasesRegion")]
names(brazil) <- c("code_state", "abbrev_state", "code_muni", "total_cases", "totalCases_per_100k_inhabitants", "region", "code_region", "total_cases_region")
brazil$code_state <- as.numeric(brazil$code_state)
brazil$code_muni <- as.numeric(brazil$code_muni)

myData <- left_join(munArea, pop, by=c("code_muni", "code_state", "abbrev_state", "region", "code_region"))
myData <- left_join(myData, brazil, by=c("code_muni", "code_state", "abbrev_state", "region", "code_region"))

myData$density_muni <- myData$totalCases_per_100k_inhabitants / myData$area_muni
myData$totalCases_per_100k_inhabitants_region <- (myData$total_cases_region / myData$pop_region) * 100000
myData$density_region <- myData$totalCases_per_100k_inhabitants_region / myData$area_region

myData$pop_state <- NA
myData$area_state <- NA
myData$total_cases_state <- NA
myUfs <- names(table(myData$code_state))

for (i in 1:length(myUfs)){
  myData$pop_state[which(myData$code_state == myUfs[i])] <- sum(myData$populacao[which(myData$code_state == myUfs[i])])
  myData$area_state[which(myData$code_state == myUfs[i])] <- sum(myData$area_muni[which(myData$code_state == myUfs[i])])
  myData$total_cases_state[which(myData$code_state == myUfs[i])] <- sum(myData$total_cases[which(myData$code_state == myUfs[i])])
  
}

myData$totalCases_per_100k_inhabitants_state <- (myData$total_cases_state / myData$pop_state) * 100000
myData$density_state <- myData$totalCases_per_100k_inhabitants_state / myData$area_state


region <- read_region(year=2020)
states <- read_state(year=2020)
muni <- read_municipality(year=2020)
region$density <- NA
region$totalCases_per_100k_inhabitants <- NA
region$population <- NA
region$total_cases <- NA
codRegion <- names(table(myData$code_region))
for(i in 1:length(codRegion)){
  region$density[which(region$code_region == codRegion[i])] <- as.numeric(names(table(myData$density_region[myData$code_region == codRegion[i]])))
  region$totalCases_per_100k_inhabitants[which(region$code_region == codRegion[i])] <- as.numeric(names(table(myData$totalCases_per_100k_inhabitants_region[myData$code_region == codRegion[i]])))
  region$total_cases[which(region$code_region == codRegion[i])] <- as.numeric(names(table(myData$total_cases_region[myData$code_region == codRegion[i]])))
  region$population[which(region$code_region == codRegion[i])] <- as.numeric(names(table(myData$pop_region[myData$code_region == codRegion[i]])))
}

states$density <- NA
states$totalCases_per_100k_inhabitants <- NA
states$population <- NA
states$total_cases <- NA
codState <- names(table(myData$code_state))
for(i in 1:length(codState)){
  states$density[which(states$code_state == codState[i])] <- as.numeric(names(table(myData$density_state[myData$code_state == codState[i]])))
  states$totalCases_per_100k_inhabitants[which(states$code_state == codState[i])] <- as.numeric(names(table(myData$totalCases_per_100k_inhabitants_state[myData$code_state == codState[i]])))
  states$total_cases[which(states$code_state == codState[i])] <- as.numeric(names(table(myData$total_cases_state[myData$code_state == codState[i]])))
  states$population[which(states$code_state == codState[i])] <- as.numeric(names(table(myData$pop_state[myData$code_state == codState[i]])))
}

muni <- read_municipality(year=2020)
muni <- dplyr::left_join(muni, myData, by = c("code_muni"))


muni<- muni %>% select("geom","total_cases","populacao","totalCases_per_100k_inhabitants")
muni$manual_incidence_100k<- 100000* muni$total_cases/muni$populacao
muni$manual_proportion<- muni$total_cases/muni$populacao

cols<- RColorBrewer::brewer.pal(n=11, name = "Spectral")

p_data <- ggplot() +
  geom_sf(data=muni, aes(fill=log10(totalCases_per_100k_inhabitants)), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_gradientn("mean", colors=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

p_manual <- ggplot() +
  geom_sf(data=muni, aes(fill=log10(manual_incidence_100k)), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_gradientn("mean", colors=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

##estes mapas mostram que a incidencia do CSV esta de facto certa
cowplot::plot_grid(p_data, p_manual, ncol=2)

###################################################################################################
##############

muni_fig<- muni
# brr<- c(0,1,2,3,4,5,6)
brr<- seq(0,6,length.out=10)
muni_fig$manual_incidence_100k<- cut(log10(muni_fig$manual_incidence_100k), breaks=brr, include.lowest = T)
cols<- RColorBrewer::brewer.pal(n=length(unique(muni_fig$manual_incidence_100k)), name = "Reds")
p_manual1 <- ggplot() +
  geom_sf(data=muni_fig, aes(fill=manual_incidence_100k), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_manual("mean", values=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
p_manual1

muni_fig<- muni
brr<- c(0,5000,10000,30000,64000)
muni_fig$manual_incidence_100k<- cut((muni_fig$manual_incidence_100k), breaks=brr, include.lowest = T)
cols<- RColorBrewer::brewer.pal(n=length(levels(muni_fig$manual_incidence_100k)), name = "Reds")
p_manual2 <- ggplot() +
  geom_sf(data=muni_fig, aes(fill=manual_incidence_100k), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_manual("mean", values=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
p_manual2

cols<- RColorBrewer::brewer.pal(n=11, name = "Reds")
p_manual3 <- ggplot() +
  geom_sf(data=muni, aes(fill=(manual_proportion)), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_gradientn("mean", colors=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
p_manual3

cowplot::plot_grid(p_manual1, p_manual2, p_manual3, ncol=3)

#################################################################################
#################################################################################

states_manual<- states %>% select("geom","total_cases","population")
states_manual$manual_incidence_100k<- 100000* states_manual$total_cases/states_manual$population
states_manual$manual_proportion<- states_manual$total_cases/states_manual$population

range(states_manual$manual_incidence_100k)
range(states_manual$manual_proportion)

cols<- RColorBrewer::brewer.pal(n=11, name = "Reds")

p_manual_a <- ggplot() +
  geom_sf(data=states_manual, aes(fill=(total_cases)/1000000), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_gradientn("Total cases\n(millions)", colors=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

p_manual_b <- ggplot() +
  geom_sf(data=states_manual, aes(fill=(population/1000000)), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_gradientn("population\n(millions)", colors=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

p_manual_c <- ggplot() +
  geom_sf(data=states_manual, aes(fill=manual_incidence_100k), color=NA, size=.15) +
  geom_sf(data = states,fill = "transparent", colour = "black", size = 0.2) + scale_fill_gradientn("Incidence\n(per 100k)", colors=cols) +
  theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())



