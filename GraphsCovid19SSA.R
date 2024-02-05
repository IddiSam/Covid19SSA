rm(list=ls())

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
getwd()

load('SubAData.rdata')
library(R0)
library(tidyverse)
library(magrittr)
library(lubridate)
library(tibble)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(rvest)
library(rlang)
library(gt)
library(deSolve)
library(EpiEstim)
library(incidence)
library(distcrete)
library(epitrix)
library(projections)
library(dplyr)
library(earlyR)
library(projections)
library(gridExtra) # multiple grid-based plots on a page
library(ggforce) # accelerating ggplot2
library(kableExtra) # complex tables
library(leaflet) # map
library(remotes)

confirmed_color <- "blue"
active_color <- "purple" # #1f77b4"
recovered_color <- "green"
death_color <- "red"

SubAData <- SubAData %>%
  filter(Date >= "2020-03-14" & Date <= "2020-05-10")

startDate<-SubAData %>%
  ungroup() %>%
  filter(Cases >0) %>% 
  arrange(Date) %>% 
  slice(1)
startDate<-startDate$Date
SubAData$Type[SubAData$Type=="Unrecovered"]<-"Active"
SubAData <- SubAData %>%
  filter(Date >= startDate)


#### Graphics #################
### Bar plot of total confirmed cases
TotCases<-SubAData %>%
  group_by(Country.Region, Type) %>%
  summarise(Cases=sum(Cases))%>%
  filter(Type=="Confirmed")

ggplot(TotCases, aes(y=Cases, x=Country.Region, fill= Country.Region)) + 
  geom_bar(position=position_stack(reverse = F), stat="identity")+
  theme_bw() +theme(axis.text.x = element_text(angle = 90),legend.position="none")+
  xlab("Country")+ylab("Total cases")+
  labs(fill="")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = Cases), vjust = 0)


########## Cumulative cases #########
CumAllCountry <-SubAData %>%
  arrange(Date, Type) %>%
  group_by(Date, Type) %>%
  summarise(Cases_cum=sum(Cases_cum)) %>%
  filter(Type=="Confirmed")
CumAllCountry<-as.data.frame(CumAllCountry)  

ggplot(CumAllCountry,aes(x=Date, y=Cases_cum)) +
  geom_line(aes(color=Type), size=1.5, color=confirmed_color) +
  theme_bw() +theme(axis.text.x = element_text(angle = 90))+
  xlab("Date")+ylab("Cumulative cases")+theme(legend.title=element_blank())+  
  theme(plot.title = element_text(hjust = 0.5)) 


############### Top 10 Countries #################
TotCases<-SubAData %>%
  group_by(Country.Region, Type) %>%
  summarise(Cases=sum(Cases))%>%
  filter(Type=="Confirmed")
Top10<-TotCases%>%
  arrange(-Cases)
Top10<-Top10$Country.Region[1:10]
cumActive<-SubAData[SubAData$Type %in% "Active",] %>%
  rename(Country="Country.Region")

ggplot(cumActive[cumActive$Country %in% Top10,],aes(x=Date, y=Cases_cum, group=Country)) +
  geom_line(aes(color=Country, linetype=Country), size=1) +
  theme_bw() +theme(axis.text.x = element_text(angle = 90),legend.position="right")+
  xlab("Date")+ylab("Cumulative cases") +
  theme(plot.title = element_text(hjust = 0.5)) 


