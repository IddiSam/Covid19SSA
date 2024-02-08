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


#####Generating Plots For Time Varying Reproduction Number Plots for Top 6 windows
#at the onset of the pandemic##########
#####Apply same codes to generate the time varying plot
#for the entire SSA
#####################################
######################################################################
#####################Ghana###################################################
##Extracting Ghana Data#########################################################################
Ghana<- subset(SubAData, Country.Region == "Ghana", select = c("Country.Region","Date","Type","Cases","Cases_cum"))
Ghana1<-subset(Ghana,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
######Filtering Ghana Data to have only Confirmed Cases####################################################
Ghana11<-subset(Ghana1,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
table(Ghana11$Type)
Ghana11=as.data.frame(Ghana11)
####Estimating the effective reproduction number in R using the Parametric Serial Interval###########################
Ghanauoi=Ghana11[,c("Date","Cases")]
names(Ghanauoi)[names(Ghanauoi) == "Cases"] <- "I"
Ghanauyu=Ghanauoi%>% filter(Date > "2020-03-13")

G <- nrow(Ghanauyu)
t_start <- seq(2, G-20) 
t_end <- t_start + 20 
Ghana_res_parametric_si <- estimate_R(Ghanauyu, 
                                      method="parametric_si",
                                      config = make_config(list(
                                        mean_si = 7.5, 
                                        std_si = 3.4,
                                        t_start=t_start,
                                        t_end=t_end))
)
plot(Ghana_res_parametric_si, what = c("R"),options_R= list(col ="blue",xlab = "Time", ylab = "R"),
     options_SI=list(prob_min = 0.001, col = "blue"),options_I = list(col ="blue"), legend = T)



#########################Cameroon######################################################
##Extracting Cameroon Data#########################################################################
Cameroon<- subset(SubAData, Country.Region == "Cameroon", select = c("Country.Region","Date","Type","Cases","Cases_cum"))
Cameroon1<-subset(Cameroon,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
######Filtering Cameroon Data to have only Confirmed Cases####################################################
Cameroon11<-subset(Cameroon1,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
table(Cameroon11$Type)
Cameroon11=as.data.frame(Cameroon11)
####Estimating the effective reproduction number in R using the Parametric Serial Interval###########################
Cameroonuoi=Cameroon11[,c("Date","Cases")]
names(Cameroonuoi)[names(Cameroonuoi) == "Cases"] <- "I"
Cameroonuyu=Cameroonuoi%>% filter(Date > "2020-03-05")
C <- nrow(Cameroonuyu)
t_start <- seq(2, C-20) 
t_end <- t_start + 20 

Cameroon_res_parametric_si <- estimate_R(Cameroonuyu, 
                                         method="parametric_si",
                                         config = make_config(list(
                                           mean_si = 7.5, 
                                           std_si = 3.4, t_start=t_start,
                                           t_end=t_end))
)
plot(Cameroon_res_parametric_si, what = c("R"),options_R= list(col ="blue",xlab = "Time", ylab = "R"),
     options_SI=list(prob_min = 0.001, col = "blue"),options_I = list(col ="blue"), legend = T)

#########################South Africa######################################################
##Extracting South African Data#########################################################################
SA<- subset(SubAData, Country.Region == "S.Africa", select = c("Country.Region","Date","Type","Cases","Cases_cum"))
SA1<-subset(SA,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
######Filtering South African  Data to have only Confirmed Cases####################################################
SA11<-subset(SA1,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
table(SA11$Type)
SA11=as.data.frame(SA11)
####Estimating the effective reproduction number in R using the Parametric Serial Interval###########################
SAuoi=SA11[,c("Date","Cases")]
names(SAuoi)[names(SAuoi) == "Cases"] <- "I"
SAuyu=SAuoi%>% filter(Date > "2020-03-04")
S <- nrow(SAuyu)
t_start <- seq(2,S-20) 
t_end <- t_start + 20 

SA_res_parametric_si <- estimate_R(SAuyu, 
                                   method="parametric_si",
                                   config = make_config(list(
                                     mean_si = 7.5, 
                                     std_si = 3.4, t_start=t_start,
                                     t_end=t_end))
)
plot(SA_res_parametric_si, what = c("R"),options_R= list(col ="blue",xlab = "Time", ylab = "R"),
     options_SI=list(prob_min = 0.001, col = "blue"),options_I = list(col ="blue"), legend = T)

#########################Nigeria######################################################
##Extracting Nigeria Data#########################################################################
Nigeria<- subset(SubAData, Country.Region == "Nigeria", select = c("Country.Region","Date","Type","Cases","Cases_cum"))
Nigeria1<-subset(Nigeria,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
######Filtering Nigeria Data to have only Confirmed Cases####################################################
Nigeria11<-subset(Nigeria1,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
table(Nigeria11$Type)
Nigeria11=as.data.frame(Nigeria11)
####Estimating the effective reproduction number in R using the Parametric Serial Interval###########################
Nigeriauoi=Nigeria11[,c("Date","Cases")]
names(Nigeriauoi)[names(Nigeriauoi) == "Cases"] <- "I"
Nigeriauyu=Nigeriauoi%>% filter(Date > "2020-03-04")
N <- nrow(Nigeriauyu)
t_start <- seq(2,N-20) 
t_end <- t_start + 20 

Nigeria_res_parametric_si <- estimate_R(Nigeriauyu, 
                                        method="parametric_si",
                                        config = make_config(list(
                                          mean_si = 7.5, 
                                          std_si = 3.4, t_start=t_start,
                                          t_end=t_end))
)
plot(Nigeria_res_parametric_si, what = c("R"),options_R= list(col ="blue",xlab = "Time", ylab = "R"),
     options_SI=list(prob_min = 0.001, col = "blue"),options_I = list(col ="blue"), legend = T)


#########################Guinea######################################################
##Extracting Guinea Data#########################################################################
Guinea<- subset(SubAData, Country.Region == "Guinea", select = c("Country.Region","Date","Type","Cases","Cases_cum"))
Guinea1<-subset(Guinea,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
######Filtering Guinea Data to have only Confirmed Cases####################################################
Guinea11<-subset(Guinea1,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
table(Guinea11$Type)
Guinea11=as.data.frame(Guinea11)
####Estimating the effective reproduction number in R using the Parametric Serial Interval###########################
Guineauoi=Guinea11[,c("Date","Cases")]
names(Guineauoi)[names(Guineauoi) == "Cases"] <- "I"
Guineauyu=Guineauoi%>% filter(Date > "2020-03-12")
GU <- nrow(Guineauyu)
t_start <- seq(2,GU-20) 
t_end <- t_start + 20 

Guinea_res_parametric_si <- estimate_R(Guineauyu, 
                                       method="parametric_si",
                                       config = make_config(list(
                                         mean_si = 7.5, 
                                         std_si = 3.4, t_start=t_start,
                                         t_end=t_end))
)
plot(Guinea_res_parametric_si, what = c("R"),options_R= list(col ="blue",xlab = "Time", ylab = "R"),
     options_SI=list(prob_min = 0.001, col = "blue"),options_I = list(col ="blue"), legend = T)


#########################Ivory Coast######################################################
##Extracting Ivory Coast Data#########################################################################
IVC<- subset(SubAData, Country.Region == "Ivory.Coast", select = c("Country.Region","Date","Type","Cases","Cases_cum"))
IVC1<-subset(IVC,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
######Filtering Ivory Coast Data to have only Confirmed Cases####################################################
IVC11<-subset(IVC1,Type=="Confirmed",  select = c("Country.Region","Date","Type","Cases","Cases_cum"))
table(IVC11$Type)
IVC11=as.data.frame(IVC11)

####Estimating the effective reproduction number in R using the Parametric Serial Interval###########################
IVCuoi=IVC11[,c("Date","Cases")]
names(IVCuoi)[names(IVCuoi) == "Cases"] <- "I"
IVCuyu=IVCuoi%>% filter(Date > "2020-03-10")
IV <- nrow(IVCuyu)
t_start <- seq(2,IV-20) 
t_end <- t_start + 20 

IVC_res_parametric_si <- estimate_R(IVCuyu, 
                                    method="parametric_si",
                                    config = make_config(list(
                                      mean_si = 7.5, 
                                      std_si = 3.4, t_start=t_start,
                                      t_end=t_end))
)
plot(IVC_res_parametric_si, what = c("R"),options_R= list(col ="blue",xlab = "Time", ylab = "R"),
     options_SI=list(prob_min = 0.001, col = "blue"),options_I = list(col ="blue"), legend = T)