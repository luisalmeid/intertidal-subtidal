#### SETUP ####
rm(list=ls())
setwd("C:/Users/luis_/Desktop/paper/code/csv")
getwd()
RStudio.Version()

### packages ###

library(dplyr)
library(ggplot2)

### load and prepare data ### 
intsub <- read.csv("intsub.csv")
noaa<- read.csv("noaa.csv")

temperature <- intsub %>% group_by(date,time,Source) %>% 
  summarise(mean_temp=mean(temp, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()

str(temperature)
temperature$date <- as.Date(temperature$date)

ggplot(data=temperature, aes(x=date,y=mean_temp,group=Source))+
  geom_line(aes(color=Source))+
  scale_color_manual(values=c("#FF0000", "#0000FF"))+
  theme_light()+
  xlab("Date") + ylab("Sea Surface Temperature (ºC)")
                
temp <- temperature[,-2]

tem <- temp %>% group_by(date,Source) %>% 
  summarise(mean_temp=mean(mean_temp, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()       

noaa2 <- noaa %>% group_by(date,Source) %>% 
  summarise(mean_temp=mean(temp, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()

noaa2$date <- as.Date(noaa2$date)
str(noaa2)

full<-rbind(tem,noaa2)

ggplot(data=full, aes(x=date,y=mean_temp,group=Source))+
  geom_line(aes(color=Source))+
  scale_color_manual(values=c("#FF0000","#000000","#0000FF" ))+
  theme_light()+
  xlab("Month") + ylab("Sea Surface Temperature (ºC)")
