#### SETUP ####
rm(list=ls())
setwd("C:/Users/luis_/Desktop/paper/code/csv")
getwd()
RStudio.Version()

### LOAD PACKAGES ### 

library(dplyr)
library(ggplot2)
library(nlme)
library(PerformanceAnalytics)
library(car)

#### 5-DAY THERMAL STRESS RESPONSE #### 

## load and explore data
bleach <- read.csv("bleaching.csv") #raw data

table(bleach$tree)
table(bleach$coral)
table(bleach$origin)
table(bleach$culture)
table(bleach$treatment)

hist(bleach$brightness)
skewness(bleach$brightness) 
boxplot(brightness ~ treatment, data = bleach)

# transform with ln and sqrt
bleach$ln=log(bleach$brightness)
bleach$sqrt=sqrt(bleach$brightness)

par(mfrow = c(1, 2))
hist(bleach$ln)
hist(bleach$sqrt)
skewness(bleach$ln) 
skewness(bleach$sqrt) #ln transformations with results closer to normality 

# create mean table 
bleach_mean <- bleach %>% group_by(tree,day,treatment) %>% 
  summarise(mean_brightness=mean(ln, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()

bleach_mean$treatment <- as.factor(bleach_mean$treatment)
bleach_mean$day <- as.factor(bleach_mean$day)

#bleach_mean$day <- as.numeric(bleach_mean$day)

str(bleach_mean)

lmm2 <- lme(mean_brightness~treatment*day, random = ~1|tree/day, data=bleach_mean, na.action=na.exclude)
anova(lmm2)
par(mfrow = c(1, 2))
plot(residuals(lmm2) ~ fitted(lmm2), main = "Residuals vs. Fitted")
qqnorm(residuals(lmm2))


#### BRIGHTNESS DAY 1 ####

day1<-subset(bleach,day=='1' ) #remove rows where day is not equal to 1

day1_mean <- day1 %>% group_by(tree,treatment) %>% 
  summarise(mean_brightness=mean(ln, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()


str(day1_mean)
day1_mean$treatment <- as.factor(day1_mean$treatment)

shapiro.test(day1_mean$mean_brightness)
leveneTest(day1_mean$mean_brightness~treatment,data=day1_mean)

aov<-aov(mean_brightness~treatment, data=day1_mean)
summary(aov)
TukeyHSD(x=aov,"treatment",conf.level = 0.95)

#### CONTROL ####

###load data
ctrl <- read.csv("ctrl.csv")

table(ctrl$coral)
table(ctrl$origin)
table(ctrl$tree)

hist(ctrl$brightness)
skewness(ctrl$brightness) 
boxplot(brightness ~ day, data = ctrl)

ctrl$origin <- as.factor(ctrl$origin)
ctrl$day <- as.factor(ctrl$day)

lmm3<- lme(brightness~origin*day, random = ~1|coral/day, data=ctrl, na.action=na.exclude)
anova(lmm3)

par(mfrow = c(1, 2))
plot(residuals(lmm3) ~ fitted(lmm3), main = "Residuals vs. Fitted")
qqnorm(residuals(lmm3))

#### PLOT DATA ####

## Setting up ##
## 5 - day thermal stress ##
p <- bleach %>% group_by(tree,day,treatment) %>% 
  summarise(mean_brightness=mean(brightness,na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()

## bar graphs day1 ##
d <- day1 %>% group_by(tree,day,treatment) %>% 
  summarise(mean_brightness=mean(brightness,na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()

library(Rmisc)

dd<-summarySE(d, measurevar="mean_brightness",groupvars=c("treatment","day"))
df <- c("a","b","a","a")
sig<-data.frame(df)
dd1<-bind_cols(dd,sig)


#plots

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

## 5 - day thermal stress ##
pp <- data_summary(p, varname="mean_brightness", 
                          groupnames=c("treatment", "day"))

ggplot(pp, aes(x = day, y = mean_brightness, color=treatment)) +  
  geom_point() +theme_classic()+coord_cartesian(ylim=c(0,150))+ stat_smooth(method="lm", se=F)+
  labs(y= "Brightness (bpp)", x = "Day")+guides(fill=FALSE)+ scale_color_brewer(palette = "RdBu")
  #geom_errorbar(aes(ymin=mean_brightness-sd, ymax=mean_brightness+sd), width=.2, position=position_dodge(0.05))

## bar graphs day 1 ##
ggplot(dd1, aes(x=treatment, y=mean_brightness, fill=treatment)) + 
  theme_classic()+ labs(y= "Brightness (bpp)", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,150))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=mean_brightness-se, ymax=mean_brightness+se), width=.15,
                position=position_dodge(.9))+
  geom_text(aes(label = df, y = mean_brightness + se), vjust = -0.5)


## control line graphs ##
ctrl.avg <- data_summary(ctrl, varname="brightness", 
                         groupnames=c("origin", "day"))
ggplot(ctrl.avg, aes(x=day, y=brightness, group=origin, color=origin)) + 
  geom_point() +theme_classic()+coord_cartesian(ylim=c(0,150))+ stat_smooth(method="lm", se=F)+
  labs(y= "Brightness (bpp)", x = "Day")+guides(fill=FALSE)+ scale_color_brewer(palette = "Set1")

