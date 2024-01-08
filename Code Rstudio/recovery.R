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

#### 13-DAY RECOVERY PERIOD #### 

## load and explore data
recovery <- read.csv("recovery.csv") #raw data

table(recovery$tree)
table(recovery$coral)
table(recovery$origin)
str(recovery)

recovery$brightness <- as.numeric(recovery$brightness)

hist(recovery$brightness)
skewness(recovery$brightness) 
boxplot(brightness ~ origin, data = recovery)

# transform with ln and sqrt
recovery$ln=log(recovery$brightness)
recovery$sqrt=sqrt(recovery$brightness)

par(mfrow = c(1, 2))
hist(recovery$ln)
hist(recovery$sqrt)
skewness(recovery$ln) 
skewness(recovery$sqrt) #ln transformations with results closer to normality 

# create mean table 
recovery_mean <- recovery %>% group_by(tree,day,origin) %>% 
  summarise(mean_brightness=mean(ln, na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()

recovery_mean$origin <- as.factor(recovery_mean$origin)
recovery_mean$day <- as.factor(recovery_mean$day)

str(recovery_mean)

## linear mixed model 
lmm <- lme(mean_brightness~origin*day, random = ~1|tree/day, data=recovery_mean, na.action=na.exclude)

anova(lmm)
par(mfrow = c(1, 2))
plot(residuals(lmm) ~ fitted(lmm), main = "Residuals vs. Fitted")
qqnorm(residuals(lmm))

#### PLOT DATA ####

## 5 - day thermal stress ##
r <- recovery %>% group_by(tree,day,origin) %>% 
  summarise(mean_brightness=mean(brightness,na.rm = TRUE),
            .groups = 'drop') %>%
  as.data.frame()

## Line graphs 5 days
library(plyr)
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

rr <- data_summary(r, varname="mean_brightness", 
                   groupnames=c("origin", "day"))

ggplot(rr, aes(x = day, y = mean_brightness, color=origin)) +  
  geom_point() +theme_classic()+coord_cartesian(ylim=c(0,150))+ stat_smooth(method="lm", se=F)+
  labs(y= "Brightness (bpp)", x = "Day")+guides(fill=FALSE)+ scale_color_brewer(palette = "Set1")
#geom_errorbar(aes(ymin=mean_brightness-sd, ymax=mean_brightness+sd), width=.2, position=position_dodge(0.05))


