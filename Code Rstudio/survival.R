#### SETUP ####

rm(list=ls())
setwd("C:/Users/luis_/Desktop/paper/code/csv")
getwd()
RStudio.Version()

### packages

library(car)
library(dplyr)
library(Rmisc) 
library(ggplot2)
library(moments)

#### Tissue cover ####

### load data
sur <- read.csv("survival.csv")
info <- read.csv("info.csv")

### explore data
summary(sur)
table(sur$treatment)
table(sur$stressed)
table(sur$serie)
str(sur)


### October - March ###

## prepare data
sur.oct.mar <- sur$oct.mar <- sur %>%
  transmute(tree,
            survival = rowMeans(select(., 47:54),na.rm = TRUE))

t <- merge(info,sur.oct.mar,by="tree")

octmar <-  t %>% filter(stressed == "no")

## explore data, normality + homocedasticity 
summary (octmar)

boxplot(survival~treatment,data=octmar,
        main = "",
        xlab="Treatment",
        ylab="Survival",
        ylim = c(0,1.2))

hist(octmar$survival, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(octmar$survival), sd=sd(octmar$survival)), add=TRUE, col="blue")

skewness(octmar$survival)
shapiro.test(octmar$survival)
leveneTest(survival~treatment,data=octmar)

#data non-normal but homocedastic -> arcsin transformation
asin.octmar <- octmar %>% 
  mutate(asin = asin(octmar$survival))

hist(asin.octmar $asin, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(asin.octmar$asin), sd=sd(asin.octmar$asin)), add=TRUE, col="blue")

shapiro.test(asin.octmar$asin)
leveneTest(asin~treatment,data=asin.octmar)


### ANOVA
aov2<-aov(asin~treatment, data=asin.octmar)
summary(aov2)

plot(aov2, 1)
plot(aov2, 2)

TukeyHSD(x=aov2,"treatment",conf.level = 0.95)

### bar graphs
om <-summarySE(octmar, measurevar="survival",groupvars=c("treatment","treatment"),na.rm = TRUE)
df <- c("a","b","a","a")
sig<-data.frame(df)
om1<-bind_cols(om,sig)

ggplot(om1, aes(x=treatment, y=survival, fill=treatment)) + 
  theme_classic()+ labs(y= "Live Tissue Cover", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,1))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.15,
                position=position_dodge(.9))+
  geom_text(aes(label = df, y = survival + se), vjust = -0.5)



### stressed frags ###

## prepare data
sur.dec.mar <- sur$dec.mar <- sur %>%
  transmute(tree,
            survival = rowMeans(select(., 39:46),na.rm = TRUE))

s <- merge(info,sur.dec.mar,by="tree")

decmar1 <-  s %>% filter(stressed == "yes")

## explore data, normality + homocedasticity 
summary (decmar1)

boxplot(survival~treatment,data=decmar1,
        main = "",
        xlab="Treatment",
        ylab="Survival",
        ylim = c(0,1.2))

hist(decmar1$survival, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(decmar1$survival), sd=sd(decmar1$survival)), add=TRUE, col="blue")

skewness(decmar1$survival)
shapiro.test(decmar1$survival)
leveneTest(survival~treatment,data=decmar1)


### ANOVA
aov2<-aov(survival~treatment, data=decmar1)
summary(aov2)

plot(aov2, 1)
plot(aov2, 2)

TukeyHSD(x=aov2,"treatment",conf.level = 0.95)

### bar graphs
dms <-summarySE(decmar1, measurevar="survival",groupvars=c("treatment","treatment"),na.rm = TRUE)
df <- c("a","a","a","a")
sig<-data.frame(df)
dms1<-bind_cols(dms,sig)

ggplot(dms1, aes(x=treatment, y=survival, fill=treatment)) + 
  theme_classic()+ labs(y= "Live Tissue Cover", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,1))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.15,
                position=position_dodge(.9))+
  geom_text(aes(label = df, y = survival + se), vjust = -0.5)