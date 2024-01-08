#### SETUP ####
rm(list=ls())
setwd("C:/Users/luis_/Desktop/paper/code/csv")
c

### packages

library(car)
library(dplyr)
library(Rmisc) 
library(ggplot2)

##### GROWTH #####

### load data
grow <- read.csv("growth.csv")
info <- read.csv("info.csv")

### explore data
table(grow$treatment)
table(grow$stressed)
table(grow$serie)


### October - December ###

## prepare data
SGR.oct.dec <- grow$oct.dec <- grow %>%
            transmute(tree,
            SGR = rowMeans(select(., 103:110),na.rm = TRUE))

r <- merge(info,SGR.oct.dec,by="tree")

octdec <-  r%>% filter(stressed == "no", serie == "original" )

## explore data, normality + homocedasticity 
summary (octdec)

boxplot(SGR~treatment,data=octdec,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

SGR <- octdec$SGR

hist(SGR, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(SGR), sd=sd(SGR)), add=TRUE, col="blue")

shapiro.test(SGR)
leveneTest(SGR~treatment,data=octdec)

str(octdec)
octdec$treatment <- as.factor(octdec$treatment)

### ANOVA
aov<-aov(SGR~treatment, data=octdec)
summary(aov)

plot(aov, 1)
plot(aov, 2)

TukeyHSD(x=aov,"treatment",conf.level = 0.95)


### bar graphs
od <-summarySE(octdec, measurevar="SGR",groupvars=c("treatment","treatment"),na.rm = TRUE)
df <- c("a","b","ab","ab")
sig<-data.frame(df)
od1<-bind_cols(od,sig)

ggplot(od1, aes(x=treatment, y=SGR, fill=treatment)) + 
  theme_classic()+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.15,
                position=position_dodge(.9))+
  geom_text(aes(label = df, y = SGR + se), vjust = -0.5)


### December - March ###

## prepare data
SGR.dec.mar <- grow$dec.mar <- grow %>%
            transmute(tree,
            SGR = rowMeans(select(., 111:118),na.rm = TRUE))

s <- merge(info,SGR.dec.mar,by="tree")

decmar <-  s%>% filter(stressed == "no")

## explore data, normality + homocedasticity 
summary (decmar)

boxplot(SGR~treatment,data=decmar,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

SGR1 <- decmar$SGR

hist(SGR1, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(SGR1), sd=sd(SGR1)), add=TRUE, col="blue")

shapiro.test(SGR1)
leveneTest(SGR1~treatment,data=decmar)

decmar$treatment <- as.factor(decmar$treatment)

### ANOVA
aov1<-aov(SGR~treatment, data=decmar)
summary(aov1)

plot(aov1, 1)
plot(aov1, 2)

TukeyHSD(x=aov1,"treatment",conf.level = 0.95)

### bar graphs
dm <-summarySE(decmar, measurevar="SGR",groupvars=c("treatment","treatment"),na.rm = TRUE)
df <- c("a","ab","a","b")
sig<-data.frame(df)
dm1<-bind_cols(dm,sig)

ggplot(dm1, aes(x=treatment, y=SGR, fill=treatment)) + 
  theme_classic()+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.15,
                position=position_dodge(.9))+
  geom_text(aes(label = df, y = SGR + se), vjust = -0.5)
  


### stressed fragments growth ###

SGR.dec.mar1 <- grow$dec.mar <- grow %>%
  transmute(tree,
            SGR = rowMeans(select(., 111:118),na.rm = TRUE))

s1 <- merge(info,SGR.dec.mar1,by="tree")

decmar1 <-  s1%>% filter(stressed == "yes")

## explore data, normality + homocedasticity 
summary (decmar1)

boxplot(SGR~treatment,data=decmar1,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

SGR2 <- decmar1$SGR

hist(SGR2, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(SGR2), sd=sd(SGR2)), add=TRUE, col="blue")

shapiro.test(SGR2)
leveneTest(SGR2~treatment,data=decmar1)

decmar1$growsqrt=sqrt(decmar1$SGR)

growsqrt <- decmar1$growsqrt

hist(growsqrt, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(growsqrt), sd=sd(growsqrt)), add=TRUE, col="blue")

shapiro.test(growsqrt)
leveneTest(growsqrt~treatment,data=decmar1)

### ANOVA
aov1<-aov(growsqrt~treatment, data=decmar1)
summary(aov1)

plot(aov1, 1)
plot(aov1, 2)

TukeyHSD(x=aov1,"treatment",conf.level = 0.95)

### bar graphs
dms <-summarySE(decmar1, measurevar="SGR",groupvars=c("treatment","treatment"),na.rm = TRUE)
df <- c("ab","a","b","b")
sig<-data.frame(df)
dms1<-bind_cols(dms,sig)

ggplot(dms1, aes(x=treatment, y=SGR, fill=treatment)) + 
  theme_classic()+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.15,
                position=position_dodge(.9))+
  geom_text(aes(label = df, y = SGR + se), vjust = -0.5)
  


##
##
##
##
### October - March ###

## prepare data
SGR.oct.mar <- grow$oct.mar <- grow %>%
            transmute(tree,
            SGR = rowMeans(select(., 119:126),na.rm = TRUE))

t <- merge(info,SGR.oct.mar,by="tree")

octmar <-  t%>% filter(stressed == "no", serie == "original")

## explore data, normality + homocedasticity 
summary (octmar)

boxplot(SGR~treatment,data=octmar,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

SGR2 <- octmar$SGR

hist(SGR2, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(SGR2), sd=sd(SGR2)), add=TRUE, col="blue")

shapiro.test(SGR2)
leveneTest(SGR2~treatment,data=octmar)

### ANOVA
aov2<-aov(SGR~treatment, data=octmar)
summary(aov2)

plot(aov2, 1)
plot(aov2, 2)

TukeyHSD(x=aov2,"treatment",conf.level = 0.95)

### bar graphs
om<-summarySE(octmar, measurevar="SGR",groupvars=c("treatment","treatment"),na.rm = TRUE)

ggplot(om, aes(x=treatment, y=SGR, fill=treatment)) + 
  theme_classic()+ ggtitle("Growth Oct - Mar")+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.15,
                position=position_dodge(.9))

