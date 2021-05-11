setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

data <- read.csv('fullData.csv', h = T)
datab <- read.csv('fullDataZoomed.csv', h = T)

shapiro.test(data$NearestNeighbor)
shapiro.test(data$PairwiseDistance)
library(car)
leveneTest(NearestNeighbor ~ Treatment,data = data)
leveneTest(PairwiseDistance ~ Treatment, data = data)
##Assumptions are good



##Full 5 minutes (21 frames, each 15 seconds##
##############################################
library(MASS)
library(nlme)
fitPWaD.aut <- glmmPQL(PairwiseDistance~ Treatment, 
                       random = list(~1|Day, ~1|Frame, ~1|Pointer),
                       correlation = corAR1(),
                       data = data,
                       family = 'gaussian')
summary(fitPWaD.aut)

fitNN.aut <- glmmPQL(NearestNeighbor~ Treatment, 
                     random = list(~1|Day, ~1|Frame, ~1|Pointer),
                     correlation = corAR1(),
                     data = data,
                     family = 'gaussian')
summary(fitNN.aut) #no signifcance

#PostHoc
#install.packages("multcomp")
library(multcomp)
model.matrix.gls <- function(object, ...) {model.matrix(terms(object), data = getData(object), ...)} # nodig voor posthoc
model.frame.gls <- function(object, ...) {model.frame(formula(object), data = getData(object), ...)} # nodig voor posthoc
terms.gls <- function(object, ...) {terms(model.frame(object), ...)} #--> nodig voor posthoc

#PWaD
summary(glht(fitPWaD.aut, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) # de posthoc test
#combined vs conspecific is significant w P = 0.00673, Consp -Contr P = 0.04887


##########
##GGPlot##
##########
coll <- c('#9cacbc','#3c6c7c', '#144c9c', '#000c2e') #some nice blue colors
#Plot NN through Frame
library(ggplot2)
ggplot(data = data, aes(y = NearestNeighbor, 
                        x = Frame, 
                        color = Treatment)) +
  geom_point()+
  geom_smooth(se = T)+
  ggtitle('Average Nearest Neighbor')+
  theme_classic()+
  scale_color_manual(values = coll)

#Plot PWaD
ggplot(data = data, aes(y = PairwiseDistance, 
                        x = Frame, 
                        color = Treatment)) +
  geom_point()+
  geom_smooth(se = T)+
  ggtitle('Average Pairwise Distance')+
  theme_classic()+
  scale_color_manual(values = coll)

#Barplot
library(effects)
#install.packages("ggsignif")
library(ggsignif)

#PWaD
dataCorrPWaD <- data.frame(effect(mod =fitPWaD.aut, term = 'Treatment'))
lvl_pwad <- factor(dataCorrPWaD$Treatment, level = c('Control', 'Predator', 'Conspecific', 'Combined'))
ggplot(data=dataCorrPWaD, aes(x= lvl_pwad, y = fit, color = Treatment))+
  geom_bar(stat="identity", fill=coll, color = 'black', alpha = 0.6, 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=.2,
                position=position_dodge(.9), color = 'black') +
  theme_classic() +shapiro.test(data$NearestNeighbor)

##Detailed Analysis##
#####################
#1 frame every 5 seconds from 2,5 to 5 minutes

##MODELS
#PWaD
fitPWaDb.aut <- glmmPQL(PairwiseDistance~ Treatment, 
                        random = list(~1|Day, ~1|Frame, ~1|Pointer),
                        correlation = corAR1(),
                        data = datab,
                        family = 'gaussian')
summary(fitPWaDb.aut)

#Post Hoc
summary(glht(fitPWaDb.aut, linfct = mcp(Treatment = "Tukey")), test = adjusted("holm")) # de posthoc test
#many significant combos

#NN
fitNNb.aut <- glmmPQL(NearestNeighbor~ Treatment, 
                      random = list(~1|Day, ~1|Frame, ~1|Pointer),
                      correlation = corAR1(),
                      data = datab,
                      family = 'gaussian')
summary(fitNNb.aut) #No signifcance

#autocorrelation plot
#PWaD
x = seq(1,length(fitPWaDb.aut$residuals))
ggplot() + 
  geom_line(aes(x = x, y = fitPWaDb.aut$residuals), color = "red") +
  geom_hline(yintercept=0, linetype="dashed", color='blue')+
  theme_classic()+
  ylab('Residuals')+
  ggtitle('PWD - Autocorrelation Plot - Per 25 Frames')

#NN
x = seq(1,length(fitNNb.aut$residuals))
ggplot() + 
  geom_line(aes(x = x, y = fitNNb.aut$residuals), color = "red") +
  geom_hline(yintercept=0, linetype="dashed", color='blue')+
  theme_classic()+
  ylab('Residuals')+
  ggtitle('NN - Autocorrelation Plot - Per 25 Frames')

##########
##GGPlot##
##########
str(datab)
data$Treatment <- as.factor(data$Treatment)
#Plot NN
ggplot(data = datab, aes(y = NearestNeighbor, 
                         x = Frame, 
                         color = Treatment)) +
  geom_point(alpha = 0.4, size = 0.8)+
  geom_smooth(se = T)+
  ggtitle('Average Nearest Neighbor - Detailed')+
  theme_classic()+
  scale_color_manual(values = coll) 

#Plot PWaD
ggplot(data = datab, aes(y = PairwiseDistance, 
                         x = Frame, 
                         color = Treatment)) +
  geom_point(alpha = 0.4, size = 0.8)+
  geom_smooth(se = T)+
  ggtitle('Average Pairwise Distance - Detailed')+
  theme_classic()+
  scale_color_manual(values = coll)

#Barplot w error 

#install.packages('ggsignif')
library(ggsignif)
library(effects)

#NN
dataCorrNNb <- data.frame(effect(mod =fitNNb.aut, term = 'Treatment'))
lvl_NNb <- factor(dataCorrNNb$Treatment, level = c('Control', 'Predator', 'Conspecific', 'Combined'))
ggplot(data=dataCorrNNb, aes(x= lvl_NNb, y = fit, color = Treatment))+
  geom_bar(stat="identity", fill=coll, color = 'black', alpha = 0.6, 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=.2,
                position=position_dodge(.9), color = 'black') +
  theme_classic() +
  ggtitle('Nearest Neighbor - Detailed')+
  ylab('Average Nearest Neighbor') +
  scale_fill_manual(values = coll)+
  xlab('Treatment')+
  theme(legend.position = 'none')

#PWaD
dataCorrPWaDb <- data.frame(effect(mod =fitPWaDb.aut, term = 'Treatment'))
lvl_PWaDb <- factor(dataCorrPWaDb$Treatment, level = c('Control', 'Predator', 'Conspecific', 'Combined'))
ggplot(data=dataCorrPWaDb, aes(x= lvl_PWaDb, y = fit, color = Treatment))+
  geom_bar(stat="identity", fill=coll, color = 'black', alpha = 0.6, 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=.2,
                position=position_dodge(.9), color = 'black') +
  theme_classic() +
  ggtitle('Pairwise Distance - Detailed')+
  ylab('Average Pairwise Distance') +
  geom_signif(comparisons = list(c("Control", "Conspecific"), c("Conspecific", "Combined"), c("Conspecific","Predator"), c("Combined", "Predator")),
              annotations = c("***", "***", "*", "**"), textsize = 5, 
              y_position = 450, extend_line = -0.01, step_increase = 0.3, 
              color = 'black', vjust = 0.7)+
  scale_fill_manual(values = coll)+
  xlab('Treatment')+
  theme(legend.position = 'none')

#some correlation between nearest neighbor and average pairwaise distance
cor.test(data[,7], data[,8])
scatterplot(data[,7], data[,8], xlab = 'Nearest Neighbor', ylab = 'Pairwise Distance')
cor.test(datab[,7], datab[,8])
scatterplot(datab[,7], datab[,8], xlab = 'Nearest Neighbor', ylab = 'Pairwise Distance')
