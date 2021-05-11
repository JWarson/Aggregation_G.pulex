setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #setwd to directory where script is located

##FUNCTIONS
#function to order data
dataorder <- function(data){
  d <- data[,4:5] #make sure column 4 and 5 are X and Y coord.
  colnames(d) <- c('x', 'y')
  d <- cbind(ID = rep(1:(length(d[,1])/21), times = 21), 
                Frame = rep(1:21, each = (length(d[,1])/21)), d)
  return(d)
} 

#function to calculate average nearest neighbor in each frame
NN <- function(experiment){
  avNN <- numeric()
  for (i in 1:21){
    dist <- as.data.frame(as.matrix(dist(experiment[experiment$Frame==i,3:4], 
                                         upper = T, diag = F)))
    dist[dist == 0] <- NA
    nearestn <- numeric()
    for (j in 1:length(dist)){
      nearestn[j] <- min(dist[,j], na.rm = TRUE)
    }
    avNN[i] <- mean(nearestn)
  }
  return(avNN)
}

#function to calculate PWaD
PWaD <- function(experiment){
  PWaD <- numeric()
  for (i in 1:21){
    dist <- as.data.frame(as.matrix(dist(experiment[experiment$Frame==i,3:4], 
                                         upper = T, diag = F)))
    dist[dist == 0] <- NA
    averagedist <- numeric()
    for (j in 1:length(dist)){
      averagedist[j] <- mean(dist[,j], na.rm = TRUE)
    }
    PWaD[i] <- mean(averagedist)
  }
  return(PWaD)
}

#Exp 1 (Control)
exp1 <- read.csv('test_1.csv', h = T)
exp1 <- dataorder(exp1)
NNexp1 <- NN(exp1)
PWaDexp1 <- PWaD(exp1)

#Exp 2 (Conspecifics)
exp2 <- read.csv('test_2.csv', h = T)
exp2 <- dataorder(exp2)
NNexp2 <- NN(exp2)
PWaDexp2 <- PWaD(exp2)

#Exp 3 (Combined)
exp3 <- read.csv('test_3.csv', h = T)
exp3 <- dataorder(exp3)
NNexp3 <- NN(exp3)
PWaDexp3 <- PWaD(exp3)

#Exp 4 (Kairomones)
exp4 <- read.csv('test_4.csv', h = T)
exp4 <- exp4[,4:5] #make sure column 4 and 5 are X and Y coord.
colnames(exp4) <- c('x', 'y')
#manual annotation of frames and ind
exp4 <- cbind(ID = c(rep(1:7, times = 12),rep(1:8, times = 4),rep(1:7, times = 5)), 
           Frame = c(rep(1:12, each = 7), rep(13:16, each = 8),rep(17:21, each = 7)), exp4)
NNexp4 <- NN(exp4)
PWaDexp4 <- PWaD(exp4)

#Exp 5
exp5 <- read.csv('test_5.csv', h = T)
exp5 <- dataorder(exp5)
NNexp5 <- NN(exp5)
PWaDexp5 <- PWaD(exp5)

#Exp 6
exp6 <- read.csv('test_6.csv', h = T)
exp6 <- dataorder(exp6)
NNexp6 <- NN(exp6)
PWaDexp6 <- PWaD(exp6)

#Exp 7
exp7 <- read.csv('test_7.csv', h = T)
exp7 <- dataorder(exp7)
NNexp7 <- NN(exp7)
PWaDexp7 <- PWaD(exp7)

#Exp 8
exp8 <- read.csv('test_8.csv', h = T)
exp8 <- dataorder(exp8)
NNexp8 <- NN(exp8)
PWaDexp8 <- PWaD(exp8)

#Exp 9
exp9 <- read.csv('test_9.csv', h = T)
exp9 <- dataorder(exp9)
NNexp9 <- NN(exp9)
PWaDexp9 <- PWaD(exp9)

#Exp 10
exp10 <- read.csv('test_10.csv', h = T)
exp10 <- dataorder(exp10)
NNexp10 <- NN(exp10)
PWaDexp10 <- PWaD(exp10)

#Exp 11
exp11 <- read.csv('test_11.csv', h = T)
exp11 <- dataorder(exp11)
NNexp11 <- NN(exp11)
PWaDexp11 <- PWaD(exp11)

#Exp 12
exp12 <- read.csv('test_12.csv', h = T)
exp12 <- dataorder(exp12)
NNexp12 <- NN(exp12)
PWaDexp12 <- PWaD(exp12)

#Exp 13
exp13 <- read.csv('test_13.csv', h = T)
exp13 <- dataorder(exp13)
NNexp13 <- NN(exp13)
PWaDexp13 <- PWaD(exp13)

#Exp 14
exp14 <- read.csv('test_14.csv', h = T)
exp14 <- dataorder(exp14)
NNexp14 <- NN(exp14)
PWaDexp14 <- PWaD(exp14)

#Exp 15
exp15 <- read.csv('test_15.csv', h = T)
exp15 <- dataorder(exp15)
NNexp15 <- NN(exp15)
PWaDexp15 <- PWaD(exp15)

#Exp 16
exp16 <- read.csv('test_16.csv', h = T)
exp16 <- dataorder(exp16)
NNexp16 <- NN(exp16)
PWaDexp16 <- PWaD(exp16)

##TESTING AND VISUALISATION
###########################
data <- as.data.frame(cbind(Treatment = rep(c(1,3,4,2,3,4,2,1,2,1,3,4,3,2,4,1), each = 21),
                            Frame = as.integer(rep(1:21, times = 16)), 
                            Experiment = as.integer(rep(1:16, each = 21)),
                            Day = as.integer(c(rep(1:3, each = 84), rep(3, times = 84))),
                            Replicate = as.integer(rep(1:4, each = 21*4)),
                            Pointer = c(rep('Robby', 21),rep('Vincent', 21),rep('Robby', 21),rep('Vincent', 21),rep('Robby', 21),rep('Vincent', 11*21)),
                            NearestNeighbor = c(NNexp1, NNexp2, NNexp3, NNexp4,
                                                NNexp5, NNexp6, NNexp7, NNexp8,
                                                NNexp9, NNexp10, NNexp11, NNexp12,
                                                NNexp13, NNexp14, NNexp15, NNexp16),
                            PairwiseDistance = c(PWaDexp1, PWaDexp2,PWaDexp3, PWaDexp4,
                                                 PWaDexp5, PWaDexp6, PWaDexp7, PWaDexp8,
                                                 PWaDexp9, PWaDexp10, PWaDexp11, PWaDexp12,
                                                 PWaDexp13, PWaDexp14, PWaDexp15,PWaDexp16)))

                      
data[,c(1:5,7:8)] <- sapply(data[,c(1:5,7:8)], as.numeric)
data$Treatment <- as.factor(data$Treatment)
levels(data$Treatment) <- c("Control","Predator","Conspecific","Combined")
data$Pointer <- as.factor(data$Pointer)
str(data)

write.csv(data, 'fullData.csv', row.names = F)
write.csv(datab, 'fullDataZoomed.csv', row.names = F)

shapiro.test(data$NearestNeighbor)
shapiro.test(data$PairwiseDistance)
library(car)
leveneTest(NearestNeighbor ~ Treatment,data = data)
leveneTest(PairwiseDistance ~ Treatment, data = data)
##Assumptions are good

###################
##Autocorrelation##
###################
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
  theme_classic() +
  ggtitle('Pairwise Distance')+
  ylab('Average Pairwise Distance') +
  geom_signif(comparisons = list(c("Control", "Conspecific"), c("Conspecific", "Combined")),
              annotations = c("*", "**"), textsize = 5, 
              y_position = 450, extend_line = -0.01, step_increase = 0, 
              color = 'black', vjust = 0.7, tip_length = 0.2)+
  scale_fill_manual(values = coll)+
  xlab('Treatment')+
  theme(legend.position = 'none')

#NN
dataCorrNN <- data.frame(effect(mod =fitNN.aut, term = 'Treatment'))
lvl_NN <- factor(dataCorrNN$Treatment, level = c('Control', 'Predator', 'Conspecific', 'Combined'))
ggplot(data=dataCorrNN, aes(x= lvl_NN, y = fit, color = Treatment))+
  geom_bar(stat="identity", fill=coll, color = 'black', alpha = 0.6, 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=.2,
                position=position_dodge(.9), color = 'black') +
  theme_classic() +
  ggtitle('Nearest Neighbor')+
  ylab('Average Nearest Neighbor') +
  scale_fill_manual(values = coll)+
  xlab('Treatment')+
  theme(legend.position = 'none')

########################
########################
#More Detailed Analysis#
########################
########################

#every 25 frames from frame 751 (150 seconds) to 1501 (300 sec) -> 31 frames

##FUNCTIONS
#function to order data
dataorderb <- function(data){
  d <- data[,4:5] #make sure column 4 and 5 are X and Y coord.
  colnames(d) <- c('x', 'y')
  d <- cbind(ID = rep(1:(length(d[,1])/31), times = 31), 
             Frame = rep(1:31, each = (length(d[,1])/31)), d)
  return(d)
} 

#function to calculate average nearest neighbor in each frame
NNb <- function(experiment){
  avNN <- numeric()
  for (i in 1:31){
    dist <- as.data.frame(as.matrix(dist(experiment[experiment$Frame==i,3:4], 
                                         upper = T, diag = F)))
    dist[dist == 0] <- NA
    nearestn <- numeric()
    for (j in 1:length(dist)){
      nearestn[j] <- min(dist[,j], na.rm = TRUE)
    }
    avNN[i] <- mean(nearestn)
  }
  return(avNN)
}

#function to calculate PWaD
PWaDb <- function(experiment){
  PWaD <- numeric()
  for (i in 1:31){
    dist <- as.data.frame(as.matrix(dist(experiment[experiment$Frame==i,3:4], 
                                         upper = T, diag = F)))
    dist[dist == 0] <- NA
    averagedist <- numeric()
    for (j in 1:length(dist)){
      averagedist[j] <- mean(dist[,j], na.rm = TRUE)
    }
    PWaD[i] <- mean(averagedist)
  }
  return(PWaD)
}


#ANALYSIS#
##########
#exp1
exp1b <- read.csv('test_1_2.csv', h = T)
exp1b <- dataorderb(exp1b)
NNexp1b <- NNb(exp1b)
PWaDexp1b <- PWaDb(exp1b)

#exp2
exp2b <- read.csv('test_2_2.csv', h = T)
exp2b <- dataorderb(exp2b)
NNexp2b <- NNb(exp2b)
PWaDexp2b <- PWaDb(exp2b)

#exp3
exp3b <- read.csv('test_3_2.csv', h = T)
exp3b <- dataorderb(exp3b)
NNexp3b <- NNb(exp3b)
PWaDexp3b <- PWaDb(exp3b)

#exp4 ->> couple
exp4b <- read.csv('test_4_2.csv', h = T)
exp4b <- exp4b[,4:5] #make sure column 4 and 5 are X and Y coord.
colnames(exp4b) <- c('x', 'y')
#assign number individuals depending on whether couple was coupled
exp4b <- cbind(ID = c(rep(1:7, times = 6),rep(1:8, times = 13),rep(1:7, times = 12)), 
              Frame = c(rep(1:6, each = 7), rep(7:19, each = 8),rep(20:31, each = 7)), 
              exp4b)

NNexp4b <- NNb(exp4b)
PWaDexp4b <- PWaDb(exp4b)

#exp5
exp5b <- read.csv('test_5_2.csv', h = T)
exp5b <- dataorderb(exp5b)
NNexp5b <- NNb(exp5b)
PWaDexp5b <- PWaDb(exp5b)

#exp6
exp6b <- read.csv('test_6_2.csv', h = T)
exp6b <- dataorderb(exp6b)
NNexp6b <- NNb(exp6b)
PWaDexp6b <- PWaDb(exp6b)

#exp7
exp7b <- read.csv('test_7_2.csv', h = T)
exp7b <- dataorderb(exp7b)
NNexp7b <- NNb(exp7b)
PWaDexp7b <- PWaDb(exp7b)

#exp8
exp8b <- read.csv('test_8_2.csv', h = T)
exp8b <- dataorderb(exp8b)
NNexp8b <- NNb(exp8b)
PWaDexp8b <- PWaDb(exp8b)

#exp9
exp9b <- read.csv('test_9_2.csv', h = T)
exp9b <- dataorderb(exp9b)
NNexp9b <- NNb(exp9b)
PWaDexp9b <- PWaDb(exp9b)

#exp10
exp10b <- read.csv('test_10_2.csv', h = T)
exp10b <- dataorderb(exp10b)
NNexp10b <- NNb(exp10b)
PWaDexp10b <- PWaDb(exp10b)

#exp11
exp11b <- read.csv('test_11_2.csv', h = T)
exp11b <- dataorderb(exp11b)
NNexp11b <- NNb(exp11b)
PWaDexp11b <- PWaDb(exp11b)

#exp12
exp12b <- read.csv('test_12_2.csv', h = T)
exp12b <- dataorderb(exp12b)
NNexp12b <- NNb(exp12b)
PWaDexp12b <- PWaDb(exp12b)

#exp13
exp13b <- read.csv('test_13_2.csv', h = T)
exp13b <- dataorderb(exp13b)
NNexp13b <- NNb(exp13b)
PWaDexp13b <- PWaDb(exp13b)

#exp14
exp14b <- read.csv('test_14_2.csv', h = T)
exp14b <- dataorderb(exp14b)
NNexp14b <- NNb(exp14b)
PWaDexp14b <- PWaDb(exp14b)

#exp15
exp15b <- read.csv('test_15_2.csv', h = T)
exp15b <- dataorderb(exp15b)
NNexp15b <- NNb(exp15b)
PWaDexp15b <- PWaDb(exp15b)

#exp16
exp16b <- read.csv('test_16_2.csv', h = T)
exp16b <- dataorderb(exp16b)
NNexp16b <- NNb(exp16b)
PWaDexp16b <- PWaDb(exp16b)

###Analysis
datab <- as.data.frame(cbind(Treatment = rep(c(1,3,4,2,3,4,2,1,2,1,3,4,3,2,4,1), each = 31),
                            Frame = as.integer(rep(1:31, times = 16)), 
                            Experiment = as.integer(rep(1:16, each = 31)),
                            Day = as.factor(c(rep(1:3, each = 31*4), rep(3, times = 31*4))),
                            Replicate = as.integer(rep(1:4, each = 31*4)),
                            Pointer = as.factor(c(rep('Vincent', times = 31*8),rep('Michiel', times = 31*4), rep('Jonas', times = 31*2), rep('Vincent', times = 31),rep('Jonas', times=31))),
                            NearestNeighbor = c(NNexp1b, NNexp2b, NNexp3b, NNexp4b,
                                                NNexp5b, NNexp6b, NNexp7b, NNexp8b,
                                                NNexp9b, NNexp10b, NNexp11b, NNexp12b,
                                                NNexp13b, NNexp14b, NNexp15b, NNexp16b),
                            PairwiseDistance = c(PWaDexp1b, PWaDexp2b,PWaDexp3b, PWaDexp4b,
                                                 PWaDexp5b, PWaDexp6b, PWaDexp7b, PWaDexp8b,
                                                 PWaDexp9b, PWaDexp10b, PWaDexp11b, PWaDexp12b,
                                                 PWaDexp13b, PWaDexp14b, PWaDexp15b,PWaDexp16b)))
datab$Treatment <- as.factor(datab$Treatment)
levels(datab$Treatment) <- c("Control","Predator","Conspecific","Combined")
datab$Pointer <- as.factor(datab$Pointer)
datab$Replicate <- as.integer(datab$Replicate)
str(datab)

shapiro.test(datab$NearestNeighbor)
shapiro.test(datab$PairwiseDistance)
leveneTest(NearestNeighbor ~ Treatment,data = datab)
leveneTest(PairwiseDistance ~ Treatment, data = datab)

###################
##Autocorrelation##
###################

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
install.packages('stringi')
library(stringi)
library(car)

data <- read.csv('fullData.csv', h =T)
datab <- read.csv('fullDataZoomed.csv', h =T)
cor.test(data[,7], data[,8])
scatterplot(data[,7], data[,8], xlab = 'Nearest Neighbor (pixels)', ylab = 'Inter-Individual Distance (pixels)')
cor.test(datab[,7], datab[,8])
scatterplot(datab[,7], datab[,8], xlab = 'Nearest Neighbor (pixels)', ylab = 'Inter-Individual Distance (pixels)')
