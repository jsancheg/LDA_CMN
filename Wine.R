library(dplyr)
library(tidyr)
library(tidyverse)
library(gclus)
library(RColorBrewer)

pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")
source("FuncWine.R")

data("wine")
colnames(wine)
Xwine <- wine %>% subset(select = -c(Type))
colnames(Xwine)
y <- as.numeric(wine$Type)
G <- length(unique(y))
ind_1 <- y == 1
ind_2 <- y == 2
ind_3 <- y == 3

meanWine_1 <- Xwine[ind_1,] %>% apply(2,mean)
meanWine_2 <- Xwine[ind_2,] %>% apply(2,mean)
meanWine_3 <- Xwine[ind_3,] %>% apply(2,mean)

# p number of variables
p <- length(meanWine_1)

GWine_1 <- Xwine[ind_1,] %>% var
GWine_2 <- Xwine[ind_2,] %>% var
GWine_3 <- Xwine[ind_3,] %>% var

summary(factor(wine$Class))/nrow(wine)

mug <- matrix (0.0, nrow = p, ncol = 3)

mug[,1] <- meanWine_1
mug[,2] <- meanWine_2
mug[,3] <- meanWine_3

sg <- array(0.0, dim = c(p,p,G))

sg[,,1] <- GWine_1

sg[,,2] <- GWine_2

sg[,,3] <- GWine_3

lab <- y
ns<-1

  

alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)

sA80E_EQ_BAL <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfAEQ_EEQ_BAL <- sA80E_EQ_BAL$Metrics_models
dfAEQ_EEQ_BAL$alpha <- "Equal"
dfAEQ_EEQ_BAL$eta <- "Equal"
saveRDS(dfAEQ_EEQ_BAL,"AEQ_EEQBALWine.RDS")


alpha <- c(0.8,0.8,0.8)
eta <- c(15,10,5)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)


sA80E_NEQ_BAL <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAEQ_ENEQ_BAL <- sA80E_NEQ_BAL$Metrics_models
dfAEQ_ENEQ_BAL$alpha <- "Equal"
dfAEQ_ENEQ_BAL$eta <- "Inequal"
saveRDS(dfAEQ_ENEQ_BAL,"AEQ_ENEQBALWine.RDS")


alpha <- c(0.9,0.85,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)

# run this line
sANEQ_BNEQ_BAL <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfANEQ_BNEQ_BAL <- sANEQ_BNEQ_BAL$Metrics_models
dfANEQ_BNEQ_BAL$alpha <-"Inequal"
dfANEQ_BNEQ_BAL$eta <- "Equal"
saveRDS(dfANEQ_BNEQ_BAL,"ANEQ_EEQBALWine.RDS")



alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.8,0.8,0.8)
vpi <- c(0.5,0.25,0.25)


sA80E_NEQ_UNB <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)


# Read RDS ----------------------------------------------------------------
dfAEQ_EEQBALWine <- readRDS(paste0(pathWd,"AEQ_EEQBALWine.RDS") )
dfAEQ_ENEQBALWine <- readRDS(paste0(pathWd,"AEQ_ENEQBALWine.RDS") )
dfANEQ_EEQBALWine <-readRDS(paste0(pathWd,"ANEQ_EEQBALWine.RDS"))


dfAll <- rbind.data.frame(dfAEQ_EEQBALWine,
                          dfAEQ_ENEQBALWine,
                          dfANEQ_EEQBALWine)

freqVar <- table(unlist(str_split(dfAll$Model,"-")))
coul <- brewer.pal(5,"Set2")

barplot(prop.table(freqVar),col = coul)

colnames(dfAll)
dfAll <- dfAll %>% mutate (DifCCR = CR_SV - CR_SatMC)
dfAll <- dfAll %>% mutate (DifAccuracy = Accuracy_SVCont- Accuracy_SatCont  )
nrow(dfAll)

dfAll <- na.omit(dfAll)
nrow(dfAll)


dfDifLong<- dfAll %>% dplyr::select(alpha,eta,DifCCR,DifAccuracy) %>% 
  pivot_longer(c(DifCCR,DifAccuracy),
               names_to = "Variables",
               values_to = "Dif")

head(dfDifLong)


dfDifLong <- dfDifLong %>% mutate(Variables = recode(Variables,
                                                     DifCCR = "CCR",
                                                     DifAccuracy = "Accuracy"))


ggplot(dfDifLong, aes(x = Variables, y = Dif, color = eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.1,0.2) + geom_boxplot()


ggplot(dfDifLong, aes(x = Variables, y = Dif, color = alpha)) +
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.1,0.2) + geom_boxplot()




