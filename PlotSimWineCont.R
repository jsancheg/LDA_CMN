library(dplyr)
library(tidyr)
library(tidyverse)
library(gclus)
library(RColorBrewer)
library(corrplot)


pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")
source("FuncWine.R")
source("https://www.sthda.com/upload/rquery_cormat.r")
data("wine")

colnames(wine)
Xwine <- wine %>% subset(select = -c(Type))
colnames(Xwine)

Winedf <- Xwine
Winedf$y <- as.numeric(wine$Type)

colnames(Winedf)

Xwine %>% as.matrix %>% cor %>% corrplot::corrplot(method = "circle")
rquery.cormat(Xwine)

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
eta <- c(5,5,5)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)


sA80_E5_BALWine <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA80_E5_BalWine_Models <- sA80_E5_BALWine$Metrics_models
dfA80_E5_BalWine_Models$alpha <- "Equal"
dfA80_E5_BalWine_Models$eta <- "Equal"
saveRDS(dfA80_E5_BalWine_Models,"A80_E5_BalWine_Models.RDS")


dfA80_E5_BalWine <- sA80_E5_BALWine$Train[[1]]

WineTraindf <- dfA80_E5_BalWine %>% dplyr::select(-index)
WineTraindf$class <- as.factor(WineTraindf$class)
WineTraindf$Cont <- as.factor(WineTraindf$Cont)


mycols <- c("green","cornflowerblue","purple")
pairs(WineTraindf%>%dplyr::select(-c(class,Cont, Magnesium, Phenols,
                                     Ash,Alcalinity,Alcohol,
                                     Nonflavanoid,Proanthocyanins
)), oma = c(3,3,6,3),
      col = mycols[as.numeric(WineTraindf$class)],
      pch = c(19,2)[as.numeric(WineTraindf$Cont)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(WineTraindf$class),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)
legend("top", pch = c(19,2), legend = levels(WineTraindf$Cont), col = "black",
       xpd = NA, ncol = 3, bty = "n", inset = -0.03)




alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)

sA80_E10_BALWine <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA80_E10_BalWine_Models <- sA80_E10_BALWine$Metrics_models
dfA80_E10_BalWine_Models$alpha <- "Equal"
dfA80_E10_BalWine_Models$eta <- "Equal"
saveRDS(dfA80_E10_BalWine_Models,"A80_E10_BalWine_Models.RDS")


dfA80_E10_BalWine <- sA80_E10_BALWine$Train[[1]]


WineTraindf <- dfA80_E10_BalWine %>% dplyr::select(-index)
WineTraindf$class <- as.factor(WineTraindf$class)
WineTraindf$Cont <- as.factor(WineTraindf$Cont)

summary(WineTraindf$Cont)
colnames(WineTraindf)

mycols <- c("green","cornflowerblue","purple")
pairs(WineTraindf%>%dplyr::select(-c(class,Cont, Magnesium, Phenols,
                                     Ash,Alcalinity,Alcohol,
                                     Nonflavanoid,Proanthocyanins
)), oma = c(3,3,6,3),
col = mycols[as.numeric(WineTraindf$class)],
      pch = c(19,2)[as.numeric(WineTraindf$Cont)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(WineTraindf$class),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)
legend("top", pch = c(19,2), legend = levels(WineTraindf$Cont), col = "black",
       xpd = NA, ncol = 3, bty = "n", inset = -0.03)



alpha <- c(0.8,0.8,0.8)
eta <- c(15,15,15)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)

sA80_E15_BALWine <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA80_E15_BalWine_Models <- sA80_E15_BALWine$Metrics_models
dfA80_E15_BalWine_Models$alpha <- "Equal"
dfA80_E15_BalWine_Models$eta <- "Equal"
saveRDS(dfA80_E15_BalWine_Models,"A80_E15_BalWine_Models.RDS")


dfA80_E15_BalWine <- sA80_E15_BALWine$Train[[1]]

colnames(dfA80_E15_BalWine)

WineTraindf <- dfA80_E15_BalWine %>% dplyr::select(-index)
WineTraindf$class <- as.factor(WineTraindf$class)
WineTraindf$Cont <- as.factor(WineTraindf$Cont)

 

# --- Scatter plots ---

A80_E5_BalWine_ModelsDf <- readRDS(paste0(pathWd,"A80_E15_BalWine_Models.RDS") )
A80_E10_BalWine_ModelsDf <- readRDS(paste0(pathWd,"A80_E15_BalWine_Models.RDS") )
A80_E15_BalWine_ModelsDf <-readRDS(paste0(pathWd,"A80_E15_BalWine_Models.RDS"))



dfAll <- rbind.data.frame(A80_E5_BalWine_ModelsDf,
                          A80_E10_BalWine_ModelsDf,
                          A80_E15_BalWine_ModelsDf)



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








dfA80_E10_BalWine %>% dplyr::select(-index) %>%as.matrix %>% cor %>% corrplot::corrplot(method = "circle")

colnames(dplyr::select(-index))





GenContSamples <- SimCont(mug,sg,unique(y),ncont,eta)
GenContSamples$index <- (nrow(X)+1):(nrow(X) +nrow(GenContSamples) )


