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
eta <- c(5,5,5)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)


sA80_E5_BALWine <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA80_E5_BalWine <- sA80_E5_BALWine$Train[[1]]



alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)

sA80_E10_BALWine <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA80_E10_BalWine <- sA80_E10_BALWine$Train[[1]]




corrplot(dfA80_E10_BalWine %>% dplyr::select(-index),"square" )

colnames(dplyr::select(-index))





GenContSamples <- SimCont(mug,sg,unique(y),ncont,eta)
GenContSamples$index <- (nrow(X)+1):(nrow(X) +nrow(GenContSamples) )


