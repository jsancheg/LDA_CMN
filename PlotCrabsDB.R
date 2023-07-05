library(dplyr)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(caret)


pathWD <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWD)
source("FuncCrabs.R")
source("FuncWine.R")
source("VSCMN.R")

pathProcessDf <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_Crabs/"
setwd(pathProcessDf)

# Eta 5

load(paste0(pathProcessDf,"A0.8_0.8_E5_5_Crabs.Rdata"))

TrainDB <- auxSim$Train[[1]]
colnames(TrainDB)
TrainXDB <- TrainDB %>% dplyr::select(-c(index,class,Cont))
TrainDB$sex <- factor(TrainDB$class)
levels(TrainDB$sex) <- c("M","F")
TrainDB$sex
TrainDB$Cont <- factor(TrainDB$Cont)



mycols <- c("blue","green")
pairs(TrainXDB %>% dplyr::select(CL,RW), oma = c(3,3,6,3),
      col = mycols[as.numeric(TrainDB$class)],
      pch = c(19,2)[as.numeric(TrainDB$Cont)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(TrainDB$sex),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)
legend("top", pch = c(19,2), legend = levels(TrainDB$Cont), col = "black",
       xpd = NA, ncol = 3, bty = "n", inset = -0.03)



load(paste0(pathProcessDf,"A0.8_0.8_E10_10_Crabs.Rdata"))

TrainDB <- auxSim$Train[[1]]
colnames(TrainDB)
TrainXDB <- TrainDB %>% dplyr::select(-c(index,class,Cont))
TrainDB$sex <- factor(TrainDB$class)
levels(TrainDB$sex) <- c("M","F")
TrainDB$sex
TrainDB$Cont <- factor(TrainDB$Cont)



mycols <- c("blue","green")
pairs(TrainXDB %>% dplyr::select(CL,RW), oma = c(3,3,6,3),
      col = mycols[as.numeric(TrainDB$class)],
      pch = c(19,2)[as.numeric(TrainDB$Cont)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(TrainDB$sex),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)
legend("top", pch = c(19,2), legend = levels(TrainDB$Cont), col = "black",
       xpd = NA, ncol = 3, bty = "n", inset = -0.03)



load(paste0(pathProcessDf,"A0.8_0.8_E15_15_Crabs.Rdata"))

TrainDB <- auxSim$Train[[1]]
colnames(TrainDB)
TrainXDB <- TrainDB %>% dplyr::select(-c(index,class,Cont))
TrainDB$sex <- factor(TrainDB$class)
levels(TrainDB$sex) <- c("M","F")
TrainDB$sex
TrainDB$Cont <- factor(TrainDB$Cont)



mycols <- c("blue","green")
pairs(TrainXDB %>% dplyr::select(CL,RW), oma = c(3,3,6,3),
      col = mycols[as.numeric(TrainDB$class)],
      pch = c(19,2)[as.numeric(TrainDB$Cont)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(TrainDB$sex),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)
legend("top", pch = c(19,2), legend = levels(TrainDB$Cont), col = "black",
       xpd = NA, ncol = 3, bty = "n", inset = -0.03)

