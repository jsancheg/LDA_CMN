library(dplyr)
library(tidyr)
library(tidyverse)
library(gclus)
library(RColorBrewer)

pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")

pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
pathOutput <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_wdbc/"

data("wdbc")
Xwdbc <- wdbc %>% subset(select = -c(ID, Diagnosis))
y <- as.numeric(wdbc$Diagnosis)
p <- ncol(Xwdbc)
G <- length(unique(y))


lab <- 1:G
alpha_values <- c(0.75,0.8,0.85)
eta_values <- c(5,10,15)
ptrain <- c(0.7,0.7)
vpi <- as.vector(as.vector(table(wdbc$Diagnosis)/length(wdbc$Diagnosis)))

etaM <- as.matrix(expand.grid(eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values))
nameDf <- "Wdbc"

pattern <- "A[\\d]+_[\\d]+_E[\\d]+_[\\d]+_Wdbc"

# Run simulations of contaminated samples
ContSimulations(pathOutput,nameDf,Xwine,y,lab,vpi,alphaM,etaM,ptrain,ns = 10)

dfAll<- Summarise_Files(pathOutput, nameDf, pattern, alphaM, etaM)
ncol(alphaM)


