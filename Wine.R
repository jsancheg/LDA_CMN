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
library(gclus)

pathWd <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("LoadWine.R")
source("VSCMN.R")
pathWd <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"

source(paste0(pathWd,"FunctionsConsolidate.R"))

pathOutput <-"E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_WineNew/"

data(wine)
colnames(wine)
Xwine <- wine %>% subset(select = -c(Type))
colnames(Xwine)
y <- as.numeric(wine$Type)
p <- ncol(Xwine)
G <- length(unique(y))

lab <- 1:G
alpha_values <- c(0.75,0.8,0.85)
eta_values <- c(5,10,15)
vpi <-  as.vector(as.vector(table(wine$Type)/length(wine$Type)))
ptrain <- c(0.7,0.7,0.7)

alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))
etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
nameDf <- "Wine"

pattern <- "A[\\d]+_[\\d]+_[\\d]+_E[\\d]+_[\\d]+_[\\d]+_Wine"





# Run simulations of contaminated samples
ContSimulations(pathOutput,nameDf,Xwine,y,lab,vpi,alphaM,etaM,ptrain,c("Color"),ns = 10)


dfAll<- Summarise_Files(pathOutput, nameDf, pattern, alphaM, etaM)
ncol(alphaM)

