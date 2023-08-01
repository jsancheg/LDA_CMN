

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

pathWd <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source(paste0(pathWd,"FunctionsConsolidate.R"))
source("FuncCrabs.R")
source("VSCMN.R")

pathWd <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"



pathOutput <-"E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_CrabsNew/"

source("ReadCrabsDf.R")

BlueCrabs <- CrabsDf %>% filter(sp == "B")
XBlueCrabs <- BlueCrabs %>% subset(select = c(FL, RW, CL, CW, BD))
y <- as.numeric(BlueCrabs$sex)
p <- ncol(XBlueCrabs)
vpi <- as.vector(as.vector(table(BlueCrabs$sex)/length(BlueCrabs$sex)))

lab <- c(1:2)
ptrain <- c(0.7,0.7)
eta_values <- c(5,10,15)
alpha_values <-c(0.75,0.8,0.85)
list_files <- list()


etaM <- as.matrix(expand.grid(eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values))
nameDf <- "Crabs"

pattern <- "A[\\d]+_[\\d]+_E[\\d]+_[\\d]+_Crabs"

# run this line to generate the contaminated datasets
ContSimulations(pathOutput,nameDf,XBlueCrabs,y,lab,vpi,alphaM,etaM,ptrain,ns = 10)


dfAll <- Summarise_Files(pathOutput,nameDf,pattern,alphaM,etaM)
nrow(dfAll)

