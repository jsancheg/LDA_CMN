# Greedy Search version

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
source("Semisupervised.R")

pathWd <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"



pathOutput <-"E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/ProcCrabs_GS/"

source("ReadCrabsDf.R")

data(crabs)
CrabsDf <- crabs

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

pathwd <- getwd()

pathOutput <- paste0(pathwd,"/s_crabs/")

nfiles <-length(dir(pathOutput))

scenarios_crabs <- dir(pathOutput)
i <- 1
dfs <- list()
for (i in 1:nfiles)
{
    filename <- scenarios_crabs[i]
    df <- readRDS(paste0(pathOutput,filename))
    df$source <- filename
    
    dfs[[i]] <- df
}

combine_Df <- do.call(rbind,dfs)

saveRDS(combine_Df,paste0(pathwd,"/Crabs_Metrics_25_03_2024.RDS") )



dfAll <- Summarise_Files(pathOutput,nameDf,pattern,alphaM,etaM)
nrow(dfAll)

