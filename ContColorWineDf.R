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

pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("LoadWine.R")
source("VSCMN.R")
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"


source(paste0(pathWd,"FunctionsConsolidate.R"))

pathOutput <-"E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_WineContColor/"

data(wine)
colnames(wine)

Xwine <- wine %>% subset(select = -c(Class))
colnames(Xwine)
y <- as.numeric(wine$Class)
p <- ncol(Xwine)
G <- length(unique(y))

ng <- table(y)

lab <- 1:G
alpha_values <- c(0.75,0.8,0.85)
eta_values <- c(5,10,15)
ptrain <- c(0.7,0.7,0.7)


vpi <-  as.vector(as.vector(table(wine$Class)/length(wine$Class)))


alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))
etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
nameDf <- "WineContColor"

alphaM
etaM

size_sets <- fun_CalNcont(G,ng,ptrain,alphaM[1,])



pattern <- "A[\\d]+_[\\d]+_[\\d]+_E[\\d]+_[\\d]+_[\\d]+_Wine"


# call function that run simulations only contaminating one variable in this case 
# the variable Colour



cont_vars <- "Color"

# Run simulations of contaminated samples
ContSimulationsVars(pathOutput,nameDf,Xwine,y,lab,vpi,alphaM,etaM,ptrain,cont_vars ,ns = 10)



summary(factor(wine$Class))/nrow(wine)
p <- ncol(wine)

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

dfRW <- getOW(Xwine,as.numeric(y))
head(dfRW,6)

dfRWsort <- dfRW[order(-dfRW$Ftest),]


options(repr.plot.width=8, repr.plot.height=3)
ggplot(dfRWsort, aes(x = reorder(Var, +Ftest),y = Ftest) ) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_continuous(name="Variables") +
  scale_x_discrete(name="F score") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))


colnames(Xwine)
table(wine$Type)/length(wine$Type)
as.vector(as.vector(table(wine$Type)/length(wine$Type)))

alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)
vpi <- as.vector(as.vector(table(wine$Type)/length(wine$Type)))



eta_values <- c(5,10,15)
alpha_values <-c(0.75,0.8,0.85)

etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))


paste(eta, collapse = "_")

file_names <- list()
cont <- 1
for(i_a in 1:nrow(alphaM))
  for(i_eta in 1:nrow(etaM))
  {
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    file_names[[cont]]<- paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Wine.Rdata")
    #    file_names[[cont]]<- paste0("A",name_alpha,"_E",name_eta,"_Wine.Rdata")
    
    cont <- cont + 1
  }

str_detect(unlist(file_names), "A0.8_0.75_0.75_E10_15_10_Wine.Rdata")


tic("simulation")
for(i_a in 1:4)
  for(i_eta in 1:nrow(etaM))
  {

#    auxSim <- contDf (Xwine,y,lab,vpi,alphaM[i_a,],etaM[i_eta,],ptrain,ns = 10)
    contSamples <- vector("list",length = G)
    
    if(G >1)
    {
      for( g in 1:G)
      {
          SimSamples =  rMVNorm(ncontg[g],mug[,g],sg[,,g])
          aux =  rnorm(ncontg[g],mug[10,g],etaM[i_eta]*sg[10,10,g] )
          SimSamples[,10] <- aux
          contSamples[[g]] = SimSamples
        
      }
      
    }else if(G == 1) 
      {
        SimSamples =  rMVNorm(ncontg[g],mug[,g],sg[,,g])
        aux =  rnorm(ncontg[g],mug[10,g],etaM[i_eta]*sg[10,10] )
        SimSamples[,10] <- aux
        contSamples[[g]] = SimSamples
      
      }
    
    
    
    XC <- ldply(contSamples)
    
    
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    name_file <- paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Wine.Rdata")
    save(auxSim,file = name_file)
    cat("\n -- saving ", name_file,"---\n")
  }
toc()
