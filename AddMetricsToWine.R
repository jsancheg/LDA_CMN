library(dplyr)
library(tidyr)
library(tidyverse)
library(gclus)
library(RColorBrewer)
library(tictoc)


pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"

data("wine")
colnames(wine)
table(wine$Type)

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




eta_values <- c(5,10,15)
alpha_values <-c(0.75,0.8,0.85)
vpi <- as.vector(as.vector(table(wine$Type)/length(wine$Type)))

etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))



name_file<-list()
ind_file <- list()
cont <- 1
for(i_a in 1:nrow(alphaM))
  for(i_eta in 1:nrow(etaM))
  {
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    name_file[[cont]] <- paste0("A",name_alpha,"_E",name_eta,"_Wine.Rdata")
    ind_file[[cont]] <-which(str_detect(dir(pathWd),name_file[[cont]])==TRUE)
    cont <- cont + 1
  }

name_files <- unlist(name_file)
ind_files <- unlist(ind_file)
i_files <- length(name_files)
Metrics_Res <- list()
Metrics_Models <- list()

name_files1 <- name_files %>% str_replace_all("Rdata","")
name_files1<- name_files1 %>% str_replace_all("\\.","")

length(name_files)
name_files[49]
name_files[300:359]


name_files1[49]
name_files1[50]

# for to read all the files
for (i in ind_files[-1])
{  
  load(name_files[i])
  #load("A08_08_075_E5_5_5_Wine.Rdata")
  # for to read all the train and test data
  j_Sim <- length(auxSim$Train)
  specificity_SatM <- rep(0,j_Sim)
  sensitivity_SatM <- rep(0,j_Sim)
  specificity_SelM <- rep(0,j_Sim)
  sensitivity_SelM <- rep(0,j_Sim)
  
  for(j in 1:j_Sim)
  {
    
    DfTrain <- auxSim$Train[[j]]
    DfTrainX <- DfTrain %>% dplyr::select(-c(index,class,Cont))
    DfTrainl <- DfTrain$class
    DfTest <- auxSim$Test[[j]]
    DfTestX <- DfTest %>% dplyr::select(-c(index,class,Cont))
    DfTestl <- DfTest$class
    
    variables_selected <- unlist(auxSim$Metrics_models$Model[[j]] %>% str_split("-"))
    
    colnames(DfTest)
    
    dfRW <- getOW(DfTrainX,DfTrainl)
    RW <- dfRW$Var
    variables_saturated_model <- RW
    
    # model including all variables
    saturated_mod  <- ModelAccuracy2(DfTrainX,
                                     DfTestX,
                                     DfTrainl,
                                     DfTestl,"EII",
                                     alpharef = 0.98, 
                                     tol = 0.01,iterations = 10)
    
    DfTrainXSel <- DfTrainX %>% dplyr::select(variables_selected)
    DfTestXSel <- DfTestX %>% dplyr::select(variables_selected)
    colnames(DfTrainXSel)
    
    saturated_mod
    
    # saturated_mod$predv
    # 1 non contaminated
    # 0 contaminated
    
    
    # pred_cont_Sat_M
    # 0 non contaminated
    # 1 contaminated
    pred_cont_Sat_M <- 1- sapply(1:length(DfTestl), function(k) { 
      saturated_mod$predv[k,DfTestl[k]] } )
    
    
    
    selected_mod <- ModelAccuracy2(DfTrainXSel,
                                   DfTestXSel,
                                   DfTrainl,
                                   DfTestl,"EII",
                                   alpharef = 0.98, 
                                   tol = 0.01, iterations = 10)
    
    
    selected_mod$accTestC
    
    # selected_mod$predv
    # 1 non contaminated
    # 0 contaminated
    
    
    # pred_cont_Sel_M
    # 0 non contaminated
    # 1 contaminated
    pred_cont_Sel_M <- 1- sapply(1:length(DfTestl), function(k) { 
      selected_mod$predv[k,DfTestl[k]] } )
    
    sat_mod_cfm <- matrix(0,ncol = 2, nrow = 2)
    sel_mod_cfm <- matrix(0,ncol = 2, nrow = 2)
    
    if (all(pred_cont_Sat_M == 1))
    {
      specificity_SatM[j] <- 0
      Sensitivity_SatM[j] <- sum(DfTest$Cont==1)/sum(pred_cont_Sat_M==1)
      
    }else if(all(pred_cont_Sat_M == 0))
    {
      specificity_SatM[j] <- sum(DfTest$Cont==0)/sum(pred_cont_Sat_M==0)
      sensitivity_SatM[j] <- 0
      
    }else {
      sat_mod_cfm <- table(DfTest$Cont,pred_cont_Sat_M)
      sensitivity_SatM[j] <- sensitivity(sat_mod_cfm)
      specificity_SatM[j] <- specificity(sat_mod_cfm)
      
    }
    
    if (all(pred_cont_Sel_M == 1))
    {
      specificity_SelM[j] <- 0
      Sensitivity_SelM[j] <- sum(DfTest$Cont==1)/sum(pred_cont_Sel_M==1)
      
    }else if(all(pred_cont_Sel_M == 0))
    {
      
      specificity_SelM[j] <- sum(DfTest$Cont==0)/sum(pred_cont_Sel_M==0)
      sensitivity_SelM[j] <- 0
      
    }else {
      sel_mod_cfm <- table(DfTest$Cont,pred_cont_Sel_M)
      
      
      sensitivity_SelM[j] <- sensitivity(sel_mod_cfm)
      specificity_SelM[j] <- specificity(sel_mod_cfm)
      
    }
    
    
    cat("\n -- ",name_files[i], "-- Simulation -- ", j , " processed \n")
  }
  
  auxDf <- auxSim$Metrics_models
  auxDf$Sensitivity_SatM <- sensitivity_SatM
  auxDf$Specificity_SatM <- specificity_SatM
  auxDf$Sensitivity_SelM <- sensitivity_SelM
  auxDf$Specificity_SelM <- specificity_SelM
  
  Metrics_Res[[i_files]] <- auxDf$Metrics_res
  Metrics_Models[[i_files]]<-  auxDf$Metrics_models
  saveRDS(auxDf,paste0(name_files1[i],".RDS"))
  
}





dfRW <- getOW(XBlueCrabs,BlueCrabs$sex)
RW <- dfRW$Var
variables_saturated_model <- RW


i_file <- 1
aux <- list()
for( i_file in 1:length(name_files1))
{
  auxDf <- readRDS(paste0(name_files1[i_file],".RDS"))
  auxDf$file <- name_files1[i_file]
  auxDf <- auxDf %>% dplyr::select(file,everything())
  aux[[i_file]] <- auxDf
  
}
