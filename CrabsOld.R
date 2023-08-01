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


pathProcessDf <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathProcessDf)
source("FuncCrabs.R")
source("FuncWine.R")
source("VSCMN.R")

pathProcessDf <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathProcessDf)
pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data"
pathProCrabs <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_Crabs/"

#work_path <- "E:/University of Glasgow/Literature review/R Code/"
#setwd(work_path)

# read the file 
CrabsDf <- read.csv(paste0(pathFile,"/","Crabs.csv"))
CrabsDf$sex <- as.factor(CrabsDf$sex)
head(CrabsDf)
BlueCrabs <- CrabsDf %>% filter(sp == "B")
XBlueCrabs <- BlueCrabs %>% subset(select = c(FL, RW, CL, CW, BD))
head(XBlueCrabs)

XBlueCrabs %>% as.matrix %>% cor %>% corrplot::corrplot(method = "circle")
?corrplot


colnames(CrabsDf)

mycols <- c("cornflowerblue","salmon")
pairs(CrabsDf%>%dplyr::select(c(FL,RW,CL, CW,BD)), oma = c(3,3,6,3),
col = mycols[as.numeric(factor(CrabsDf$sp))],
pch = c(19,1)[as.numeric(CrabsDf$sex)],
gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(factor(CrabsDf$sp)),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)
legend("top", pch = c(19,1), legend = levels(CrabsDf$sex), col = "black",
       xpd = NA, ncol = 3, bty = "n", inset = -0.03)



y <- as.numeric(BlueCrabs$sex)
ind_fem <- y=="F"



meanBlueCrabsF <- XBlueCrabs[ind_fem,] %>% apply(2,mean)
meanBlueCrabsM <- XBlueCrabs[!ind_fem,] %>% apply(2,mean)
p <- length(meanBlueCrabsF)

GBlueCrabsF <- XBlueCrabs[ind_fem,] %>% var
GBlueCrabsM <- XBlueCrabs[!ind_fem,] %>% var

mug <- matrix(0.0, nrow = p, ncol = 2)

mug[,1] <- meanBlueCrabsM
mug[,2] <- meanBlueCrabsF

vpi <- c(0.5,0.5)
ptrain <- c(0.7,0.7)

lab <-c("F","M")
# 1 - F
# 2 - M
BlueCrabs$sex
as.numeric(BlueCrabs$sex)

sg <- array(0.0, dim = c(p,p,2))
sg[,,1]= GBlueCrabsM
sg[,,2]= GBlueCrabsF



vpi <- as.vector(as.vector(table(BlueCrabs$sex)/length(BlueCrabs$sex)))
vpi


eta_values <- c(5,10,15)
alpha_values <-c(0.75,0.8,0.85)

etaM <- as.matrix(expand.grid(eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values))


tic("simulation")
for(i_a in 1:nrow(alphaM))
  for(i_eta in 1:nrow(etaM))
  {
    auxSim <- contDf (XBlueCrabs,y,lab,vpi,alphaM[i_a,],etaM[i_eta,],ptrain,ns = 10)
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    name_file <- paste0("A",name_alpha,"_E",name_eta,"_Crabs.Rdata")
    save(auxSim,file = name_file)
    cat("\n -- saving ", name_file,"---\n")
  }
toc()

name_file<-list()
cont <- 1
for(i_a in 1:nrow(alphaM))
  for(i_eta in 1:nrow(etaM))
  {
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    name_file[[cont]] <- paste0("A",name_alpha,"_E",name_eta,"_Crabs.Rdata")
    cont <- cont + 1
  }

name_files <- unlist(name_file)
i_files <- length(name_files)
Metrics_Res <- list()
Metrics_Models <- list()

name_files1 <- name_files %>% str_replace_all("Rdata","")
name_files1<- name_files1 %>% str_replace_all("\\.","")

name_files1[49]
name_files1[50]

# for to read all the files
for (i in 50:i_files)
{  
  load(name_files[i])
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
                                   tol = 0.01)

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
                               tol = 0.01)


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
  cat("\n -- Procesando ", name_files1[i_file])
  auxDf <- readRDS(paste0(pathProCrabs, name_files1[i_file],".RDS"))
  auxDf$file <- name_files1[i_file]
  auxDf <- auxDf %>% dplyr::select(file,everything())
  aux[[i_file]] <- auxDf

}
  auxDf <- ldply(aux)
  auxDf$file <- factor(auxDf$file)
  summary(auxDf$file)
  auxDf$Eta <- "Inequal"
  auxDf$Alpha <- "Inequal"
  findFile_Alpha <- c("A075_075_E5_5_Crabs","A075_075_E5_10_Crabs","A075_075_E5_15_Crabs",
                "A075_075_E10_5_Crabs","A075_075_E10_10_Crabs","A075_075_E10_15_Crabs",
    "A075_075_E15_5_Crabs","A075_075_E15_10_Crabs","A075_075_E15_15_Crabs",
    "A08_08_E5_5_Crabs","A08_08_E5_10_Crabs","A08_08_E5_15_Crabs",
    "A08_08_E10_5_Crabs","A08_08_E10_10_Crabs","A08_08_E10_15_Crabs",
    "A08_08_E15_5_Crabs","A08_08_E15_10_Crabs","A08_08_E15_15_Crabs",
    "A085_085_E5_5_Crabs","A085_085_E5_10_Crabs","A085_085_E5_15_Crabs",
    "A085_085_E10_5_Crabs","A085_085_E10_10_Crabs","A085_085_E10_15_Crabs",
    "A085_085_E15_5_Crabs","A085_085_E15_10_Crabs","A085_085_E15_15_Crabs")
  ind_alpha<-rep(0,length(findFile_Alpha))
  for(i in 1:length(findFile_Alpha))
  {
    fila <- str_detect(auxDf$file,findFile_Alpha[i])
    auxDf[fila,]$Alpha <- "Equal"
  }
  
  findFile_Eta <- c("A075_075_E5_5_Crabs","A075_08_E5_5_Crabs","A075_085_E5_5_Crabs",
                    "A08_075_E5_5_Crabs","A08_08_E5_5_Crabs","A08_085_E5_5_Crabs",
                    "A085_075_E5_5_Crabs","A085_08_E5_5_Crabs","A085_085_E5_5_Crabs",
                    "A075_075_E10_10_Crabs","A075_08_E10_10_Crabs","A075_085_E10_10_Crabs",
                    "A08_075_E10_10_Crabs","A08_08_E10_10_Crabs","A08_085_E10_10_Crabs",
                    "A085_075_E10_10_Crabs","A085_08_E10_10_Crabs","A085_085_E10_10_Crabs",
                    "A075_075_E15_15_Crabs","A075_08_E15_15_Crabs","A075_085_E15_15_Crabs",
                    "A08_075_E15_15_Crabs","A08_08_E15_15_Crabs","A08_085_E15_15_Crabs",
                    "A085_075_E15_15_Crabs","A085_08_E15_15_Crabs","A085_085_E15_15_Crabs")
    
  ind_eta<-rep(0,length(findFile_Eta))
  for(i in 1:length(findFile_Eta))
  {
    fila <- str_detect(auxDf$file,findFile_Eta[i])
    auxDf[fila,]$Eta <- "Equal"
  }
   auxDf$Alpha <- factor(auxDf$Alpha)
   auxDf$Eta <- factor(auxDf$Eta)
   
   summary(auxDf$Alpha)
   summary(auxDf$Eta)
   

   dfAll <- auxDf
   nModels <- length(dfAll$Model)
   ModelSize<-rep(0,nModels)
   for(i_model in 1:nModels)
   {
     ModelSize[i_model] <- length(unlist(str_split(dfAll$Model[i_model],"-")))
   }
   
   df_resumen <- mutate(dfAll,Model1 = Model)
   df_resumen <- df_resumen %>% relocate(Model1, .after = Model)
   colnames(df_resumen)
   df_resumen <- df_resumen %>% relocate(Model, .after = Model1)
   colnames(df_resumen)
   
   #  df_resumen <- rename(df_resumen,Model1 = Model)
   
   
   labels<-df_resumen$Model1
   source("FunctionsConsolidate.R")
   
   find_unique_labels(df_resumen$Model1)
   
   df_resumen$Model <- find_unique_labels(df_resumen$Model1)$newlabels
   head(df_resumen)
   
   colnames(df_resumen)
   metrics_res <- df_resumen %>% group_by(Model) %>%
     summarise(CCR_SatMNc = mean(CR_SatMNc),
               CCR_SatMC = mean(CR_SatMC),
               CCR_SV = mean(CR_SV),
               Accuracy_Sat = mean(Accuracy_SatCont),
               Accuracy_SV = mean(Accuracy_SVCont),
               Sensitivity_SatM = mean(Sensitivity_SatM),
               Sensitivity_SelM = mean(Sensitivity_SelM),
               Specificity_SatM = mean(Sensitivity_SelM),
               Specificity_SelM = mean(Specificity_SelM))
   
   
   tab1 <- table(df_resumen$Model)
   my_tab1_sort <- tab1[order(tab1, decreasing = TRUE)]
   
   round(prop.table(my_tab1_sort),2)
   cumsum(   round(prop.table(my_tab1_sort),2))
   
   
   df_models <- data.frame(frequency = as.vector(my_tab1_sort),
                           model = rownames(my_tab1_sort))
   
   plt_boxplotmodels <- ggplot(df_models) +
     geom_col(aes(reorder(model,-frequency),frequency),fill = "#076fa2", width = 0.6)
   
   # 8 first models represents the 80% 8f the 810 simulations
   sum(tab1)
   plt_boxplotmodels
   df_models_topn <- df_models %>% top_n(8,df_models$frequency)
   nrow(df_models_topn)

   
      ggp <- ggplot(df_models_topn, 
                 aes(x = reorder(model,+frequency), y = frequency)) +
     geom_bar(stat = "identity", fill = "lightblue") +
     coord_flip()+
     ylab("Model") + xlab("Frequency") +
     geom_text(aes(x = model, y = frequency + 0.3, label = frequency),check_overlap = TRUE)
   ggp
   
   head(df_models)
   library(plotly)
   ggp %>% ggplotly
   
   
   metrics_res %>% filter(Model %in% c("BD-CL-FL-CW-RW","CL-RW",
                                       "BD-CL-RW", "BD-CL-CW-RW",
                                       "FL-CL-RW",
                                       "CL-CW-FL-RW",
                                       "BD-CL-FL-RW"))
   
   
      
   
   freqModelSize <- table(ModelSize)
   freqModelSize  
   coul <- brewer.pal(4,"Set2")
   
   par(mfrow = c(1,2))
   barplot(prop.table(freqModelSize),col = coul,ylim = c(0,0.4))
   
   
   freqVar <- table(unlist(str_split(dfAll$Model,"-")))
   coul <- brewer.pal(5,"Set2")
   
   barplot(prop.table(freqVar),col = coul, ylim = c(0,0.4))
   
   par(mfrow = c(1,1))
    
   colnames(dfAll)
   dfAll <- dfAll %>% mutate (DifCCR = CR_SV - CR_SatMC)
   dfAll <- dfAll %>% mutate (DifAccuracy = Accuracy_SVCont - Accuracy_SatCont  )
   dfAll <- dfAll %>% mutate (DifSensitivity = Sensitivity_SelM - Sensitivity_SatM  )
   dfAll <- dfAll %>% mutate (DifSpecificity = Specificity_SelM - Specificity_SatM  )
     
   summary(dfAll$DifAccuracy)
   summary(dfAll$DifCCR)
   
   summary(dfAll$DifSensitivity)
   summary(dfAll$DifSpecificity)
   
   dfDifLong<- dfAll %>% dplyr::select(Alpha,Eta,DifCCR,DifAccuracy,
                                       DifSensitivity,DifSpecificity) %>% 
     pivot_longer(c(DifCCR,DifAccuracy,DifSensitivity,DifSpecificity),
                  names_to = "Variables",
                  values_to = "Dif")
   
   head(dfDifLong)
   dfDifLong <- dfDifLong %>% mutate(Variables = recode(Variables,
                                                        DifCCR = "CCR",
                                                        DifAccuracy = "Accuracy",
                                                        DifSensitivity = "Sensitivity",
                                                        DifSpecificity = "Specificity"))
   
   g1<- ggplot(dfDifLong %>% filter(Variables %in% c("CCR","Accuracy")), 
          aes(x = Variables, y = Dif, color = Alpha)) + 
     ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot()+
     geom_hline(yintercept = 0)
   
   g2 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
          aes(x = Variables, y = Dif, color = Alpha)) + 
     ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,0.7) + geom_boxplot() + 
     geom_hline(yintercept = 0)
      
   g3 <- ggplot(dfDifLong %>% filter(Variables %in% c("CCR","Accuracy")), 
          aes(x = Variables, y = Dif, color = Eta)) + 
     ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot() +
     geom_hline(yintercept = 0)
   
   g4 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
          aes(x = Variables, y = Dif, color = Eta)) + 
     ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,0.7) + geom_boxplot() + 
     geom_hline(yintercept = 0)
   
   
   

   
   
   
   
   sA8020 <- contDf (XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 100)
save(sA8020,"sA80_E20_BAL.RData")


dfA80E20<-sA8020$Metrics_models
dfA80E20$alpha <- "Equal"
dfA80E20$eta <- "Equal"
saveRDS(dfA80E20,"A80E20.RDS")

vpi <- c(0.5,0.5)
alpha <- c(0.8,0.8)
eta <- c(20,5)
ptrain <- c(0.8,0.8)


sA80E20_5<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 100)

dfA80E20_5<-sA80E20_5$Metrics_models
dfA80E20_5$alpha <- "Equal"
dfA80E20_5$eta <- "Inequal"
dfA80E20_5$proportion <- "Balanced"
saveRDS(dfA80E20_5,"A80E20_5.RDS")


vpi <- c(0.5,0.5)
alpha <- c(0.8,0.8)
eta <- c(5,20)
ptrain <- c(0.8,0.8)


sA80E5_20<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 100)

dfA80E5_20<-sA80E5_20$Metrics_models
dfA80E5_20$alpha <- "Equal"
dfA80E5_20$eta <- "Inequal"
dfA80E5_20$proportion <- "Balanced"
saveRDS(dfA80E5_20,"A80E5_20.RDS")


vpi <- c(0.5,0.5)
alpha <- c(0.9,0.8)
eta <- c(20,20)
ptrain <- c(0.8,0.8)


sA90_80E20<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA90_80E20 <- sA90_80E20$Metrics_models
dfA90_80E20$alpha <- "Inequal"
dfA90_80E20$eta <- "Equal"
dfA90_80E20$proportion <-"Balanced"
saveRDS(dfA90_80E20,"A90_80E20.RDS")


vpi <- c(0.5,0.5)
alpha <- c(0.8,0.9)
eta <- c(20,20)
ptrain <- c(0.8,0.8)


sA80_90E20<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA80_90E20 <- sA90_80E20$Metrics_models
dfA80_90E20$alpha <- "Inequal"
dfA80_90E20$eta <- "Equal"
dfA80_90E20$proportion <-"Balanced"
saveRDS(dfA80_90E20,"A80_90E20.RDS")


vpi <- c(0.7,0.3)
alpha <- c(0.8,0.8)
eta <- c(20,20)
ptrain <- c(0.8,0.8)


sA80_E20_UNBAL<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfA80_E20_UNBAL <- sA80_E20_UNBAL$Metrics_models
dfA80_E20_UNBAL$alpha <- "Equal"
dfA80_E20_UNBAL$eta <- "Equal"
dfA80_E20_UNBAL$proportion <-"Unbalanced"
saveRDS(dfA80_E20_UNBAL,"A80_E20_UNBAL.RDS")






vpi <- c(0.7,0.3)
alpha <- c(0.9,0.8)
eta <- c(20,20)
ptrain <- c(0.8,0.8)

sANEQ_EEQ_UNBAL<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfANEQ_EEQ_UNBAL<- sANEQ_EEQ_UNBAL$Metrics_models
dfANEQ_EEQ_UNBAL$alpha <- "Inequal"
dfANEQ_EEQ_UNBAL$eta <- "Equal"
dfANEQ_EEQ_UNBAL$proportion <- "Unbalanced"
saveRDS(dfANEQ_EEQ_UNBAL,"ANEQ_EEQ_UNBAL.RDS")


vpi <- c(0.7,0.3)
alpha <- c(0.8,0.9)
eta <- c(20,20)
ptrain <- c(0.8,0.8)

sA80_90_EEQ_UNBAL<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfANEQ_EEQ_UNBAL<- sA80_90_EEQ_UNBAL$Metrics_models
dfANEQ_EEQ_UNBAL$alpha <- "Inequal"
dfANEQ_EEQ_UNBAL$eta <- "Equal"
dfANEQ_EEQ_UNBAL$proportion <- "Unbalanced"
saveRDS(dfANEQ_EEQ_UNBAL,"ANEQ_EEQ_UNBAL.RDS")




vpi <- c(0.7,0.3)
alpha <- c(0.8,0.8)
eta <- c(20,5)
ptrain <- c(0.8,0.8)

sA80_ENEQ_UNBAL<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfAEQ_ENEQ_UNBAL<- sA80_ENEQ_UNBAL$Metrics_models
dfAEQ_ENEQ_UNBAL$alpha <- "Equal"
dfAEQ_ENEQ_UNBAL$eta <- "Equal"
dfAEQ_ENEQ_UNBAL$proportion <- "Unbalanced"
saveRDS(dfAEQ_ENEQ_UNBAL,"AEQ_ENEQ_UNBAL.RDS")


vpi <- c(0.7,0.3)
alpha <- c(0.8,0.8)
eta <- c(5,20)
ptrain <- c(0.8,0.8)

sA80_E5_20_UNBAL<-contDf(XBlueCrabs[,-1],y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfAEQ_E5_20_UNBAL<- sA80_E5_20_UNBAL$Metrics_models
dfAEQ_E5_20_UNBAL$alpha <- "Equal"
dfAEQ_E5_20_UNBAL$eta <- "Equal"
dfAEQ_E5_20_UNBAL$proportion <- "Unbalanced"
saveRDS(dfAEQ_E5_20_UNBAL,"AEQ_E5_20_UNBAL.RDS")




# Read RDS ----------------------------------------------------------------
dfAEQ_EEQ <- readRDS(paste0(pathProcessDf,"A80E20.RDS") )

dfAEQ_ENEQ <- readRDS(paste0(pathProcessDf,"A80E20_5.RDS") )

dfANEQ_EEQ <-readRDS(paste0(pathProcessDf,"A90_80E20.RDS"))

dfAEQ_EEQ_UNBAL <-readRDS(paste0(pathProcessDf,"A80_E20_UNBAL.RDS"))

dfANEQ_EEQ_UNBAL <-readRDS(paste0(pathProcessDf,"ANEQ_EEQ_UNBAL.RDS"))

dfAEQ_ENEQ_UNBAL <-readRDS(paste0(pathProcessDf,"AEQ_ENEQ_UNBAL.RDS"))

dfAll <- rbind.data.frame(dfAEQ_EEQ[1:10,],
                          dfAEQ_ENEQ[1:10,],
                          dfANEQ_EEQ,
                          dfAEQ_EEQ_UNBAL,
                          dfANEQ_EEQ_UNBAL,
                          dfAEQ_ENEQ_UNBAL)

freqVar <- table(unlist(str_split(dfAll$Model,"-")))
coul <- brewer.pal(5,"Set2")

barplot(prop.table(freqVar),col = coul)

colnames(dfAll)
dfAll <- dfAll %>% mutate (DifCCR = CR_SV - CR_SatMC)
dfAll <- dfAll %>% mutate (DifAccuracy = Accuracy_SVCont- Accuracy_SatCont  )


dfDifLong<- dfAll %>% dplyr::select(alpha,eta,proportion,DifCCR,DifAccuracy) %>% 
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


ggplot(dfDifLong, aes(x = Variables, y = Dif, color = proportion)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.1,0.2) + geom_boxplot()

# Summarise simulations ---------------------------------------------------

dfAll <- rbind.data.frame(dfA80E20,
                          dfA80E20_5,
                          dfA90_80E20)
colnames(dfAll)
dfAll <- dfAll %>% mutate (DifCCR = CR_SV - CR_SatMC)
dfAll <- dfAll %>% mutate (DifAccuracy = Accuracy_SVCont- Accuracy_SatCont  )

freqVar <- table(unlist(str_split(dfAll$Model,"-")))
coul <- brewer.pal(5,"Set2")

barplot(prop.table(freqVar),col = coul)

colnames(dfAll)


dfDifLong<- dfAll %>% dplyr::select(alpha,eta,DifCCR,DifAccuracy) %>% 
  pivot_longer(c(DifCCR,DifAccuracy),
               names_to = "Variables",
               values_to = "Dif")

head(dfDifLong)
dfDifLong <- dfDifLong %>% mutate(Variables = recode(Variables,
                                                   DifCCR = "CCR",
                                                   DifAccuracy = "Accuracy"))


ggplot(dfDifLong, aes(x = Variables, y = Dif, color = eta)) + geom_boxplot()


ggplot(dfDifLong, aes(x = Variables, y = Dif, color = alpha)) + geom_boxplot()


dfCRLong<- dfAll %>% dplyr::select(alpha,eta,CR_SatMC,CR_SV) %>% 
  pivot_longer(c(CR_SatMC,CR_SV),
               names_to = "Variables",
               values_to = "CCR")
head(dfCRLong)
dfCRLong <- dfCRLong %>% mutate(Variables = recode(Variables,
                                                   CR_SatMC = "All",
                                                   CR_SV = "Selected"))


ggplot(dfCRLong, aes(x = Variables, y = CCR, color = eta)) + geom_boxplot()


dfAccLong<- dfAll %>% dplyr::select(alpha,eta,Accuracy_SatCont,Accuracy_SVCont) %>% 
  pivot_longer(c(Accuracy_SatCont,Accuracy_SVCont),
               names_to = "Variables",
               values_to = "Accuracy")

dfAccLong <- dfAccLong %>% mutate(Variables = recode(Variables,
                                                     Accuracy_SatCont = "All",
                                                     Accuracy_SVCont = "Selected"))

ggplot(dfAccLong, aes(x = Variables, y = Accuracy, color = eta)) + geom_boxplot()


head(dfA80E20Long)


sA80E20$Metrics_res  
sA80E20$Metrics_models

dfA80E20 <- sA80E20$Metrics_models
dfA80E20$alpha <- "Equal"
dfA80E20$eta <- "Equal"


ptrain <- c(0.8,0.8)
alpha <- c(0.8,0.8)
eta <- c(20,5)

sA90EM20EF5<-contCrabs(mug,sg,lab,alpha,eta, ptrain, ns = 100)

sA90EM20EF10$Metrics_models

dfA90EM20EF5 <- sA90EM20EF5$Metrics_models
dfA90EM20EF5$alpha <- "Equal"
dfA90EM20EF5$eta <- "Dif 15%"


ptrain <- c(0.8,0.8)
alpha <- c(0.8,0.8)
eta <- c(20,10)

sA90EM20EF10<-contCrabs(mug,sg,lab,alpha,eta, ptrain, ns = 100)

dfA90EM20EF10<- sA90EM20EF10$Metrics_models
dfA90EM20EF10$alpha <- "Equal"
dfA90EM20EF10$eta <- "Dif 10%"

ptrain <- c(0.8,0.8)
alpha <- c(0.95,0.8)
eta <- c(20,20)

sAM95AF80E20<-contCrabs(mug,sg,lab,alpha,eta, ptrain, ns = 100)

ptrain <- c(0.8,0.8)
alpha <- c(0.85,0.8)
eta <- c(20,10)

sAM85AF80EM20EF10<-contCrabs(mug,sg,lab,alpha,eta, ptrain, ns = 100)



dfAll <- rbind.data.frame(dfA90EM20EF10,
                          dfA90EM20EF5,
                          dfA80E20)
freqVar <- table(unlist(str_split(dfAll$Model,"-")))
coul <- brewer.pal(5,"Set2")

barplot(prop.table(freqVar),col = coul)

colnames(dfAll)



dfCRLong<- dfAll %>% dplyr::select(alpha,eta,CR_SatMC,CR_SV) %>% 
  pivot_longer(c(CR_SatMC,CR_SV),
                                          names_to = "Variables",
                                          values_to = "CCR")
dfCRLong <- dfCRLong %>% mutate(Variables = recode(Variables,
                                                   CR_SatMC = "All",
                                                   CR_SV = "Selected"))


ggplot(dfCRLong, aes(x = Variables, y = CCR, color = eta)) + geom_boxplot()


dfAccLong<- dfAll %>% dplyr::select(alpha,eta,Accuracy_SatCont,Accuracy_SVCont) %>% 
  pivot_longer(c(Accuracy_SatCont,Accuracy_SVCont),
               names_to = "Variables",
               values_to = "Accuracy")

dfAccLong <- dfAccLong %>% mutate(Variables = recode(Variables,
                                                   Accuracy_SatCont = "All",
                                                   Accuracy_SVCont = "Selected"))

ggplot(dfAccLong, aes(x = Variables, y = Accuracy, color = eta)) + geom_boxplot()


head(dfA80E20Long)


sA80E20<-contCrabs(mug,sg,lab,alpha,eta, ptrain, ns = 100)



set.seed(123)
SVmodel <- list() 
AccuracyClassSV <- rep(0,100)
AccuracyContSV <-rep(0,100)
AccuracyClassSatM_C <- rep(0,100)
AccuracyContSatM_C <- rep(0,100)
AccuracyClassSatM_Nc <- rep(0,100)

BlueCrabs$Sex <- BlueCrabs$sex
#BlueCrabsCont$sex <- ifelse(BlueCrabsCont$sex == "F",1,2)
colnames(BlueCrabs)

i<-1
for (i in 1:100)
{
  GenContSamples <- SimCont(mug,sg,lab,16,eta)
  GenContSamples$index <- 100+1:nrow(GenContSamples) 
  colnames(GenContSamples) <-  c("FL","RW","CL","CW","BD","sex","index")
  GenContSamples <- GenContSamples %>% relocate(sex,.before = "FL")
  GenContSamples <- GenContSamples %>% relocate(index,.after = "sex")
  GenContSamples$Cont <- 1
  colnames(GenContSamples)
  head(GenContSamples)
  
  indMnc <- sample(1:50,40,replace =  FALSE)
  indFnc <- sample(51:100,40,replace =  FALSE)
  
  tail(GenContSamples,17)
  
  indMc <- sample(1:16,10,replace = FALSE)
  indFc <- sample(17:32,10,replace = FALSE)
  
  BlueCrabs$Cont <- 0
  
  colnames(GenContSamples)
  colnames(BlueCrabs)
  colnames(BlueCrabs[,-c(1:3,11)])

  BlueCrabsCont <- rbind.data.frame(BlueCrabs[,-c(1:3,11)],GenContSamples)
  
  # sex: "F" - 1
  #      "M" - 2
  
  GenContSamples$Sex <- GenContSamples$sex
#  GenContSamples$sex <- ifelse(GenContSamples$Sex == "F",1,2)
#  GenContSamples$sex <- as.factor(GenContSamples$sex)
  levels(GenContSamples$sex)
  levels(GenContSamples$Sex)
  
  head(BlueCrabs)
  head(GenContSamples)
  colnames(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,11)])
  colnames(GenContSamples[c(indMc,indFc),-c(2,9)])
  str(GenContSamples)
  str(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,11)])
  str(GenContSamples[c(indMc,indFc),-c(2,9)])
  
  
  BlueCrabsTrain <- rbind.data.frame(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,11)],
                                    GenContSamples[c(indMc,indFc),-c(2,9)])
  
  BlueCrabsTrain <- BlueCrabsTrain[sample(1:nrow(BlueCrabsTrain)),]
  
  BlueCrabsTest <- rbind.data.frame(BlueCrabs[-c(indMnc,indFnc),-c(1:3,5,11)], 
                                    GenContSamples[-c(indMc,indFc),-c(2,9)])
  
  BlueCrabsTest <- BlueCrabsTest[sample(1:nrow(BlueCrabsTest)),]
  
  
  BlueCrabsTrainX <- BlueCrabsTrain[,-c(1,7)]
  BlueCrabsTrainl <- BlueCrabsTrain$sex
  BlueCrabsTestX <- BlueCrabsTest[,-c(1,7)]
  BlueCrabsTestl <- BlueCrabsTest$sex
#  SexTrain <- ifelse(BlueCrabsTrain$sex == 1, "F","M")
#  SexTrain <- factor(SexTrain)
  ContTrain <- ifelse(BlueCrabsTrain$Cont == 0, "NC","C")
  ContTrain <- factor(ContTrain)
  
  #SexTest <- ifelse(BlueCrabsTest$sex == 1, "F", "M")
  ContTest <- ifelse(BlueCrabsTest$Cont == 0, "NC","C")
  
  
  colnames(BlueCrabsTrain)
  colnames(BlueCrabsTrainX)
  
  dfRW <- getOW(BlueCrabsTrainX,BlueCrabsTrainl)
  RW <- dfRW$Var
  variables_saturated_model <- RW
    
  # model including all variables
  saturated_mod  <- ModelAccuracy2(BlueCrabsTrainX,
                                   BlueCrabsTestX,
                                   as.numeric(BlueCrabsTrainl),
                                   as.numeric(BlueCrabsTestl),"EII",
                                   alpharef = 0.98, 
                                   tol = 0.01)
  
  saturated_mod
  AccuracyClassSatM_Nc[i] <- saturated_mod$accTestNc
  AccuracyClassSatM_C[i] <- saturated_mod$accTestC
  
  saturated_mod$accTestNc
  
  auxTestCont <- rep(0,32)
  
  for (j in 1:length(auxTestCont))
  {
    auxTestCont[j] <- 1-saturated_mod$predv[j,BlueCrabsTestl[j]]
  }
  
  AccuracyContSatM_C[i] <-sum(auxTestCont == BlueCrabsTest$Cont)/length(BlueCrabsTest$Cont)
  
  # model including selected variables
  modSV <-fHLvarSearch2(BlueCrabsTrainX
                        ,BlueCrabsTestX,RW,
                        as.numeric(BlueCrabsTrainl),
                        as.numeric(BlueCrabsTestl),"E",
                        alpharef =0.99,tol=0.01,epsilon = 0)
  
  SVmodel[[i]] <- modSV$Selectedmodel
  AccuracyClassSV[i] <-  modSV$Accuracy
  
  modSV$posCM
  TestContSV <- rep(0,32)
  for (j in 1:length(TestContSV))
  {
    TestContSV[j] <- 1- modSV$models[[modSV$posCM]]$predv[j,BlueCrabsTestl[j]]
  }
  AccuracyContSV[i] <- sum(TestContSV == BlueCrabsTest$Cont)/ length(BlueCrabsTest$Cont)
  
  
}

SVmodel
AccuracyClassSatM_C
AccuracyClassSatM_Nc
AccuracyClassSV

SVmodel1 <- lapply(SVmodel, function(i) paste(unlist(i),collapse = "-"))
SVmodel1 <- ldply(SVmodel1)
colnames(SVmodel1) <- "Model"
SVmodel1

aux_df <- data.frame(Accuracy_SatMNc = AccuracyClassSatM_Nc,
                     Accuracy_SatMC = AccuracyClassSatM_C,
                     Accuracy_SatCont = AccuracyContSatM_C,
                     Accuracy_SV = AccuracyClassSV,
                     Accuracy_SVCont = AccuracyContSV)

df_resumen <- cbind.data.frame(SVmodel1,aux_df)
colnames(df_resumen)

df_resumen <- mutate(df_resumen,Model1 = Model)
df_resumen <- df_resumen %>% relocate(Model1, .after = Model)
colnames(df_resumen)
df_resumen <- df_resumen %>% relocate(Model, .after = Model1)
colnames(df_resumen)

#  df_resumen <- rename(df_resumen,Model1 = Model)


labels<-df_resumen$Model1

find_unique_labels(df_resumen$Model1)

df_resumen$Model <- find_unique_labels(df_resumen$Model1)$newlabels
head(df_resumen)

colnames(df_resumen)
metrics_res <- df_resumen %>% group_by(Model) %>%
  summarise(Accuracy_SatMNc = mean(Accuracy_SatMNc),
                                  Accuracy_SatMC = mean(Accuracy_SatMC),
            Accuracy_SatCont = mean(Accuracy_SatCont),
            Accuracy_SV = mean(Accuracy_SV),
            Accuracy_SVCont = mean(Accuracy_SVCont))

metrics_res %>% filter(Model %in% c("BD-CL-FL-CW-RW","CL-RW",
                                    "BD-CL-RW", "BD-CL-CW-RW",
                                    "FL-CL-RW",
                                    "CL-CW-FL-RW",
                                    "BD-CL-FL-RW"))

tab1 <- table(df_resumen$Model)
my_tab1_sort <- tab1[order(tab1, decreasing = TRUE)]


df_models <- data.frame(frequency = as.vector(my_tab1_sort),
                        model = rownames(my_tab1_sort))

plt_boxplotmodels <- ggplot(df_models) +
  geom_col(aes(reorder(model,-frequency),frequency),fill = "#076fa2", width = 0.6)

plt_boxplotmodels
df_models_topn <- df_models %>% top_n(7,df_models$frequency)
nrow(df_models_topn)

ggp <- ggplot(df_models_topn, 
              aes(x = reorder(model,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Model") + xlab("Frequency") +
  geom_text(aes(x = model, y = frequency + 0.3, label = frequency),check_overlap = TRUE)
ggp

head(df_models)
library(plotly)
ggp %>% ggplotly


AccuracyCont_SV <- AccuracyContSV
boxplot(AccuracyClass_SV)
boxplot(AccuracyCont_SV)

# Equal Eta for groups ----------------------------------------------------
# read the file 
CrabsDf <- read.csv(paste0(pathFile,"/","Crabs.csv"))
CrabsDf$sex <- as.factor(CrabsDf$sex)
head(CrabsDf)
BlueCrabs <- CrabsDf %>% filter(sp == "B")
XBlueCrabs <- BlueCrabs %>% subset(select = c(index,FL, RW, CL, CW, BD))
y <- BlueCrabs$sex
ind_fem <- y=="F"

meanBlueCrabsF <- XBlueCrabs[ind_fem,-1] %>% apply(2,mean)
meanBlueCrabsM <- XBlueCrabs[!ind_fem,-1] %>% apply(2,mean)
p <- length(meanBlueCrabsF)

GBlueCrabsF <- XBlueCrabs[ind_fem,-1] %>% var
GBlueCrabsM <- XBlueCrabs[!ind_fem,-1] %>% var

mug <- matrix(0.0, nrow = p, ncol = 2)

mug[,1] <- meanBlueCrabsM
mug[,2] <- meanBlueCrabsF

eta <- c(20,20)
lab <- c("M","F")
ncont <- c(13,13)
  
  GenContSamples <- SimCont(mug,sg,lab,ncont,eta)
  GenContSamples$index <- 100+1:nrow(GenContSamples) 
  colnames(GenContSamples) <-  c("FL","RW","CL","CW","BD","sex","index")
  GenContSamples <- GenContSamples %>% relocate(sex,.before = "FL")
  GenContSamples <- GenContSamples %>% relocate(index,.after = "sex")
  GenContSamples$Cont <- 1
  colnames(GenContSamples)
  head(GenContSamples)
  # training
  indMnc <- sample(1:50,40,replace =  FALSE)
  indFnc <- sample(51:100,40,replace =  FALSE)
  
  tail(GenContSamples,22)
  
  indMc <- sample(1:13,10,replace = FALSE)
  indFc <- sample(14:26,10,replace = FALSE)
  
  BlueCrabs$Cont <- 0
  
  colnames(BlueCrabs)
  colnames(BlueCrabs[,-c(1:3)])
  colnames(GenContSamples)
  
  BlueCrabsCont <- rbind.data.frame(BlueCrabs[,-c(1:3)],GenContSamples)
  # sex: "F" - 1
  #      "M" - 2
  
  BlueCrabs$Sex <- BlueCrabs$sex
  BlueCrabs$sex <- ifelse(BlueCrabs$Sex == "F",1,2)
  GenContSamples$Sex <- GenContSamples$sex
  GenContSamples$sex <- ifelse(GenContSamples$Sex == "F",1,2)
  
  BlueCrabsCont$Sex <- BlueCrabsCont$sex
  BlueCrabsCont$sex <- ifelse(BlueCrabsCont$sex == "F",1,2)
  
  
  head(BlueCrabs)
  head(GenContSamples)
  
  colnames(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,12)])
  colnames(GenContSamples[c(indMc,indFc),-c(2,9)])
  nrow(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,12)])
  nrow(GenContSamples[c(indMc,indFc),-c(2,9)])
  
    
  BlueCrabsTrain <-rbind.data.frame(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,12)],
                                  GenContSamples[c(indMc,indFc),-c(2,9)])

  BlueCrabsTrain <- BlueCrabsTrain[sample(1:nrow(BlueCrabsTrain)),]
  
  nrow(BlueCrabsTrain)
  
  BlueCrabsTest <- rbind.data.frame(BlueCrabs[-c(indMnc,indFnc),-c(1:3,5,12)], 
                                    GenContSamples[-c(indMc,indFc),-c(2,9)])
  
  BlueCrabsTest <- BlueCrabsTest[sample(1:nrow(BlueCrabsTest)),]
  
  nrow(BlueCrabsTest)
  
  BlueCrabsTrainX <- BlueCrabsTrain[,-c(1,7)]
  BlueCrabsTrainl <- BlueCrabsTrain$sex
  BlueCrabsTestX <- BlueCrabsTest[,-c(1,7)]
  BlueCrabsTestl <- BlueCrabsTest$sex
  SexTrain <- ifelse(BlueCrabsTrain$sex == 1, "F","M")
  ContTrain <- ifelse(BlueCrabsTrain$Cont == 0, "NC","C")
  SexTrain <- factor(SexTrain)
  ContTrain <- factor(ContTrain)
  
  SexTest <- ifelse(BlueCrabsTest$sex == 1, "F", "M")
  ContTest <- ifelse(BlueCrabsTest$Cont == 0, "NC","C")

  
  colnames(BlueCrabsTrain)
  colnames(BlueCrabsTrainX)


mycols <- c("blue","green")
pairs(BlueCrabsTrainX, oma = c(3,3,6,3),
      col = mycols[as.numeric(BlueCrabsTrain$sex)],
      pch = c(1:2)[as.numeric(BlueCrabsTrain$Cont)+1],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(SexTrain),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)
legend("top", pch = c(2:1), legend = levels(ContTrain), col = "black",
       xpd = NA, ncol = 3, bty = "n", inset = -0.03)
eta
alpha

dfRW <- getOW(BlueCrabsTrainX,BlueCrabsTrainl)
RW <- dfRW$Var
variables_saturated_model <- RW

# model including all variables
saturated_mod  <- ModelAccuracy2(BlueCrabsTrainX,
                                 BlueCrabsTestX,
                                 BlueCrabsTrainl,
                                 BlueCrabsTestl,"EII",
                                 alpharef = 0.98, 
                                 tol = 0.01)


saturated_mod$accTestNc
saturated_mod$accTestC
BlueCrabsTestl

apply(1:32,2,)
sapply(1:32, function(i) {saturated_mod$predv[,BlueCrabsTestl[i]] })
saturated_mod$predv[,BlueCrabsTestl]

auxTestCont <- rep(0,32)

for (i in 1:length(auxTestCont))
{
  auxTestCont[i] <- 1-saturated_mod$predv[i,BlueCrabsTestl[i]]
}

sum(auxTestCont == BlueCrabsTest$Cont)/length(BlueCrabsTest$Cont)

# model including selected variables
modSV <-fHLvarSearch2(BlueCrabsTrainX
                      ,BlueCrabsTestX,RW,
                    BlueCrabsTrainl,BlueCrabsTestl,"E",
                    alpharef =0.99,tol=0.01,epsilon = 0)

modSV$Selectedmodel
modSV$Accuracy

modSV$posCM
TestContSV <- rep(0,32)
for (i in 1:length(TestContSV))
{
  TestContSV[i] <- 1- modSV$models[[modSV$posCM]]$predv[i,BlueCrabsTestl[i]]
}
sum(TestContSV == BlueCrabsTest$Cont)/ length(BlueCrabsTest$Cont)





MmetricsSaturatedM <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                 Recall = rep(0,G), F1 = rep(0,G)) 
MmetricsTM <- data.frame(Group = 1:G, Precision = rep(0,G), 
                         Recall = rep(0,G), F1 = rep(0,G)) 
MmetricsSM <- data.frame(Group = 1:G, Precision = rep(0,G), 
                         Recall = rep(0,G), F1 = rep(0,G)) 



mod <-fHLvarSearch2(GenData$Xtrain,GenData$Xtest,RW,
                    GenData$ltrain,GenData$ltest,"E",
                    alpharef =0.99,tol=0.01,epsilon = 0)



pos_True_model <- findPosModel(mod$models,variables_True_Model)  
if(pos_True_model != 0 & is.numeric(pos_True_model))
{
  TrueModel <- mod$models[[pos_True_model]]
}else {
  Xtrain_TM <- data.frame(GenData$Xtrain) %>% dplyr::select(all_of(variables_True_Model))
  Xtest_TM <- data.frame(GenData$Xtest) %>% dplyr::select(all_of(variables_True_Model))
  
  TrueModel  <- ModelAccuracy2(Xtrain_TM,
                               Xtest_TM,
                               GenData$ltrain,
                               GenData$ltest,"EII",
                               alpharef = 0.98, 
                               tol = 0.01)
} 





