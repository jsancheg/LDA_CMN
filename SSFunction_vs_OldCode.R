

#SSV_2_2_5_3000_75_BAL_SCBSV_VD_A8080_E2020_10


# pathSSFiles
pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"
file_name <-"SSV_2_2_5_3000_75_BAL_SCBSV_VD_A8080_E2020_10"
Sfile <- readRDS(paste0(pathSFiles,file_name,".RDS"))

Sfile

variables_True_Model <- c("X2","X4")
pig <- c(0.5,0.5)
GenData <- Sfile$GenData[[1]]
GenData

G = max(length(unique(GenData$l)),length(pig))
nVarSel <- 0 

MmetricsSaturatedM <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                 Recall = rep(0,G), F1 = rep(0,G)) 
MmetricsTM <- data.frame(Group = 1:G, Precision = rep(0,G), 
                         Recall = rep(0,G), F1 = rep(0,G)) 
MmetricsSM <- data.frame(Group = 1:G, Precision = rep(0,G), 
                         Recall = rep(0,G), F1 = rep(0,G)) 


dfRW <- getOW(GenData$Xtrain,GenData$ltrain)
RW <- dfRW$Var
variables_saturated_model <- RW

saturated_mod  <- ModelAccuracy2(GenData$Xtrain,
                                 GenData$Xtest,
                                 GenData$ltrain,
                                 GenData$ltest,"EII",
                                 alpharef = 0.98, 
                                 tol = 0.01)


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

pos <- mod$posCM
nVarSel <- length(mod$Selectedmodel)

PM <-mod$Selectedmodel

Xsubset <- data.frame(GenData$Xtrain) %>% dplyr::select(all_of(PM))

#actualPar <- TrueParameters(as.matrix(Xsubset),GenData$ltrain,
#                            GenData$vtrain)


t_test <- table(GenData$ltest,mod$models[[pos]]$predlabel)
acctest <- sum(diag(t_test))/sum(t_test)

cat("\n", "selected model", "test set ",mod$Selectedmodel,"-",acctest,"\n")

# Filter contaminated vs non-contaminated samples
lind_nocont_class <- list()
lind_cont_class <- list()

for (i_g in 1:G)
{
  # non contaminated samples in classes
  lind_nocont_class[[i_g]] <- which(GenData$vtest == 1 & GenData$ltest == i_g)
  # contaminated samples in classes
  lind_cont_class[[i_g]] <- which(GenData$vtest==0 & GenData$ltest == i_g)
  MmetricsSaturatedM[i_g,2] <- Precision(GenData$ltest,saturated_mod$predlabel,positive = i_g)
  MmetricsTM[i_g,2] <- Precision(GenData$ltest,TrueModel$predlabel,positive = i_g)
  MmetricsSM[i_g,2] <-Precision(GenData$ltest,mod$models[[pos]]$predlabel,positive = i_g)
  MmetricsSaturatedM[i_g,3] <- Recall(GenData$ltest,saturated_mod$predlabel,positive = i_g)
  MmetricsTM[i_g,3] <- Recall(GenData$ltest,TrueModel$predlabel,positive = i_g)
  MmetricsSM[i_g,3] <-Recall(GenData$ltest,mod$models[[pos]]$predlabel,positive = i_g)
}
MmetricsSaturatedM$F1 <- 2*(MmetricsSaturatedM$Precision*MmetricsSaturatedM$Recall)/(MmetricsSaturatedM$Precision+MmetricsSaturatedM$Recall)
MmetricsTM$F1 <- 2*(MmetricsTM$Precision*MmetricsTM$Recall)/(MmetricsTM$Precision+MmetricsTM$Recall)
MmetricsSM$F1 <- 2*(MmetricsSM$Precision*MmetricsSM$Recall)/(MmetricsSM$Precision+MmetricsSM$Recall)

ind_nocont_samples <- unlist(lind_nocont_class)
ind_cont_samples <- unlist(lind_cont_class)

no_cont_samples <- GenData$ltest[ind_nocont_samples]
cont_samples <- GenData$ltest[ind_cont_samples]

Nsteps <- length(TrueModel$lpredlabel)

accSM_cont_samples <- rep(0,Nsteps)
accSM_no_cont_samples <- rep(0,Nsteps)
accTM_cont_samples <- rep(0,Nsteps)
accTM_no_cont_samples <- rep(0,Nsteps)
accSaturated_no_cont_samples <- rep(0,Nsteps)
accSaturated_cont_samples <- rep(0,Nsteps)


# Calculating precision, recall, and F1 metrics
Vtest <- GenData$vtest

saturated_Vtest <- apply(saturated_mod$predv,1,max)
TM_Vtest <- apply(TrueModel$predv,1,max) # check why all predicted V are 1
SM_Vtest <- apply(mod$models[[pos]]$predv,1,max)


# Calculating accuracy of detecting whether sample is contaminated or not, precision, recall, and F1 
accSaturated_V_test <- 100*sum(Vtest ==saturated_Vtest)/length(Vtest) 
accTM_V_test <- 100*sum(Vtest ==TM_Vtest)/length(Vtest)
accSM_V_test <- 100*sum(Vtest==SM_Vtest)/length(Vtest)

library("MLmetrics")
table(saturated_Vtest,GenData$vtest)

precision_saturated_V <- ifelse(!is.na(Precision(GenData$vtest,saturated_Vtest,positive = 0)), 
                                Precision(GenData$vtest,saturated_Vtest,positive = 0),0)

precision_TM_V <- ifelse(!is.na(Precision(GenData$vtest,TM_Vtest,positive = 0)),
                         Precision(GenData$vtest,TM_Vtest,positive = 0),0 )

precision_SM_V <- ifelse(!is.na(Precision(GenData$vtest,SM_Vtest,positive = 0)),
                         Precision(GenData$vtest,SM_Vtest,positive = 0),0 )

recall_saturated_V <- ifelse( !is.na(Recall(GenData$vtest,saturated_Vtest,positive = 0)) 
                             ,Recall(GenData$vtest,saturated_Vtest,positive = 0),0)
recall_TM_V <- ifelse( !is.na( Recall(GenData$vtest,TM_Vtest,positive = 0) ) 
                              ,Recall(GenData$vtest,TM_Vtest,positive = 0),0)
recall_SM_V <- ifelse ( is.na(Recall(GenData$vtest,SM_Vtest,positive = 0))
                              ,Recall(GenData$vtest,SM_Vtest,positive = 0),0)

F1_Saturated_V <- 2*(precision_saturated_V * recall_saturated_V)/(precision_saturated_V+recall_saturated_V)
F1_TM_V <- 2*(precision_TM_V * recall_TM_V)/(precision_TM_V + recall_TM_V)
F1_SM_V <- 2*(precision_SM_V * recall_SM_V)/(precision_SM_V + recall_SM_V)

#precision_saturated_V <- 100*sum(GenData$vectorVtest[ind_cont_samples]==saturated_Vtest[ind_cont_samples])/sum(saturated_Vtest==0)
#precision_TM_V <- 100*sum(GenData$vectorVtest[ind_cont_samples]== TM_Vtest[ind_cont_samples])/sum(TM_Vtest==0)
#precision_SM_V <- 100*sum(GenData$vectorVtest[ind_cont_samples]==SM_Vtest[ind_cont_samples])/sum(SM_Vtest==0)

accSM_Identifying_cont_samples <- 0
accSM_Identifying_no_cont_samples <- 0
accTM_Identifying_cont_samples <- 0
accTM_Identifying_no_cont_samples <- 0
accSaturated_Identifying_cont_samples <- 0
accSaturated_Identifying_no_cont_samples <- 0



for(i_step in 1:Nsteps)
{
  # predicted class for contaminated and non-contaminated samples for the true model
  predTM_nocont_samples <- TrueModel$lpredlabel[[i_step]][ind_nocont_samples]
  predTM_cont_samples <- TrueModel$lpredlabel[[i_step]][ind_cont_samples]
  
  # predicted class for contaminated and non-contaminated samples for selected model
  predSM_nocont_samples <- mod$first20EMclassprediction[[i_step]][ind_nocont_samples]
  predSM_cont_samples <- mod$first20EMclassprediction[[i_step]][ind_cont_samples]
  
  # predicted class for contaminated and non-contaminated samples for saturated model
  predSaturated_nocont_samples <- saturated_mod$predlabel[ind_nocont_samples]
  predSaturated_cont_samples <- saturated_mod$predlabel[ind_cont_samples]
  
  # Accuracy no-contaminated True Model
  accTM_no_cont_samples[[i_step]] <- (sum(no_cont_samples == predTM_nocont_samples)/length(no_cont_samples))
  accTM_cont_samples[[i_step]] <- (sum(cont_samples == predTM_cont_samples)/length(cont_samples))
  accSM_no_cont_samples[[i_step]] <- (sum(no_cont_samples == predSM_nocont_samples)/length(no_cont_samples))
  accSM_cont_samples[[i_step]] <- (sum(cont_samples == predSM_cont_samples)/length(cont_samples))
  accSaturated_no_cont_samples[i_step] <- (sum(no_cont_samples == predSaturated_nocont_samples)/length(no_cont_samples))
  accSaturated_cont_samples[i_step] <-     (sum(cont_samples == predSaturated_cont_samples)/length(cont_samples))
}


Output <- list(models = mod$models ,
               CM = mod$Selectedmodel, AccuracyCM = mod$Accuracy,
               AccuracyCM_Cont = accSM_cont_samples[Nsteps],
               AccuracyCM_NoCont = accSM_no_cont_samples[Nsteps],
               AccuracyCM_Cont_list = accSM_cont_samples,
               AccuracyCM_NoCont_list = accSM_no_cont_samples,
               nVarSel = nVarSel,
               AccuracyTM = TrueModel$accTestC,
               AccuracyTM_Cont = accTM_cont_samples,
               AccuracyTM_NoCont = accTM_no_cont_samples)

Output
Output$CM
Output$AccuracyCM

Sfile$Metrics[1,]
