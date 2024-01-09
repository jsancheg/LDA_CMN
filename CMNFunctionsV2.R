# File that contains all the functions used in 
# variable selection with contaminated mixture models

# Create dataset


#work_path <- "E:/University of Glasgow/Literature review/R Code/"
#setwd(work_path)

work_path <- "/home/jsancheg/git_environment/LDA_CMN"

dir(work_path)
setwd(work_path)
source("utilities.R")
pathOutput <- "/home/jsancheg/git_environment/LDA_CMN/Scenarios/"
dir(pathOutput)

SimGClasses <- function(mug,sg,pig,nobs,ptraining,alphag,etag)
  #n : number of observations
{
  output <- list()
  G <- length(pig)
  if(is.matrix(mug)) p<-nrow(mug) else p <- length(mug)
  X <- matrix(0.0, ncol = p , nrow = nobs)
#  v <- matrix(-1,nrow = nobs)
  # 1: good observation
  # 0: contaminated observation
  #Validate parameters
  if(sum(pig)!=1) stop("proportions do not sum 1")
  if(any(pig<0) | any(pig>1)) stop("alpha is not a probability")
  if(any(ptraining<0) | any(ptraining>1)) stop("ptraining is not a probability")
  if(any(alphag<0) | any(alphag>1)) stop("alpha takes values in the interval (0,1)")
  if(any(etag < 1))stop("eta has to be greater than 1")  
  aux <- (rmultinom(nobs,1,pig))
  l <- apply(aux,2,which.max)
  v <- rep(0,nobs)
  
  for(i in 1:nobs)
    v[i] <- as.numeric(rbinom(1,1,alphag[l[i]]))
  if(length(dim(sg)) == 2){
    # X[i,] <- unlist(gen(p, mu = 0, sigma = 1))
    l <- sample(1:G,nobs,replace = T, prob = pig)
    mg <- apply(unmap(l),2,sum)
    #     if (any(alphag!=1)){
    #      }
    for (i in 1:nobs)
    {
      if(v[i] == 1)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg)
        else  X[i,] <- rMVNorm(1,mug,sg)
        
      }else if(v[i]==0)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],etag[l[i]]*sg)
        else  X[i,] <- rMVNorm(1,mug,etag[l[i]]*sg)
      }  
      
    }
    
  }else if(length(dim(sg)) > 2)
  {
    
    for (i in 1:nobs)
      if(v[i] == 1)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg[,,l[i]])
        else
          X[i,] <- rMVNorm(1,mug,sg[,,l[i]])
      }else if(v[i] == 0)
      {
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],etag[l[i]]*sg[,,l[i]])
        else  X[i,] <- rMVNorm(1,mug,etag[l[i]]*sg[,,l[i]])
        
      }
  } #End-if
  
  colnames(X) <- paste("X",1:p,sep = "")
  ind <- sample(1:nobs, round(nobs* ptraining))
  Xtrain <- X[ind,]
  Xtest <- X[-ind,]
  ltrain <- l[ind]
  ltest <- l[-ind]
  vtrain <- v[ind]
  vtest <- v[-ind]

    output <- list(X = X,l =l, ind = ind,Xtrain = Xtrain,
                 Xtest = Xtest,ltrain = ltrain,ltest = ltest,
                 v = v, vtrain = vtrain, vtest = vtest)
  return(output)
}



MultSimPar3 <- function(nruns)
{
  mu1 <- rep(0,100)
  mu2 <- c(0,3,0,3,0,rep(0,95))
  mu <- cbind(mu1,mu2)
  sg <- diag(1,100)
  sg[2,4] = 0.8
  sg[4,2] =0.8
  pig<- c(0.9,0.1)
  nobservations = 2000
  ptraining = 0.75
  alphag <-c(0.9,0.8)
  etag <- c(20,30)
  SM <- list()
  selectedvariables <- list()
  AccuracySM <- list()
  ModelSizeSM <- list()
  Inclusion_correctness <- list()
  Exclusion_correctness <- list()
  Number_var_incorrect_included <- list()
  # Accuracy true model
  AccuracyTM <- list()
  # Accuracy true model in contaminated samples
  Accuracy_TM_contaminated <- list()
  # Accuracy true model in non-contaminated samples
  Accuracy_TM_no_contaminated <- list()
  # Accuracy selected model in contaminated samples
  Accuracy_SM_contaminated <- list()
  # Accuracy selected model in non-contaminated samples
  Accuracy_SM_no_contaminated <- list()
  # Accuracy saturated model
  Accuracy_SaturatedM <- list()
  Accuracy_Saturated_Cont <- list()
  Accuracy_Saturated_NoCont <- list()
  Precision_TM <- list()
  Recall_TM <- list()
  # F1 score
  F1_TM <- list()
  Precision_SM <-list()
  Recall_SM <- list()
  F1_SM <- list()
  Precision_SaturatedM <- list()
  Recall_SaturatedM <- list()
  F1_SaturatedM <- list()
  precision_TM_V <- list()
  precision_SM_V <-list()
  precision_saturated_V <- list()
  recall_TM_V <- list()
  recall_SM_V <- list()
  recall_saturated_V <- list()
  F1_TM_V <- list()
  F1_SM_V <- list()
  F1_Saturated_V <- list()
  Metrics_SaturatedM <- list()
  Metrics_SM <- list()
  Metrics_TM <- list()
  pred_Saturated_LabelTest <- list() 
  pred_TM_LabelTest <- list()
  pred_SM_LabelTest <- list()
  pred_Saturated_VTest <- list()
  pred_TM_VTest <- list()
  pred_SM_VTest <- list()
  
  GenData <- list()
  variables_True_model <- c("X2","X4")
  
  for (i_runs in 1:nruns)
  {
    cat("\n ---- Run: ", i_runs,"-----\n")
    aux <- MultSimSetting3(mu, sg, pig, nobservations, ptraining, alphag , 
                           etag, variables_True_model)
    
    #Check code from this line
    #    pos_True_model <- findPosModel(aux$models,True_model)  
    #    AccuracyTrueModel[[i_runs]] <- aux$models[[pos_True_model]]$accTestC
    
    Precision_TM[[i_runs]] <- aux$PrecisionTM
    Recall_TM[[i_runs]] <- aux$RecallTM
    F1_TM[[i_runs]] <- aux$F1TM
    Precision_SM[[i_runs]] <- aux$PrecisionCM
    Recall_SM[[i_runs]] <- aux$RecallCM
    F1_SM[[i_runs]] <- aux$F1CM
    Precision_SaturatedM[[i_runs]] <- aux$PrecisionSaturatedM
    Recall_SaturatedM[[i_runs]] <- aux$RecallSaturatedM
    F1_SaturatedM[[i_runs]] <- aux$F1SaturatedM    
    AccuracyTM[[i_runs]] <- aux$AccuracyTM
    SM[[i_runs]] <- paste(unlist(aux$CM),collapse="-")
    AccuracySM[[i_runs]]<-aux$AccuracyCM
    Accuracy_SaturatedM[[i_runs]]<-aux$Accuracy_SaturatedM
    Accuracy_Saturated_Cont[[i_runs]]<-aux$Accuracy_Saturated_Cont
    Accuracy_Saturated_NoCont[[i_runs]]<-aux$Accuracy_Saturated_NoCont
    ModelSizeSM[[i_runs]] <- aux$nVarSel
    Inclusion_correctness[[i_runs]] <- sum(length(intersect(aux$CM,variables_True_model)) == length(variables_True_model))
    Number_var_incorrect_included[[i_runs]] <- length(setdiff(aux$CM,variables_True_model)) 
    Exclusion_correctness[[i_runs]] <- sum(length(setdiff(aux$CM,variables_True_model)) == 0)
    Accuracy_TM_contaminated[[i_runs]]<- aux$AccuracyTM_Cont
    Accuracy_TM_no_contaminated[[i_runs]]<- aux$AccuracyTM_NoCont
    Accuracy_SM_contaminated[[i_runs]] <- aux$AccuracyCM_Cont_list
    Accuracy_SM_no_contaminated[[i_runs]] <- aux$AccuracyCM_NoCont_list
    precision_saturated_V[[i_runs]] <- aux$precision_saturated_V
    precision_SM_V[[i_runs]] <- aux$precision_SM_V
    precision_TM_V[[i_runs]] <- aux$precision_TM_V
    recall_saturated_V[[i_runs]] <- aux$recall_saturated_V
    recall_SM_V[[i_runs]] <- aux$recall_SM_V
    recall_TM_V[[i_runs]] <- aux$recall_TM_V
    F1_Saturated_V[[i_runs]] <- aux$F1_Saturated_V
    F1_SM_V[[i_runs]] <- aux$F1_SM_V
    F1_TM_V[[i_runs]] <- aux$F1_TM_V
    GenData[[i_runs]] <- aux$GenData
    pred_Saturated_LabelTest[[i_runs]] <- aux$pred_Saturated_Test
    pred_TM_LabelTest[[i_runs]]<-aux$pred_TM_Test
    pred_SM_LabelTest[[i_runs]]<-aux$pred_SM_Test
    pred_Saturated_VTest[[i_runs]]<-aux$pred_Saturated_Vtest
    pred_TM_VTest[[i_runs]]<-aux$pred_TM_Vtest
    pred_SM_VTest[[i_runs]]<-aux$pred_SM_Vtest
    #25:54
  }
  
  MetricsSM <- data.frame(SelectedVariables = SM,
                          ModelSizeSM = unlist(ModelSizeSM),
                          Inclusion_correctness = unlist(Inclusion_correctness),
                          Exclusion_correctness = unlist(Exclusion_correctness),
                          Number_var_incorrect_included = 
                            unlist(Number_var_incorrect_included)
  )
  
  Metrics <- data.frame(Variables = rep(c("Saturated","True","Selected"), 
                                        each = length(unlist(Accuracy_SaturatedM)) ),
                        Accuracy_label = c(unlist(Accuracy_SaturatedM), 
                                           unlist(AccuracyTM),
                                           unlist(AccuracySM)),
                        Accuracy_No_Contaminated = c(unlist(Accuracy_Saturated_NoCont),
                                                     unlist(Accuracy_TM_no_contaminated),
                                                     unlist(Accuracy_SM_no_contaminated) ),
                        Accuracy_Contaminated = c(unlist(Accuracy_TM_contaminated),
                                                  unlist(Accuracy_SM_contaminated),
                                                  unlist(Accuracy_Saturated_NoCont)),                                      
                        Precision_label = c(unlist(Precision_SaturatedM),
                                            unlist(Precision_TM),
                                            unlist(Precision_SM)),
                        Recall_label =    c(unlist(Recall_SaturatedM),
                                            unlist(Recall_TM),
                                            unlist(Recall_SM)),
                        F1_label =        c(unlist(F1_SaturatedM),
                                            unlist(F1_TM),
                                            unlist(F1_SM)),
                        Precision_V =     c(unlist(precision_saturated_V),
                                            unlist(precision_TM_V),
                                            unlist(precision_SM_V)),
                        Recall_V =        c(unlist(recall_saturated_V),
                                            unlist(recall_TM_V),
                                            unlist(recall_SM_V)),
                        F1_V =            c(unlist(F1_Saturated_V),
                                            unlist(F1_SM_V),
                                            unlist(F1_TM_V))
  )  
  
  res1 <- data.frame(SelectedVariables = SM, AccuracyTM = unlist(AccuracyTM),
                     AccuracySM = (unlist(AccuracySM)),
                     AccuracySaturatedM = unlist(Accuracy_SaturatedM),
                     ModelSizeSM = (unlist(ModelSizeSM)),
                     Inclusion_correctness = (unlist(Inclusion_correctness)),
                     Number_var_incorrect_included = (unlist(Number_var_incorrect_included)),
                     Exclusion_correctness = (unlist(Exclusion_correctness)),
                     Precision_TM = unlist(Precision_TM),
                     Precision_SM = unlist(Precision_SM),             
                     Precision_SaturatedM = unlist(Precision_SaturatedM),
                     Recall_TM = unlist(Recall_TM),
                     Recall_SM = unlist(Recall_SM),
                     Recall_SaturatedM = unlist(Recall_SaturatedM),
                     F1_TM = unlist(F1_TM),       
                     F1_SM = unlist(F1_SM),
                     F1_SaturatedM = unlist(F1_SaturatedM),
                     Accuracy_TM_no_contaminated = unlist(Accuracy_TM_no_contaminated),
                     Accuracy_SM_no_contaminated = unlist(Accuracy_SM_no_contaminated),
                     Accuracy_Saturated_NoCont = unlist(Accuracy_Saturated_NoCont),                     
                     Accuracy_TM_contaminated = unlist(Accuracy_TM_contaminated),
                     Accuracy_SM_contaminated = unlist(Accuracy_SM_contaminated),
                     Accuracy_Saturated_Cont = unlist(Accuracy_Saturated_Cont),
                     precision_saturated_V = unlist(precision_saturated_V),
                     precision_SM_V = unlist(precision_SM_V),
                     precision_TM_V = unlist(precision_TM_V),
                     recall_saturated_V = unlist(recall_saturated_V),
                     recall_SM_V = unlist(recall_SM_V),
                     recall_TM_V = unlist(recall_TM_V),
                     F1_Saturated_V = unlist(F1_Saturated_V),
                     F1_SM_V = unlist(F1_SM_V),
                     F1_TM_V = unlist(F1_TM_V))
  
  res1detail <- data.frame(Accuracy_TM_contaminated = unlist(Accuracy_TM_contaminated),
                           Accuracy_TM_no_contaminated = unlist(Accuracy_TM_no_contaminated),
                           Accuracy_SM_contaminated = (unlist(Accuracy_SM_contaminated)),
                           Accuracy_SM_no_contaminated = (unlist(Accuracy_SM_no_contaminated)))
  
  res1Prediction <- data.frame(Variables = c(rep("Saturated",length(unlist(pred_Saturated_LabelTest))),
                                             rep("True",length(unlist(pred_TM_LabelTest))),
                                             rep("Selected",length(unlist(pred_SM_LabelTest)))),
                               Labels = c(unlist(pred_Saturated_LabelTest),
                                          unlist(pred_TM_LabelTest),
                                          unlist(pred_SM_LabelTest)),
                               Contamination = c(unlist(pred_Saturated_VTest),
                                                 unlist(pred_TM_VTest),
                                                 unlist(pred_SM_VTest)) )
  # Contamination: 1 non-contaminated , 0 contaminated
  
  # ruta <- "/home/pgrad1/2201449s/R/CMN/Output/" 
  # saveRDS(salida,paste0(ruta,"Seg_",i_i,"_node_",i_x,"_warping_",i_u,".RDS"))
  
  output <- list(MetricsSM = MetricsSM,
                 Metrics = Metrics,
                 resumen = res1, 
                 details = res1detail,
                 GenData = GenData, 
                 Prediction = res1Prediction)
  return(output)
  
}




findPosModel <- function(ListofModels,modeltofind)
{
  posinList <- 0
  nmodels <- length(ListofModels)
  for (i_model in 1:nmodels)
  {
    model<-ListofModels[[i_model]]$PM
    if(setequal(model,modeltofind)==TRUE) 
    {
      posinList <- i_model
      break
    }    
  }
  return(posinList)
}






MultSimSetting3 <- function(mu, sg, pig, nobservations,ptraining,alphag,etag,
                            variables_True_Model, niterations = 10)
{
  # Return metrics for true model, selected model, and saturated model
  # mu: vector or matrix containing mu
  # check how code behaves when alphag is c(1,1) 
  # and etag is c(1,1)
  
  nVarSel <- 0 
  GenData <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
  G = max(length(unique(GenData$l)),length(pig))
  
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
                                   niterations = niterations,
                                   alpharef = 0.98, 
                                   tol = 0.01)
  
  
  mod <-fHLvarSearch2(GenData$Xtrain,GenData$Xtest,RW,
                      GenData$ltrain,GenData$ltest,"E",
                      niterations = niterations, alpharef =0.99,
                      tol=0.01,epsilon = 0)
  
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
                                 niterations = niterations,
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
    lind_nocont_class[[i_g]] <- which(GenData$vtest[,i_g]!=0 & GenData$vtest[,i_g]!=-1)
    # contaminated samples in classes
    lind_cont_class[[i_g]] <- which(GenData$vtest[,i_g]==0 & GenData$vtest[,i_g]!=-1)
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
  Vtest <- GenData$vectorVtest
  saturated_Vtest <- apply(saturated_mod$predv,1,max)
  TM_Vtest <- apply(TrueModel$predv,1,max) # check why all predicted V are 1
  SM_Vtest <- apply(mod$models[[pos]]$predv,1,max)
  
  
  # Calculating accuracy of detecting whether sample is contaminated or not, precision, recall, and F1 
  accSaturated_V_test <- 100*sum(Vtest ==saturated_Vtest)/length(Vtest) 
  accTM_V_test <- 100*sum(Vtest ==TM_Vtest)/length(Vtest)
  accSM_V_test <- 100*sum(Vtest==SM_Vtest)/length(Vtest)
  
  library("MLmetrics")
  table(saturated_Vtest,GenData$vectorVtest)
  
  precision_saturated_V <- Precision(GenData$vectorVtest,saturated_Vtest,positive = 0)
  precision_TM_V <- Precision(GenData$vectorVtest,TM_Vtest,positive = 0)
  precision_SM_V <- Precision(GenData$vectorVtest,SM_Vtest,positive = 0)
  
  recall_saturated_V <- Recall(GenData$vectorVtest,saturated_Vtest,positive = 0)
  recall_TM_V <- Recall(GenData$vectorVtest,TM_Vtest,positive = 0)
  recall_SM_V <- Recall(GenData$vectorVtest,SM_Vtest,positive = 0)
  
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
  
  #  accTM_no_cont_samples  
  #  accTM_cont_samples  
  #  accSM_no_cont_samples  
  #  accSM_cont_samples  
  
  
  output <-  list(models = mod$models , 
                  CM = mod$Selectedmodel, AccuracyCM = mod$Accuracy,
                  PrecisionCM = mean(MmetricsSM$Precision),
                  RecallCM = mean(MmetricsSM$Recall),
                  F1CM = mean(MmetricsSM$F1),
                  AccuracyCM_Cont = accSM_cont_samples[Nsteps],
                  AccuracyCM_NoCont = accSM_no_cont_samples[Nsteps],
                  AccuracyCM_Cont_list = accSM_cont_samples,
                  AccuracyCM_NoCont_list = accSM_no_cont_samples,
                  nVarSel = nVarSel,
                  AccuracyTM = TrueModel$accTestC,
                  PrecisionTM = mean(MmetricsTM$Precision),
                  RecallTM = mean(MmetricsTM$Recall),
                  F1TM = mean(MmetricsTM$F1),
                  PrecisionSaturatedM = mean(MmetricsSaturatedM$Precision),
                  RecallSaturatedM = mean(MmetricsSaturatedM$Recall),
                  F1SaturatedM = mean(MmetricsSaturatedM$F1),
                  AccuracyTM_Cont = accTM_cont_samples,
                  AccuracyTM_NoCont = accTM_no_cont_samples,
                  Accuracy_SaturatedM = saturated_mod$accTestC,
                  Accuracy_Saturated_NoCont = accSaturated_no_cont_samples,
                  Accuracy_Saturated_Cont = accSaturated_cont_samples,
                  precision_saturated_V = precision_saturated_V,
                  precision_SM_V = precision_SM_V,
                  precision_TM_V = precision_TM_V,
                  recall_saturated_V = recall_saturated_V,
                  recall_SM_V = recall_SM_V,
                  recall_TM_V = recall_TM_V,
                  F1_Saturated_V = F1_Saturated_V,
                  F1_TM_V = F1_TM_V,
                  F1_SM_V = F1_SM_V,
                  Metrics_SaturatedM = MmetricsSaturatedM,
                  Metrics_SM = MmetricsSM,
                  Metrics_TM = MmetricsTM,
                  GenData = GenData,
                  pred_Saturated_Test = saturated_mod$predlabel,
                  pred_TM_Test = TrueModel$predlabel,
                  pred_SM_Test = mod$models[[pos]]$predlabel,
                  pred_Saturated_Vtest = saturated_Vtest,
                  pred_TM_Vtest = TM_Vtest,
                  pred_SM_Vtest = SM_Vtest)  
  
  return( output )
  
}


TrueParameters<-function(Xtrain,ltrain, v)
{
  
  indNoCont <- apply(v,2,function(x) which(x==1))
  indCont <- apply(v,2,function(x) which(x==0))
  
  Xtrain <- as.matrix(Xtrain)  
  G <- length(unique(ltrain))  
  l <- unmap(ltrain)
  mg <- apply(l,2,sum)
  m <- sum(mg)
  pg <- apply(l,2,sum)/m
  p<-ncol(Xtrain)
  S <- array(0,dim=c(p,p,G))
  Sg <- array(0,dim=c(p,p,G))
  Sgc <- array(0,dim=c(p,p,G))
  a <- rep(0,G)
  b <- rep(0,G)
  eta <- rep(0,G)
  
  mNoCont <- apply(v,2,function(row) length(which(row==1)) )
  mCont <- apply(v,2,function(row) length(which(row==0)) )
  
  alpha <- mNoCont/mg
  mu <- t(aggregate(Xtrain,by = list(ltrain),FUN = "mean")[,-1])
  v1 <- as.matrix(v)
  
  # factor1: matrix that contains the distance
  factor1 <- matrix(0, nrow = m,ncol = G)
  
  # factor2: matrix that contains the Mahalanobis distance for each
  # observation in each group
  factor2 <- matrix(0, nrow = m,ncol = G)
  
  for(g in 1:G)
    for(i in 1:nrow(v1))
      if(v1[i,g]==-1) v1[i,g]<-1 else v1[i,g]<-0 
  
  
  
  for (g in 1:G)
  {
    one <- rep(1,mg[g])
    Xc <- (diag(1,mg[g]) - (one %*% t(one))/mg[g])%*% Xtrain[l[,g]==1,]
    S[,,g] <- (t(Xc)%*%Xc)/(mg[g]-1)
    
    # Sigma group G no contaminated samples
    one <-rep(1,mNoCont[g])
    Xc <- (diag(1,mNoCont[g]) - (one %*% t(one))/mNoCont[g])%*% Xtrain[indNoCont[[g]],]
    Sg[,,g] <- (t(Xc)%*%Xc)/(mNoCont[g]-1)
    
    # Sigma group G contaminated samples
    one <-rep(1,mCont[g])
    Xc <- (diag(1,mCont[g]) - (one %*% t(one))/mCont[g])%*% Xtrain[indCont[[g]],]
    Sgc[,,g] <- (t(Xc)%*%Xc)/(mCont[g]-1)
    
    
    
  }
  #Sg
  #Sgc
  
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      factor1[i,g] <- l[i,g]*(1-v1[i,g])
      factor2[i,g] <- mahalanobis(Xtrain[i,],mu[,g],Sg[,,g])
      b[g] <- b[g] + factor1[i,g]*factor2[i,g]
    }
    a[g] <- sum(factor1[,g])
    eta[g] <- b[g]/(p*a[g])
  }
  
  
  return(list(pig = pg, mu = mu,  S=S,Sgnc = Sg, Sgc=Sgc, alpha = alpha, eta = eta))
  
}


getOW <- function(df_train, l_train)
  # obtain wavelengths ordering in descending order by F-statistics
  
{
  # Rank variables which give more separation of classes 
  if(!is.data.frame(df_train))  df_train <- data.frame(df_train)
  
  c <- ncol(df_train)
  ncol(df_train)
  
  CharVar <- colnames(df_train %>% dplyr::select(where(negate(is.numeric))))
  NumVar <- colnames(df_train %>% dplyr::select(where(is.numeric)))
  if(length(CharVar) >0 ) stop('Hey, first argument only should contain numeric variables')
  fvalue <- rep(0.0,(length(NumVar)))
  
  for(i in 1:c)
  { 
    df1<- data.frame(df_train[,i])
    colnames(df1) <- "x"
    df1$Class <- l_train
    
    aov.r = lm(x ~ Class, data = df1)
    fvalue[i] <-summary(aov.r)$fstatistic[1]
  }
  
  output <- data.frame(Var = NumVar, Ftest = fvalue)
  output <- output[order(-output$Ftest),]
  return(output)
}

fHLvarSearch3 <- function(X_train, X_test, RW,l_train, l_test, CE, 
                          alpharef =0.99,tol=0.01,epsilon = 0)
  # forward Headlong  variable selection
{
  stop2 = F
  p <- length(RW)
  ARW <- NULL
  CM <- NULL
  # position of the current model selected
  posCM <- 0
  OldCM <- "NA"
  PM <- NULL
  AccPM <- 0
  AccCM <- 0
  OldAccCM <-0
  cont <- 0
  model <- list()
  
  if(!is.data.frame(X_train)) X_train <- data.frame(X_train)
  if(!is.data.frame(X_test)) X_test <- data.frame(X_test)
  
  for (cont in 1:p)
  {
    
    PM <-RW[cont]
    X_train1 <- X_train %>% select(all_of(PM))
    X_test1 <- X_test %>% select(all_of(PM))
    
    head(X_train1)
    head(X_test1)
    
    cat("\n Model ",unlist(PM))
    
    model[[cont]] <- ModelAccuracy3(X_train1,X_test1,l_train,l_test,"E",
                                    alpharef, tol)
    model[[cont]]$PM <- PM
    AccPM <-model[[cont]]$accTestC
    
    if(AccPM > AccCM)
    {
      CM <- PM
      AccCM <- AccPM
      # save the position on the list 
      # of the model selected
      posCM <- cont
    }
  }
  
  cont <- cont + 1
  while(!setequal(OldCM,CM) & OldAccCM != AccCM)
  {
    OldCM <- CM
    OldAccCM <- AccCM
    ARW <- setdiff(RW,as.vector(CM))
    nARW <- length(ARW)
    
    if (nARW == 0) stop("The set of non selected variables is empty")
    
    j <- 1
    
    
    while(stop2 == F & j <= nARW)
    {
      PM <- union(CM,ARW[j])
      
      X_train1 <- X_train %>% select(all_of(PM))
      X_test1 <- X_test %>% select(all_of(PM))
      
      model[[cont]] <- ModelAccuracy3(X_train1,X_test1,l_train,l_test,"EEI",
                                      alpharef, tol)
      model[[cont]]$PM <- PM
      cat("\n",cont," ,model = ",unlist(PM),"\n")
      AccPM <- model[[cont]]$accTestC
      cont <- cont + 1
      
      if(AccPM > AccCM)
      {
        CM <- PM
        AccCM <- AccPM 
        posCM <- cont
        break
      } # end if
      
      j <- j + 1
    } # end while (stop2 == F & j <= nARW)
    
  } # end while
  
  return(list(Selectedmodel = CM, Accuracy = AccCM, posCM = posCM,  models = model))
}



fHLvarSearch2 <- function(X_train, X_test, RW,l_train, l_test, CE, 
                          niterations = 10,alpharef =0.99,tol=0.01,epsilon = 0)
  # forward Headlong  variable selection
{
  stop2 = F
  p <- length(RW)
  ARW <- NULL
  CM <- NULL
  # position of the current model selected
  posCM <- 0
  OldCM <- "NA"
  PM <- NULL
  AccPM <- 0
  AccCM <- 0
  OldAccCM <-0
  cont <- 0
  nIterToConvergence <- 0
  model <- list()
  
  if(!is.data.frame(X_train)) X_train <- data.frame(X_train)
  if(!is.data.frame(X_test)) X_test <- data.frame(X_test)
  
  for (cont in 1:p)
  {
    
    PM <-RW[cont]
    
    X_train1 <- X_train %>% dplyr::select(all_of(PM))
    X_test1 <- X_test %>% dplyr::select(all_of(PM))
    
    head(X_train1)
    head(X_test1)
    
    cat("\n Model ",unlist(PM))
    
    model[[cont]] <- ModelAccuracy2(X_train1,X_test1,l_train,l_test,"E",
                                    niterations = niterations,alpharef, tol)
    model[[cont]]$PM <- PM
    AccPM <-model[[cont]]$accTestC
    if(AccPM > AccCM)
    {
      CM <- PM
      AccCM <- AccPM
      # save the position on the list 
      # of the model selected
      posCM <- cont
    }
  }
  
  cont <- cont + 1
  while(!setequal(OldCM,CM) & OldAccCM != AccCM)
  {
    OldCM <- CM
    OldAccCM <- AccCM
    ARW <- setdiff(RW,as.vector(CM))
    nARW <- length(ARW)
    
    if (nARW != 0) 
    {
      
      j <- 1
      
      while(stop2 == F & j <= nARW)
      {
        PM <- union(CM,ARW[j])
        
        X_train1 <- X_train %>% dplyr::select(all_of(PM))
        X_test1 <- X_test %>% dplyr::select(all_of(PM))
        
        model[[cont]] <- ModelAccuracy2(X_train1,X_test1,l_train,l_test,"EEI",
                                        alpharef, tol)
        model[[cont]]$PM <- PM
        cat("\n",cont," ,model = ",unlist(PM),"\n")
        AccPM <- model[[cont]]$accTestC
        
        cont <- cont + 1
        
        if(AccPM > AccCM)
        {
          CM <- PM
          AccCM <- AccPM 
          posCM <- cont-1
          nIterToConvergence <- model[[cont-1]]$niterations
          break
        } # end if
        j <- j + 1
      } # end while (stop2 == F & j <= nARW)
    } # end-f
    
  } # end while
  
  # laccTest_c : contains the global accuracy of predicting classes 
  # without separating samples in groups of contaminated and 
  # non-contaminated samples for the first 20
  # iterations of the EM algorithm
  
  return(list(Selectedmodel = CM, Accuracy = AccCM, 
              first20AccCM = model[[posCM]]$laccTest_c,
              first20EMclassprediction = model[[posCM]]$lpredlabel,
              posCM = posCM,  models = model))
}

fHLvarSearch <- function(X_train, X_test, RW,l_train, l_test, CE, epsilon = 0)
  # forward Headlong  variable selection
{
  stop2 = F
  p <- length(RW)
  ARW <- NULL
  CM <- NULL
  OldCM <- "NA"
  PM <- NULL
  AccPM <- 0
  AccCM <- 0
  OldAccCM <-0
  
  if(!is.data.frame(X_train)) X_train <- data.frame(X_train)
  if(!is.data.frame(X_test)) X_test <- data.frame(X_test)
  
  for (j in 1:p)
  {
    PM <-RW[j]
    
    X_train1 <- X_train %>% select(all_of(PM))
    X_test1 <- X_test %>% select(all_of(PM))
    
    head(X_train1)
    head(X_test1)
    
    AccPM <- modelAccuracy(X_train1,X_test1,l_train,l_test,"E")
    AccPM
    
    if(AccPM > AccCM)
    {
      CM <- PM
      AccCM <- AccPM
    }
  }
  
  
  while(!setequal(OldCM,CM) & OldAccCM != AccCM)
  {
    OldCM <- CM
    OldAccCM <- AccCM
    ARW <- setdiff(RW,as.vector(CM))
    nARW <- length(ARW)
    
    if (nARW == 0) stop("The set of non selected variables is empty")
    
    j <- 1
    
    
    while(stop2 == F & j <= nARW)
    {
      PM <- union(CM,ARW[j])
      
      X_train1 <- X_train %>% select(all_of(PM))
      X_test1 <- X_test %>% select(all_of(PM))
      
      AccPM <- modelAccuracy(X_train1,X_test1,l_train,l_test,"EEI")
      
      if(AccPM > AccCM)
      {
        CM <- PM
        AccCM <- AccPM 
        stop2 = T
      } else {
        j <- j + 1
      }
      
      
      
    }
    
  }
  return(list(model = CM, Accuracy = AccCM))
}


modelAccuracy <- function(X_train,X_test,l_train,l_test,CE)
{
  if(!is.matrix(X_train)) X_train1 <- as.matrix(X_train)
  if(!is.matrix(X_test)) X_test1 <- as.matrix(X_test)
  accTest <- 0.0
  
  nvar <- ncol(X_train1)
  nobs <- nrow(X_train1)
  
  msEst <-mstep(data = X_train1,modelName = CE, z = unmap(l_train))
  estEstep <- estep(data = X_test1, modelName = CE, parameters = msEst$parameters)
  
  z <- estEstep$z  
  ltest <- apply(z,1,which.max)
  
  accTest <- sum(ltest == l_test)/length(l_test)
  
  return(accTest)
}


SimCont <- function(mu, s, lab, ncont, eta)
{
  # mu   : matrix where each rows are the mean of the classes
  # s    : array with the variance covariance matrix of each class
  # ncont: vector that contains the number of contaminated samples to be return for each group g
  # eta  : inflation factor vector for classes
  
  # validate parameters
  G = length(unique(lab))
  ncontg = rep(0,G)
  contSamples <- vector("list",length = G)
  # check parameter ncont
  if (class(ncont) == "numeric")
  {
    if(G == 1 & length(ncont) == 1) {
      ncontg[1] <- ncont 
    }else if (G>1 & length(ncont) == 1) {
      ncontg <- rep(ncont,G)
    } else if(G>1 & length(ncont) == G){
      ncontg <- ncont
    } else if (G > 1 & length(ncont )!= G & length(ncont) > 1){
      stop("Error vector with the number of contaminated samples has different dimension to the number of groups")
    }
  }
  
  #  check parameter mu
  if(all(class(mu) == "numeric")) 
  {
    if (G == 1)
    {
      mug = as.matrix(mu)
    }
    if(G > 1)
    {
      p = length(mu)
      mug = matrix(0.0, ncol = G, nrow = p )
      for (g in 1:G)
      {
        mug[,g] = mu
      }
    }
    
  }else if(length(dim(mu))==2)
  {
    mug = mu
    p = nrow(mu)
  }
  
  XC <- array(0.0,dim = c(nrow = sum(ncontg), ncol = p))
  sg = array(0.0, dim = c(p,p,G))
  # check parameter Sigma
  if (class(s) == "matrix" & length(dim(s)) == 2 & G == 1 )
  {
    sg[,,1] = s
  }else if(class(s) == "matrix" & length(dim(s)) == 2 & G > 1)
  {
    for (g in 1:G) sg[,,g]= s
    
  }else if(class(s) == "array" & length(dim(s)) == 3 & G == 1 & G == dim(s)[3])
  {
    sg[,,1] = s
  }else if(class(s) == "array" & length(dim(s)) == 3 & G > 1 & G == dim(s)[3])
  {
    sg = s
  }else if(class(s) == "array" & length(dim(s)) == 3 & G > 1 & G != dim(s)[3])
    stop("Error in dimeision of variance covariance matrix")
  
  #  check parameter mu
  if(class(eta) == "numeric") 
  {
    if (G == 1)
    {
      etag = eta
    }
    if(G > 1)
    {
      etag = rep(eta, G)
    }
    
  }else stop("eta has to be a vector of dimension g where g is the number of groups")
  
  
  if(G >1)
  {
    for( g in 1:G)
    {
      contSamples[[g]] =  rMVNorm(ncontg[g],mug[,g],etag[g]*sg[,,g])
      
      
    }
    
  }else if(G == 1) contSamples[[1]] =rMVNorm(ncontg,mug,etag*sg)
  
  XC <- ldply(contSamples)
  vlab <- rep(lab,c(ncontg))
  if(length(lab) == length(ncontg)){
    XC <- XC %>% mutate(class = vlab)%>% dplyr::select(class, everything())
    #  XC$class <- rep(lab,ncontg)
  } else stop("Error length of vector of contaminated samples differ with the number of groups")
  return(XC)
}


funcSample <- function(X,y,vpi)
{
  # X: variables 
  # y: vector of response variables with labels
  # vpi: vector of the percentage contribution for each class
  
  if (sum(vpi)!=1) stop("The vector of contribution percentage for groups should sum 1")
  
  G = length(unique(y))
  
  if (length(vpi)!= G) stop("The vector must be the same length as the number of groups")
  
  # ng  number of observations in each group
  ng <- summary(factor(y))
  aux <- matrix(0.0,ncol = G, nrow = G)
  val <- rep(0,G)
  bolval <- NA
  # mg <- number of samples taken from each group
  for (g_1 in 1:G)
  {
    total = round(ng[g_1]/vpi[1],0)
    for(g_2 in 1:(G-1))
    {
      aux[g_1,g_2] = floor(total * vpi[g_2])
    }
    aux[g_1,G] = floor(total * (1-sum(vpi[1:(G-1)])))
    
    bolval[g_1] = all(aux[g_1,]<=ng)
  }
  
  
  aux
  ng
  bolval
  if (all(bolval == FALSE)) stop("Change the proportions one of the group does not have enough observations")
  
  val <- apply(aux,1,sum)
  
  maximo <- max(val[bolval])
  
  indmax <- which(val == maximo)
  
  mg <- aux[indmax,]
  
  mg_samples <- vector("list",G)
  ind_samples <- vector("list",G)
  
  for (g in 1:G)
  {
    
    mg_samples[[g]] <- sample(1:ng[g], mg[g] ,replace = FALSE )
  }
  
  return(mg_samples)
}


contDf <- function(X,y,lab,vpi,alpha,eta,ptrain,ns = 100)
{
  # Function that contaminate the wine data set
  # mug : matrix where each column is the mean of a group
  # sg : matrix or array that contains the variance-covariance matrix for a group
  # X  : matrix or array containing the covariates
  # y  : vector containing the response (group information)
  # lab: vector containing label for the corresponding groups
  # vpi: vector containing the proportion of the sample for each group
  # alpha: vector containing the percentage of non contaminated observations for each group
  # eta:   vector containing the inflation factor for each group
  # ptrain: vector containing the percentage of samples included in the training set for groups
  # ns:    number of data set simulated
  
  SVmodel <- list() 
  Train_subset <- list()
  Test_subset <- list()
  
  AccuracyClassSV <- rep(0,ns)
  AccuracyContSV <-rep(0,ns)
  AccuracyClassSatM_C <- rep(0,ns)
  AccuracyContSatM_C <- rep(0,ns)
  AccuracyClassSatM_Nc <- rep(0,ns)
  G <- length(unique(y))
  ncont <- rep (0,G)
  nocont <- rep(0,G)
  nocont_train <- rep(0,G)
  nocont_test <- rep(0,G)
  ncont_train <- rep(0,G)
  ncont_test <- rep(0,G)
  ntrain <- rep(0,G)
  ntest <- rep(0,G)
  ng <- rep(0,G)
  indsamples <- funcSample(X,y,vpi)
  p <- ncol(X)
  mug <- matrix(0.0,nrow = p, ncol = G)
  sg <- array(0.0, dim = c(p,p,G))
  
  
  for (g in 1:G) 
  {
    mug[,g] <- X[y==g,] %>% apply(2,mean)
    sg[,,g] <- X[y == g,] %>% var
    ng[g] <- length(indsamples[[g]])
  }
  
  #BlueCrabsCont$sex <- ifelse(BlueCrabsCont$sex == "F",1,2)
  for (g in 1:G)
  {
    nocont_train[g] = round(ng[g] * ptrain[g],0) 
    nocont_test[g] = ng[g] - nocont_train[g]
    
    # number of contaminated samples in the train set
    ncont_train[g] = round(nocont_train[g]/alpha[g],0) - nocont_train[g]
    
    # number of contaminated samples in the test set
    ncont_test[g] = round(nocont_test[g]/alpha[g],0) - nocont_test[g]
    
    nocont[g] = nocont_train[g] + nocont_test[g]
    
    # number of contaminated observations to be simulated
    ncont[g] = ncont_train[g] + ncont_test[g]
    
    # train size
    ntrain[g] = nocont_train[g] + ncont_train[g]
    
    # test set for each sex
    ntest[g]= nocont_test[g] + ncont_test[g]
    
  }
  
  indnc <- vector("list",G)
  indc <- vector("list",G)
  Xgnc_train <- vector("list",G)
  Xgnc_test <- vector("list",G)
  Xgc_train <- vector("list",G)
  Xgc_test <- vector("list",G)
  i<-1    
  for (i in 1:ns)
  {
    cat("\n simulation = ", i, "\n")
    GenContSamples <- SimCont(mug,sg,unique(y),ncont,eta)
    GenContSamples$index <- (nrow(X)+1):(nrow(X) +nrow(GenContSamples) )
    GenContSamples <- GenContSamples %>% dplyr::select(index, everything())
    colnames(GenContSamples)
    ncolumns <- ncol(GenContSamples)
    colnames(GenContSamples)[3:ncolumns] <-  colnames(X)
    GenContSamples$Cont <- 1
    colnames(GenContSamples)
    head(GenContSamples)
    
    # Generate a set with the composition required for each group  
    auxindnc <- funcSample(X,y,vpi)
    winedf <- data.frame(X)
    winedf$Cont <- 0
    winedf$class <- y
    winedf$index <- 1:nrow(X)
    winedf <- winedf %>% dplyr::select(index,class, everything())
    colnames(winedf)
    
    # Generate contaminated observation with the sane group composition used 
    # in non contaminated set
    auxindc <- funcSample(GenContSamples[,-1], GenContSamples$class, vpi)
    #    indsampleTrain_nc <- vector("list",G)
    GenContSamples$class
    
    for (g in 1:G)
    {
      # non contaminated set
      auxDfg <- winedf %>% filter(class == g)
      subsetg <- auxDfg[auxindnc[[g]],]
      Xgnc_train[[g]] <- subsetg %>% slice_sample(n=nocont_train[g], replace = FALSE)
      
      indsampleTrain_nc <- Xgnc_train[[g]]$index
      indsampleTest_nc <- setdiff(subsetg$index,indsampleTrain_nc)
      
      Xgnc_test[[g]] <-   subsetg %>% filter(index %in% indsampleTest_nc)
      
      
      # contaminated
      auxDfcont <- GenContSamples %>% filter (class == g)
      subsetgcont <- auxDfcont[auxindc[[g]],]
      
      Xgc_train[[g]] <- subsetgcont %>% slice_sample(n = ncont_train[g], replace = FALSE)
      
      indsampleTrain_c <- Xgc_train[[g]]$index
      indsampleTest_c <- setdiff(subsetgcont$index,indsampleTrain_c)
      
      Xgc_test[[g]] <- subsetgcont %>% filter(index %in% indsampleTest_c)
      
    }
    nrow(winedf)
    
    colnames(GenContSamples)
    table(GenContSamples$Cont)
    table(winedf$Cont)
    
    table(GenContSamples$class)
    table(winedf$class)
    
    # getting rid off index column    
    WineCont <- rbind.data.frame(winedf %>% dplyr::select(-index),
                                 GenContSamples %>% dplyr::select(-index)
    )
    colnames(WineCont)
    nrow(WineCont)
    
    auxTrain_nc <- ldply(Xgnc_train)
    auxTest_nc <- ldply(Xgnc_test)
    indnc_train <- auxTrain_nc$index
    
    table(auxTrain_nc$class)
    table(auxTest_nc$class)
    
    auxTrain_c <- ldply(Xgc_train)
    auxTest_c <- ldply(Xgc_test)
    indc_train <- auxTrain_c$index
    
    auxTrain_c$class
    
    #  DfTrain <- rbind.data.frame(winedf[indnc_train,]%>% dplyr::select(-index),
    #                                 GenContSamples[unlist(indc_train),] %>% dplyr::select(-index))
    
    
    # apply(Xgnc_train[[3]],2,function(x) any(is.na(x)))
    # apply(Xgc_train[[3]],2,function(x) any(is.na(x)))
    
    # apply(ldply(Xgnc_train),2,function(x) any(is.na(x)))
    
    # apply(ldply(Xgc_train),2,function(x) any(is.na(x)))
    Xgc_train[[2]]$class  
    
    dfTrain <- rbind.data.frame(ldply(Xgnc_train),ldply(Xgc_train))
    
    apply(dfTrain,2,function(x) any(is.na(x)))
    dfTrain$class
    dfTrain$Cont
    
    colnames(dfTrain)
    DfTrain <- dfTrain[sample(1:nrow(dfTrain)),]
    apply(dfTrain,2,function(x) any(is.na(x)))
    
    table(DfTrain$Cont)
    table(DfTrain$class)
    DfTrain$class
    
    apply(DfTrain,2,function(x) any(is.na(x)))
    
    dfTest <- rbind.data.frame(ldply(Xgnc_test),ldply(Xgc_test))
    
    # DfTest <- rbind.data.frame(winedf[-indnc_train,-2], 
    #                              GenContSamples[-indc_train,-2])
    
    colnames(dfTest)
    
    DfTest <- dfTest[sample(1:nrow(dfTest)),]
    table(DfTest$Cont)
    table(DfTest$class)    
    
    colnames(DfTrain)
    DfTrainX <- DfTrain %>% dplyr::select(-c(class,index,Cont))
    DfTrainl <- DfTrain$class
    DfTestX <- DfTest %>% dplyr::select(-c(class,index,Cont))
    DfTestl <- DfTest$class
    Train_subset[[i]] <-DfTrain
    Test_subset[[i]] <- DfTest
    #  SexTrain <- ifelse(BlueCrabsTrain$sex == 1, "F","M")
    #  SexTrain <- factor(SexTrain)
    ContTrain <- ifelse(DfTrain$Cont == 0, "NC","C")
    ContTrain <- factor(ContTrain)
    
    #SexTest <- ifelse(BlueCrabsTest$sex == 1, "F", "M")
    ContTest <- ifelse(DfTest$Cont == 0, "NC","C")
    
    
    colnames(DfTrain)
    colnames(DfTrainX)
    
    dfRW <- getOW(DfTrainX,DfTrainl)
    RW <- dfRW$Var
    variables_saturated_model <- RW
    
    # model including all variables
    saturated_mod  <- ModelAccuracy2(DfTrainX,
                                     DfTestX,
                                     as.numeric(DfTrainl),
                                     as.numeric(DfTestl),"EII",
                                     alpharef = 0.98, 
                                     tol = 0.01)
    
    saturated_mod
    AccuracyClassSatM_Nc[i] <- saturated_mod$accTestNc
    AccuracyClassSatM_C[i] <- saturated_mod$accTestC
    
    saturated_mod$accTestNc
    
    auxTestCont <- rep(0,length(DfTestl))
    
    for (j in 1:length(auxTestCont))
    {
      auxTestCont[j] <- 1-saturated_mod$predv[j,DfTestl[j]]
    }
    
    AccuracyContSatM_C[i] <-sum(auxTestCont == DfTest$Cont)/length(DfTest$Cont)
    
    # model including selected variables
    modSV <-fHLvarSearch2(DfTrainX
                          ,DfTestX,RW,
                          as.numeric(DfTrainl),
                          as.numeric(DfTestl),"E",
                          alpharef =0.99,tol=0.01,epsilon = 0)
    
    SVmodel[[i]] <- modSV$Selectedmodel
    AccuracyClassSV[i] <-  modSV$Accuracy
    
    modSV$posCM
    TestContSV <- rep(0,length(DfTestl))
    for (j in 1:length(TestContSV))
    {
      TestContSV[j] <- 1- modSV$models[[modSV$posCM]]$predv[j,DfTestl[j]]
    }
    AccuracyContSV[i] <- sum(TestContSV == DfTest$Cont)/ length(DfTest$Cont)
    
    
  }
  
  SVmodel1 <- lapply(SVmodel, function(i) paste(unlist(i),collapse = "-"))
  SVmodel1 <- ldply(SVmodel1)
  colnames(SVmodel1) <- "Model"
  SVmodel1
  
  aux_df <- data.frame(CR_SatMNc = AccuracyClassSatM_Nc,
                       CR_SatMC = AccuracyClassSatM_C,
                       Accuracy_SatCont = AccuracyContSatM_C,
                       CR_SV = AccuracyClassSV,
                       Accuracy_SVCont = AccuracyContSV)
  
  metrics_res <- aux_df %>% 
    summarise(Accuracy_SatMNc = mean(CR_SatMNc),
              Accuracy_SatMC = mean(CR_SatMC),
              Accuracy_SatCont = mean(Accuracy_SatCont),
              Accuracy_SV = mean(CR_SV),
              Accuracy_SVCont = mean(Accuracy_SVCont))
  
  
  df_resumen <- cbind.data.frame(SVmodel1,aux_df)
  
  output <-list ( Metrics_res = metrics_res, 
                  Metrics_models = df_resumen,
                  Train = Train_subset,
                  Test = Test_subset)
  
  return(output)
  
  
}




theta <- function(x,mu,sigma)
{
  output <- 0
  p <- length(x)
  invsigma <- solve(sigma)
  factor1 <- (2*pi)^(-p/2)
  factor2 <- (det(sigma))^(1/2)
  factor3 <- exp( -0.5*t(x-mu)%*%invsigma%*%(x-mu) )
  
  output <- factor1 * factor2 * factor3
  return(output)
}


loglikCMN<-function(X,l, par)
{
  
  mu <- as.matrix(par$mu)
  if(is.matrix(X) & ncol(X)==1) 
  { 
    if(length(dim(par$sigma))>2)
    {
      sg <- as.vector(par$sigma)
    }else if(!is.matrix(par$sigma))
    {
      sg <- rep(par$sigma,par$G)
    }
  }else  sg <- par$sigma
  
  G <- par$G
  pig <- par$pig
  alpha <- par$alpha
  eta <- par$eta
  v <- par$v
  
  m <- nrow(X)
  p <- ncol(X)
  
  l <- unmap(l)
  M <- matrix(0.0, nrow = m, ncol = G)
  
  
  
  for (g in 1:G)
  {
    for(i in 1:m)
    {
      term1 <- log(pig[g])
      if(length(dim(sg)) > 2)
      {
        term2 <- v[i,g] * (log(alpha[g]) + dMVNorm(X[i,],mu[,g],sg[,,g],log = TRUE) )
        term3<-(1-v[i,g]) * (log(1-alpha[g]) + dMVNorm(X[i,],mu[,g],eta[g]*sg[,,g], log = TRUE) )
        
      }else if(length(dim(sg))<=2)
      {
        if(ncol(X)>1)
        {
          s <- matrix(sg, ncol = p, nrow = p)
          term2 <- v[i,g] * ( log(alpha[g])+ dMVNorm(X[i,],mu[,g],data.matrix(s),log = TRUE ) )
          term3 <-(1-v[i,g]) * ( log (1-alpha[g]) + dMVNorm(X[i,],mu[,g],eta[g]*s,log = TRUE ) )
        } else if(ncol(X)==1)
        {
          mu <- as.vector(mu)
          term2 <- v[i,g] * log(alpha[g]) + dnorm(X[i,],mu[g],sg[g] , log = TRUE) 
          term3<-(1-v[i,g]) * log( (1-alpha[g])) + dnorm(X[i,],mu[g],eta[g]*sg[g], log = TRUE ) 
        }
      }
      
      if(ncol(l)> 1)
        M[i,g] <- l[i,g]*(term1 + term2 + term3)   
      else  M[i,g] <- l[i]*(term1 + term2 + term3)   
      
      
    }
  }
  
  loglik <- sum(M)
  
  return(loglik)
}


ModelAccuracy3 <- function(X_train1,X_test1,l_train,l_test,CE,
                           alpharef=0.98, tol = 0.0001)
{
  if(!is.matrix(X_train1)) X_train1 <- as.matrix(X_train1)
  if(!is.matrix(X_test1)) X_test1 <- as.matrix(X_test1)
  
  
  accTest_nc <- 0.0
  
  
  accTest_c <- 0.0
  
  output <- list()
  lmu <- list()
  lsigma <- list()
  lalpha <- list()
  leta <- list()
  diflog <- list()
  par <- list()
  nvar <- ncol(X_train1)
  nobs <- nrow(X_train1)
  G <- length(unique(l_train))
  if(is.null(alpharef)) alpharef <- rep(0.95,G)
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  if(ncol(X_train1)==1) CE <- "E" else CE <- "EII"
  
  #  if(ncol(X_train1)>1)
  #  {
  #    mstep0 <- CNmixt(X = X_train1, contamination = F, model = CE,
  #                     initialization = "mixt", start.z = unmap(l_train), G = 2 )
  #    estep0 <- mstep0$models
  #  }
  
  # Estimated parameters assuming no contaminated set
  mstep1 <-mstep(data = X_train1,modelName = CE, z = unmap(l_train))
  estep1 <- estep(data = X_test1, modelName = CE, 
                  parameters = mstep1$parameters)
  z <- estep1$z  
  ltest <- apply(z,1,which.max)
  accTest_nc <- sum(ltest == l_test)/length(l_test)
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma
  par$G <- mstep1$parameters$variance$G
  par$pig <- apply(unmap(l_train),2,sum)/nrow(X_train1)
  # give initial values for alpha
  par$alpha <- alpharef
  par$eta <- rep(1.011,G)
  #  cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
  estep2 <- eCmn(X_train1,par)
  vhat <- estep2$v
  #cat("\n","vij = ", estep2$v, "\n")
  lhat <-estep2$lhat
  par$v <- vhat  
  # Estimate parameters assuming contaminated set
  iter <- 1
  vr <- list()
  logc <- list()
  lmu[[iter]] <- par$mu
  lsigma[[iter]] <- par$sigma
  leta[[iter]] <- par$eta
  vr[[iter]] <- vhat
  logc[[iter]] <- loglikCMN(X_train1, l_train,par) 
  vr[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  diflog[[iter]] <- NA
  while ( iter < 3  | (diflog[[iter]] > tol & iter < 20) )
  {
    mstep2 <- mCmn(X_train1,l_train,par)
    par$mu <- mstep2$mu
    par$sigma <- mstep2$sigma
    par$eta <- mstep2$eta
    #cat("\n",par$eta,"\n")
    par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
    #par$alpha <- mstep2$alpha
    estep3 <- eCmn(X_train1,mstep2)
    iter <- iter + 1
    lmu[[iter]]<-par$mu
    lsigma[[iter]]<-par$sigma
    lalpha[[iter]]<-par$alpha
    leta[[iter]] <- par$eta
    vr[[iter]] <- estep3$v
    par$v <- vr[[iter]]
    
    logc[[iter-1]] <- loglikCMN(X_train1,l_train,par)
    
    if(iter > 2) { diflog[[iter]] <- abs(logc[[iter-2]] - logc[[iter - 1]])
    }else diflog[[iter]] <- NA    
    
    cat("\n","iter=",iter-1,";",logc[[iter-1]],";","diflog=",diflog[[iter]])
    #    cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
    #    cat("\n","Sigma =",par$sigma,"\n")
    #    cat(estep3$v, "\n")
    
    
  }
  estep4 <- eCmn(X_test1, mstep2)
  z <- estep4$z
  v <- estep4$v
  predv <- apply(v,2,function(x) ifelse(x<0.5,0,1))
  
  ltest <- apply(z,1,which.max)
  accTest_c <- sum(ltest == l_test)/length(l_test)
  
  
  output <- list(accTestNc = accTest_nc,accTestC = accTest_c, 
                 predlabel = ltest, predv = predv,
                 loglikelihood_nc = estep1$loglik,
                 loglikelihod = logc,
                 mu = lmu, sigma = lsigma, 
                 alpha = lalpha, eta = leta,
                 v = vr, diflog = diflog)
  
  return(output)
}

Supervised_fitting <- function(X_train1,X_test1,l_train,l_test,CE,alpharef=0.98, tol = 0.01)
{
  if(!is.matrix(X_train1)) X_train1 <- as.matrix(X_train1)
  if(!is.matrix(X_test1)) X_test1 <- as.matrix(X_test1)
  accTest_nc <- 0.0
  accTest_c <- 0.0
  laccTest_c <- list()
  ltest_r <- list()
  output <- list()
  lmu <- list()
  lsigma <- list()
  lalpha <- list()
  leta <- list()
  diflog <- list()
  par <- list()
  nvar <- ncol(X_train1)
  nobs <- nrow(X_train1)
  G <- length(unique(l_train))
  if(is.null(alpharef)) alpharef <- rep(0.95,G)
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  if(ncol(X_train1)==1) CE <- "E" else CE <- "EII"
  
  
  # Estimated parameters assuming no contaminated set
  
  
  mstep1 <-mstep(data = X_train1,modelName = CE, z = unmap(l_train))
  estep1 <- estep(data = X_test1, modelName = CE, 
                  parameters = mstep1$parameters)
  model0 <- CNmixt(X =X_train1, contamination = TRUE,
                   G = 2, model = "EII",label = l_train)
  summary(model0)
  str(model0$models)

  model0$models[[1]]$model

  model0$models[[1]]$contamination

  model0$models[[1]]$mu
  
  model0$models[[1]]$Sigma

  model0$models[[1]]$alpha

  model0$models[[1]]$eta  
  
  model0$models[[1]]$detection
  
  table(model0$models[[1]]$detection)
  
  model0$models[[1]]$v
  
  model0$models[[1]]$label
  
  model0$models[[1]]$group
  
  table(model0$models[[1]]$posterior)
  
  table(model0$models[[1]]$label,model0$models[[1]]$group)
  
  vest <- as.numeric(apply(model0$models[[1]]$v,1,function(x) which.max(x)))

  tail(model0$models[[1]]$detection)
  
  table(lv,vest)
  
  lalpha <- model0$models[[1]]$alpha

  z <- estep1$z  
  ltest <- apply(z,1,which.max)
  accTest_nc <- sum(ltest == l_test)/length(l_test)
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma
  par$G <- mstep1$parameters$variance$G
  par$pig <- apply(unmap(l_train),2,sum)/nrow(X_train1)
  # give initial values for alpha
  par$alpha <- alpharef
  par$eta <- rep(1.011,G)
  #  cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
  estep2 <- eCmn(X_train1,par)
  estep2_test <- eCmn(X_test1,par)
  vhat <- estep2$v
  #cat("\n","vij = ", estep2$v, "\n")
  lhat <-estep2$lhat
  par$v <- vhat  
  # Estimate parameters assuming contaminated set
  iter <- 1
  vtrain_r <- list()
  vtest_r <- list()
  logc <- list()
  lmu[[iter]] <- par$mu
  lsigma[[iter]] <- par$sigma
  leta[[iter]] <- par$eta
  vtrain_r[[iter]] <- vhat
  vtest_r[[iter]]<-estep2_test$v
  ltest_r[[iter]] <- lhat 
  logc[[iter]] <- loglikCMN(X_train1, l_train,par) 
  vtrain_r[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  diflog[[iter]] <- NA
  #cat("\n","iter=",iter,";","diflog=",diflog[[iter]])
  
  
  while ( iter < 3  | (iter <21) | (is.na(diflog[[1]]) & iter <=2)  )
  {
    mstep2 <- mCmn(X_train1,l_train,par)
    par$mu <- mstep2$mu
    par$sigma <- mstep2$sigma
    par$eta <- mstep2$eta
    #cat("\n",par$eta,"\n")
    par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
    #par$alpha <- mstep2$alpha
    estep3 <- eCmn(X_train1,mstep2)
    estep4 <- eCmn(X_test1, mstep2)
    z <- estep4$z
    v <- estep4$v
    ltest_r[[iter]]<-apply(z,1,which.max)
    vtest_r[[iter]]<-v
    predv <- apply(v,2,function(x) ifelse(x<0.5,0,1))
    ltest <- apply(z,1,which.max)
    laccTest_c[[iter]] <- sum(ltest == l_test)/length(l_test)
    iter <- iter + 1
    lmu[[iter]]<-par$mu
    lsigma[[iter]]<-par$sigma
    lalpha[[iter]]<-par$alpha
    leta[[iter]] <- par$eta
    vtrain_r[[iter]] <- estep3$v
    par$v <- vtrain_r[[iter]]
    
    logc[[iter-1]] <- loglikCMN(X_train1,l_train,par)
    
    if(iter > 2) { diflog[[iter]] <- abs(logc[[iter-2]] - logc[[iter - 1]])
    }else diflog[[iter]] <- NA    
    
    #cat("\n","iter=",iter-1,";",logc[[iter-1]],";","diflog=",diflog[[iter]])
    
    cat("\n","iter=",iter-1,";","diflog=",diflog[[iter]])
    
    #    cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
    #    cat("\n","Sigma =",par$sigma,"\n")
    #    cat(estep3$v, "\n")
    
    
  }
  accTest_c <- sum(ltest == l_test)/length(l_test)
  
  #laccTest_c: List containing global accuracy with contamination without
  # dividing it into contaminated and non-contaminated samples for the first
  # 20 steps of the EM algorithm
  #
  # ltest_r: List containing class predictions in the test set
  # predv: contain prediction whether samples are contaminated or non-contaminated
  #        in the test set at the last step of the EM-algorithm
  # lpredv: List containing prediction whether samples are contaminated or non-contaminated
  #         in the test set for all the steps run of the EM-algorithm
  
  output <- list(accTestNc = accTest_nc,accTestC = accTest_c,
                 niterations = iter, laccTest_c = laccTest_c,
                 predlabel = ltest, lpredlabel = ltest_r, 
                 predv = predv,lpredv = vtest_r,
                 loglikelihood_nc = estep1$loglik,
                 loglikelihod = logc,
                 mu = lmu, sigma = lsigma, 
                 alpha = lalpha, eta = leta,
                 v = vtrain_r, diflog = diflog)
  
  
  return(output)
}



ModelAccuracy2 <- function(X_train1,X_test1,l_train,l_test,CE,
                           niterations = 10,alpharef=0.98, tol = 0.01 )
{
  if(!is.matrix(X_train1)) X_train1 <- as.matrix(X_train1)
  if(!is.matrix(X_test1)) X_test1 <- as.matrix(X_test1)
  
  
  accTest_nc <- 0.0
  accTest_c <- 0.0
  laccTest_c <- list()
  ltest_r <- list()
  output <- list()
  lmu <- list()
  lsigma <- list()
  lalpha <- list()
  leta <- list()
  diflog <- list()
  par <- list()
  nvar <- ncol(X_train1)
  nobs <- nrow(X_train1)
  G <- length(unique(l_train))
  if(is.null(alpharef)) alpharef <- rep(0.95,G)
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  if(ncol(X_train1)==1) CE <- "E" else CE <- "EII"
  
  
  # Estimated parameters assuming no contaminated set
  mstep1 <-mstep(data = X_train1,modelName = CE, z = unmap(l_train))
  estep1 <- estep(data = X_test1, modelName = CE, 
                  parameters = mstep1$parameters)
  z <- estep1$z  
  ltest <- apply(z,1,which.max)
  accTest_nc <- sum(ltest == l_test)/length(l_test)
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma
  par$G <- mstep1$parameters$variance$G
  par$pig <- apply(unmap(l_train),2,sum)/nrow(X_train1)
  # give initial values for alpha
  par$alpha <- alpharef
  par$eta <- rep(1.011,G)
  #  cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
  estep2 <- eCmn(X_train1,l_train,par)
  estep2_test <- eCmn(X_test1,l_test,par)
  vhat <- estep2$v
  #cat("\n","vij = ", estep2$v, "\n")
  lhat <-estep2$lhat
  par$v <- vhat  
  # Estimate parameters assuming contaminated set
  iter <- 1
  vtrain_r <- list()
  vtest_r <- list()
  logc <- list()
  lmu[[iter]] <- par$mu
  lsigma[[iter]] <- par$sigma
  leta[[iter]] <- par$eta
  vtrain_r[[iter]] <- vhat
  vtest_r[[iter]]<-estep2_test$v
  ltest_r[[iter]] <- lhat 
  logc[[iter]] <- loglikCMN(X_train1, l_train,par) 
  vtrain_r[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  diflog[[iter]] <- NA
  #cat("\n","iter=",iter,";","diflog=",diflog[[iter]])
  
  
  while ( iter < 3  | (iter <= niterations) | (is.na(diflog[[1]]) & iter <=2)  )
  {
    mstep2 <- mCmn(X_train1,l_train,par)
    par$mu <- mstep2$mu
    par$sigma <- mstep2$sigma
    par$eta <- mstep2$eta
    #cat("\n",par$eta,"\n")
    par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
    #par$alpha <- mstep2$alpha
    estep3 <- eCmn(X_train1,l_train,mstep2)
    estep4 <- eCmn(X_test1,l_test,mstep2)
    z <- estep4$z
    v <- estep4$v
    ltest_r[[iter]]<-apply(z,1,which.max)
    vtest_r[[iter]]<-v
    predv <- apply(v,2,function(x) ifelse(x<0.5,0,1))
    ltest <- apply(z,1,which.max)
    laccTest_c[[iter]] <- sum(ltest == l_test)/length(l_test)
    iter <- iter + 1
    lmu[[iter]]<-par$mu
    lsigma[[iter]]<-par$sigma
    lalpha[[iter]]<-par$alpha
    leta[[iter]] <- par$eta
    vtrain_r[[iter]] <- estep3$v
    par$v <- vtrain_r[[iter]]
    
    logc[[iter-1]] <- loglikCMN(X_train1,l_train,par)
    
    if(iter > 2) { diflog[[iter]] <- abs(logc[[iter-2]] - logc[[iter - 1]])
    }else diflog[[iter]] <- NA    
    
    #cat("\n","iter=",iter-1,";",logc[[iter-1]],";","diflog=",diflog[[iter]])
    
    cat("\n","iter=",iter-1,";","diflog=",diflog[[iter]])
    
    #    cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
    #    cat("\n","Sigma =",par$sigma,"\n")
    #    cat(estep3$v, "\n")
    
    
  }
  accTest_c <- sum(ltest == l_test)/length(l_test)
  
  #laccTest_c: List containing global accuracy with contamination without
  # dividing it into contaminated and non-contaminated samples for the first
  # 20 steps of the EM algorithm
  #
  # ltest_r: List containing class predictions in the test set
  # predv: contain prediction whether samples are contaminated or non-contaminated
  #        in the test set at the last step of the EM-algorithm
  # lpredv: List containing prediction whether samples are contaminated or non-contaminated
  #         in the test set for all the steps run of the EM-algorithm
  
  output <- list(accTestNc = accTest_nc,accTestC = accTest_c,
                 niterations = iter, laccTest_c = laccTest_c,
                 predlabel = ltest, lpredlabel = ltest_r, 
                 predv = predv,lpredv = vtest_r,
                 loglikelihood_nc = estep1$loglik,
                 loglikelihod = logc,
                 mu = lmu, sigma = lsigma, 
                 alpha = lalpha, eta = leta,
                 v = vtrain_r, diflog = diflog)
  
  
  return(output)
}






ModelAccuracy1 <- function(X_train,X_test,l_train,l_test,CE,alpharef)
{
  if(!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if(!is.matrix(X_test)) X_test <- as.matrix(X_test)
  accTest <- 0.0
  par <- list()
  nvar <- ncol(X_train)
  nobs <- nrow(X_train)
  G <- length(unique(l_train))
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  
  mstep0 <- CNmixt(X = X_train, contamination = F, model = CE,
                   initialization = "mixt", start.z = unmap(l_train), G = 2 )
  estep0 <- 
    mstep0$models
  # Estimate parameters assuming uncontaminated set
  mstep1 <-mstep(data = X_train,modelName = CE, z = unmap(l_train))
  estep1 <- estep(data = X_test, modelName = CE, 
                  parameters = mstep1$parameters)
  z <- estep1$z  
  ltest <- apply(z,1,which.max)
  accTest <- sum(ltest == l_test)/length(l_test)
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma
  par$G <- mstep1$parameters$variance$G
  par$pig <- apply(unmap(l_train),2,sum)/nrow(X_train)
  # give initial values for alpha
  par$alpha <- alpharef
  par$eta <- rep(1.011,G)
  
  estep2 <- eCmn(X_train,par)
  vhat <- estep2$v
  lhat <-estep2$lhat
  par$v <- vhat  
  # Estimate parameters assuming contaminated set
  iter <- 1
  vr <- list()
  vr[[1]] <- vhat
  vr[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  flag2 = 0
  while (flag2 < 15)
  {
    mstep2 <- mCmn(X_train,l_train,par)
    par$mu <- mstep2$mu
    par$sigma <- mstep2$sigma
    par$eta <- mstep2$eta
    par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
    estep3 <- eCmn(X_train,mstep2)
    iter <- iter + 1
    vr[[iter]] <- estep3$v
    par$v <- estep3$v
    flag2 <- flag2 + 1
    
  }
  
  par$alpha
  par$eta
  par$v >0.5
  return(accTest)
}


eCmn1 <- function(Xtrain,par)
{
  m <- nrow(Xtrain)
  alpha <- par$alpha
  mu <- par$mu
  sigma <- par$sigma
  eta<-par$eta
  G <- par$G
  pig <- par$pig
  v <- matrix(0.0, ncol = G, nrow = m)
  z <- matrix(0.0, ncol = G, nrow = m)
  num1 <-matrix(0.0,ncol = G, nrow = m)  
  lhat <- rep(0,m)
  sum1 <- 0
  sum2 <- 0
  
  output <- list()
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      if(length(dim(par$sigma))==2)
      {
        num <- alpha[g] * dMVNorm(Xtrain[i,],mu[,g],sigma)
        den <- num + (1-alpha[g])*dMVNorm(Xtrain[i,],mu[,g],eta[g]*sigma)
        cat("\n",den,"\n")
        v[i,g] <- num/den 
        # num1 : numerator for the e-step for z[i,g]
        
      } else if(length(dim(par$sigma)) > 2)
      {
        num <- alpha[g] * dMVNorm(Xtrain[i,],mu[,g],sigma[,,g])
        den <- num + (1-alpha[g])*dMVNorm(Xtrain[i,],mu[,g],eta[g]*sigma[,,g])
        v[i,g] <- num/den 
        
      } #End-f
      
    }#End-for
    
  }#End=for
  
  # calculating zhat and lhat
  
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      if(length(dim(par$sigma))==2)
      {
        sum1 <- alpha[g] * dMVNorm(Xtrain[i,],mu[,g],sigma) 
        sum2 <- (1-alpha[g])*dMVNorm(Xtrain[i,],mu[,g],eta[g]*sigma)
      } else if(length(dim(par$sigma))>2)
      {
        sum1 <- alpha[g] * dMVNorm(Xtrain[i,],mu[,g],sigma[,,g])
        sum2 <- (1-alpha[g])*dMVNorm(Xtrain[i,],mu[,g],eta[g]*sigma[,,g])
      }
      num1[i,g] <- sum1 + sum2
    }
  }
  
  
  for (i in 1:m)
  {
    z[i,] <- num1[i,]/sum(num1[i,])
  }
  
  
  lhat<-apply(z,1,which.max)
  
  output <- list(v = v, z = z, lhat = lhat )
  return(output)
}

f_eta <- function(eta,z, v , X, mu, sigma)
{
  p <- ncol(X)
  m <- nrow(X)
  fv <- rep(0.0,m)
  output <- 0.0
  if(any(nrow(z),nrow(v))== m) stop("z and v has to be same length")
  sum1 <- 0
  sum2 <- 0
  
  for(i in 1:m)
  {
    sum1 <- sum1 + z[i]*(1-v[i])*log(eta)
    sum2 <- sum2 + mahalanobis(X[i,],mu,sigma)*z[i]* (1-v[i])/eta
  }
  
  sum1 <-(-p/2) *sum1
  sum2 <- (-1/2) * sum2
  output <- sum1 + sum2
  
  return(output)
  
}

mCmn1 <- function(Xtrain,ltrain,par,eta_max = 1000)
{
  m <- nrow(Xtrain)
  p <- ncol(Xtrain)
  G <- length(unique(ltrain))
  l <- unmap(ltrain)
  mu <- par$mu
  sigma <- par$sigma
  alpha <- par$alpha
  eta<-par$eta
  v <- par$v
  mg <- apply(unmap(ltrain),2,sum)
  pig <- mg/m
  fw <- matrix(0.0,nrow = m, ncol = G)
  fz <- matrix(0.0,nrow = m,ncol = G)
  fx <- array(0.0, dim = c(p,m,G))
  faux <- array(0.0, dim = c(p,p,m,G))
  
  factor1 <- 0
  factor2 <- 0
  factor3 <- 0 # Malahanobis distance
  
  S <- rep(0,G)
  
  eta1 <-  rep(0,G)
  alpha1 <- rep(0,G)
  mu1 <- matrix(0.0,nrow = p, ncol = G)
  sigma1 <- array(0.0, dim = c(p,p,G))
  W <- array(0.0, dim = c(p,p,G) )
  
  output <- list()
  
  # Calculate factor weight #
  for(g in 1:G)
    for(i in 1:m)
      fw[i,g] <- v[i,g] + (1-v[i,g])/eta[g]
  
  # Calculate Sg #
  
  for (g in 1:G)
  {
    for(i in 1:m)
      fz[i,g] <- l[i,g]*fw[i,g]    
  }
  S <- apply(fz,2,sum)
  
  # Calculate alpha1 #
  for(g in 1:G)
    alpha1[g] <- (l[,g] %*% v[,g])/mg[g]
  
  # Calculate mu1 #
  for (g in 1:G)
    for (i in 1:m)
      fx[,i,g] <- fz[i,g] * Xtrain[i,] /S[g]
  
  for(g in 1:G)
    mu1[,g] <- apply(fx[,,g],1,sum)
  
  # Calculate Sigma1 #
  for(g in 1:G)
  {  
    
    for(i in 1:m)
      faux[,,i,g] <- fz[i,g] * (Xtrain[i,] - mu1[,g]) %*% t(Xtrain[i,]-mu1[,g])
    
    for(i in 1:m)
      W[,,g] <- W[,,g] + faux[,,i,g]
    
    sigma1[,,g] <- W[,,g]/mg[g]
  }  
  
  # calculate eta1 #
  
  for(g in 1:G)
  {
    factor1 <- optimize(f_eta,c(1,eta_max),tol=0.0001, 
                        z = l[,g], v = v[,g], X = Xtrain,
                        mu = mu1[,g], sigma = sigma1[,,g], maximum = T)
    eta1[g] <- factor1$maximum 
    
  }
  
  output <- list(mu = mu1, sigma = sigma1, eta = eta1, 
                 alpha = alpha1, v = v, pig = pig, G = G)
  return(output)
}


Supervised_eCmn <- function(Xtrain,ltrain,par)
{
  m <- nrow(Xtrain)
  alpha <- par$alpha
  mu <- par$mu
  sigma <- par$sigma
  eta<-par$eta
  G <- par$G
  pig <- par$pig
  v <- matrix(0.0, ncol = G, nrow = m)
  z <- ltrain
  num <- matrix(0.0, ncol = G, nrow = m)
  den <- matrix(0.0, ncol = G, nrow = m)
  num1 <-matrix(0.0,ncol = G, nrow = m)  
  lhat <- rep(0,m)
  
  output <- list()
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      if(ncol(Xtrain) == 1 & is.vector(par$sigma) & length(par$sigma)==1)
      {
        num[i,g] <- alpha[g] * dnorm(Xtrain[i,],mu[g],sigma)
        den[i,g] <- num[i,g] + (1-alpha[g])*dnorm(Xtrain[i,],mu[g],eta[g]*sigma)
        
      } else if(length(dim(par$sigma))==2)
      {
        num[i,g] <- alpha[g] * dMVNorm(Xtrain[i,],mu[,g],sigma)
        den[i,g] <- num[i,g] + (1-alpha[g])*dMVNorm(Xtrain[i,],mu[,g],eta[g]*sigma)
        
      } else if(length(dim(par$sigma)) > 2)
      {
        if(ncol(Xtrain)>1)
        {
          num[i,g] <- alpha[g] * dMVNorm(Xtrain[i,],mu[,g],sigma[,,g])
          den[i,g] <- num[i,g] + (1-alpha[g])*dMVNorm(Xtrain[i,],mu[,g],eta[g]*sigma[,,g])
          if(den[i,g] == 0) den[i,g] <- 0.001
        }else if(ncol(Xtrain)==1)
        {
          if(is.null(dim(sigma)))
          {
            num[i,g] <- alpha[g] * dnorm(Xtrain[i,],mu[g],sigma)
            den[i,g] <- num[i,g] + (1-alpha[g])*dnorm(Xtrain[i,],mu[g],eta[g]*sigma)
          } else if(!is.null(dim(sigma)) )
          {
            num[i,g] <- alpha[g] * dnorm(Xtrain[i,],mu[g],sigma[,,g])
            den[i,g] <- num[i,g] + (1-alpha[g])*dnorm(Xtrain[i,],mu[g],eta[g]*sigma[,,g])
            
          }
          
        }
        
      } #End-f
      
      # avoid division by zero
      if (den[i,g]!= 0)
      {
        v[i,g] <- num[i,g]/den[i,g] 
        num1[i,g] <- pig[g]*den[i,g]
      }else if(den[i,g] == 0)
      {
        v[i,g] <- 1 
        num1[i,g] <- pig[g]*den[i,g]
        
      }
      
      
    }#End-for
    
  }#End=for
  
  # calculating zhat and lhat
  
  for (i in 1:m)
  {
    z[i,] <- num1[i,]/sum(num1[i,])
  }
  lhat<-apply(z,1,which.max)
  
  output <- list(v = v, z = z, lhat = lhat )
  return(output)
  
}

eCmn <- function(X,l,par)
{
  m <- nrow(X)
  alpha <- par$alpha
  mu <- par$mu
  sigma <- par$sigma
  eta<-par$eta
  G <- par$G
  pig <- par$pig
  v <- matrix(0.0, ncol = G, nrow = m)
  z <- matrix(0.0, ncol = G, nrow = m)
  thetaig <- matrix(0.0, ncol = G, nrow = m)
  denv <- matrix(0.0, ncol = G, nrow = m)
  fxig <- matrix(0.0, ncol = G, nrow = m)
  numz <- matrix(0.0, ncol = G, nrow = m)
  numv <- matrix(0.0, ncol = G, nrow = m)
  denzi <- rep(0,m)
  lhat <- rep(0,m)
  vhat <- rep(0,m)
  

  output <- list()
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      if(ncol(X) == 1 & is.vector(sigma) & length(sigma)==1)
      {
        # thetaig : matrix containing the probability of i-th observation in group g 
        # is not contaminated
        thetaig[i,g] <- dnorm(X[i,],mu[g],sigma)
        # fxig: matrix containing the probability of contaminated normal distribution for
        # observation i in group g
        fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dnorm(X[i,],mu[g],eta[g]*sigma)
        
      } else if(length(dim(sigma))==2)
      {
        thetaig[i,g] <- dMVNorm(X[i,],mu[,g],sigma)
        fxig[i,g] <- alpha[g]*thethaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],eta[g]*sigma)
        
      } else if(length(dim(sigma)) > 2)
      {
        if(ncol(X)>1)
        {
          thetaig[i,g] <-  dMVNorm(X[i,],mu[,g],sigma[,,g])
          fxig[i,g] <- alpha[g] * thetaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],eta[g]*sigma[,,g])

        }else if(ncol(X)==1)
        {
          if(is.null(dim(sigma)))
          {
            thetaig[i,g] <- dnorm(X[i,],mu[g],sigma)
            fxig[i,g] <- alpha[g] * thetaig[i,g] + (1-alpha[g])*dnorm(X[i,],mu[g],eta[g]*sigma)
          } else if(!is.null(dim(sigma)) )
          {
            thetaig[i,g] <- dnorm(X[i,],mu[g],sigma[,,g])
            fxig[i,g] <- alpha[g] * thetaig[i,g] +  (1-alpha[g])*dnorm(X[i,],mu[g],eta[g]*sigma[,,g])
            
          }
          
        }
        
      } #End-f
      
      # avoid division by zero
      numz[i,g] <- pig[g] * fxig[i,g]
      numv[i,g] <- alpha[g] * thetaig[i,g]
      denv[i,g] <- fxig[i,g]
      v[i,g] <- numv[i,g]/denv[i,g]
      
      
    }#End-for i
    
  }#End-for G
  
  # calculating zhat and lhat
  
  # calculating zhat and lhat
  denzi <- apply(numz,1,sum)
  for (i in 1:m)
  {
    z[i,] <- numz[i,]/denzi[i]
  }
  lhat<-apply(z,1,which.max)
  for (i in 1:m)
  {
    vhat[i] <- ifelse(v[i,l[i]]>0.5,1,0)
  }
  
  output <- list(z = z, v = v, lhat = lhat, vhat = vhat )
  return(output)
}

mCmn <- function(Xtrain,ltrain,par)
{
  Xtrain <- as.matrix(Xtrain)
  m <- nrow(Xtrain)
  p <- ncol(Xtrain)
  G <- length(unique(ltrain))
  l <- unmap(ltrain)
  mu <- par$mu
  sigma <- par$sigma
  alpha <- par$alpha
  eta<-par$eta
  v <- par$v
  mg <- apply(unmap(ltrain),2,sum)
  pig <- mg/m
  factor1 <- 0
  factor2 <- 0
  factor3 <- 0 # Malahanobis distance
  a <- rep(0,G)
  b <- rep(0,G)
  S <- rep(0,G)
  
  eta1 <-  rep(0,G)
  alpha1 <- rep(0,G)
  mu1 <- matrix(0.0,nrow = p, ncol = G)
  sigma1 <- array(0.0, dim = c(p,p,G))
  W <- array(0.0, dim = c(p,p,G) )
  
  output <- list()
  # Calculate Sg, (r+1)th iteration Sg,mu1, alpha1, and (r-th) a  
  
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      factor1 <-l[i,g]*(v[i,g] + (1-v[i,g])/eta[g] )
      S[g] <- S[g] + factor1
      mu1[,g] <- mu1[,g] + factor1 * Xtrain[i,]      
      alpha1[g] <- alpha1[g]+ (l[i,g]*v[i,g])
      a[g] <- a[g] + l[i,g]*(1-v[i,g])
      
      # equal covariance matrix
      
    }#End-for
    
    mu1[,g] <- mu1[,g]/S[g]  
    alpha1[g] <- alpha1[g]/mg[g]
  }#End=for
  
  # Calculate Wg and (r+1)th Sigma
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      factor2 <- l[i,g]*(v[i,g] + (1-v[i,g])/eta[g])
      W[,,g] <- W[,,g] + factor2 * (Xtrain[i,]-mu1[,g])%*%t(Xtrain[i,]-mu1[,g])
    }#End-for
    # Calculating Sigma1
    sigma1[,,g] <- W[,,g]/mg[g]
  }#End-for
  
  for (g in 1:G)
  {
    for( i in 1:m)
    {
      # factor 3: mahalanobis distance
      factor3 <- mahalanobis(Xtrain[i,],mu1[,g],sigma1[,,g])
      b[g] <- b[g] + l[i,g]*(1-v[i,g])*factor3
    }
    #    if(any(b[g] == 0))
    #      eta1[g] <- 1.001
    #    else  eta1[g]<-max(1.001,b[g]/(p*a[g]))
    if(a[g]!=0) 
    {
      eta1[g]<-max(1.001,b[g]/(p*a[g]))
    }else eta1[g] <- 1
  }
  
  output <- list(mu = mu1, sigma = sigma1, eta = eta1, 
                 v = v,alpha = alpha1, pig = pig, G = G)
  return(output)
}

emCmn <-function(Xtrain,ltrain,par)
{
  estep1<-eCmn(Xtrain,par)
  par$v <- estep1$v
  mstep1 <- mCmn(Xtrain,ltrain,par)
  return(mstep1)
}

EMContMN <- function(X_train,l_train,CE)
{
  if(length(dim(X_train)) == 2)
  {
    p <- ncol(X_train)
    m <- nrow(X_train)
  }
  z <- unmap(l_train)
  G <- ncol(z)
  mg <- rep(0,G)
  pig <- rep(0.0,G)
  Sg <- rep(0.0,G)
  etag <- rep(1.0, G)
  bg <- rep(0.0,G)
  mug <- matrix(0.0,ncol = p, nrow = G)
  W <- array(0.0, dim = c( p, p,G))
  
  weights <- matrix(0.0,ncol = G,nrow =m)
  Sigmag <- array(0.0, dim = c(p,p,G) )
  ag <- rep(0.0, G)
  ng <- apply(z,2,sum)
  pig <- ng/n
  # Initialize v's
  v <- matrix(runif(n*p),nrow = n, ncol = G)
  v_1 <- rep(0.0, nrow = nrow(z), ncol = ncol(z))
  v_2 <- rep(0.0, nrow = nrow(z), ncol = ncol(z))
  
  stop = F
  while(stop == F)
  {
    #  Updating r+1 mu_g 
    for (g in 1:G)
    {
      Sigmag[,,g] <- W[,,g]/ng[g]
    }
    
    
    
    for(g in 1:G)
    {
      # Sigma g (r+1)
      c 
      for(i in 1:n)
      {
        weights[i,g] <- ( v[i,g] + ( 1-v[i,g] ) /etag[g] )
        Sg[g] <- Sg[g] + z[i,g]* weights[i,g]
        mug[g,] <- mug[g,] + ( z[i,g] *  weights[i,g]*X_train[i,] )
      } # End-For
      mug[g,] <-mug[g,]/Sg[g]
      for(i in 1:n)
      {
        W[,,g] <- W[,,g]+ ( z[i,g] *  weights[i,g] ) * ( (X_train[i,]-mug[g,]) %*% t(X_train[i,]-mug[,g]) )
      }# End-For
    }
    mug
    Sg
    W[,,1]
    W[,,2]
  }
  
}
