# setwd("/home/jsancheg/git_environment/LDA_CMN/")
source("CMNFunctionsV2.R")

E_StepCMN <- function(X,l,par)
{
  # calculate z'ij
  # calculate v wheter the observation is contaminated or not
    m <- nrow(X)
    p <- ncol(X)
    G <- par$G
    pig <- par$pig
    mu <- par$mu
    sigma <- par$Sigma
    alpha <- par$alpha
    eta<-par$eta
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
    
    for(g in 1:G)
    {
      for(i in 1:m)
      {
        
        if(p == 1 )
        { 
          if(is.vector(sigma))
          { 
            if(length(sigma)==1)
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

 #           if(denv[i,g] == 0) denv[i,g] <- 0.001
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
            
#            if(denv[i,g] == 0) denv[i,g] <- 0.001
            
          }
          
        } 
          } #End-if Sigma is vector
          if(is.array(sigma))
          {
              if(length(sigma) == G)
              {
                thetaig[i,g] <- dnorm(X[i,],mu[g],sigma[,,g])
                fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dnorm(X[i,],mu[g],eta[g]*sigma[,,g])
                
              }
          }
        }#End-f ncol(X) == 1
        
        if(p > 1)
        {
          thetaig[i,g] <- dMVNorm(X[i,],mu[,g],sigma[,,g])
          fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],eta[g]*sigma[,,g])
          
        }
        
        numz[i,g] <- pig[g] * fxig[i,g]
        numv[i,g] <- alpha[g] * thetaig[i,g]
        denv[i,g] <- fxig[i,g]
        v[i,g] <- numv[i,g]/denv[i,g]
        
      }#End-for i
      
    }#End-for G
    
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
#    fvlabel <- function(i,l,v) {
#      return( ifelse(v[i,l[i]]>0.5,1,0)  )
#    }
#    vhat <- sapply(1:length(lhat),fvlabel,x,l,v) 
    
    output <- list(z = z, v = v,  lhat = lhat, vhat = vhat)
    return(output)
}



SemiSupervisedFitting_Version1 <- function(X_train, X_test, ltrain, ltest,
                                  vtest, model = "EEI",
                                  pnolabeled = 0.5,
                                  iterations = 10, 
                                  alpharef = 0.75, tol = 0.01)
  # X_train:       matrix with the observations used in training
  # X_test:        matrix with the observations used in testing
  # ltrain:       labels used in training
  # ltest:        labels used in test
  # pnolabeled:   percentage of no labeled observations in ltrain 
  # model:        model used
  # iterations:   maximum number of iterations
  # alpharef:     reference for alpha
  # tol:          tolerance
{
  parameters_C <- list()
  parameters_Nc <- list()
  estimate <- list()
  # logl_c: log likelihood for contaminated model
  logl_c <- 0
  obslll_c <- 0
  accTest_C <- 0
  CCRTest_C <- 0
  
  # logl_nc: log likelihood for non-contaminated model
  logl_nc <- 0
  obslll_nc <- 0
  accTest_Nc <- 0
  
  G <- length(unique(ltrain))
  
  ntrain <- length(ltrain)
  ltrain1 <- ltrain
  # nolebeled: contains the number of unlabeled
  nolabeled <- floor(pnolabeled*ntrain)
  ind_nolabeled <- sample(1:ntrain,nolabeled,replace = FALSE)
  ltrain1[ind_nolabeled] <- 0
  #table(ltrain1)
  
  if(ncol(X_train) == 1) 
  {
    ModelNonCont <- CNmixt(X_train,G, contamination = FALSE,model = model,
                           initialization = "random.post",alphamin = alpharef,
                           label = ltrain1, iter.max = iterations)
    resNC <- getBestModel(ModelNonCont, criterion = "BIC")
    ModelCont <- CNmixt(X_train,G, contamination = TRUE,model = model,
                        initialization = "random.post",alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    resC <- getBestModel(ModelCont,criterion = "BIC")
    #    res1 <- CNmixt(X_train,G,  model = model, 
    #                  initialization = "random.post", alphamin = alpharef,
    #                  label = ltrain1,iter.max = iterations)
    
  }else if(ncol(X_train) > 1)
  {
    ModelNonCont <- CNmixt(X_train,G, contamination = FALSE,model = model,
                           initialization = "mixt",alphamin = alpharef,
                           label = ltrain1, iter.max = iterations)
    resNC <- getBestModel(ModelNonCont, criterion = "BIC")
    ModelCont <- CNmixt(X_train,G, contamination = TRUE,model = model,
                        initialization = "mixt",alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    resC <- getBestModel(ModelCont,criterion = "BIC")
  }
  
  #res1 <- ModelAccuracy2(X_train,X_test,ltrain,ltest,"EEI")
  
  logl_nc <- resNC$models[[1]]$loglik
  obslll_nc <- resNC$models[[1]]$obslll
  
  logl_c <- resC$models[[1]]$loglik
  obslll_c <- resC$models[[1]]$obslll
  
  parameters_C$G <- resC$models[[1]]$G
  parameters_C$pig <- resC$models[[1]]$prior
  parameters_C$mu <- resC$models[[1]]$mu
  parameters_C$Sigma <-resC$models[[1]]$Sigma
  parameters_C$InvSigma <- resC$models[[1]]$invSigma
  parameters_C$alpha <-resC$models[[1]]$alpha
  parameters_C$eta <- resC$models[[1]]$eta
  
  # estimate contaminated model  
  estimate$ztrain_hat <- resC$models[[1]]$posterior
  # estimated class labels
  estimate$ltrain_hat <- resC$models[[1]]$group
  estimate$vtrain_hat <- resC$models[[1]]$v
  estimate$badPoints <- resC$models[[1]]$detection
  
  parameters_Nc$pro <- resNC$models[[1]]$prior
  parameters_Nc$mean <- resNC$models[[1]]$mean
  parameters_Nc$variance <- resNC$models[[1]]$Sigma
  
  table(ltrain1,resC$models[[1]]$group)
  
  if(ncol(X_train) == 1)
  {
    mstep_nc <- mclust::mstep( data = as.matrix(X_train), modelName = resNC$models[[1]]$model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =as.matrix(X_test), modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }else if(ncol(X_train) > 1)
  {
    mstep_nc <- mclust::mstep( data = X_train, modelName = resNC$models[[1]]$model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =X_test, modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }
  mstep_nc$modelName
  mstep_nc$parameters
  
  ltest_hat_nc <- apply(estep_nc$z,1,which.max)
  CCRTest_Nc <- sum((ltest_hat_nc == ltest)) / length(ltest)
  CCRTest_Nc
  
  ExpectedValues_C <- E_StepCMN(X_test,ltest,parameters_C)
  
  if (length(ExpectedValues_C$lhat)==length(ltest)){
    CCRTest_C <- sum((ExpectedValues_C$lhat == ltest)) / length(ltest)
  }  else CCRTest_C = -1 
  
  if (length(ExpectedValues_C$vhat) == length(vtest))
  {
    AccTest_C <- sum((ExpectedValues_C$vhat == vtest)) / length(vtest)
  } else AccTest_C =  - 1
  
  resC$models[[1]]$v
  
  resC$models[[1]]$label
  resC$models[[1]]$entropy
  resC$models[[1]]$IC
  
  table(resC$models[[1]]$label,resC$models[[1]]$group)
  table(ltrain,resC$models[[1]]$group)
  table(ltrain1,ltrain)
  length(resC$models[[1]]$group)
  
  
  
  Output <- list(CCRTestNc = CCRTest_Nc,CCRTestC = CCRTest_C,
                 ztest_hat_NC = estep_nc$z,
                 ltest_hat_NC =  ltest_hat_nc,
                 ztest_hat_C = ExpectedValues_C$z,
                 ltest_hat_C = ExpectedValues_C$lhat,
                 Expected_v = ExpectedValues_C$v,
                 vtest_hat = ExpectedValues_C$vhat,
                 niterations = iterations, 
                 fitted_NC_model = resNC$models[[1]]$model,
                 fitted_C_model = resC$models[[1]]$model,                 
                 par = parameters_C)
  return(Output)  
}


SemiSupervisedFitting <- function(X_train, X_test, ltrain, ltest,
                                 vtest, model = "EEI",
                                 pnolabeled = 0.5,
                                 iterations = 10, 
                                 alpharef = 0.75, tol = 0.01)
# X_train:       matrix with the observations used in training
# X_test:        matrix with the observations used in testing
# ltrain:       labels used in training
# ltest:        labels used in test
# pnolabeled:   percentage of no labeled observations in ltrain 
# model:        model used
# iterations:   maximum number of iterations
# alpharef:     reference for alpha
# tol:          tolerance
{
  parameters_C <- list()
  parameters_Nc <- list()
  estimate <- list()
  # logl_c: log likelihood for contaminated model
  logl_c <- 0
  obslll_c <- 0
  accTest_C <- 0
  CCRTest_C <- 0
  
  # logl_nc: log likelihood for non-contaminated model
  logl_nc <- 0
  obslll_nc <- 0
  accTest_Nc <- 0
  
  G <- length(unique(ltrain))
  
  ntrain <- length(ltrain)
  ltrain1 <- ltrain
  # nolebeled: contains the number of unlabeled
  nolabeled <- floor(pnolabeled*ntrain)
  ind_nolabeled <- sample(1:ntrain,nolabeled,replace = FALSE)
  ltrain1[ind_nolabeled] <- 0
  #table(ltrain1)
  
  if(ncol(X_train) == 1) 
  {
    ModelNonCont <- CNmixt(X_train,G, contamination = FALSE,model = model,
                   initialization = "random.post",alphamin = alpharef,
                   label = ltrain1, iter.max = iterations)
    resNC <- getBestModel(ModelNonCont, criterion = "BIC")
    ModelCont <- CNmixt(X_train,G, contamination = TRUE,model = model,
                        initialization = "random.post",alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    resC <- getBestModel(ModelCont,criterion = "BIC")
#    res1 <- CNmixt(X_train,G,  model = model, 
#                  initialization = "random.post", alphamin = alpharef,
#                  label = ltrain1,iter.max = iterations)
    
  }else if(ncol(X_train) > 1)
  {
    ModelNonCont <- CNmixt(X_train,G, contamination = FALSE,model = model,
                           initialization = "mixt",alphamin = alpharef,
                           label = ltrain1, iter.max = iterations)
    resNC <- getBestModel(ModelNonCont, criterion = "BIC")
    ModelCont <- CNmixt(X_train,G, contamination = TRUE,model = model,
                        initialization = "mixt",alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    resC <- getBestModel(ModelCont,criterion = "BIC")
  }
        
  #res1 <- ModelAccuracy2(X_train,X_test,ltrain,ltest,"EEI")
  
  logl_nc <- resNC$models[[1]]$loglik
  obslll_nc <- resNC$models[[1]]$obslll
  
  logl_c <- resC$models[[1]]$loglik
  obslll_c <- resC$models[[1]]$obslll
  
  parameters_C$G <- resC$models[[1]]$G
  parameters_C$pig <- resC$models[[1]]$prior
  parameters_C$mu <- resC$models[[1]]$mu
  parameters_C$Sigma <-resC$models[[1]]$Sigma
  parameters_C$InvSigma <- resC$models[[1]]$invSigma
  parameters_C$alpha <-resC$models[[1]]$alpha
  parameters_C$eta <- resC$models[[1]]$eta
  
  # estimate contaminated model  
  estimate$ztrain_hat <- resC$models[[1]]$posterior
  # estimated class labels
  estimate$ltrain_hat <- resC$models[[1]]$group
  estimate$vtrain_hat <- resC$models[[1]]$v
  estimate$badPoints <- resC$models[[1]]$detection

  parameters_Nc$pro <- resNC$models[[1]]$prior
  parameters_Nc$mean <- resNC$models[[1]]$mean
  parameters_Nc$variance <- resNC$models[[1]]$Sigma
  
  table(ltrain1,resC$models[[1]]$group)
  
  if(ncol(X_train) == 1)
  {
    mstep_nc <- mclust::mstep( data = as.matrix(X_train), modelName = resNC$models[[1]]$model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =as.matrix(X_test), modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }else if(ncol(X_train) > 1)
  {
    mstep_nc <- mclust::mstep( data = X_train, modelName = resNC$models[[1]]$model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =X_test, modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }
  mstep_nc$modelName
  mstep_nc$parameters
  
  ltest_hat_nc <- apply(estep_nc$z,1,which.max)
  CCRTest_Nc <- sum((ltest_hat_nc == ltest)) / length(ltest)
  CCRTest_Nc

  ExpectedValues_C <- E_StepCMN(X_test,ltest,parameters_C)
  
  if (length(ExpectedValues_C$lhat)==length(ltest)){
    CCRTest_C <- sum((ExpectedValues_C$lhat == ltest)) / length(ltest)
  }  else CCRTest_C = -1 
  
  if (length(ExpectedValues_C$vhat) == length(vtest))
  {
    AccTest_C <- sum((ExpectedValues_C$vhat == vtest)) / length(vtest)
  } else AccTest_C =  - 1
  
  resC$models[[1]]$v
  
  resC$models[[1]]$label
  resC$models[[1]]$entropy
  resC$models[[1]]$IC
  
  table(resC$models[[1]]$label,resC$models[[1]]$group)
  table(ltrain,resC$models[[1]]$group)
  table(ltrain1,ltrain)
  length(resC$models[[1]]$group)
  
  ExpectedValues_C_train <- E_StepCMN(as.matrix(X_train),ltrain, parameters_C )
  
  pseudo_label_info <- list(Unlabelled_index = ind_nolabeled,
                            Unlabelled_data = X_train[ind_nolabeled, ],
                            ltrain <- ltrain, 
#                            vtrain <- vtrain, 
                            True_labels_Class_train = vtrain[ind_nolabeled],
#                           True_labels_V_train = vtrain[ind_nolaneled],
                            lhat_train <- ExpectedValues_C_train$lhat,
                            vhat_Train <- ExpectedValues_C_train$vhat,
                            Pseudo_labels_Class_Train = ExpectedValues_C_train$lhat[ind_nolabeled],
                            Pseudo_labels_V_Train = ExpectedValues_C_train$vhat[ind_nolabeled],
                            Percentage_of_no_labeled = pnolabeled)
  
  Output <- list(CCRTestNc = CCRTest_Nc,CCRTestC = CCRTest_C,
                 ztest_hat_NC = estep_nc$z,
                 ltest_hat_NC =  ltest_hat_nc,
                 ztest_hat_C = ExpectedValues_C$z,
                 ltest_hat_C = ExpectedValues_C$lhat,
                 Expected_v = ExpectedValues_C$v,
                 vtest_hat = ExpectedValues_C$vhat,
                 niterations = iterations, 
                 fitted_NC_model = resNC$models[[1]]$model,
                 fitted_C_model = resC$models[[1]]$model,                 
                 par = parameters_C,
                 pseudo_label_info = pseudo_label_info)
    return(Output)  
}


GreedySearch <- function(Xtrain, Xtest, RW, ltrain, ltest,
                           vtest,CE,
                           pnolabeled = 0.5,iterations = 10,
                           alpharef = 0.75, 
                           tol = 0.01, epsilon = 0)
  # Xtrain :  matrix containing observations used in training
  # Xtest  :  matrix containing observations used in test
  # RW     :  list of variables ordered according the F-statistics
  # ltrain :  vector containing the group information for observations in the training
  # ltest  :  vector containing the group information for observations in the test
  # vtest  :
  # CE     :  vector with the models to be tested
  
  
{
  # forward heading variable selection
  
  p <- length(RW)
  
  # Available ranking   
  ARW <- NULL
  CM <- NULL
  VisitededVariables1 <- NULL
  VisitededVariables2 <- NULL
  
  posCM <- 0
  OldCM <- "NA"
  PM <- NULL
  CCRPM <- 0
  CCRCM <- 0
  OldCCRCM <- 0
  cont <- 0
  nIterToConvergence <- 0
  models <- list()
  
  if(!is.data.frame(Xtrain)) Xtrain <- data.frame(Xtrain)
  if(!is.data.frame(Xtest)) Xtest <- data.frame(Xtest)
  
  for (cont in 1:p)
  {
    
    PM <- RW[cont]
    X_train <- Xtrain %>% dplyr::select(all_of(PM))
    X_test <- Xtest %>% dplyr::select(all_of(PM))
    
    
    cat("\n Model ",unlist(PM))
    
    models[[cont]] <- SemiSupervisedFitting(X_train,X_test,ltrain,ltest,
                                            vtest,model = c("E","V"), pnolabeled, 
                                            iterations = iterations, 
                                            alpharef = alpharef )
    
    models[[cont]]$PM <- PM
    CCRPM <-models[[cont]]$CCRTestC
    if(CCRPM > CCRCM)
    {
      CM <- PM
      CCRCM <- CCRPM
      # save the position on the list 
      # of the model selected
      posCM <- cont
    }
  }
  
  CM <- RW[[which.max(sapply(models, function(m) m$CCRTestC  ) ) ]]
  CCRCM <- max(sapply(models,function(m) m$CCRTestC ))
  posCM <- which.max(sapply(models,function(m) m$CCRTestC ))
  cont <- length(models) + 1
  # !setequal(CM,RW) continue while the variables in the current model does not include
  # all the variables
  # (OldCCRCM < CCRCM & !setequal(CM,RW) ) continue while all variables are not 
  # included in the model and the inclusion of all the variables is the best model
  # 
  
  while( (!setequal(OldCM,CM) & OldCCRCM < CCRCM & 
          !setequal(VisitededVariables2,ARW) ) | 
         (OldCCRCM < CCRCM & !setequal(CM,RW) )   )
  {
    OldCM <- CM
    OldCCRCM <- CCRCM
    # ARW: available ranked variables
    # difference between set of all ranked variables and variables in the current model
    ARW <- setdiff(RW,as.vector(CM))
    nARW <- length(ARW)
    
    if (nARW != 0) 
    {
      
      PMs <- lapply(ARW, function(var) union(CM,var) )
      models_new <- lapply(PMs, function(PM) {
        X_train1 <- Xtrain[,PM]
        X_test1 <- Xtest[,PM]
        SemiSupervisedFitting(X_train1, X_test1, ltrain, ltest, 
                              vtest, pnolabeled, model = CE,
                              iterations = iterations, 
                              alpharef = alpharef)
      })
    } # end-f nARW
    
    CCRPMs <- sapply(models_new, function(m) m$CCRTestC )
    
    best_index <- which.max(CCRPMs)
    
    for(i in 1:length(models_new))
    {
      models[[cont]] <- models_new[[i]]
      models[[cont]]$PM <- PMs[[i]]
      
      models_new[[i]] <- PMs[[i]]
      cont = cont + 1
    }
    
    best_index <- which.max(CCRPMs)
    
    if(CCRPMs[best_index] > CCRCM)
    {
      CM <- PMs[[best_index]]
      CCRCM <- CCRPMs[best_index]
      posCM <- cont - 1
    }
    
#    cat("\n", " File: ", file_name, " Simulation: ", i_sim, " - ", cont - 1, ", model = ", unlist(CM), "\n")
    
  }  

  # laccTest_c : contains the global accuracy of predicting classes 
  # without separating samples in groups of contaminated and 
  # non-contaminated samples for the first 20
  # iterations of the EM algorithm
  
  return(list(fitted_NC_model = models[[posCM]]$fitted_NC_model,
              fitted_C_model = models[[posCM]]$fitted_NC_model,
              Selectedmodel = CM,CCRCM = CCRCM, 
              Classprediction = models[[posCM]]$ltest_hat_C,
              ContaminatedSamplesprediction = models[[posCM]]$vtest_hat,
              posCM = posCM,  iterations = cont ,models = models))             
}



HeadLongSearch <- function(Xtrain, Xtest, RW, ltrain, ltest,
                           vtest,CE,
                           pnolabeled = 0.5,iterations = 10,
                           alpharef = 0.75, 
                           tol = 0.01, epsilon = 0)
  # Xtrain :  matrix containing observations used in training
  # Xtest  :  matrix containing observations used in test
  # RW     :  list of variables ordered according the F-statistics
  # ltrain :  vector containing the group information for observations in the training
  # ltest  :  vector containing the group information for observations in the test
  # vtest  :
  # CE     :  vector with the models to be tested
  
  
{
  # forward heading variable selection
  
  p <- length(RW)
  
  # Available ranking   
  ARW <- NULL
  CM <- NULL
  VisitededVariables1 <- NULL
  VisitededVariables2 <- NULL
  
  posCM <- 0
  OldCM <- "NA"
  PM <- NULL
  CCRPM <- 0
  CCRCM <- 0
  OldCCRCM <- 0
  cont <- 0
  nIterToConvergence <- 0
  models <- list()
  
  if(!is.data.frame(Xtrain)) Xtrain <- data.frame(Xtrain)
  if(!is.data.frame(Xtest)) Xtest <- data.frame(Xtest)
  
  for (cont in 1:p)
  {
    
    PM <- RW[cont]
    X_train <- Xtrain %>% dplyr::select(all_of(PM))
    X_test <- Xtest %>% dplyr::select(all_of(PM))
    
    
    cat("\n Model ",unlist(PM))
      
    models[[cont]] <- SemiSupervisedFitting(X_train,X_test,ltrain,ltest,
                                            vtest,model = c("E","V"), pnolabeled, 
                                           iterations = iterations, 
                                           alpharef = alpharef )
    
    models[[cont]]$PM <- PM
    CCRPM <-models[[cont]]$CCRTestC
    if(CCRPM > CCRCM)
    {
      CM <- PM
      CCRCM <- CCRPM
      # save the position on the list 
      # of the model selected
      posCM <- cont
    }
  }
  
  cont <- cont + 1
  # !setequal(CM,RW) continue while the variables in the current model does not include
  # all the variables
  # (OldCCRCM < CCRCM & !setequal(CM,RW) ) continue while all variables are not 
  # included in the model and the inclusion of all the variables is the best model
  # 
  
  while( (!setequal(OldCM,CM) & OldCCRCM < CCRCM & 
        !setequal(VisitededVariables2,ARW) ) | 
        (OldCCRCM < CCRCM & !setequal(CM,RW) )   )
  {
    OldCM <- CM
    OldCCRCM <- CCRCM
    # ARW: available ranked variables
    # difference between set of all ranked variables and variables in the current model
    ARW <- setdiff(RW,as.vector(CM))
    nARW <- length(ARW)
    
    if (nARW != 0) 
    {
      
      j <- 1
      VisitededVariables2 <- NULL
      
      while( !setequal(ARW,VisitededVariables2) & j <= nARW)
      {
        PM <- union(CM,ARW[j])
        VisitededVariables2 <- union(VisitededVariables2,ARW[j])
        X_train1 <- Xtrain %>% dplyr::select(all_of(PM))
        X_test1 <- Xtest %>% dplyr::select(all_of(PM))

        models[[cont]] <-  SemiSupervisedFitting(X_train1,X_test1,ltrain,ltest,
                              vtest,pnolabeled, model = CE,
                              iterations = iterations, 
                              alpharef = alpharef )
        
        
        models[[cont]]$PM <- PM
        cat("\n",cont," ,model = ",unlist(PM),"\n")
        CCRPM <- models[[cont]]$CCRTestC

        cont <- cont + 1
        
          if(CCRPM > CCRCM)
          {
            CM <- PM
            CCRCM <- CCRPM 
            posCM <- cont-1
            nIterToConvergence <- models[[cont-1]]$niterations
          } # end if
          j <- j + 1
      } # end while (stop2 == F & j <= nARW)
    } # end-if
    
  } # end while
  
  # laccTest_c : contains the global accuracy of predicting classes 
  # without separating samples in groups of contaminated and 
  # non-contaminated samples for the first 20
  # iterations of the EM algorithm
  
  return(list(fitted_NC_model = models[[posCM]]$fitted_NC_model,
              fitted_C_model = models[[posCM]]$fitted_NC_model,
              Selectedmodel = CM,CCRCM = CCRCM, 
              Classprediction = models[[posCM]]$ltest_hat_C,
              ContaminatedSamplesprediction = models[[posCM]]$vtest_hat,
              posCM = posCM,  iterations = cont ,models = models))             
}


HeadLongSearch_mod <- function(Xtrain, Xtest, RW, ltrain, ltest,
                           vtest,CE,
                           pnolabeled = 0.5,iterations = 10,
                           alpharef = 0.75, 
                           tol = 0.01, epsilon = 0,i_sim = NULL,file_name = NULL )
  # Xtrain :  matrix containing observations used in training
  # Xtest  :  matrix containing observations used in test
  # RW     :  list of variables ordered according the F-statistics
  # ltrain :  vector containing the group information for observations in the training
  # ltest  :  vector containing the group information for observations in the test
  # vtest  : vector containing the contaminated information for observations in the test
  # CE     :  vector with the models to be tested
  
  
{
  # forward heading variable selection
  
  p <- length(RW)
  
  # Available ranking   
  ARW <- NULL
  CM <- NULL
  VisitededVariables1 <- NULL
  VisitededVariables2 <- NULL
  
  posCM <- 0
  OldCM <- "NA"
  PM <- NULL
  CCRPM <- 0
  CCRCM <- 0
  OldCCRCM <- 0
  cont <- 0
  nIterToConvergence <- 0
  models <- list()
  
  if(!is.data.frame(Xtrain)) Xtrain <- data.frame(Xtrain)
  if(!is.data.frame(Xtest)) Xtest <- data.frame(Xtest)
  
  for (cont in 1:p)
  {
    
    PM <- RW[cont]
    X_train <- Xtrain %>% dplyr::select(all_of(PM))
    X_test <- Xtest %>% dplyr::select(all_of(PM))
    
    
    cat("\n"," File : ", file_name, " - Simulation: ",i_sim,"Model ",unlist(PM))
    
    models[[cont]] <- SemiSupervisedFitting(X_train,X_test,ltrain,ltest,
                                            vtest,model = c("E","V"), pnolabeled, 
                                            iterations = iterations, 
                                            alpharef = alpharef )
    
    models[[cont]]$PM <- PM
    CCRPM <-models[[cont]]$CCRTestC
    if(CCRPM > CCRCM)
    {
      CM <- PM
      CCRCM <- CCRPM
      # save the position on the list 
      # of the model selected
      posCM <- cont
    }
  }
  
  cont <- cont + 1
  # !setequal(CM,RW) continue while the variables in the current model does not include
  # all the variables
  # (OldCCRCM < CCRCM & !setequal(CM,RW) ) continue while all variables are not 
  # included in the model and the inclusion of all the variables is the best model
  # 
  exit <- 0
  
  while( (!setequal(OldCM,CM) & OldCCRCM < CCRCM & 
          !setequal(VisitededVariables2,ARW) ) & exit == 0 | 
         (OldCCRCM < CCRCM & !setequal(CM,RW) ) & exit == 0 )
  {
    OldCM <- CM
    OldCCRCM <- CCRCM
    # ARW: available ranked variables
    # difference between set of all ranked variables and variables in the current model
    ARW <- setdiff(RW,as.vector(CM))
    nARW <- length(ARW)
    
    if (nARW != 0) 
    {
      
      j <- 1
      VisitededVariables2 <- NULL
      
      while( !setequal(ARW,VisitededVariables2) & j <= nARW)
      {
        PM <- union(CM,ARW[j])
        VisitededVariables2 <- union(VisitededVariables2,ARW[j])
        X_train1 <- Xtrain %>% dplyr::select(all_of(PM))
        X_test1 <- Xtest %>% dplyr::select(all_of(PM))
        
        models[[cont]] <-  SemiSupervisedFitting(X_train1,X_test1,ltrain,ltest,
                                                 vtest,pnolabeled, model = CE,
                                                 iterations = iterations, 
                                                 alpharef = alpharef )
        
        
        models[[cont]]$PM <- PM
        cat("\n"," File : ", file_name, " - Simulation: ",i_sim,  " ,model = ",unlist(PM),"\n")
        CCRPM <- models[[cont]]$CCRTestC
        
        cont <- cont + 1
        
        if(CCRPM > CCRCM)
        {
          CM <- PM
          CCRCM <- CCRPM 
          posCM <- cont-1
          nIterToConvergence <- models[[cont-1]]$niterations
          exit <- 1
          break
        } # end if
        j <- j + 1
      } # end while (stop2 == F & j <= nARW)
    } # end-if
    
  } # end while
  
  # laccTest_c : contains the global accuracy of predicting classes 
  # without separating samples in groups of contaminated and 
  # non-contaminated samples for the first 20
  # iterations of the EM algorithm
  
  return(list(fitted_NC_model = models[[posCM]]$fitted_NC_model,
              fitted_C_model = models[[posCM]]$fitted_NC_model,
              Selectedmodel = CM,CCRCM = CCRCM, 
              Classprediction = models[[posCM]]$ltest_hat_C,
              ContaminatedSamplesprediction = models[[posCM]]$vtest_hat,
              posCM = posCM,  iterations = cont ,models = models))             
}




SemiSupervised_HLS <- function(file_name,pathScenarios,CE,variables_True_Model, 
                               pnolabeled = 0.5, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0)
  # Semisupervised_HLS: function for semi-supervised fitting with head long search algorithm
{
  
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_100_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_5_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  filePathScenario <-paste0(pathScenarios,file_name) 
  fileRDS <- readRDS(filePathScenario)  
  nsimulations <- length(fileRDS$GenData)
  if(!is.numeric(nsimulations) | nsimulations == 0) stop("The file doesn't contain any simulation")
  MmetricsSaturatedM <- vector("list",nsimulations)
  MmetricsTM <- vector("list",nsimulations)
  MmetricsSM <- vector("list",nsimulations)
  GenData <- vector("list",nsimulations)
  dfRW <- vector("list",nsimulations)
  estimates <- vector("list",nsimulations)
  #    Metrics <- data.frame(Nsim = rep(1:nsimulations,each = 3),
  #                          )
  
  Metrics = data.frame(Nsim = numeric(),
                       FittedContModel_SM = character(),
                       FittedNoContModel_SM = character(),
                       Iterations_SM = numeric(),
                       Model_SM = character(),
                       Nvars_SM = numeric(),
                       CCR_SM = numeric(),Precision_SM = numeric(),
                       Recall_SM = numeric(), F1_SM = numeric(),
                       CCRCont_SM = numeric(), CCRNoCont_SM = numeric(),
                       PrecisionV_SM = numeric(), RecallV_SM = numeric(),
                       Specificity_SM = numeric(), 
                       F1V_SM = numeric(),
                       FittedContModel_TM = character(),
                       FittedNoContModel_TM = character(),
                       Iterations_TM = numeric(),
                       Model_TM = character(), Nvars_TM = numeric(), 
                       CCR_TM  = numeric(), Precision_TM = numeric(),
                       Recall_TM = numeric(), F1_TM = numeric(),
                       CCRCont_TM = numeric(), CCRNoCont_TM = numeric(),
                       PrecisionV_TM = numeric(), RecallV_TM = numeric(),
                       Specificiy_TM = numeric(),
                       F1V_TM = numeric(), 
                       FittedContModel_SaturatedM = character(),
                       FittedNoContModel_SaturatedM = character(),
                       Iterations_SaturatedM = numeric(),
                       Model_SaturatedM = character(),
                       Nvars_SaturatedM = numeric(),
                       CCR_SaturatedM = numeric(),Precision_SaturatedM = numeric(),
                       Recall_SaturatedM = numeric(), Specificity_SaturatedM = numeric(), 
                       F1_SaturatedM = numeric(),
                       CCRCont_SaturatedM = numeric(), CCRNoCont_SaturatedM = numeric(),
                       PrecisionV_SaturatedM = numeric(), RecallV_SaturatedM = numeric(),
                       F1V_SaturatedM = numeric(), stringsAsFactors = FALSE)
  #                          ltesthat_SM = numeric(), ltesthat_TM = numeric(), 
  #                          ltesthat_SaturatedM = numeric(),
  #                          vtesthat_SM = numeric(), vtesthat_TM = numeric(),
  #                          vtesthat_SaturatedM = numeric() , stringsAsFactors = FALSE )   
  
          time_df <- data.frame(File = character(), Simulation = numeric(), Stage = character(),
                                Steps = numeric(), Time_Execution = numeric())    
          if( any(str_detect(dir(getwd()),"Time_Execution.RDS")))
          {
             aux_time <- readRDS(paste0(getwd(),"/Time_Execution.RDS"))
             time_df <- aux_time
             index_time <- nrow(time_df)
          }else{
            index_time = 1
          }
          
          
  for (i_sim in 1:nsimulations)
  {
    time_begin <- Sys.time()
    
    GenData[[i_sim]] <-fileRDS$GenData[[i_sim]]
    par <- fileRDS$par
    #GenData <- SimGClasses(mug = matrix(c(0,0,0,2,0,0,0,2,0,0),ncol = 2, nrow = 5, byrow = TRUE),
    #                       sg = diag(1,5),
    #                       pig = c(0.5,0.5), 
    #                       nobs = 3000, 
    #                       ptraining = 0.75,
    #                       alphag = c(0.8,0.8),
    #                       etag = c(20,20))
    G = length(unique(GenData[[i_sim]]$l))
    
    Xtrain <- GenData[[i_sim]]$Xtrain
    Xtest <- GenData[[i_sim]]$Xtest
    ltrain <- GenData[[i_sim]]$ltrain
    ltest <- GenData[[i_sim]]$ltest
    vtest <- GenData[[i_sim]]$vtest
    
    
    MmetricsSaturatedM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                              Recall = rep(0,G), Specificity = rep(0,G),
                                              F1 = rep(0,G)) 
    MmetricsTM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), Specificity = rep(00,G),
                                      F1 = rep(0,G)) 
    MmetricsSM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), Specificity = rep(0,G),
                                      F1 = rep(0,G)) 
    
    
    dfRW[[i_sim]] <- getOW(Xtrain,ltrain)
    RW <- dfRW[[i_sim]]$Var
    variables_saturated_model <- RW
    
    tic("Saturated Model")
    saturated_mod <-  SemiSupervisedFitting(Xtrain,Xtest,ltrain,ltest,
                                            vtest, CE,pnolabeled) 
    elapsed_time <- toc()
    
    time_df[index_time,"File"] <- file_name
    time_df[index_time,"Simulation"] <- i_sim
    time_df[index_time,"Stage"] <- "All"
    time_df[index_time,"Steps"] <- 1
    time_df[index_time,"Time_Execution"] <- elapsed_time$toc - elapsed_time$tic
    index_time <- index_time + 1
    
    tic("Selected Model")
    selectedVar_mod <- HeadLongSearch(Xtrain,Xtest,RW,ltrain,ltest,vtest, 
                                      CE = CE, pnolabeled = 0.5, iterations = niterations,
                                      alpharef = 0.75, tol = 0.01, epsilon = 0)
    
    elapsed_time <- toc()
    
    pos_True_Model <- findPosModel(selectedVar_mod$models, variables_True_Model)
    
    time_df[index_time,"File"] <- file_name
    time_df[index_time,"Simulation"] <- i_sim
    time_df[index_time,"Stage"] <- "Selected"
    time_df[index_time,"Steps"] <- length(selectedVar_mod$Selectedmodel)
    time_df[index_time,"Time_Execution"] <- elapsed_time$toc - elapsed_time$tic
    index_time <- index_time + 1

    tic("True Model")
    
    if(pos_True_Model != 0 & is.numeric(pos_True_Model))
    {
      TrueModel <- selectedVar_mod$models[[pos_True_Model]]
    }else {
      Xtrain_TM <- data.frame(Xtrain) %>% dplyr::select(all_of(variables_True_Model))
      Xtest_TM <- data.frame(Xtest) %>% dplyr::select(all_of(variables_True_Model))
      
      TrueModel  <- SemiSupervisedFitting(Xtrain_TM,
                                          Xtest_TM,ltrain,ltest,
                                          vtest,CE,pnolabeled,
                                          iterations = niterations,
                                          alpharef = 0.75, 
                                          tol = 0.01)
      TrueModel$PM <- variables_True_Model
    } 

    time_df[index_time,"File"] <- file_name
    time_df[index_time,"Simulation"] <-i_sim
    time_df[index_time,"Stage"] <- "True"
    time_df[index_time,"Steps"] <- length(TrueModel)
    time_df[index_time,"Time_Execution"] <- elapsed_time$toc - elapsed_time$tic
    index_time <- index_time + 1
    
    # pos: position model obtained by variable selection
    pos <- selectedVar_mod$posCM
    nVarSel <- length(selectedVar_mod$Selectedmodel)
    
    PM <-selectedVar_mod$Selectedmodel
    Xsubset <- data.frame(Xtrain) %>% dplyr::select(all_of(PM))
    
    t_test <- table(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C)
    CCRltesthatSM <- sum(diag(t_test))/sum(t_test)
    
    cat("\n", "selected model", "test set ",selectedVar_mod$Selectedmodel,"-",CCRltesthatSM,"\n")
    
    # Filter contaminated vs non-contaminated samples
    lind_nocont_class <- list()
    lind_cont_class <- list()
    
    
    for (i_g in 1:G)
    {
      # non contaminated samples in classes
      lind_nocont_class[[i_g]] <- which(ltest == i_g & vtest == 1)
      # contaminated samples in classes
      lind_cont_class[[i_g]] <- which(ltest== i_g & vtest== 0)
      MmetricsSaturatedM[[i_sim]][i_g,2] <- Precision(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,2] <- Precision(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,2] <-Precision(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      MmetricsSaturatedM[[i_sim]][i_g,3] <- Recall(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,3] <- Recall(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,3] <-Recall(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      MmetricsSaturatedM[[i_sim]][i_g,4] <- Specificity(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,4] <- Specificity(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,4] <-Specificity(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      
    }
    MmetricsSaturatedM[[i_sim]]$F1 <- 2*(MmetricsSaturatedM[[i_sim]]$Precision*MmetricsSaturatedM[[i_sim]]$Recall)/(MmetricsSaturatedM[[i_sim]]$Precision+MmetricsSaturatedM[[i_sim]]$Recall)
    MmetricsTM[[i_sim]]$F1 <- 2*(MmetricsTM[[i_sim]]$Precision*MmetricsTM[[i_sim]]$Recall)/(MmetricsTM[[i_sim]]$Precision+MmetricsTM[[i_sim]]$Recall)
    MmetricsSM[[i_sim]]$F1 <- 2*(MmetricsSM[[i_sim]]$Precision*MmetricsSM[[i_sim]]$Recall)/(MmetricsSM[[i_sim]]$Precision+MmetricsSM[[i_sim]]$Recall)
    
    ind_nocont_samples <- unlist(lind_nocont_class)
    ind_cont_samples <- unlist(lind_cont_class)
    
    no_cont_samples <- ltest[ind_nocont_samples]
    cont_samples <- ltest[ind_cont_samples]
    
    length(TrueModel$ltest_hat_C)
    
    # Calculating precision, recall, and F1 metrics
    
    saturated_vtest <- saturated_mod$vtest_hat
    TM_vtest <- TrueModel$vtest_hat
    SM_vtest <- selectedVar_mod$models[[pos]]$vtest_hat
    
    
    # Calculating accuracy of detecting whether sample is contaminated
    # or not, precision, recall, and F1
    
    CCRSaturated_vtest <- 100*sum(vtest == saturated_vtest)/length(vtest)
    CCRTM_vtest <- 100*sum(vtest == TM_vtest)/length(vtest)
    CCRSM_vtest <- 100*sum(vtest == SM_vtest)/length(vtest)
    
    
    precision_saturated_V <- Precision(vtest,saturated_vtest,positive = 0)
    precision_TM_V <- Precision(vtest,TM_vtest,positive = 0)
    precision_SM_V <- Precision(vtest,SM_vtest,positive = 0)
    
    recall_saturated_V <- Recall(vtest,saturated_vtest,positive = 0)
    recall_TM_V <- Recall(vtest,TM_vtest,positive = 0)
    recall_SM_V <- Recall(vtest,SM_vtest,positive = 0)
    
    specificity_saturated_V <- Specificity(vtest,saturated_vtest,positive = 0)
    specificity_TM_V <- Specificity(vtest,TM_vtest,positive = 0)
    specificity_SM_V <- Specificity(vtest,SM_vtest,positive = 0)
    
    F1_Saturated_V <- 2*(precision_saturated_V * recall_saturated_V)/(precision_saturated_V+recall_saturated_V) 
    F1_TM_V <- 2*(precision_TM_V * recall_TM_V)/(precision_TM_V + recall_TM_V)
    F1_SM_V <- 2*(precision_SM_V * recall_SM_V)/(precision_SM_V + recall_SM_V)
    
    
    CCRSM_Identifying_cont_samples <- 0
    CCRSM_Identifying_no_cont_samples <- 0
    CCRTM_Identifying_cont_samples <- 0
    CCRTM_Identifying_no_cont_samples <- 0
    CCRSaturated_Identifying_cont_samples <- 0
    CCRSaturated_Identifying_no_cont_samples <- 0
    
    
    # predicted class for contaminated and non-contaminated samples for the true model
    TM_nocont_lhat <- TrueModel$ltest_hat_C[ind_nocont_samples]
    TM_cont_lhat <- TrueModel$ltest_hat_C[ind_cont_samples]
    
    # predicted class for contaminated and non-contaminated samples for selected model
    SM_nocont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_nocont_samples]
    SM_cont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_cont_samples]  
    
    # predicted class for contaminated and non-contaminated samples for saturated model 
    Saturated_nocont_lhat <- saturated_mod$ltest_hat_C[ind_nocont_samples]
    Saturated_cont_lhat <- saturated_mod$ltest_hat_C[ind_cont_samples]
    
    CCRTM_no_cont_samples <- 0
    CCRTM_cont_samples <- 0
    CCRSM_no_cont_samples <- 0
    CCRSM_cont_samples <- 0
    CCRSaturated_no_cont_samples <- 0
    CCRSaturated_cont_samples <- 0
    
    # Accuracy no-contaminated True Model
    CCRTM_no_cont_samples <- (sum(no_cont_samples == TM_nocont_lhat)/length(no_cont_samples))
    CCRTM_cont_samples <- (sum(cont_samples == TM_cont_lhat)/length(cont_samples))
    CCRSM_no_cont_samples <- (sum(no_cont_samples == SM_nocont_lhat)/length(no_cont_samples))
    CCRSM_cont_samples <- (sum(cont_samples == SM_cont_lhat)/length(cont_samples))
    CCRSaturated_no_cont_samples <- (sum(no_cont_samples == Saturated_nocont_lhat)/length(no_cont_samples))
    CCRSaturated_cont_samples <-     (sum(cont_samples == Saturated_cont_lhat)/length(cont_samples))
    Metrics[i_sim,"Nsim"] <- i_sim
    Metrics[i_sim,"FittedContModel_SM"] <- selectedVar_mod$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_SM"] <- selectedVar_mod$fitted_NC_model
    Metrics[i_sim,"Iterations_SM"] <- selectedVar_mod$iterations
    Metrics[i_sim,"Model_SM"] <- paste(PM,collapse = "-")
    Metrics[i_sim,"Nvars_SM"] <- nVarSel
    Metrics[i_sim,"CCR_SM"] <-CCRltesthatSM
    Metrics[i_sim,"Precision_SM"] <- mean(MmetricsSM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SM"] <- mean(MmetricsSM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_SM"] <- mean(MmetricsSM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_SM"] <- mean(MmetricsSM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SM"] <- CCRSM_cont_samples
    Metrics[i_sim,"CCRNoCont_SM"] <- CCRSM_no_cont_samples
    Metrics[i_sim,"PrecisionV_SM"] <- precision_SM_V
    Metrics[i_sim,"RecallV_SM"] <- recall_SM_V
    Metrics[i_sim,"SpecificityV_SM"] <- specificity_SM_V
    Metrics[i_sim,"F1V_SM"] <- F1_SM_V
    Metrics[i_sim,"FittedContModel_TM"] <- TrueModel$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_TM"] <- TrueModel$fitted_NC_model
    Metrics[i_sim,"Iterations_TM"] <- TrueModel$niterations
    Metrics[i_sim,"Model_TM"] <- paste(TrueModel$PM,collapse = "-")
    Metrics[i_sim,"Nvars_TM"] <-length(TrueModel$PM)
    Metrics[i_sim,"CCR_TM"] <- TrueModel$CCRTestC
    Metrics[i_sim,"Precision_TM"] <- mean(MmetricsTM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_TM"] <- mean(MmetricsTM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_TM"] <- mean(MmetricsTM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_TM"] <- mean(MmetricsTM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_TM"] <-CCRTM_cont_samples
    Metrics[i_sim,"CCRNoCont_TM"] <- CCRTM_no_cont_samples
    Metrics[i_sim,"PrecisionV_TM"] <- precision_TM_V
    Metrics[i_sim,"RecallV_TM"] <- recall_TM_V
    Metrics[i_sim,"SpecificityV_TM"] <- specificity_TM_V
    Metrics[i_sim,"F1V_TM"] <- F1_TM_V
    Metrics[i_sim,"FittedContModel_SaturatedM"] <- saturated_mod$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_SaturatedM"] <- saturated_mod$fitted_NC_model
    Metrics[i_sim,"Iterations_SaturatedM"] <- saturated_mod$niterations
    Metrics[i_sim,"Model_SaturatedM"] <- paste(RW,collapse = "-")
    Metrics[i_sim,"Nvars_SaturatedM"] <- length(RW)
    Metrics[i_sim,"CCR_SaturatedM"] <- saturated_mod$CCRTestC
    Metrics[i_sim,"Precision_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_SaturatedM"]<-mean(MmetricsSaturatedM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SaturatedM"] <- CCRSaturated_cont_samples
    Metrics[i_sim,"CCRNoCont_SaturatedM"] <- CCRSaturated_no_cont_samples
    Metrics[i_sim,"PrecisionV_SaturatedM"] <- precision_saturated_V
    Metrics[i_sim,"RecallV_SaturatedM"] <- recall_saturated_V
    Metrics[i_sim,"SpecificityV_SaturatedM"] <- specificity_saturated_V
    Metrics[i_sim,"F1V_SaturatedM"] <- F1_Saturated_V
    estimates[[i_sim]]$models <- selectedVar_mod$models
    estimates[[i_sim]]$posSM <- pos
    estimates[[i_sim]]$SM <- selectedVar_mod$models[[pos]]$PM
    estimates[[i_sim]]$par <- selectedVar_mod$models[[pos]]$par
    estimates[[i_sim]]$par$Fitted_Model <- CE
    estimates[[i_sim]]$lTestHat_SM <- selectedVar_mod$models[[pos]]$ltest_hat_C
    estimates[[i_sim]]$lTestHat_TM <- TrueModel$ltest_hat_C
    estimates[[i_sim]]$lTestHat_SaturatedM <- saturated_mod$ltest_hat_C
    estimates[[i_sim]]$vTestHat_SM <- SM_vtest
    estimates[[i_sim]]$vTestHat_TM <- TM_vtest
    estimates[[i_sim]]$vTestHat_SaturatedM <- saturated_vtest
    
    time_end <- Sys.time()
    time_df[index_time,"File"] <- file_name
    time_df[index_time,"Simulation"] <- i_sim
    time_df[index_time,"Stage"] <- "Complete"
    time_df[index_time,"Steps"] <- length(TrueModel$PM)
    time_df[index_time,"Time_Execution"] <- time_end - time_begin
    index_time <- index_time + 1
  } # end-for i_sim
  
          
          saveRDS(time_df,paste0(getwd(),"/Time_Execution.RDS"))
  Output <-  list(Metrics = Metrics , 
                  # Matrix of metrics
                  Metrics_SaturatedM = MmetricsSaturatedM,
                  Metrics_SM = MmetricsSM,
                  Metrics_TM = MmetricsTM,
                  # Generated Data
                  GenData = GenData,
                  # Estimates
                  Estimates = estimates,
                  Time = time_df)
  
  #Check SSFilesToPRocessed[[4]]
  
  return( Output )
  
}


SemiSupervised_HLS_Mod <- function(file_name,pathScenarios,CE,variables_True_Model, 
                               pnolabeled = 0.5, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0)
  # Semisupervised_HLS: function for semi-supervised fitting with head long search algorithm
{
  
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_100_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_5_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  filePathScenario <-paste0(pathScenarios,file_name) 
  fileRDS <- readRDS(filePathScenario)  
  nsimulations <- length(fileRDS$GenData)
  if(!is.numeric(nsimulations) | nsimulations == 0) stop("The file doesn't contain any simulation")
  MmetricsSaturatedM <- vector("list",nsimulations)
  MmetricsTM <- vector("list",nsimulations)
  MmetricsSM <- vector("list",nsimulations)
  GenData <- vector("list",nsimulations)
  dfRW <- vector("list",nsimulations)
  estimates <- vector("list",nsimulations)
  #    Metrics <- data.frame(Nsim = rep(1:nsimulations,each = 3),
  #                          )
  
  Metrics = data.frame(Nsim = numeric(),
                       FittedContModel_SM = character(),
                       FittedNoContModel_SM = character(),
                       Iterations_SM = numeric(),
                       Model_SM = character(),
                       Nvars_SM = numeric(),
                       CCR_SM = numeric(),Precision_SM = numeric(),
                       Recall_SM = numeric(), F1_SM = numeric(),
                       CCRCont_SM = numeric(), CCRNoCont_SM = numeric(),
                       PrecisionV_SM = numeric(), RecallV_SM = numeric(),
                       Specificity_SM = numeric(), 
                       F1V_SM = numeric(),
                       FittedContModel_TM = character(),
                       FittedNoContModel_TM = character(),
                       Iterations_TM = numeric(),
                       Model_TM = character(), Nvars_TM = numeric(), 
                       CCR_TM  = numeric(), Precision_TM = numeric(),
                       Recall_TM = numeric(), F1_TM = numeric(),
                       CCRCont_TM = numeric(), CCRNoCont_TM = numeric(),
                       PrecisionV_TM = numeric(), RecallV_TM = numeric(),
                       Specificiy_TM = numeric(),
                       F1V_TM = numeric(), 
                       FittedContModel_SaturatedM = character(),
                       FittedNoContModel_SaturatedM = character(),
                       Iterations_SaturatedM = numeric(),
                       Model_SaturatedM = character(),
                       Nvars_SaturatedM = numeric(),
                       CCR_SaturatedM = numeric(),Precision_SaturatedM = numeric(),
                       Recall_SaturatedM = numeric(), Specificity_SaturatedM = numeric(), 
                       F1_SaturatedM = numeric(),
                       CCRCont_SaturatedM = numeric(), CCRNoCont_SaturatedM = numeric(),
                       PrecisionV_SaturatedM = numeric(), RecallV_SaturatedM = numeric(),
                       F1V_SaturatedM = numeric(), stringsAsFactors = FALSE)
  #                          ltesthat_SM = numeric(), ltesthat_TM = numeric(), 
  #                          ltesthat_SaturatedM = numeric(),
  #                          vtesthat_SM = numeric(), vtesthat_TM = numeric(),
  #                          vtesthat_SaturatedM = numeric() , stringsAsFactors = FALSE )   
  
  for (i_sim in 1:nsimulations)
  {
    GenData[[i_sim]] <-fileRDS$GenData[[i_sim]]
    par <- fileRDS$par
    #GenData <- SimGClasses(mug = matrix(c(0,0,0,2,0,0,0,2,0,0),ncol = 2, nrow = 5, byrow = TRUE),
    #                       sg = diag(1,5),
    #                       pig = c(0.5,0.5), 
    #                       nobs = 3000, 
    #                       ptraining = 0.75,
    #                       alphag = c(0.8,0.8),
    #                       etag = c(20,20))
    G = length(unique(GenData[[i_sim]]$l))
    
    Xtrain <- GenData[[i_sim]]$Xtrain
    Xtest <- GenData[[i_sim]]$Xtest
    ltrain <- GenData[[i_sim]]$ltrain
    ltest <- GenData[[i_sim]]$ltest
    vtest <- GenData[[i_sim]]$vtest
    
    
    MmetricsSaturatedM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                              Recall = rep(0,G), Specificity = rep(0,G),
                                              F1 = rep(0,G)) 
    MmetricsTM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), Specificity = rep(00,G),
                                      F1 = rep(0,G)) 
    MmetricsSM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), Specificity = rep(0,G),
                                      F1 = rep(0,G)) 
    
    
    dfRW[[i_sim]] <- getOW(Xtrain,ltrain)
    RW <- dfRW[[i_sim]]$Var
    variables_saturated_model <- RW
    
    
    saturated_mod <-  SemiSupervisedFitting(Xtrain,Xtest,ltrain,ltest,
                                            vtest, CE,pnolabeled) 
    
    selectedVar_mod <- HeadLongSearch_mod(Xtrain,Xtest,RW,ltrain,ltest,vtest,
                                      CE = CE, pnolabeled = 0.5, iterations = niterations,
                                      alpharef = 0.75, tol = 0.01, epsilon = 0,i_sim,file_name)
    
    
    pos_True_Model <- findPosModel(selectedVar_mod$models, variables_True_Model)
    
    if(pos_True_Model != 0 & is.numeric(pos_True_Model))
    {
      TrueModel <- selectedVar_mod$models[[pos_True_Model]]
    }else {
      Xtrain_TM <- data.frame(Xtrain) %>% dplyr::select(all_of(variables_True_Model))
      Xtest_TM <- data.frame(Xtest) %>% dplyr::select(all_of(variables_True_Model))
      
      TrueModel  <- SemiSupervisedFitting(Xtrain_TM,
                                          Xtest_TM,ltrain,ltest,
                                          vtest,CE,pnolabeled,
                                          iterations = niterations,
                                          alpharef = 0.75, 
                                          tol = 0.01)
      TrueModel$PM <- variables_True_Model
    } 
    
    # pos: position model obtained by variable selection
    pos <- selectedVar_mod$posCM
    nVarSel <- length(selectedVar_mod$Selectedmodel)
    
    PM <-selectedVar_mod$Selectedmodel
    Xsubset <- data.frame(Xtrain) %>% dplyr::select(all_of(PM))
    
    t_test <- table(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C)
    CCRltesthatSM <- sum(diag(t_test))/sum(t_test)
    
    cat("\n ", "Scenario: ", file_name, " completed - ","selected model", "test set ",selectedVar_mod$Selectedmodel,"-",CCRltesthatSM,"\n")
    
    # Filter contaminated vs non-contaminated samples
    lind_nocont_class <- list()
    lind_cont_class <- list()
    
    
    for (i_g in 1:G)
    {
      # non contaminated samples in classes
      lind_nocont_class[[i_g]] <- which(ltest == i_g & vtest == 1)
      # contaminated samples in classes
      lind_cont_class[[i_g]] <- which(ltest== i_g & vtest== 0)
      MmetricsSaturatedM[[i_sim]][i_g,2] <- Precision(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,2] <- Precision(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,2] <-Precision(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      MmetricsSaturatedM[[i_sim]][i_g,3] <- Recall(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,3] <- Recall(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,3] <-Recall(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      MmetricsSaturatedM[[i_sim]][i_g,4] <- Specificity(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,4] <- Specificity(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,4] <-Specificity(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      
    }
    MmetricsSaturatedM[[i_sim]]$F1 <- 2*(MmetricsSaturatedM[[i_sim]]$Precision*MmetricsSaturatedM[[i_sim]]$Recall)/(MmetricsSaturatedM[[i_sim]]$Precision+MmetricsSaturatedM[[i_sim]]$Recall)
    MmetricsTM[[i_sim]]$F1 <- 2*(MmetricsTM[[i_sim]]$Precision*MmetricsTM[[i_sim]]$Recall)/(MmetricsTM[[i_sim]]$Precision+MmetricsTM[[i_sim]]$Recall)
    MmetricsSM[[i_sim]]$F1 <- 2*(MmetricsSM[[i_sim]]$Precision*MmetricsSM[[i_sim]]$Recall)/(MmetricsSM[[i_sim]]$Precision+MmetricsSM[[i_sim]]$Recall)
    
    ind_nocont_samples <- unlist(lind_nocont_class)
    ind_cont_samples <- unlist(lind_cont_class)
    
    no_cont_samples <- ltest[ind_nocont_samples]
    cont_samples <- ltest[ind_cont_samples]
    
    length(TrueModel$ltest_hat_C)
    
    # Calculating precision, recall, and F1 metrics
    
    saturated_vtest <- saturated_mod$vtest_hat
    TM_vtest <- TrueModel$vtest_hat
    SM_vtest <- selectedVar_mod$models[[pos]]$vtest_hat
    
    
    # Calculating accuracy of detecting whether sample is contaminated
    # or not, precision, recall, and F1
    
    CCRSaturated_vtest <- 100*sum(vtest == saturated_vtest)/length(vtest)
    CCRTM_vtest <- 100*sum(vtest == TM_vtest)/length(vtest)
    CCRSM_vtest <- 100*sum(vtest == SM_vtest)/length(vtest)
    
    
    precision_saturated_V <- Precision(vtest,saturated_vtest,positive = 0)
    precision_TM_V <- Precision(vtest,TM_vtest,positive = 0)
    precision_SM_V <- Precision(vtest,SM_vtest,positive = 0)
    
    recall_saturated_V <- Recall(vtest,saturated_vtest,positive = 0)
    recall_TM_V <- Recall(vtest,TM_vtest,positive = 0)
    recall_SM_V <- Recall(vtest,SM_vtest,positive = 0)
    
    specificity_saturated_V <- Specificity(vtest,saturated_vtest,positive = 0)
    specificity_TM_V <- Specificity(vtest,TM_vtest,positive = 0)
    specificity_SM_V <- Specificity(vtest,SM_vtest,positive = 0)
    
    F1_Saturated_V <- 2*(precision_saturated_V * recall_saturated_V)/(precision_saturated_V+recall_saturated_V) 
    F1_TM_V <- 2*(precision_TM_V * recall_TM_V)/(precision_TM_V + recall_TM_V)
    F1_SM_V <- 2*(precision_SM_V * recall_SM_V)/(precision_SM_V + recall_SM_V)
    
    
    CCRSM_Identifying_cont_samples <- 0
    CCRSM_Identifying_no_cont_samples <- 0
    CCRTM_Identifying_cont_samples <- 0
    CCRTM_Identifying_no_cont_samples <- 0
    CCRSaturated_Identifying_cont_samples <- 0
    CCRSaturated_Identifying_no_cont_samples <- 0
    
    
    # predicted class for contaminated and non-contaminated samples for the true model
    TM_nocont_lhat <- TrueModel$ltest_hat_C[ind_nocont_samples]
    TM_cont_lhat <- TrueModel$ltest_hat_C[ind_cont_samples]
    
    # predicted class for contaminated and non-contaminated samples for selected model
    SM_nocont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_nocont_samples]
    SM_cont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_cont_samples]  
    
    # predicted class for contaminated and non-contaminated samples for saturated model 
    Saturated_nocont_lhat <- saturated_mod$ltest_hat_C[ind_nocont_samples]
    Saturated_cont_lhat <- saturated_mod$ltest_hat_C[ind_cont_samples]
    
    CCRTM_no_cont_samples <- 0
    CCRTM_cont_samples <- 0
    CCRSM_no_cont_samples <- 0
    CCRSM_cont_samples <- 0
    CCRSaturated_no_cont_samples <- 0
    CCRSaturated_cont_samples <- 0
    
    # Accuracy no-contaminated True Model
    CCRTM_no_cont_samples <- (sum(no_cont_samples == TM_nocont_lhat)/length(no_cont_samples))
    CCRTM_cont_samples <- (sum(cont_samples == TM_cont_lhat)/length(cont_samples))
    CCRSM_no_cont_samples <- (sum(no_cont_samples == SM_nocont_lhat)/length(no_cont_samples))
    CCRSM_cont_samples <- (sum(cont_samples == SM_cont_lhat)/length(cont_samples))
    CCRSaturated_no_cont_samples <- (sum(no_cont_samples == Saturated_nocont_lhat)/length(no_cont_samples))
    CCRSaturated_cont_samples <-     (sum(cont_samples == Saturated_cont_lhat)/length(cont_samples))
    Metrics[i_sim,"Nsim"] <- i_sim
    Metrics[i_sim,"FittedContModel_SM"] <- selectedVar_mod$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_SM"] <- selectedVar_mod$fitted_NC_model
    Metrics[i_sim,"Iterations_SM"] <- selectedVar_mod$iterations
    Metrics[i_sim,"Model_SM"] <- paste(PM,collapse = "-")
    Metrics[i_sim,"Nvars_SM"] <- nVarSel
    Metrics[i_sim,"CCR_SM"] <-CCRltesthatSM
    Metrics[i_sim,"Precision_SM"] <- mean(MmetricsSM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SM"] <- mean(MmetricsSM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_SM"] <- mean(MmetricsSM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_SM"] <- mean(MmetricsSM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SM"] <- CCRSM_cont_samples
    Metrics[i_sim,"CCRNoCont_SM"] <- CCRSM_no_cont_samples
    Metrics[i_sim,"PrecisionV_SM"] <- precision_SM_V
    Metrics[i_sim,"RecallV_SM"] <- recall_SM_V
    Metrics[i_sim,"SpecificityV_SM"] <- specificity_SM_V
    Metrics[i_sim,"F1V_SM"] <- F1_SM_V
    Metrics[i_sim,"FittedContModel_TM"] <- TrueModel$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_TM"] <- TrueModel$fitted_NC_model
    Metrics[i_sim,"Iterations_TM"] <- TrueModel$niterations
    Metrics[i_sim,"Model_TM"] <- paste(TrueModel$PM,collapse = "-")
    Metrics[i_sim,"Nvars_TM"] <-length(TrueModel$PM)
    Metrics[i_sim,"CCR_TM"] <- TrueModel$CCRTestC
    Metrics[i_sim,"Precision_TM"] <- mean(MmetricsTM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_TM"] <- mean(MmetricsTM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_TM"] <- mean(MmetricsTM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_TM"] <- mean(MmetricsTM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_TM"] <-CCRTM_cont_samples
    Metrics[i_sim,"CCRNoCont_TM"] <- CCRTM_no_cont_samples
    Metrics[i_sim,"PrecisionV_TM"] <- precision_TM_V
    Metrics[i_sim,"RecallV_TM"] <- recall_TM_V
    Metrics[i_sim,"SpecificityV_TM"] <- specificity_TM_V
    Metrics[i_sim,"F1V_TM"] <- F1_TM_V
    Metrics[i_sim,"FittedContModel_SaturatedM"] <- saturated_mod$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_SaturatedM"] <- saturated_mod$fitted_NC_model
    Metrics[i_sim,"Iterations_SaturatedM"] <- saturated_mod$niterations
    Metrics[i_sim,"Model_SaturatedM"] <- paste(RW,collapse = "-")
    Metrics[i_sim,"Nvars_SaturatedM"] <- length(RW)
    Metrics[i_sim,"CCR_SaturatedM"] <- saturated_mod$CCRTestC
    Metrics[i_sim,"Precision_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_SaturatedM"]<-mean(MmetricsSaturatedM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SaturatedM"] <- CCRSaturated_cont_samples
    Metrics[i_sim,"CCRNoCont_SaturatedM"] <- CCRSaturated_no_cont_samples
    Metrics[i_sim,"PrecisionV_SaturatedM"] <- precision_saturated_V
    Metrics[i_sim,"RecallV_SaturatedM"] <- recall_saturated_V
    Metrics[i_sim,"SpecificityV_SaturatedM"] <- specificity_saturated_V
    Metrics[i_sim,"F1V_SaturatedM"] <- F1_Saturated_V
    estimates[[i_sim]]$models <- selectedVar_mod$models
    estimates[[i_sim]]$posSM <- pos
    estimates[[i_sim]]$SM <- selectedVar_mod$models[[pos]]$PM
    estimates[[i_sim]]$par <- selectedVar_mod$models[[pos]]$par
    estimates[[i_sim]]$par$Fitted_Model <- CE
    estimates[[i_sim]]$lTestHat_SM <- selectedVar_mod$models[[pos]]$ltest_hat_C
    estimates[[i_sim]]$lTestHat_TM <- TrueModel$ltest_hat_C
    estimates[[i_sim]]$lTestHat_SaturatedM <- saturated_mod$ltest_hat_C
    estimates[[i_sim]]$vTestHat_SM <- SM_vtest
    estimates[[i_sim]]$vTestHat_TM <- TM_vtest
    estimates[[i_sim]]$vTestHat_SaturatedM <- saturated_vtest
  } # end-for i_sim
  
  Output <-  list(Metrics = Metrics , 
                  # Matrix of metrics
                  Metrics_SaturatedM = MmetricsSaturatedM,
                  Metrics_SM = MmetricsSM,
                  Metrics_TM = MmetricsTM,
                  # Generated Data
                  GenData = GenData,
                  # Estimates
                  Estimates = estimates)
  
  #Check SSFilesToPRocessed[[4]]
  
  return( Output )
  
}

SemiSupervised_GS <- function(file_name,pathScenarios,CE,variables_True_Model, 
                               pnolabeled = 0.5, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0)
# Semisupervised_HLS: function for semi-supervised fitting with head long search algorithm
{

  
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_100_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_5_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  filePathScenario <-paste0(pathScenarios,file_name) 
  fileRDS <- readRDS(filePathScenario)  
  nsimulations <- length(fileRDS$GenData)
  if(!is.numeric(nsimulations) | nsimulations == 0) stop("The file doesn't contain any simulation")
  MmetricsSaturatedM <- vector("list",nsimulations)
  MmetricsTM <- vector("list",nsimulations)
  MmetricsSM <- vector("list",nsimulations)
  GenData <- vector("list",nsimulations)
  dfRW <- vector("list",nsimulations)
  estimates <- vector("list",nsimulations)
  #    Metrics <- data.frame(Nsim = rep(1:nsimulations,each = 3),
  #                          )
  MetricsPseudoValidation <- data.frame(File = numeric(), Nsim = numeric(),
                                        Percentage_Unlabelled_Data = numeric(),
                                        FittedContModel_TM = character(),
                                        FittedNoContModel_TM = character(),
                                        Model_TM = character(),
                                        Nvars_TM = numeric(),
                                        CCR_Class_Train_TM = numeric(),
                                        CCR_Class_Test_TM = numeric(),
                                        CCR_Pseudo_Label_Train_TM = numeric(),
                                        Recall_Class_Train_TM = numeric(),
                                        Recall_Class_Test_TM = numeric(),
                                        Recall_Pseudo_Label_Train_TM = numeric(),
                                        Precision_Class_Train_TM = numeric(),
                                        Precision_Class_Test_TM = numeric(),
                                        Precision_Pseudo_Label_Train_TM = numeric(),
                                        Specificity_Class_Train_TM = numeric(),
                                        Specificity_Class_Test_TM = numeric(),
                                        Specificity_Pseudo_Label_Train_TM = numeric(),
                                        
                                        Recall_Cont_Class_Train_TM = numeric(),
                                        Recall_Cont_Class_Test_TM = numeric(),
                                        Recall_Cont_Pseudo_Label_Train_TM = numeric(),
                                        Precision_Cont_Class_Train_TM = numeric(),
                                        Precision_Cont_Class_Test_TM = numeric(),
                                        Precision_Cont_Pseudo_Label_Train_TM = numeric(),
                                        Specificity_Class_Train_TM = numeric(),
                                        Specificity_Pseudo_Label_Train_TM = numeric(),
                                        
                                        Recall_Cont_Class_Train_TM = numeric(),
                                        Recall_Cont_Class_Test_TM = numeric(),
                                        Recall_Cont_Pseudo_Label_Train_TM = numeric(),
                                        Precision_Cont_Class_Train_TM = numeric(),
                                        Precision_Cont_Class_Test_TM = numeric(),
                                        Precision_Cont_Pseudo_Label_Train_TM = numeric(),
                                        Specificity_Cont_Class_Train_TM = numeric(),
                                        Specificity_Cont_Class_Test_TM = numeric(),
                                        Specificity_Cont_Pseudo_Label_Train_TM = numeric(),
                                        FittedContModel_TM = character(),
                                        FittedNoContModel_TM = character(),
                                        Model_TM = character(),
                                        Nvars_TM = numeric(),
                                        
                                        FittedContModel_SM = character(),
                                        FittedNoContModel_SM = character(),
                                        Model_SM = character(),
                                        Nvars_SM = character(),
                                        CCR_Class_Train_SM = numeric(),
                                        CCR_Class_Test_SM = numeric(),
                                        CCR_Pseudo_Label_Train_SM = numeric(),
                                        Recall_Class_Train_SM = numeric(),
                                        Recall_Class_Test_SM = numeric(),
                                        Recall_Pseudo_Label_Train_SM = numeroc(),
                                        Precision_Class_Train_SM = numeric(),
                                        Precision_Class_Test_SM = numeric(),
                                        Precision_Pseudo_Label_Train_SM = numeric(),
                                        Specificity_Class_Train_SM = numeric(),
                                        Specificity_Class_Test_SM = numeric(),
                                        Specificity_Pseudo_Label_Train_SM = numeric(),
                                        
                                        Recall_Cont_Class_Train_SM = numeric(),
                                        Recall_Cont_Class_Test_SM = numeric(),
                                        Recall_Cont_Pseudo_Label_Train_SM = numeric(),
                                        Precision_Cont_Class_Train_SM = numeric(),
                                        Precision_Cont_Class_Test_SM = numeric(),
                                        Precision_Cont_Pseudo_Label_Train_SM = numeric(),
                                        Specificity_Cont_Class_Train_SM = numeric(),
                                        Specificity_Cont_Class_Test_SM = numeric(),
                                        Specificity_Cont_Pseudo_Label_Train_SM = numeric(),
                                        
                                        FittedContModel_SaturatedM = character(),
                                        FittedNoContModel_SaturatedM = character(),
                                        Model_SaturatedM = character(),
                                        Nvars_SaturatedM = numeric(),
                                        CCR_Class_Train_SaturatedM= numeric(),
                                        CCR_Class_Test_SaturatedM = numeric(),
                                        CCR_Pseudo_Label_Train_SaturatedM = numeric(),
                                        Recall_Class_Train_SaturatedM = numeric(),
                                        Recall_Class_Test_SaturatedM = numeric(),
                                        Recall_Pseudo_Label_Train_SaturatedM =  numeric(),
                                        Precision_Class_Train_SaturatedM = numeric(),
                                        Precision_Class_Test_SaturatedM = numeric(),
                                        Precision_Pseudo_Label_Train_SaturatedM = numeric(),
                                        Specificity_Class_Train_SaturatedM = numeric(),
                                        Specificity_Class_Test_SaturatedM = numeric(),
                                        Specificity_Pseudo_Label_Train_SaturatedM = numeric(),
                                        
                                        Recall_Cont_Class_Train_SaturatedM =  numeric(),
                                        Recall_Cont_Class_Test_SaturatedM = numeric(),
                                        Recall_Cont_Pseudo_Label_Train_SaturatedM = numeric(),
                                        Precision_Cont_Class_Train_SaturatedM = numeric(),
                                        Precision_Cont_Class_Test_SaturatedM = numeric(),
                                        Precision_Cont_Pseudo_Label_SaturatedM = numeric(),
                                        Specificity_Cont_Class_Train_SaturatedM = numeric(),
                                        Specificity_Cont_Class_Test_SaturatedM = numeric(),
                                        Specificity_Cont_Pseudo_Label_Train_SaturatedM = numeric()
                                        
                                        
                                        )
  
  
  Metrics = data.frame(Nsim = numeric(),
                       FittedContModel_SM = character(),
                       FittedNoContModel_SM = character(),
                       Iterations_SM = numeric(),
                       Model_SM = character(),
                       Nvars_SM = numeric(),
                       CCR_SM = numeric(),Precision_SM = numeric(),
                       Recall_SM = numeric(), F1_SM = numeric(),
                       CCRCont_SM = numeric(), CCRNoCont_SM = numeric(),
                       PrecisionV_SM = numeric(), RecallV_SM = numeric(),
                       Specificity_SM = numeric(), 
                       F1V_SM = numeric(),
                       FittedContModel_TM = character(),
                       FittedNoContModel_TM = character(),
                       Iterations_TM = numeric(),
                       Model_TM = character(), Nvars_TM = numeric(), 
                       CCR_TM  = numeric(), Precision_TM = numeric(),
                       Recall_TM = numeric(), F1_TM = numeric(),
                       CCRCont_TM = numeric(), CCRNoCont_TM = numeric(),
                       PrecisionV_TM = numeric(), RecallV_TM = numeric(),
                       Specificiy_TM = numeric(),
                       F1V_TM = numeric(), 
                       FittedContModel_SaturatedM = character(),
                       FittedNoContModel_SaturatedM = character(),
                       Iterations_SaturatedM = numeric(),
                       Model_SaturatedM = character(),
                       Nvars_SaturatedM = numeric(),
                       CCR_SaturatedM = numeric(),Precision_SaturatedM = numeric(),
                       Recall_SaturatedM = numeric(), Specificity_SaturatedM = numeric(), 
                       F1_SaturatedM = numeric(),
                       CCRCont_SaturatedM = numeric(), CCRNoCont_SaturatedM = numeric(),
                       PrecisionV_SaturatedM = numeric(), RecallV_SaturatedM = numeric(),
                       F1V_SaturatedM = numeric(), stringsAsFactors = FALSE)
  #                          ltesthat_SM = numeric(), ltesthat_TM = numeric(), 
  #                          ltesthat_SaturatedM = numeric(),
  #                          vtesthat_SM = numeric(), vtesthat_TM = numeric(),
  #                          vtesthat_SaturatedM = numeric() , stringsAsFactors = FALSE )   
  
  for (i_sim in 1:nsimulations)
  {
    GenData[[i_sim]] <-fileRDS$GenData[[i_sim]]
    par <- fileRDS$par
    #GenData <- SimGClasses(mug = matrix(c(0,0,0,2,0,0,0,2,0,0),ncol = 2, nrow = 5, byrow = TRUE),
    #                       sg = diag(1,5),
    #                       pig = c(0.5,0.5), 
    #                       nobs = 3000, 
    #                       ptraining = 0.75,
    #                       alphag = c(0.8,0.8),
    #                       etag = c(20,20))
    G = length(unique(GenData[[i_sim]]$l))
    
    Xtrain <- GenData[[i_sim]]$Xtrain
    Xtest <- GenData[[i_sim]]$Xtest
    ltrain <- GenData[[i_sim]]$ltrain
    ltest <- GenData[[i_sim]]$ltest
    vtest <- GenData[[i_sim]]$vtest
    
    
    MmetricsSaturatedM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                              Recall = rep(0,G), Specificity = rep(0,G),
                                              F1 = rep(0,G)) 
    MmetricsTM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), Specificity = rep(00,G),
                                      F1 = rep(0,G)) 
    MmetricsSM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), Specificity = rep(0,G),
                                      F1 = rep(0,G)) 
    
    
    dfRW[[i_sim]] <- getOW(Xtrain,ltrain)
    RW <- dfRW[[i_sim]]$Var
    variables_saturated_model <- RW
    
    
    saturated_mod <-  SemiSupervisedFitting(Xtrain,Xtest,ltrain,ltest,
                                            vtest, CE,pnolabeled) 
    
    selectedVar_mod <- GreedySearch(Xtrain,Xtest,RW,ltrain,ltest,vtest, 
                                      CE = CE, pnolabeled = 0.5, iterations = niterations,
                                      alpharef = 0.75, tol = 0.01, epsilon = 0)
    
    
    pos_True_Model <- findPosModel(selectedVar_mod$models, variables_True_Model)
    
    if(pos_True_Model != 0 & is.numeric(pos_True_Model))
    {
      TrueModel <- selectedVar_mod$models[[pos_True_Model]]
    }else {
      Xtrain_TM <- data.frame(Xtrain) %>% dplyr::select(all_of(variables_True_Model))
      Xtest_TM <- data.frame(Xtest) %>% dplyr::select(all_of(variables_True_Model))
      
      TrueModel  <- SemiSupervisedFitting(Xtrain_TM,
                                          Xtest_TM,ltrain,ltest,
                                          vtest,CE,pnolabeled,
                                          iterations = niterations,
                                          alpharef = 0.75, 
                                          tol = 0.01)
      TrueModel$PM <- variables_True_Model
    } 
    
    saturated_mod$pseudo_label_info$vtrain <- GenData[[i_sim]]$vtrain
    saturated_mod$pseudo_label_info$True_Labels_V_train <- GenData[[i_sim]]$vtrain[saturated_mod$pseudo_label_info$Unlabelled_index]
    selectedVar_mod$pseudo_label_info$vtrain <- Gendata[[i_sim]]$vtrain
    selectedVar_mod$pseudo_label_info$True_Labels_V_Train <- GenData[[i_sim]]$vtrain[selectedVar_mod$pseudo_label_info$Unlabelled_index]
    TrueModel$pseudo_label_info$vtrain <- Gendata[i_sim]$vtrain
    TrueModel$pseudo_label_info$True_Labels_V_Train <- GenData[[i_sim]]$vtrain[TrueModel$pseudo_label_info$Unlabelled_index]
    
    # pos: position model obtained by variable selection
    pos <- selectedVar_mod$posCM
    nVarSel <- length(selectedVar_mod$Selectedmodel)
    
    PM <-selectedVar_mod$Selectedmodel
    Xsubset <- data.frame(Xtrain) %>% dplyr::select(all_of(PM))
    
    t_test <- table(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C)
    CCRltesthatSM <- sum(diag(t_test))/sum(t_test)
    
    cat("\n", "selected model", "test set ",selectedVar_mod$Selectedmodel,"-",CCRltesthatSM,"\n")
    
    # Filter contaminated vs non-contaminated samples
    lind_nocont_class <- list()
    lind_cont_class <- list()
    
    
    for (i_g in 1:G)
    {
      # non contaminated samples in classes
      lind_nocont_class[[i_g]] <- which(ltest == i_g & vtest == 1)
      # contaminated samples in classes
      lind_cont_class[[i_g]] <- which(ltest== i_g & vtest== 0)
      MmetricsSaturatedM[[i_sim]][i_g,2] <- Precision(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,2] <- Precision(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,2] <-Precision(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      MmetricsSaturatedM[[i_sim]][i_g,3] <- Recall(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,3] <- Recall(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,3] <-Recall(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      MmetricsSaturatedM[[i_sim]][i_g,4] <- Specificity(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,4] <- Specificity(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,4] <-Specificity(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      
    }
    MmetricsSaturatedM[[i_sim]]$F1 <- 2*(MmetricsSaturatedM[[i_sim]]$Precision*MmetricsSaturatedM[[i_sim]]$Recall)/(MmetricsSaturatedM[[i_sim]]$Precision+MmetricsSaturatedM[[i_sim]]$Recall)
    MmetricsTM[[i_sim]]$F1 <- 2*(MmetricsTM[[i_sim]]$Precision*MmetricsTM[[i_sim]]$Recall)/(MmetricsTM[[i_sim]]$Precision+MmetricsTM[[i_sim]]$Recall)
    MmetricsSM[[i_sim]]$F1 <- 2*(MmetricsSM[[i_sim]]$Precision*MmetricsSM[[i_sim]]$Recall)/(MmetricsSM[[i_sim]]$Precision+MmetricsSM[[i_sim]]$Recall)
    
    ind_nocont_samples <- unlist(lind_nocont_class)
    ind_cont_samples <- unlist(lind_cont_class)
    
    no_cont_samples <- ltest[ind_nocont_samples]
    cont_samples <- ltest[ind_cont_samples]
    
    length(TrueModel$ltest_hat_C)
    
    # Calculating precision, recall, and F1 metrics
    
    saturated_vtest <- saturated_mod$vtest_hat
    TM_vtest <- TrueModel$vtest_hat
    SM_vtest <- selectedVar_mod$models[[pos]]$vtest_hat
    
    
    # Calculating accuracy of detecting whether sample is contaminated
    # or not, precision, recall, and F1
    
    CCRSaturated_vtest <- 100*sum(vtest == saturated_vtest)/length(vtest)
    CCRTM_vtest <- 100*sum(vtest == TM_vtest)/length(vtest)
    CCRSM_vtest <- 100*sum(vtest == SM_vtest)/length(vtest)
    
    
    precision_saturated_V <- Precision(vtest,saturated_vtest,positive = 0)
    precision_TM_V <- Precision(vtest,TM_vtest,positive = 0)
    precision_SM_V <- Precision(vtest,SM_vtest,positive = 0)
    
    recall_saturated_V <- Recall(vtest,saturated_vtest,positive = 0)
    recall_TM_V <- Recall(vtest,TM_vtest,positive = 0)
    recall_SM_V <- Recall(vtest,SM_vtest,positive = 0)
    
    specificity_saturated_V <- Specificity(vtest,saturated_vtest,positive = 0)
    specificity_TM_V <- Specificity(vtest,TM_vtest,positive = 0)
    specificity_SM_V <- Specificity(vtest,SM_vtest,positive = 0)
    
    F1_Saturated_V <- 2*(precision_saturated_V * recall_saturated_V)/(precision_saturated_V+recall_saturated_V) 
    F1_TM_V <- 2*(precision_TM_V * recall_TM_V)/(precision_TM_V + recall_TM_V)
    F1_SM_V <- 2*(precision_SM_V * recall_SM_V)/(precision_SM_V + recall_SM_V)
    
    
    CCRSM_Identifying_cont_samples <- 0
    CCRSM_Identifying_no_cont_samples <- 0
    CCRTM_Identifying_cont_samples <- 0
    CCRTM_Identifying_no_cont_samples <- 0
    CCRSaturated_Identifying_cont_samples <- 0
    CCRSaturated_Identifying_no_cont_samples <- 0
    
    
    # predicted class for contaminated and non-contaminated samples for the true model
    TM_nocont_lhat <- TrueModel$ltest_hat_C[ind_nocont_samples]
    TM_cont_lhat <- TrueModel$ltest_hat_C[ind_cont_samples]
    
    # predicted class for contaminated and non-contaminated samples for selected model
    SM_nocont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_nocont_samples]
    SM_cont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_cont_samples]  
    
    # predicted class for contaminated and non-contaminated samples for saturated model 
    Saturated_nocont_lhat <- saturated_mod$ltest_hat_C[ind_nocont_samples]
    Saturated_cont_lhat <- saturated_mod$ltest_hat_C[ind_cont_samples]
    
    CCRTM_no_cont_samples <- 0
    CCRTM_cont_samples <- 0
    CCRSM_no_cont_samples <- 0
    CCRSM_cont_samples <- 0
    CCRSaturated_no_cont_samples <- 0
    CCRSaturated_cont_samples <- 0
    
    # Accuracy no-contaminated True Model
    CCRTM_no_cont_samples <- (sum(no_cont_samples == TM_nocont_lhat)/length(no_cont_samples))
    CCRTM_cont_samples <- (sum(cont_samples == TM_cont_lhat)/length(cont_samples))
    CCRSM_no_cont_samples <- (sum(no_cont_samples == SM_nocont_lhat)/length(no_cont_samples))
    CCRSM_cont_samples <- (sum(cont_samples == SM_cont_lhat)/length(cont_samples))
    CCRSaturated_no_cont_samples <- (sum(no_cont_samples == Saturated_nocont_lhat)/length(no_cont_samples))
    CCRSaturated_cont_samples <-     (sum(cont_samples == Saturated_cont_lhat)/length(cont_samples))
    Metrics[i_sim,"Nsim"] <- i_sim
    Metrics[i_sim,"FittedContModel_SM"] <- selectedVar_mod$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_SM"] <- selectedVar_mod$fitted_NC_model
    Metrics[i_sim,"Iterations_SM"] <- selectedVar_mod$iterations
    Metrics[i_sim,"Model_SM"] <- paste(PM,collapse = "-")
    Metrics[i_sim,"Nvars_SM"] <- nVarSel
    Metrics[i_sim,"CCR_SM"] <-CCRltesthatSM
    Metrics[i_sim,"Precision_SM"] <- mean(MmetricsSM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SM"] <- mean(MmetricsSM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_SM"] <- mean(MmetricsSM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_SM"] <- mean(MmetricsSM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SM"] <- CCRSM_cont_samples
    Metrics[i_sim,"CCRNoCont_SM"] <- CCRSM_no_cont_samples
    Metrics[i_sim,"PrecisionV_SM"] <- precision_SM_V
    Metrics[i_sim,"RecallV_SM"] <- recall_SM_V
    Metrics[i_sim,"SpecificityV_SM"] <- specificity_SM_V
    Metrics[i_sim,"F1V_SM"] <- F1_SM_V
    Metrics[i_sim,"FittedContModel_TM"] <- TrueModel$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_TM"] <- TrueModel$fitted_NC_model
    Metrics[i_sim,"Iterations_TM"] <- TrueModel$niterations
    Metrics[i_sim,"Model_TM"] <- paste(TrueModel$PM,collapse = "-")
    Metrics[i_sim,"Nvars_TM"] <-length(TrueModel$PM)
    Metrics[i_sim,"CCR_TM"] <- TrueModel$CCRTestC
    Metrics[i_sim,"Precision_TM"] <- mean(MmetricsTM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_TM"] <- mean(MmetricsTM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_TM"] <- mean(MmetricsTM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_TM"] <- mean(MmetricsTM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_TM"] <-CCRTM_cont_samples
    Metrics[i_sim,"CCRNoCont_TM"] <- CCRTM_no_cont_samples
    Metrics[i_sim,"PrecisionV_TM"] <- precision_TM_V
    Metrics[i_sim,"RecallV_TM"] <- recall_TM_V
    Metrics[i_sim,"SpecificityV_TM"] <- specificity_TM_V
    Metrics[i_sim,"F1V_TM"] <- F1_TM_V
    Metrics[i_sim,"FittedContModel_SaturatedM"] <- saturated_mod$fitted_C_model
    Metrics[i_sim,"FittedNoContModel_SaturatedM"] <- saturated_mod$fitted_NC_model
    Metrics[i_sim,"Iterations_SaturatedM"] <- saturated_mod$niterations
    Metrics[i_sim,"Model_SaturatedM"] <- paste(RW,collapse = "-")
    Metrics[i_sim,"Nvars_SaturatedM"] <- length(RW)
    Metrics[i_sim,"CCR_SaturatedM"] <- saturated_mod$CCRTestC
    Metrics[i_sim,"Precision_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Recall)
    Metrics[i_sim,"Specificity_SaturatedM"]<-mean(MmetricsSaturatedM[[i_sim]]$Specificity)
    Metrics[i_sim,"F1_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SaturatedM"] <- CCRSaturated_cont_samples
    Metrics[i_sim,"CCRNoCont_SaturatedM"] <- CCRSaturated_no_cont_samples
    Metrics[i_sim,"PrecisionV_SaturatedM"] <- precision_saturated_V
    Metrics[i_sim,"RecallV_SaturatedM"] <- recall_saturated_V
    Metrics[i_sim,"SpecificityV_SaturatedM"] <- specificity_saturated_V
    Metrics[i_sim,"F1V_SaturatedM"] <- F1_Saturated_V
    estimates[[i_sim]]$models <- selectedVar_mod$models
    estimates[[i_sim]]$posSM <- pos
    estimates[[i_sim]]$SM <- selectedVar_mod$models[[pos]]$PM
    estimates[[i_sim]]$par <- selectedVar_mod$models[[pos]]$par
    estimates[[i_sim]]$par$Fitted_Model <- CE
    estimates[[i_sim]]$lTestHat_SM <- selectedVar_mod$models[[pos]]$ltest_hat_C
    estimates[[i_sim]]$lTestHat_TM <- TrueModel$ltest_hat_C
    estimates[[i_sim]]$lTestHat_SaturatedM <- saturated_mod$ltest_hat_C
    estimates[[i_sim]]$vTestHat_SM <- SM_vtest
    estimates[[i_sim]]$vTestHat_TM <- TM_vtest
    estimates[[i_sim]]$vTestHat_SaturatedM <- saturated_vtest
    estimates[[i_sim]]$PseudoInformation_TM <- TrueModel$pseudo_label_info
    estimates[[i_sim]]$PseudoInformation_SM <- selectedVar_mod$models[[pos]]$pseudo_label_info
    estimates[[i_sim]]$PseudoInformation_SaturatedM <- saturated_mod$pseudo_label_info
    
  } # end-for i_sim
  
  Output <-  list(Metrics = Metrics , 
                  # Matrix of metrics
                  Metrics_SaturatedM = MmetricsSaturatedM,
                  Metrics_SM = MmetricsSM,
                  Metrics_TM = MmetricsTM,
                  # Generated Data
                  GenData = GenData,
                  # Estimates
                  Estimates = estimates)
  
  #Check SSFilesToPRocessed[[4]]
  
  return( Output )
  
}

SemiSupervised_HLS_SSH <- function(file_name,pathScenarios,CE,variables_True_Model, 
                               pnolabeled = 0.5, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0)
  # Semisupervised_HLS: function for semi-supervised fitting with head long search algorithm
{
  
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_100_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  #    fileRDS<- readRDS(paste0(pathOutput,"S_2_2_5_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
  filePathScenario <-paste0(pathScenarios,file_name) 
  fileRDS <- readRDS(filePathScenario)
  sys_info <- Sys.info()
  
  if(!sys_info["nodename"] == "LAPTOP-ADR3M911")
    
      if (file.exists(filePathScenario)) 
         {
    # Remove the file
            file.remove(filePathScenario)
            cat("File deleted successfully.\n")
            } else {
            cat("File does not exist.\n")
        }
  nsimulations <- length(fileRDS$GenData)
  if(!is.numeric(nsimulations) | nsimulations == 0) stop("The file doesn't contain any simulation")
  MmetricsSaturatedM <- vector("list",nsimulations)
  MmetricsTM <- vector("list",nsimulations)
  MmetricsSM <- vector("list",nsimulations)
  GenData <- vector("list",nsimulations)
  dfRW <- vector("list",nsimulations)
  estimates <- vector("list",nsimulations)
  #    Metrics <- data.frame(Nsim = rep(1:nsimulations,each = 3),
  #                          )
  
  Metrics = data.frame(Nsim = numeric(),
                       Model_SM = character(),
                       Nvars_SM = numeric(),
                       CCR_SM = numeric(),Precision_SM = numeric(),
                       Recall_SM = numeric(), F1_SM = numeric(),
                       CCRCont_SM = numeric(), CCRNoCont_SM = numeric(),
                       PrecisionV_SM = numeric(), RecallV_SM = numeric(),
                       F1V_SM = numeric(),Model_TM = character(), Nvars_TM = numeric(), 
                       CCR_TM  = numeric(), Precision_TM = numeric(),
                       Recall_TM = numeric(), F1_TM = numeric(),
                       CCRCont_TM = numeric(), CCRNoCont_TM = numeric(),
                       PrecisionV_TM = numeric(), RecallV_TM = numeric(),
                       F1V_TM = numeric(), Model_SaturatedM = character(),
                       Nvars_SaturatedM = numeric(),
                       CCR_SaturatedM = numeric(),Precision_SaturatedM = numeric(),
                       Recall_SaturatedM = numeric(), F1_SaturatedM = numeric(),
                       CCRCont_SaturatedM = numeric(), CCRNoCont_SaturatedM = numeric(),
                       PrecisionV_SaturatedM = numeric(), RecallV_SaturatedM = numeric(),
                       F1V_SaturatedM = numeric(), stringsAsFactors = FALSE)
  #                          ltesthat_SM = numeric(), ltesthat_TM = numeric(), 
  #                          ltesthat_SaturatedM = numeric(),
  #                          vtesthat_SM = numeric(), vtesthat_TM = numeric(),
  #                          vtesthat_SaturatedM = numeric() , stringsAsFactors = FALSE )   
  
  for (i_sim in 1:nsimulations)
  {
    GenData[[i_sim]] <-fileRDS$GenData[[i_sim]]
    par <- fileRDS$par
    #GenData <- SimGClasses(mug = matrix(c(0,0,0,2,0,0,0,2,0,0),ncol = 2, nrow = 5, byrow = TRUE),
    #                       sg = diag(1,5),
    #                       pig = c(0.5,0.5), 
    #                       nobs = 3000, 
    #                       ptraining = 0.75,
    #                       alphag = c(0.8,0.8),
    #                       etag = c(20,20))
    G = length(unique(GenData[[i_sim]]$l))
    Xtrain <- GenData[[i_sim]]$Xtrain
    Xtest <- GenData[[i_sim]]$Xtest
    ltrain <- GenData[[i_sim]]$ltrain
    ltest <- GenData[[i_sim]]$ltest
    vtest <- GenData[[i_sim]]$vtest
    
    
    MmetricsSaturatedM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                              Recall = rep(0,G), F1 = rep(0,G)) 
    MmetricsTM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), F1 = rep(0,G)) 
    MmetricsSM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                      Recall = rep(0,G), F1 = rep(0,G)) 
    
    
    dfRW[[i_sim]] <- getOW(Xtrain,ltrain)
    RW <- dfRW[[i_sim]]$Var
    variables_saturated_model <- RW
    
    
    saturated_mod <-  SemiSupervisedFitting(Xtrain,Xtest,ltrain,ltest,
                                            vtest, CE,pnolabeled) 
    
    selectedVar_mod <- HeadLongSearch(Xtrain,Xtest,RW,ltrain,ltest,vtest,
                                      CE , pnolabeled = 0.5, iterations = niterations,
                                      alpharef = 0.75, tol = 0.01, epsilon = 0)
    
    
    pos_True_Model <- findPosModel(selectedVar_mod$models, variables_True_Model)
    
    if(pos_True_Model != 0 & is.numeric(pos_True_Model))
    {
      TrueModel <- selectedVar_mod$models[[pos_True_Model]]
    }else {
      Xtrain_TM <- data.frame(Xtrain) %>% dplyr::select(all_of(variables_True_Model))
      Xtest_TM <- data.frame(Xtest) %>% dplyr::select(all_of(variables_True_Model))
      
      TrueModel  <- SemiSupervisedFitting(Xtrain_TM,
                                          Xtest_TM,ltrain,ltest,
                                          vtest,CE,pnolabeled,
                                          iterations = niterations,
                                          alpharef = 0.75, 
                                          tol = 0.01)
    } 
    
    # pos: position model obtained by variable selection
    pos <- selectedVar_mod$posCM
    nVarSel <- length(selectedVar_mod$Selectedmodel)
    
    PM <-selectedVar_mod$Selectedmodel
    Xsubset <- data.frame(Xtrain) %>% dplyr::select(all_of(PM))
    
    t_test <- table(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C)
    CCRltesthatSM <- sum(diag(t_test))/sum(t_test)
    
    cat("\n", "selected model", "test set ",selectedVar_mod$Selectedmodel,"-",CCRltesthatSM,"\n")
    
    # Filter contaminated vs non-contaminated samples
    lind_nocont_class <- list()
    lind_cont_class <- list()
    
    
    for (i_g in 1:G)
    {
      # non contaminated samples in classes
      lind_nocont_class[[i_g]] <- which(ltest == i_g & vtest == 1)
      # contaminated samples in classes
      lind_cont_class[[i_g]] <- which(ltest== i_g & vtest== 0)
      MmetricsSaturatedM[[i_sim]][i_g,2] <- Precision(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,2] <- Precision(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,2] <-Precision(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
      MmetricsSaturatedM[[i_sim]][i_g,3] <- Recall(ltest,saturated_mod$ltest_hat_C,positive = i_g)
      MmetricsTM[[i_sim]][i_g,3] <- Recall(ltest,TrueModel$ltest_hat_C,positive = i_g)
      MmetricsSM[[i_sim]][i_g,3] <-Recall(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
    }
    MmetricsSaturatedM[[i_sim]]$F1 <- 2*(MmetricsSaturatedM[[i_sim]]$Precision*MmetricsSaturatedM[[i_sim]]$Recall)/(MmetricsSaturatedM[[i_sim]]$Precision+MmetricsSaturatedM[[i_sim]]$Recall)
    MmetricsTM[[i_sim]]$F1 <- 2*(MmetricsTM[[i_sim]]$Precision*MmetricsTM[[i_sim]]$Recall)/(MmetricsTM[[i_sim]]$Precision+MmetricsTM[[i_sim]]$Recall)
    MmetricsSM[[i_sim]]$F1 <- 2*(MmetricsSM[[i_sim]]$Precision*MmetricsSM[[i_sim]]$Recall)/(MmetricsSM[[i_sim]]$Precision+MmetricsSM[[i_sim]]$Recall)
    
    ind_nocont_samples <- unlist(lind_nocont_class)
    ind_cont_samples <- unlist(lind_cont_class)
    
    no_cont_samples <- ltest[ind_nocont_samples]
    cont_samples <- ltest[ind_cont_samples]
    
    length(TrueModel$ltest_hat_C)
    
    # Calculating precision, recall, and F1 metrics
    
    saturated_vtest <- saturated_mod$vtest_hat
    TM_vtest <- TrueModel$vtest_hat
    SM_vtest <- selectedVar_mod$models[[pos]]$vtest_hat
    
    
    # Calculating accuracy of detecting whether sample is contaminated
    # or not, precision, recall, and F1
    
    CCRSaturated_vtest <- 100*sum(vtest == saturated_vtest)/length(vtest)
    CCRTM_vtest <- 100*sum(vtest == TM_vtest)/length(vtest)
    CCRSM_vtest <- 100*sum(vtest == SM_vtest)/length(vtest)
    
    
    precision_saturated_V <- Precision(vtest,saturated_vtest,positive = 0)
    precision_TM_V <- Precision(vtest,TM_vtest,positive = 0)
    precision_SM_V <- Precision(vtest,SM_vtest,positive = 0)
    
    recall_saturated_V <- Recall(vtest,saturated_vtest,positive = 0)
    recall_TM_V <- Recall(vtest,TM_vtest,positive = 0)
    recall_SM_V <- Recall(vtest,SM_vtest,positive = 0)
    
    F1_Saturated_V <- 2*(precision_saturated_V * recall_saturated_V)/(precision_saturated_V+recall_saturated_V) 
    F1_TM_V <- 2*(precision_TM_V * recall_TM_V)/(precision_TM_V + recall_TM_V)
    F1_SM_V <- 2*(precision_SM_V * recall_SM_V)/(precision_SM_V + recall_SM_V)
    
    
    CCRSM_Identifying_cont_samples <- 0
    CCRSM_Identifying_no_cont_samples <- 0
    CCRTM_Identifying_cont_samples <- 0
    CCRTM_Identifying_no_cont_samples <- 0
    CCRSaturated_Identifying_cont_samples <- 0
    CCRSaturated_Identifying_no_cont_samples <- 0
    
    
    # predicted class for contaminated and non-contaminated samples for the true model
    TM_nocont_lhat <- TrueModel$ltest_hat_C[ind_nocont_samples]
    TM_cont_lhat <- TrueModel$ltest_hat_C[ind_cont_samples]
    
    # predicted class for contaminated and non-contaminated samples for selected model
    SM_nocont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_nocont_samples]
    SM_cont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_cont_samples]  
    
    # predicted class for contaminated and non-contaminated samples for saturated model 
    Saturated_nocont_lhat <- saturated_mod$ltest_hat_C[ind_nocont_samples]
    Saturated_cont_lhat <- saturated_mod$ltest_hat_C[ind_cont_samples]
    
    CCRTM_no_cont_samples <- 0
    CCRTM_cont_samples <- 0
    CCRSM_no_cont_samples <- 0
    CCRSM_cont_samples <- 0
    CCRSaturated_no_cont_samples <- 0
    CCRSaturated_cont_samples <- 0
    
    # Accuracy no-contaminated True Model
    CCRTM_no_cont_samples <- (sum(no_cont_samples == TM_nocont_lhat)/length(no_cont_samples))
    CCRTM_cont_samples <- (sum(cont_samples == TM_cont_lhat)/length(cont_samples))
    CCRSM_no_cont_samples <- (sum(no_cont_samples == SM_nocont_lhat)/length(no_cont_samples))
    CCRSM_cont_samples <- (sum(cont_samples == SM_cont_lhat)/length(cont_samples))
    CCRSaturated_no_cont_samples <- (sum(no_cont_samples == Saturated_nocont_lhat)/length(no_cont_samples))
    CCRSaturated_cont_samples <-     (sum(cont_samples == Saturated_cont_lhat)/length(cont_samples))
    Metrics[i_sim,"Nsim"] <- i_sim
    Metrics[i_sim,"Model_SM"] <- paste(PM,collapse = "-")
    Metrics[i_sim,"Nvars_SM"] <- nVarSel
    Metrics[i_sim,"CCR_SM"] <-CCRltesthatSM
    Metrics[i_sim,"Precision_SM"] <- mean(MmetricsSM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SM"] <- mean(MmetricsSM[[i_sim]]$Recall)
    Metrics[i_sim,"F1_SM"] <- mean(MmetricsSM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SM"] <- CCRSM_cont_samples
    Metrics[i_sim,"CCRNoCont_SM"] <- CCRSM_no_cont_samples
    Metrics[i_sim,"PrecisionV_SM"] <- precision_SM_V
    Metrics[i_sim,"RecallV_SM"] <- recall_SM_V
    Metrics[i_sim,"F1V_SM"] <- F1_SM_V
    Metrics[i_sim,"Model_TM"] <- paste(TrueModel$PM,collapse = "-")
    Metrics[i_sim,"Nvars_TM"] <-length(TrueModel$PM)
    Metrics[i_sim,"CCR_TM"] <- TrueModel$CCRTestC
    Metrics[i_sim,"Precision_TM"] <- mean(MmetricsTM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_TM"] <- mean(MmetricsTM[[i_sim]]$Recall)
    Metrics[i_sim,"F1_TM"] <- mean(MmetricsTM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_TM"] <-CCRTM_cont_samples
    Metrics[i_sim,"CCRNoCont_TM"] <- CCRTM_no_cont_samples
    Metrics[i_sim,"PrecisionV_TM"] <- precision_TM_V
    Metrics[i_sim,"RecallV_TM"] <- recall_TM_V
    Metrics[i_sim,"F1V_TM"] <- F1_TM_V
    Metrics[i_sim,"Model_SaturatedM"] <- paste(RW,collapse = "-")
    Metrics[i_sim,"Nvars_SaturatedM"] <- length(RW)
    Metrics[i_sim,"CCR_SaturatedM"] <- saturated_mod$CCRTestC
    Metrics[i_sim,"Precision_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Precision)
    Metrics[i_sim,"Recall_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Recall)
    Metrics[i_sim,"F1_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$F1)
    Metrics[i_sim,"CCRCont_SaturatedM"] <- CCRSaturated_cont_samples
    Metrics[i_sim,"CCRNoCont_SaturatedM"] <- CCRSaturated_no_cont_samples
    Metrics[i_sim,"PrecisionV_SaturatedM"] <- precision_saturated_V
    Metrics[i_sim,"RecallV_SaturatedM"] <- recall_saturated_V
    Metrics[i_sim,"F1V_SaturatedM"] <- F1_Saturated_V
    estimates[[i_sim]]$models <- selectedVar_mod$models
    estimates[[i_sim]]$posSM <- pos
    estimates[[i_sim]]$SM <- selectedVar_mod$models[[pos]]$PM
    estimates[[i_sim]]$par <- selectedVar_mod$models[[pos]]$par
    estimates[[i_sim]]$par$Fitted_Model <- CE
    estimates[[i_sim]]$lTestHat_SM <- selectedVar_mod$models[[pos]]$ltest_hat_C
    estimates[[i_sim]]$lTestHat_TM <- TrueModel$ltest_hat_C
    estimates[[i_sim]]$lTestHat_SaturatedM <- saturated_mod$ltest_hat_C
    estimates[[i_sim]]$vTestHat_SM <- SM_vtest
    estimates[[i_sim]]$vTestHat_TM <- TM_vtest
    estimates[[i_sim]]$vTestHat_SaturatedM <- saturated_vtest
  } # end-for i_sim
  
  Output <-  list(Metrics = Metrics , 
                  # Matrix of metrics
                  Metrics_SaturatedM = MmetricsSaturatedM,
                  Metrics_SM = MmetricsSM,
                  Metrics_TM = MmetricsTM,
                  # Generated Data
                  GenData = GenData,
                  # Estimates
                  Estimates = estimates)
  
  #Check SSFilesToPRocessed[[4]]
  
  return( Output )
  
}

sort_labels <- function(model_string)
{
  sorted_model <- sort(unlist(str_split(model_string,"-")))
  return(paste(sorted_model,collapse="-"))
}


Create_MetricsFileOld <- function(filepath,ListFiles,NameMetricsFile = "Metrics")
{
  filenames <- paste0(filepath,ListFiles)
  combine_df <- data.frame()
  
  for(file in filenames)
  {
    aux_file <- readRDS(file)
    
    metrics_df <- aux_file$Metrics
    metrics_df$File <- basename(file)
    combine_df <- rbind(combine_df,metrics_df)
    
  }
  
  
  
  #    return(combine_df)
  combine_df <- combine_df %>% mutate(Replicate = Nsim)
  combine_df <- combine_df %>% mutate(Nsim = row_number())
  combine_df <- combine_df %>% relocate(File, .before = Nsim)
  combine_df <- mutate(combine_df, Model_Selected = Model_SM)
  
  aux_df <- combine_df %>% dplyr::select(File,Nsim,Replicate,
                                         Model_TM,Model_SM,Model_SaturatedM,
                                         Nvars_TM,Nvars_SM,Nvars_SaturatedM,
                                         CCR_TM,CCR_SM,CCR_SaturatedM,
                                         CCRCont_TM,CCRCont_SM,CCRCont_SaturatedM,
                                         CCRNoCont_TM,CCRNoCont_SM,CCRNoCont_SaturatedM,
                                         Precision_TM,Precision_SM,Precision_SaturatedM,
                                         Recall_TM,Recall_SM,Recall_SaturatedM,
                                         F1_TM,F1_SM,F1_SaturatedM,
                                         PrecisionV_TM,PrecisionV_SM,PrecisionV_SaturatedM,
                                         RecallV_TM,RecallV_SM,RecallV_SaturatedM,
                                         F1V_TM,F1V_SM,F1V_SaturatedM)
  colnames(aux_df)
  #1: True
  #2: Selected
  #3: Complete
  
  aux_df1 <- aux_df %>% dplyr::rename("A1" = "Model_TM",
                                      "A2" = "Model_SM",
                                      "A3" = "Model_SaturatedM",
                                      "B1" = "Nvars_TM",
                                      "B2" = "Nvars_SM",
                                      "B3" = "Nvars_SaturatedM",
                                      "C1" = "CCR_TM" ,
                                      "C2"= "CCR_SM" ,
                                      "C3" = "CCR_SaturatedM",
                                      "D1" = "CCRCont_TM"  ,
                                      "D2" = "CCRCont_SM"  ,
                                      "D3" = "CCRCont_SaturatedM",
                                      "N1" = "CCRNoCont_TM",
                                      "N2" = "CCRNoCont_SM",
                                      "N3" = "CCRNoCont_SaturatedM" ,
                                      "P1" = "Precision_TM"  ,
                                      "P2" = "Precision_SM" ,
                                      "P3" = "Precision_SaturatedM" ,
                                      "R1" = "Recall_TM" ,
                                      "R2" = "Recall_SM" ,
                                      "R3" = "Recall_SaturatedM",
                                      "S1" = "F1_TM",
                                      "S2" = "F1_SM",
                                      "S3" = "F1_SaturatedM",
                                      "V1" = "PrecisionV_TM" ,
                                      "V2" = "PrecisionV_SM" ,
                                      "V3" = "PrecisionV_SaturatedM",
                                      "W1" = "RecallV_TM",
                                      "W2" = "RecallV_SM",
                                      "W3" = "RecallV_SaturatedM",
                                      "Z1" = "F1V_TM",
                                      "Z2" = "F1V_SM",
                                      "Z3" = "F1V_SaturatedM"
  )
  
  aux_df_Long <- aux_df1 %>% 
    tidyr::pivot_longer(
      cols = A1:Z3,
      names_to = c(".value","Variables"),
      names_pattern = "(.)(.)"
    )
  
  aux_df2 <- aux_df_Long %>% dplyr::rename("Model" = "A",
                                           "Number_Variables" = "B",
                                           "CCR" = "C" ,
                                           "CCR_Cont" = "D"  ,
                                           "CCR_No_Cont" = "N",
                                           "Precision_Class" = "P"  ,
                                           "Recall_Class" = "R" ,
                                           "F1_Class" = "S",
                                           "Precision_Cont" = "V" ,
                                           "Recall_Cont" = "W",
                                           "F1_Cont" = "Z")
  
  
  aux_df2 <- aux_df2 %>% mutate(Model1 = sapply(Model, sort_labels)) %>%
    relocate(Model1, .after = Model)
  
  aux_df2 <- aux_df2 %>% mutate(Model_Size = str_count(Model,"-")+1) %>%
    relocate(Model_Size, .after = Model)
  
  aux_df2 <- aux_df2 %>% mutate(IncludeX2 = as.numeric(str_detect(Model1,"X2")),
                                IncludeX4 = as.numeric(str_detect(Model1,"X4")),
                                IncludeX5 = as.numeric(str_detect(Model1,"X5"))) %>%
    relocate(IncludeX2,IncludeX4,IncludeX5, .after = Model1)
  
  aux_df2 <- aux_df2 %>% mutate(Number_Classes = 
                                  str_split(File,"_",simplify = TRUE)[,2])
  
  aux_df2 <- aux_df2 %>% mutate(Number_Separating_Variables = 
                                  str_split(File,"_",simplify = TRUE)[,3])
  
  aux_df2 <- aux_df2 %>% mutate(Number_Variables = 
                                  str_split(File,"_",simplify = TRUE)[,4])
  
  aux_df2 <- aux_df2 %>% mutate(Number_Observations = 
                                  str_split(File,"_",simplify = TRUE)[,5])
  
  aux_df2 <- aux_df2 %>% mutate(Training_Proportion = 
                                  str_split(File,"_",simplify = TRUE)[,6])
  
  aux_df2 <- aux_df2 %>% mutate(Class_Proportion = 
                                  str_split(File,"_",simplify = TRUE)[,7])
  
  aux_df2 <- aux_df2 %>% mutate(Covariance_Structure = 
                                  str_split(File,"_",simplify = TRUE)[,8])
  
  aux_df2 <- aux_df2 %>% mutate(Group_Mean_Distance = 
                                  str_split(File,"_",simplify = TRUE)[,9])
  
  aux_df2 <- aux_df2 %>% mutate(AlphaC = 
                                  str_split(File,"_",simplify = TRUE)[,10])
  
  aux_df2 <- aux_df2 %>% mutate(EtaC = 
                                  str_split(File,"_",simplify = TRUE)[,11])
  
  aux_df2_no_na <- na.omit(aux_df2)
  
  aux_df2 <- aux_df2 %>% mutate(Variables = recode(Variables,
                                                   '1' = "True",
                                                   '2' = "Selected",
                                                   '3' = "All"))
  
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Variables = recode(Variables,
                                                   '1' = "True",
                                                   '2' = "Selected",
                                                   '3' = "All"))
  
  aux_df2$Covariance_Structure2 <- aux_df2$Covariance_Structure

  aux_df2_no_na$Covariance_Structure2 <- aux_df2_no_na$Covariance_Structure
  
  
  aux_df2 <- aux_df2 %>% mutate(Covariance_Structure2 = recode(Covariance_Structure,
                                                               "SCBSV" = "SV",
                                                               "SCBSNSV" = "SNSV",
                                                               "SCBNSV" = "NSV",
                                                               "IND" = "IND"))
  
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Covariance_Structure2 = recode(Covariance_Structure,
                                                               "SCBSV" = "SV",
                                                               "SCBSNSV" = "SNSV",
                                                               "SCBNSV" = "NSV",
                                                               "IND" = "IND"))
  
  n <- nrow(aux_df2)
  n_no_na <- nrow(aux_df2_no_na)
  
  aux_alpha <- sapply(aux_df2$AlphaC, function(alpha_str)
  {
    if(alpha_str == "A808090"){
      return(c(80,80,90))
    } else if(alpha_str == "A8090") return(c(80,90,0))
  })
  
  aux_alpha_no_na <- sapply(aux_df2_no_na$AlphaC, function(alpha_str)
  {
    if(alpha_str == "A808090"){
      return(c(80,80,90))
    } else if(alpha_str == "A8090") return(c(80,90,0))
  })
  
  Alpha_int <- matrix(aux_alpha,ncol = 3, nrow = n, byrow = TRUE)
  Alpha_int_no_na <- matrix(aux_alpha_no_na,ncol = 3, nrow = n_no_na, byrow = TRUE)
  
  aux_eta <- sapply(aux_df2$EtaC, function(Eta_str)
  {
    if(Eta_str == "E5530"){
      return(c(5,5,30))
    } else if(Eta_str == "E530") return(c(5,30,0))
  })
  
  aux_eta_no_na <- sapply(aux_df2_no_na$EtaC, function(Eta_str)
  {
    if(Eta_str == "E5530"){
      return(c(5,5,30))
    } else if(Eta_str == "E530") return(c(5,30,0))
  })
  
  
  Eta_int <- matrix(aux_eta, ncol = 3, nrow = n, byrow = TRUE)
  Eta_int_no_na <- matrix(aux_eta_no_na, ncol = 3, nrow = n_no_na, byrow = TRUE)
  
  aux_df2 <- aux_df2 %>% mutate(Alpha1 = Alpha_int[,1])
  aux_df2 <- aux_df2 %>% mutate(Alpha2 = Alpha_int[,2])
  aux_df2 <- aux_df2 %>% mutate(Alpha3 = Alpha_int[,3])
  
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Alpha1 = Alpha_int_no_na[,1])
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Alpha2 = Alpha_int_no_na[,2])
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Alpha3 = Alpha_int_no_na[,3])
  
  
  aux_df2 <- aux_df2 %>% mutate(Eta1 = Eta_int[,1])
  aux_df2 <- aux_df2 %>% mutate(Eta2 = Eta_int[,2])
  aux_df2 <- aux_df2 %>% mutate(Eta3 = Eta_int[,3])
  
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Eta1 = Eta_int_no_na[,1])
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Eta2 = Eta_int_no_na[,2])
  aux_df2_no_na <- aux_df2_no_na %>% mutate(Eta3 = Eta_int_no_na[,3])
  
  
  aux_df2$Model[aux_df2$Number_Separating_Variables == 2 & aux_df2$Model == ""] <- "X2-X4"
  aux_df2$Model[aux_df2$Number_Separating_Variables == 3 & aux_df2$Model ==""] <- "X2-X4-X5"
  aux_df2$Model1[aux_df2$Number_Separating_Variables == 2 & aux_df2$Model1 == ""] <- "X2-X4"
  aux_df2$Model1[aux_df2$Number_Separating_Variables == 3 & aux_df2$Model1 ==""] <- "X2-X4-X5"
  

  aux_df2_no_na$Model[aux_df2_no_na$Number_Separating_Variables == 2 & aux_df2_no_na$Model == ""] <- "X2-X4"
  aux_df2_no_na$Model[aux_df2_no_na$Number_Separating_Variables == 3 & aux_df2_no_na$Model ==""] <- "X2-X4-X5"
  aux_df2_no_na$Model1[aux_df2_no_na$Number_Separating_Variables == 2 & aux_df2_no_na$Model1 == ""] <- "X2-X4"
  aux_df2_no_na$Model1[aux_df2_no_na$Number_Separating_Variables == 3 & aux_df2_no_na$Model1 ==""] <- "X2-X4-X5"
  
    
  aux_df2$Model_Size[aux_df2$Number_Separating_Variables == 2 & aux_df2$Model ==""] <- 2
  aux_df2$Model_Size[aux_df2$Number_Separating_Variables == 3 & aux_df2$Model ==""] <- 3
  
  aux_df2_no_na$Model_Size[aux_df2_no_na$Number_Separating_Variables == 2 & aux_df2_no_na$Model ==""] <- 2
  aux_df2_no_na$Model_Size[aux_df2_no_na$Number_Separating_Variables == 3 & aux_df2_no_na$Model ==""] <- 3
  
  
  Output <- aux_df2 %>% relocate(c(Number_Classes,Number_Separating_Variables,
                                   Number_Variables,Number_Observations,
                                   Training_Proportion,Class_Proportion,
                                   Covariance_Structure,Covariance_Structure2,
                                   Group_Mean_Distance,AlphaC,EtaC,Alpha1,Alpha2,Alpha3,
                                   Eta1,Eta2,Eta3),
                                 .after = File )
  
  Output_no_na <- aux_df2_no_na %>% relocate(c(Number_Classes,Number_Separating_Variables,
                                   Number_Variables,Number_Observations,
                                   Training_Proportion,Class_Proportion,
                                   Covariance_Structure,Covariance_Structure2,
                                   Group_Mean_Distance,AlphaC,EtaC,Alpha1,Alpha2,Alpha3,
                                   Eta1,Eta2,Eta3),
                                 .after = File )
  
  saveRDS(combine_df,paste0(NameMetricsFile,"_LargeFormat.RDS"))
  saveRDS(Output,paste0(NameMetricsFile,".RDS"))
  saveRDS(Output_no_na,paste0(NameMetricsFile,"_no_na",".RDS"))
  
  return(Output)
}


cont_df <- function(X,y,lab,vpi.alpha,eta,ptrain)
{
  # Function that contaminate the wine data set
  # mug   : matrix where each column is the mean of a group
  # sg    : matrix or array that contains the variance-covariance matrix for a group
  # X     : matrix or array containing the covariates
  # y     : vector containing the response ( group information)
  # lab   : vector containing labels for the corresponding group
  # vpi   : vector containing the proportion of the sample for each group
  # alpha : vector containing the percentage of non contaminated observations for each group
  # eta   : vector containing the inflation factor for each group
  # ptrain : vector containing the percentage of samples included in the training ser for groups
  # pathOutput: path where the file will be saved
  
  
  G <- length(unique(y))
  ncont <- rep(0,G)
  nocont <- rep(0,G)
  nocont_train <- rep(0,G)
  nocont_test <- rep(0,G)
  ncont_train <- rep(0,G)
  ncont_test <- rep(0,G)
  ntrain <- rep(0,G)
  ntest <- rep(0,G)
  ng <- rep(0,G)
  library(mclust)
  
  
  length(indsamples[[1]])
  p <- ncol(X)
  mug <- matrix(0.0, nrow = p, ncol = G)
  sg <- array(0.0, dim = c(p, p,G))

  samples_cont_x_group <- apply(unmap(y),2,sum)
  samples_no_cont_x_group <- round(table(y) * (alpha))
  
  ncont = round(samples_cont_x_group * (1-alpha))
  
  
  for(g in 1:G)
  {
    mg[,g] <- X[y=g,] %>% apply(2,mean)
    sg[,g] <- X[y==g,] %>% var
  }
  
  GenContSamples <- SimCont(mug,sg,1:G,ncont,eta)
  colnames(GenContSamples)
  ng <- numeric()
  cont <- numeric()#
  
  x_aux <- matrix()
  x_aux <- X[y==1,]
  x_cont_aux <- GenContSamples %>% filter(class == 1) %>% select(-class) %>% as.matrix
  
  colnames(x_cont_ax) <- colnames(x_aux)
  # dim(x_aux)
  # dim(x_cont_aux)
  # x <- cbind(x_aux,x_cont_aux)
  # y_aux <- rep(1,nrow(x))
  
  x <- matrix()
  ng <- numeric()
  cont_changes <- 1
  g <- 1
  
  
  for(g in 1:G)
  {
    x_aux <- X[y==g,]
    if(g == 1)
    {
      x <- x_aux
      ng[cont_changes] <- nrow(x)
      cont_changes <- cont_changes + 1
    }else {
      x <- rbind(x,x_aux)
      ng[cont_changes] <- nrow(x_aux)
      cont_changes <- cont_changes + 1
    }
    x_cont_aux <- GenContSamples %>% filter(Class ==g) %>% select(-class) %>% as.matrix
    colnames(x_cont_aux) <- colnames(x_aux)
    ng[cont_changes] <- nrow(x_cont_aux)
    cont_changes <- cont_changes + 1
    x <- rbind(x,x_cont_aux)
  }
      # construct labels for classes
      aux_labels <- rep(1:G, each = 2)
      label_classes <- rep(aux_labels, ng)  
      
      
      # construct labels for contamination
      aux_labels_contamination <- rep(0,1:G)
      labels_contamination <- rep(aux_labels_contamination,ng)
      
      
      # update row names
      rownames(x) <- 1:nrow(x)
      
      # assuming that training percentage is the same for all classes
      ntrain <- nrow(x) * ptrain[1]
      
      # index fr splitting the set into training and set
      ind_train <- sample(1:nrow(x), round( nrow(x)/2 ), replace = FALSE )
      
      # split the set into training and test
      l <- labels_classes
      v <- labels_conamination
      
      Xtrain <- x[ind_train,]
      ltrain <- labels_classes[ind_train]
      vtrain <- v[ind_train]
      
      Xtest <- x[-ind_train,]
      ltest <- labeles_classes[-ind_train]
      vtest <- v[-ind_train]
      
      ind <- ind_train
      
      table(ltrain,vtrain)
      
      output <- list(X = x, l =l, ind = ind, Xtrain = Xtrain,
                     Xtest = Xtest, ltrain = ltrain, ltest = ltest,
                     v = v, vtrain = vtrain, vtest = vtest)
      
      return(output)
  
}