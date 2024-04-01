

getOW_vectorize <- function(df_train, l_train) {
  if (!is.data.frame(df_train)) df_train <- data.frame(df_train)
  
  NumVar <- colnames(df_train)[sapply(df_train, is.numeric)]
  if (length(NumVar) == 0) stop('Hey, the input data frame should contain numeric variables')
  
  fvalue <- rep(0.0, length(NumVar))
  
  for (i in seq_along(NumVar)) {
    aov_result <- aov(as.formula(paste(NumVar[i], '~', 'factor(l_train)')), data = df_train)
    fvalue[i] <- summary(aov_result)[[1]]$`F value`[1]
  }
  
  output <- data.frame(Var = NumVar, Ftest = fvalue)
  output <- output[order(-output$Ftest), ]
  return(output)
}

E_StepCMN_vectorize <- function(X, l, par) {
  m <- nrow(X)
  p <- ncol(X)
  G <- par$G
  pig <- par$pig
  mu <- par$mu
  sigma <- par$Sigma
  alpha <- par$alpha
  eta <- par$eta
  
  v <- matrix(0.0, ncol = G, nrow = m)
  z <- matrix(0.0, ncol = G, nrow = m)
  thetaig <- matrix(0.0, ncol = G, nrow = m)
  fxig <- matrix(0.0, ncol = G, nrow = m)
  
  for (g in 1:G) {
    if (p == 1) {
      if (is.vector(sigma)) {
        thetaig[, g] <- ifelse(length(sigma) == 1,
                               dnorm(X[, 1], mu[g], sigma),
                               dMVNorm(X[, 1], mu[, g], sigma[, , g]))
        fxig[, g] <- ifelse(length(sigma) == 1,
                            alpha[g] * thetaig[, g] + (1 - alpha[g]) * dnorm(X[, 1], mu[g], eta[g] * sigma),
                            alpha[g] * thetaig[, g] + (1 - alpha[g]) * dMVNorm(X[, 1], mu[, g], eta[g] * sigma[, , g]))
      }
    } else if (p > 1) {
      thetaig[, g] <- dMVNorm(X, mu[, g], sigma[, , g])
      fxig[, g] <- alpha[g] * thetaig[, g] + (1 - alpha[g]) * dMVNorm(X, mu[, g], eta[g] * sigma[, , g])
    }
    
    z[, g] <- pig[g] * fxig[, g]
    v[, g] <- alpha[g] * thetaig[, g] / fxig[, g]
  }
  
  denzi <- rowSums(z)
  z <- z / matrix(rep(denzi, each = G), ncol = G, byrow = TRUE)
  
  lhat <- apply(z, 1, which.max)
  vhat <- ifelse(rowSums(v * matrix(rep(1:G, each = m), ncol = G, byrow = TRUE)) > 0.5, 1, 0)
  
  output <- list(z = z, v = v, lhat = lhat, vhat = vhat)
  return(output)
}

SemiSupervisedFitting_vectorize <- function(X_train, X_test, ltrain, ltest,
                                            vtest, model = "EEI",
                                            pnolabeled = 0.5,
                                            iterations = 10, 
                                            alpharef = 0.75, tol = 0.01) {
  
  parameters_C <- list()
  parameters_Nc <- list()
  estimate <- list()
  
  G <- length(unique(ltrain))
  ntrain <- length(ltrain)
  
  ltrain1 <- ltrain
  nolabeled <- floor(pnolabeled * ntrain)
  ind_nolabeled <- sample(1:ntrain, nolabeled, replace = FALSE)
  ltrain1[ind_nolabeled] <- 0
  
  if(is.vector(X_train)) p <-1
  if(is.matrix(X_train)) p <- ncol(X_train)
  
  if(p == 1)  
  {
    ModelNonCont <- CNmixt(as.matrix(X_train), G, contamination = FALSE, model = model,
                           initialization = "random.post", alphamin = alpharef,
                           label = ltrain1, iter.max = iterations)
    
    ModelCont <- CNmixt(as.matrix(X_train), G, contamination = TRUE, model = model,
                        initialization = "random.post", alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    
  }
  
  if(p > 1)  
  {
    ModelNonCont <- CNmixt(as.matrix(X_train), G, contamination = FALSE, model = model,
                           initialization = "mixt", alphamin = alpharef,
                           label = ltrain1, iter.max = iterations)
    
    ModelCont <- CNmixt(as.matrix(X_train), G, contamination = TRUE, model = model,
                        initialization = "mixt", alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    
  }

  resNC <- getBestModel(ModelNonCont, criterion = "BIC")
  resC <- getBestModel(ModelCont, criterion = "BIC")
  
  logl_nc <- resNC$models[[1]]$loglik
  obslll_nc <- resNC$models[[1]]$obslll
  
  logl_c <- resC$models[[1]]$loglik
  obslll_c <- resC$models[[1]]$obslll
  
  parameters_C <- list(
    G = resC$models[[1]]$G,
    pig = resC$models[[1]]$prior,
    mu = resC$models[[1]]$mu,
    Sigma = resC$models[[1]]$Sigma,
    InvSigma = resC$models[[1]]$invSigma,
    alpha = resC$models[[1]]$alpha,
    eta = resC$models[[1]]$eta
  )
  
  estimate <- list(
    ztrain_hat = resC$models[[1]]$posterior,
    ltrain_hat = resC$models[[1]]$group,
    vtrain_hat = resC$models[[1]]$v,
    badPoints = resC$models[[1]]$detection
  )
  
  parameters_Nc <- list(
    pro = resNC$models[[1]]$prior,
    mean = resNC$models[[1]]$mu,
    variance = resNC$models[[1]]$Sigma
  )
  
  mstep_nc <- mclust::mstep( data = as.matrix(X_train), modelName = resNC$models[[1]]$model, 
                             z = unmap(estimate$ltrain_hat) )
  estep_nc <- mclust::estep(data =as.matrix(X_test), modelName =  mstep_nc$modelName, 
                            parameters = mstep_nc$parameters)
  
  
  ltest_hat_nc <- apply(estep_nc$z, 1, which.max)
  CCRTest_Nc <- sum((ltest_hat_nc == ltest)) / length(ltest)
  
  ExpectedValues_C <- E_StepCMN_vectorize(as.matrix(X_test), ltest, parameters_C)
  
  CCRTest_C <- ifelse(length(ExpectedValues_C$lhat) == length(ltest),
                      sum((ExpectedValues_C$lhat == ltest)) / length(ltest),
                      -1)
  
  AccTest_C <- ifelse(length(ExpectedValues_C$vhat) == length(vtest),
                      sum((ExpectedValues_C$vhat == vtest)) / length(vtest),
                      -1)
  ExpectedValues_C_train <- E_StepCMN_vectorize(as.matrix(X_train),ltrain,parameters_C)
  
  # Code for Semi-supervised Learning
  pseudo_label_info <- list(Unlabelled_index = ind_nolabeled,
                            Unlabelled_data = Xtrain[ind_nolabeled,],
                            True_labels_Class = ltrain,
                            Pseudo_labels_Class = ExpectedValues_C$lhat,
                            Percentage_of_no_labeled = pnolabeled)
  
  Output <- list(
    CCRTestNc = CCRTest_Nc,
    CCRTestC = CCRTest_C,
    ztest_hat_NC = estep_nc$z,
    ltest_hat_NC = ltest_hat_nc,
    ztest_hat_C = ExpectedValues_C$z,
    ltest_hat_C = ExpectedValues_C$lhat,
    Expected_v = ExpectedValues_C$v,
    vtest_hat = ExpectedValues_C$vhat,
    niterations = iterations,
    fitted_NC_model = resNC$models[[1]]$model,
    fitted_C_model = resC$models[[1]]$model,
    par = parameters_C,
    pseudo_label_info = pseudo_label_info
  )
  
  return(Output)
}


SemiSupervisedFitting_vectorize_pseudo_label <- function(X_train, X_test, ltrain, ltest,
                                            vtest, model = "EEI",
                                            pnolabeled = 0.5,
                                            iterations = 10, 
                                            alpharef = 0.75, tol = 0.01) {
  
  parameters_C <- list()
  parameters_Nc <- list()
  estimate <- list()
  
  G <- length(unique(ltrain))
  ntrain <- length(ltrain)
  
  ltrain1 <- ltrain
  nolabeled <- floor(pnolabeled * ntrain)
  ind_nolabeled <- sample(1:ntrain, nolabeled, replace = FALSE)
  ltrain1[ind_nolabeled] <- 0
  
  if(is.vector(X_train)) p <-1
  if(is.matrix(X_train)) p <- ncol(X_train)
  
  if(p == 1)  
  {
    ModelNonCont <- CNmixt(as.matrix(X_train), G, contamination = FALSE, model = model,
                           initialization = "random.post", alphamin = alpharef,
                           label = ltrain1, iter.max = iterations)
  
   ModelCont <- CNmixt(as.matrix(X_train), G, contamination = TRUE, model = model,
                       initialization = "random.post", alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    
 }
  
  if(p > 1)  
  {
    ModelNonCont <- CNmixt(as.matrix(X_train), G, contamination = FALSE, model = model,
                           initialization = "mixt", alphamin = alpharef,
                           label = ltrain1, iter.max = iterations)
    
    ModelCont <- CNmixt(as.matrix(X_train), G, contamination = TRUE, model = model,
                        initialization = "mixt", alphamin = alpharef,
                        label = ltrain1, iter.max = iterations)
    
 }

  resNC <- getBestModel(ModelNonCont, criterion = "BIC")
  resC <- getBestModel(ModelCont, criterion = "BIC")
  
  logl_nc <- resNC$models[[1]]$loglik
  obslll_nc <- resNC$models[[1]]$obslll
  
  logl_c <- resC$models[[1]]$loglik
  obslll_c <- resC$models[[1]]$obslll
  
  parameters_C <- list(
    G = resC$models[[1]]$G,
    pig = resC$models[[1]]$prior,
    mu = resC$models[[1]]$mu,
    Sigma = resC$models[[1]]$Sigma,
    InvSigma = resC$models[[1]]$invSigma,
    alpha = resC$models[[1]]$alpha,
    eta = resC$models[[1]]$eta
  )
  
  estimate <- list(
    ztrain_hat = resC$models[[1]]$posterior,
    ltrain_hat = resC$models[[1]]$group,
    vtrain_hat = resC$models[[1]]$v,
    badPoints = resC$models[[1]]$detection
  )
  
  parameters_Nc <- list(
    pig = resNC$models[[1]]$prior,
    mu = resNC$models[[1]]$mu,
    Sigma = resNC$models[[1]]$Sigma
  )
  
  mstep_nc <- mclust::mstep( data = as.matrix(X_train), modelName = resNC$models[[1]]$model, 
                             z = unmap(estimate$ltrain_hat) )
  estep_nc <- mclust::estep(data =as.matrix(X_test), modelName =  mstep_nc$modelName, 
                            parameters = mstep_nc$parameters)
  
  
  ltest_hat_nc <- apply(estep_nc$z, 1, which.max)
  CCRTest_Nc <- sum((ltest_hat_nc == ltest)) / length(ltest)
  
  ExpectedValues_C <- E_StepCMN_vectorize(as.matrix(X_test), ltest, parameters_C)
  
  CCRTest_C <- ifelse(length(ExpectedValues_C$lhat) == length(ltest),
                      sum((ExpectedValues_C$lhat == ltest)) / length(ltest),
                      -1)
  
  AccTest_C <- ifelse(length(ExpectedValues_C$vhat) == length(vtest),
                      sum((ExpectedValues_C$vhat == vtest)) / length(vtest),
                      -1)
  ExpectedValues_C_train <- E_StepCMN_vectorize(as.matrix(X_train),ltrain,parameters_C)
  
  # Code for Semi-supervised Learning
  pseudo_label_info <- list(Unlabelled_index = ind_nolabeled,
                            Unlabelled_data = Xtrain[ind_nolabeled,],
                            True_labels_Class = ltrain,
                            Pseudo_labels_Class = ExpectedValues_C$lhat,
                            Percentage_of_no_labeled = pnolabeled)
  
  Output <- list(
    CCRTestNc = CCRTest_Nc,
    CCRTestC = CCRTest_C,
    ztest_hat_NC = estep_nc$z,
    ltest_hat_NC = ltest_hat_nc,
    ztest_hat_C = ExpectedValues_C$z,
    ltest_hat_C = ExpectedValues_C$lhat,
    Expected_v = ExpectedValues_C$v,
    vtest_hat = ExpectedValues_C$vhat,
    niterations = iterations,
    fitted_NC_model = resNC$models[[1]]$model,
    fitted_C_model = resC$models[[1]]$model,
    par = parameters_C,
    pseudo_label_info = pseudo_label_info
  )
  
  return(Output)
}

  
  GreedySearch_vectorize <- function(Xtrain, Xtest, RW, ltrain, ltest, vtest, CE,
                                       i_sim,file_name,pnolabeled = 0.5, iterations = 10,
                                       alpharef = 0.75, tol = 0.01, epsilon = 0) 
  {
    
    p <- length(RW)
    models <- list()
    
    if(!is.data.frame(Xtrain)) Xtrain <- data.frame(Xtrain)
    if(!is.data.frame(Xtest)) Xtest <- data.frame(Xtest)
    
    for (cont in 1:p) {
    PM <- RW[cont]
    X_train <- Xtrain[, PM]
    X_test <- Xtest[, PM]
    
    cat("\n Model ", unlist(PM))
    
    models[[cont]] <- SemiSupervisedFitting_vectorize_pseudo_label(as.matrix(X_train), as.matrix(X_test), ltrain, ltest,
                                                      vtest, model = c("E", "V"), 
                                                      pnolabeled, iterations = iterations, 
                                                      alpharef = alpharef)
    
    models[[cont]]$PM <- PM
  }
  
  CM <- RW[[which.max(sapply(models, function(m) m$CCRTestC))]]
  CCRCM <- max(sapply(models, function(m) m$CCRTestC))
  posCM <- which.max(sapply(models, function(m) m$CCRTestC))
  
  cont <- length(models) + 1
  
  ARW <- RW
  OldCM <- "NA"
  OldCCRCM <- 0
  
  while ((!setequal(OldCM, CM) & OldCCRCM < CCRCM & 
          length(ARW) > 0) | 
         (OldCCRCM < CCRCM & length(setdiff(RW, CM)) > 0)) {
    
    OldCM <- CM
    OldCCRCM <- CCRCM
    
    ARW <- setdiff(RW, CM)
    nARW <- length(ARW)
  
    if (nARW != 0) {
      PMs <- lapply(ARW, function(var) union(CM, var))
      models_new <- lapply(PMs, function(PM) {
        X_train1 <- Xtrain[, PM]
        X_test1 <- Xtest[, PM]
        SemiSupervisedFitting_vectorize(X_train1, X_test1, ltrain, ltest,
                                        vtest, pnolabeled, model = CE,
                                        iterations = iterations, 
                                        alpharef = alpharef)
      })
      
      CCRPMs <- sapply(models_new, function(m) m$CCRTestC)
      
      best_index <- which.max(CCRPMs)
      if (CCRPMs[best_index] > CCRCM) {
        CM <- PMs[[best_index]]
        CCRCM <- CCRPMs[best_index]
        posCM <- cont
      }
      cat("\n"," File: ",file_name," Simulation: ",i_sim," - ", cont," ,model = ",unlist(CM),"\n")
      
      cont <- cont + 1
    }
  } 
  
  return(list(fitted_NC_model = models[[posCM]]$fitted_NC_model,
              fitted_C_model = models[[posCM]]$fitted_NC_model,
              Selectedmodel = CM, CCRCM = CCRCM, 
              Classprediction = models[[posCM]]$ltest_hat_C,
              ContaminatedSamplesprediction = models[[posCM]]$vtest_hat,
              posCM = posCM,  iterations = cont , models = models, 
              pseudo_label_info = models[[posCM]]$pseudo_label_info))             
}


SemiSupervised_GS_vectorize <- function(file_name, pathScenarios, CE, variables_True_Model, 
                                         pnolabeled = 0.5, niterations = 10,
                                         alpharef = 0.99, tol = 0.01, epsilon = 0) {
  filePathScenario <- paste0(pathScenarios, file_name) 
  fileRDS <- readRDS(filePathScenario)  
  nsimulations <- length(fileRDS$GenData)
  
  if (!is.numeric(nsimulations) || nsimulations == 0) stop("The file doesn't contain any simulation")
  
  Metrics <- data.frame(Nsim = numeric(), FittedContModel_SM = character(), FittedNoContModel_SM = character(),
                        Iterations_SM = numeric(), Model_SM = character(), Nvars_SM = numeric(),
                        CCR_SM = numeric(), Precision_SM = numeric(), Recall_SM = numeric(),
                        Specificity_SM = numeric(), F1_SM = numeric(), CCRCont_SM = numeric(),
                        CCRNoCont_SM = numeric(), PrecisionV_SM = numeric(), RecallV_SM = numeric(),
                        SpecificityV_SM = numeric(), F1V_SM = numeric(), FittedContModel_TM = character(),
                        FittedNoContModel_TM = character(), Iterations_TM = numeric(), Model_TM = character(),
                        Nvars_TM = numeric(), CCR_TM = numeric(), Precision_TM = numeric(), Recall_TM = numeric(),
                        Specificity_TM = numeric(), F1_TM = numeric(), CCRCont_TM = numeric(), CCRNoCont_TM = numeric(),
                        PrecisionV_TM = numeric(), RecallV_TM = numeric(), SpecificityV_TM = numeric(), F1V_TM = numeric(),
                        FittedContModel_SaturatedM = character(), FittedNoContModel_SaturatedM = character(),
                        Iterations_SaturatedM = numeric(), Model_SaturatedM = character(), Nvars_SaturatedM = numeric(),
                        CCR_SaturatedM = numeric(), Precision_SaturatedM = numeric(), Recall_SaturatedM = numeric(),
                        Specificity_SaturatedM = numeric(), F1_SaturatedM = numeric(), CCRCont_SaturatedM = numeric(),
                        CCRNoCont_SaturatedM = numeric(), PrecisionV_SaturatedM = numeric(), RecallV_SaturatedM = numeric(),
                        SpecificityV_SaturatedM = numeric(), F1V_SaturatedM = numeric(), percentage_no_labelled = numeric(),stringsAsFactors = FALSE)
  
  MetricsIndex <- 1
  
  for (i_sim in 1:nsimulations) {
    GenData <- fileRDS$GenData[[i_sim]]
    par <- fileRDS$par
    
    G <- length(unique(GenData$l))
    Xtrain <- GenData$Xtrain
    Xtest <- GenData$Xtest
    ltrain <- GenData$ltrain
    ltest <- GenData$ltest
    vtest <- GenData$vtest
    
    MmetricsSaturatedM <- MmetricsTM <- MmetricsSM <- data.frame(Group = 1:G, Precision = rep(0, G), 
                                                                 Recall = rep(0, G), Specificity = rep(0, G),
                                                                 F1 = rep(0, G))
    
    dfRW <- getOW_vectorize(Xtrain, ltrain)
    RW <- dfRW$Var
    variables_saturated_model <- RW
    
    saturated_mod <- SemiSupervisedFitting_vectorize(Xtrain, Xtest, ltrain, ltest, vtest, CE, pnolabeled)
    
    selectedVar_mod <- GreedySearch_vectorize(as.matrix(Xtrain), as.matrix(Xtest), RW, ltrain, ltest, vtest, 
                                                CE = CE, i_sim, file_name, pnolabeled = 0.5, iterations = niterations,
                                                alpharef = 0.75, tol = 0.01, epsilon = 0)
    
    pos_True_Model <- findPosModel(selectedVar_mod$models, variables_True_Model)
    if (pos_True_Model != 0 && is.numeric(pos_True_Model)) {
      TrueModel <- selectedVar_mod$models[[pos_True_Model]]
    } else {
      Xtrain_TM <- data.frame(Xtrain) %>% dplyr::select(all_of(variables_True_Model))
      Xtest_TM <- data.frame(Xtest) %>% dplyr::select(all_of(variables_True_Model))
      
      TrueModel <- SemiSupervisedFitting_vectorize(Xtrain_TM, Xtest_TM, ltrain, ltest, vtest, CE, pnolabeled,
                                                   iterations = niterations, alpharef = 0.75, tol = 0.01)
      TrueModel$PM <- variables_True_Model
    }
    
    pos <- selectedVar_mod$posCM
    nVarSel <- length(selectedVar_mod$Selectedmodel)
    PM <- selectedVar_mod$Selectedmodel
    Xsubset <- data.frame(Xtrain) %>% dplyr::select(all_of(PM))
    
    # ... Continue with the remaining calculations
    
    t_test <- table(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C)
    CCRltesthatSM <- sum(diag(t_test))/sum(t_test)
    
    cat("\n", "Simulation ", i_sim, " from File ",file_name,
        " selected model", "test set ",selectedVar_mod$Selectedmodel,"-",CCRltesthatSM,"\n")
    
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
    
    
    # Fill in Metrics dataframe with computed metrics for this simulation
    Metrics[MetricsIndex, ] <- c(i_sim, fittedContModelSM, fittedNoContModelSM, iterationsSM, modelSM,
                                 nVarsSM, CCRSM, precisionSM, recallSM, specificitySM, f1SM, ccrContSM,
                                 ccrNoContSM, precisionVSM, recallVSM, specificityVSM, f1VSM, fittedContModelTM,
                                 fittedNoContModelTM, iterationsTM, modelTM, nVarsTM, CCRTM, precisionTM, recallTM,
                                 specificityTM, f1TM, ccrContTM, ccrNoContTM, precisionVTM, recallVTM, specificityVTM,
                                 f1VTM, fittedContModelSaturatedM, fittedNoContModelSaturatedM, iterationsSaturatedM,
                                 modelSaturatedM, nVarsSaturatedM, CCRSaturatedM, precisionSaturatedM, recallSaturatedM,
                                 specificitySaturatedM, f1SaturatedM, ccrContSaturatedM, ccrNoContSaturatedM,
                                 precisionVSaturatedM, recallVSaturatedM, specificityVSaturatedM, f1VSaturatedM)
    
    estimates[[MetricsIndex,]] <- c(selectedVar_mod$models,pos,selectedVar_mod$models[[pos]]$PM,
                                    selectedVar_mod$models[[pos]]$par,CE,
                                    selectedVar_mod$models[[pos]]$ltest_hat_C,
                                    TrueModel$ltest_hat_C,saturated_mod$ltest_hat_C,SM_vtest,TM_vtest,
                                    saturated_vtest)
    
    
    MetricsIndex <- MetricsIndex + 1
  }
  
  Output <- list(Metrics = Metrics, Metrics_SaturatedM = MmetricsSaturatedM,
                 Metrics_SM = MmetricsSM, Metrics_TM = MmetricsTM,
                 GenData = GenData, Estimates = estimates)
  
  return(Output)
}



HeadLongSearch_vectorize <- function(Xtrain, Xtest, RW, ltrain, ltest, vtest, CE,
                                     i_sim,file_name,pnolabeled = 0.5, iterations = 10,
                                     alpharef = 0.75, tol = 0.01, epsilon = 0) 
{
  
  p <- length(RW)
  models <- list()
  
  if(!is.data.frame(Xtrain)) Xtrain <- data.frame(Xtrain)
  if(!is.data.frame(Xtest)) Xtest <- data.frame(Xtest)
  
  for (cont in 1:p) {
    PM <- RW[cont]
    X_train <- Xtrain[, PM]
    X_test <- Xtest[, PM]
    
    cat("\n Model ", unlist(PM))
    
    models[[cont]] <- SemiSupervisedFitting_vectorize(as.matrix(X_train), as.matrix(X_test), ltrain, ltest,
                                                      vtest, model = c("E", "V"), 
                                                      pnolabeled, iterations = iterations, 
                                                      alpharef = alpharef)
    
    models[[cont]]$PM <- PM
  }
  
  CM <- RW[[which.max(sapply(models, function(m) m$CCRTestC))]]
  CCRCM <- max(sapply(models, function(m) m$CCRTestC))
  posCM <- which.max(sapply(models, function(m) m$CCRTestC))
  
  cont <- length(models) + 1
  
  ARW <- RW
  OldCM <- "NA"
  OldCCRCM <- 0
  
  while ((!setequal(OldCM, CM) & OldCCRCM < CCRCM & 
          length(ARW) > 0) | 
         (OldCCRCM < CCRCM & length(setdiff(RW, CM)) > 0)) {
    
    OldCM <- CM
    OldCCRCM <- CCRCM
    
    ARW <- setdiff(RW, CM)
    nARW <- length(ARW)
    
    if (nARW != 0) {
      PMs <- lapply(ARW, function(var) union(CM, var))
      models_new <- lapply(PMs, function(PM) {
        X_train1 <- Xtrain[, PM]
        X_test1 <- Xtest[, PM]
        SemiSupervisedFitting_vectorize(X_train1, X_test1, ltrain, ltest,
                                        vtest, pnolabeled, model = CE,
                                        iterations = iterations, 
                                        alpharef = alpharef)
      })
      
      CCRPMs <- sapply(models_new, function(m) m$CCRTestC)
      
      best_index <- which.max(CCRPMs)
      if (CCRPMs[best_index] > CCRCM) {
        CM <- PMs[[best_index]]
        CCRCM <- CCRPMs[best_index]
        posCM <- cont
      }
      cat("\n"," File: ",file_name," Simulation: ",i_sim," - ", cont," ,model = ",unlist(CM),"\n")
      
      cont <- cont + 1
    }
  } 
  
  return(list(fitted_NC_model = models[[posCM]]$fitted_NC_model,
              fitted_C_model = models[[posCM]]$fitted_NC_model,
              Selectedmodel = CM, CCRCM = CCRCM, 
              Classprediction = models[[posCM]]$ltest_hat_C,
              ContaminatedSamplesprediction = models[[posCM]]$vtest_hat,
              posCM = posCM,  iterations = cont , models = models))             
}



SemiSupervised_HLS_vectorize <- function(file_name, pathScenarios, CE, variables_True_Model, 
                               pnolabeled = 0.5, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0) {
  filePathScenario <- paste0(pathScenarios, file_name) 
  fileRDS <- readRDS(filePathScenario)  
  nsimulations <- length(fileRDS$GenData)
  
  if (!is.numeric(nsimulations) || nsimulations == 0) stop("The file doesn't contain any simulation")
  
  Metrics <- data.frame(Nsim = numeric(), FittedContModel_SM = character(), FittedNoContModel_SM = character(),
                        Iterations_SM = numeric(), Model_SM = character(), Nvars_SM = numeric(),
                        CCR_SM = numeric(), Precision_SM = numeric(), Recall_SM = numeric(),
                        Specificity_SM = numeric(), F1_SM = numeric(), CCRCont_SM = numeric(),
                        CCRNoCont_SM = numeric(), PrecisionV_SM = numeric(), RecallV_SM = numeric(),
                        SpecificityV_SM = numeric(), F1V_SM = numeric(), FittedContModel_TM = character(),
                        FittedNoContModel_TM = character(), Iterations_TM = numeric(), Model_TM = character(),
                        Nvars_TM = numeric(), CCR_TM = numeric(), Precision_TM = numeric(), Recall_TM = numeric(),
                        Specificity_TM = numeric(), F1_TM = numeric(), CCRCont_TM = numeric(), CCRNoCont_TM = numeric(),
                        PrecisionV_TM = numeric(), RecallV_TM = numeric(), SpecificityV_TM = numeric(), F1V_TM = numeric(),
                        FittedContModel_SaturatedM = character(), FittedNoContModel_SaturatedM = character(),
                        Iterations_SaturatedM = numeric(), Model_SaturatedM = character(), Nvars_SaturatedM = numeric(),
                        CCR_SaturatedM = numeric(), Precision_SaturatedM = numeric(), Recall_SaturatedM = numeric(),
                        Specificity_SaturatedM = numeric(), F1_SaturatedM = numeric(), CCRCont_SaturatedM = numeric(),
                        CCRNoCont_SaturatedM = numeric(), PrecisionV_SaturatedM = numeric(), RecallV_SaturatedM = numeric(),
                        SpecificityV_SaturatedM = numeric(), F1V_SaturatedM = numeric(), stringsAsFactors = FALSE)
  
  MetricsIndex <- 1
  
  for (i_sim in 1:nsimulations) {
    GenData <- fileRDS$GenData[[i_sim]]
    par <- fileRDS$par
    
    G <- length(unique(GenData$l))
    Xtrain <- GenData$Xtrain
    Xtest <- GenData$Xtest
    ltrain <- GenData$ltrain
    ltest <- GenData$ltest
    vtest <- GenData$vtest
    
    MmetricsSaturatedM <- MmetricsTM <- MmetricsSM <- data.frame(Group = 1:G, Precision = rep(0, G), 
                                                                 Recall = rep(0, G), Specificity = rep(0, G),
                                                                 F1 = rep(0, G))
    
    dfRW <- getOW_vectorize(Xtrain, ltrain)
    RW <- dfRW$Var
    variables_saturated_model <- RW
    
    saturated_mod <- SemiSupervisedFitting_vectorize(Xtrain, Xtest, ltrain, ltest, vtest, CE, pnolabeled)
    
    selectedVar_mod <- HeadLongSearch_vectorize(Xtrain, Xtest, RW, ltrain, ltest, vtest, 
                                      CE = CE, i_sim, file_name, pnolabeled = 0.5, iterations = niterations,
                                      alpharef = 0.75, tol = 0.01, epsilon = 0)
    
    pos_True_Model <- findPosModel(selectedVar_mod$models, variables_True_Model)
    if (pos_True_Model != 0 && is.numeric(pos_True_Model)) {
      TrueModel <- selectedVar_mod$models[[pos_True_Model]]
    } else {
      Xtrain_TM <- data.frame(Xtrain) %>% dplyr::select(all_of(variables_True_Model))
      Xtest_TM <- data.frame(Xtest) %>% dplyr::select(all_of(variables_True_Model))
      
      TrueModel <- SemiSupervisedFitting_vectorize(Xtrain_TM, Xtest_TM, ltrain, ltest, vtest, CE, pnolabeled,
                                         iterations = niterations, alpharef = 0.75, tol = 0.01)
      TrueModel$PM <- variables_True_Model
    }
    
    pos <- selectedVar_mod$posCM
    nVarSel <- length(selectedVar_mod$Selectedmodel)
    PM <- selectedVar_mod$Selectedmodel
    Xsubset <- data.frame(Xtrain) %>% dplyr::select(all_of(PM))
    
    # ... Continue with the remaining calculations
    
    t_test <- table(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C)
    CCRltesthatSM <- sum(diag(t_test))/sum(t_test)
    
    cat("\n", "Simulation ", i_sim, " from File ",file_name,
        " selected model", "test set ",selectedVar_mod$Selectedmodel,"-",CCRltesthatSM,"\n")
    
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
    
    
    # Fill in Metrics dataframe with computed metrics for this simulation
    Metrics[MetricsIndex, ] <- c(i_sim, fittedContModelSM, fittedNoContModelSM, iterationsSM, modelSM,
                                 nVarsSM, CCRSM, precisionSM, recallSM, specificitySM, f1SM, ccrContSM,
                                 ccrNoContSM, precisionVSM, recallVSM, specificityVSM, f1VSM, fittedContModelTM,
                                 fittedNoContModelTM, iterationsTM, modelTM, nVarsTM, CCRTM, precisionTM, recallTM,
                                 specificityTM, f1TM, ccrContTM, ccrNoContTM, precisionVTM, recallVTM, specificityVTM,
                                 f1VTM, fittedContModelSaturatedM, fittedNoContModelSaturatedM, iterationsSaturatedM,
                                 modelSaturatedM, nVarsSaturatedM, CCRSaturatedM, precisionSaturatedM, recallSaturatedM,
                                 specificitySaturatedM, f1SaturatedM, ccrContSaturatedM, ccrNoContSaturatedM,
                                 precisionVSaturatedM, recallVSaturatedM, specificityVSaturatedM, f1VSaturatedM)

        estimates[[MetricsIndex,]] <- c(selectedVar_mod$models,pos,selectedVar_mod$models[[pos]]$PM,
                                        selectedVar_mod$models[[pos]]$par,CE,
                                        selectedVar_mod$models[[pos]]$ltest_hat_C,
                                        TrueModel$ltest_hat_C,saturated_mod$ltest_hat_C,SM_vtest,TM_vtest,
                                        saturated_vtest)
    

    MetricsIndex <- MetricsIndex + 1
  }
  
  Output <- list(Metrics = Metrics, Metrics_SaturatedM = MmetricsSaturatedM,
                 Metrics_SM = MmetricsSM, Metrics_TM = MmetricsTM,
                 GenData = GenData, Estimates = estimates)
  
  return(Output)
}
