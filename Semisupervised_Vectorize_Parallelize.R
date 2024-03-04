
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


getOW_parallel <- function(df_train, l_train) {
  if (!is.data.frame(df_train)) df_train <- data.frame(df_train)
  
  NumVar <- colnames(df_train)[sapply(df_train, is.numeric)]
  if (length(NumVar) == 0) stop('Hey, the input data frame should contain numeric variables')
  
  # Initialize a parallel backend
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  fvalue <- foreach(i = seq_along(NumVar), .combine = 'c') %dopar% {
    aov_result <- aov(as.formula(paste(NumVar[i], '~', 'factor(l_train)')), data = df_train)
    summary(aov_result)[[1]]$`F value`[1]
  }
  
  stopCluster(cl)  # Stop the parallel backend
  
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

SemiSupervisedFitting_parallel <- function(X_train, X_test, ltrain, ltest,
                                           vtest, model = "EEI",
                                           pnolabeled = 0.5,
                                           iterations = 10, 
                                           alpharef = 0.75, tol = 0.01) {
  # Initialize a parallel backend
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Define the parallelized function for CNmixt
  CNmixt_parallel <- function(X_train, G, contamination, model, alphamin, ltrain1, iter.max) {
    CNmixt(X_train, G, contamination = contamination, model = model,
           initialization = "mixt", alphamin = alphamin,
           label = ltrain1, iter.max = iter.max)
  }
  
  # Parallelize the computation of ModelNonCont
  ModelNonCont <- foreach() %dopar% {
    CNmixt_parallel(X_train, G, FALSE, model, alpharef, ltrain1, iterations)
  }
  
  # Parallelize the computation of ModelCont
  ModelCont <- foreach() %dopar% {
    CNmixt_parallel(X_train, G, TRUE, model, alpharef, ltrain1, iterations)
  }
  
  # Stop the parallel backend
  stopCluster(cl)
  
  # Combine results
  ModelNonCont <- unlist(ModelNonCont, recursive = FALSE)
  ModelCont <- unlist(ModelCont, recursive = FALSE)
  
  # Perform the rest of the computations sequentially
  
  # Combine results
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
    mean = resNC$models[[1]]$mean,
    variance = resNC$models[[1]]$Sigma
  )
  
  mstep_nc <- mclust::mstep(data = as.matrix(X_train), modelName = resNC$models[[1]]$model, 
                            z = unmap(estimate$ltrain_hat))
  estep_nc <- mclust::estep(data = as.matrix(X_test), modelName =  mstep_nc$modelName, 
                            parameters = mstep_nc$parameters)
  
  ltest_hat_nc <- apply(estep_nc$z, 1, which.max)
  CCRTest_Nc <- sum((ltest_hat_nc == ltest)) / length(ltest)
  
  ExpectedValues_C <- E_StepCMN_vectorize(X_test, ltest, parameters_C)
  
  CCRTest_C <- ifelse(length(ExpectedValues_C$lhat) == length(ltest),
                      sum((ExpectedValues_C$lhat == ltest)) / length(ltest),
                      -1)
  
  AccTest_C <- ifelse(length(ExpectedValues_C$vhat) == length(vtest),
                      sum((ExpectedValues_C$vhat == vtest)) / length(vtest),
                      -1)
  
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
    par = parameters_C
  )
  
  return(Output)
}


HeadLongSearch_parallel <- function(Xtrain, Xtest, RW, ltrain, ltest, vtest, CE,
                                    i_sim, file_name, pnolabeled = 0.5, iterations = 10,
                                    alpharef = 0.75, tol = 0.01, epsilon = 0) {
  
  p <- length(RW)
  models <- list()
  
  if (!is.data.frame(Xtrain)) Xtrain <- data.frame(Xtrain)
  if (!is.data.frame(Xtest)) Xtest <- data.frame(Xtest)
  
  # Initialize a parallel backend
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  

  # Parallelize the computation of models for different feature subsets
  models <- foreach(cont = 1:p, .combine = "list") %dopar% {
    PM <- RW[cont]
    X_train <- Xtrain[, PM]
    X_test <- Xtest[, PM]
    
    cat("\n Model ", unlist(PM))
    
    model <- SemiSupervisedFitting_parallel(X_train, X_test, ltrain, ltest,
                                            vtest, model = c("E", "V"), 
                                            pnolabeled, iterations = iterations, 
                                            alpharef = alpharef)
    model$PM <- PM
    model
  }
  
  # Stop the parallel backend
  stopCluster(cl)
  
  # Identify the best model
  CM <- RW[[which.max(sapply(models, function(m) m$CCRTestC))]]
  CCRCM <- max(sapply(models, function(m) m$CCRTestC))
  posCM <- which.max(sapply(models, function(m) m$CCRTestC))
  
  # Continue the search
  cont <- length(models) + 1
  ARW <- RW
  OldCM <- "NA"
  OldCCRCM <- 0
  
  while ((!setequal(OldCM, CM) & OldCCRCM < CCRCM & length(ARW) > 0) | 
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
      cat("\n"," File: ", file_name, " Simulation: ", i_sim, " - ", cont, " ,model = ", unlist(CM), "\n")
      
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