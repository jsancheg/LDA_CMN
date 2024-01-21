SimDfDifIF <- function(mug,sigmag,pig,nobs,ptraining,alphag,etag)
{
  # Function: SimDfDifIF.
  # Description: This function generates datasets that allow.
  # different variable inflation factors within classes.
  # Parameters
  # mu: parameter that receives a matrix of dimension p rows and G columns
  #      that contains the mean of each class in each column.
  # sigmag: parameter that receives an array where each element of its 3rd dimension 
  #         is the covariance matrix of a group.
  # pig: parameter that receives a vector containing the proportion of observations for each
  #       group.
  # ptraining: parameter that receives a variable with the percentage of observations 
  #             that will be use in training.
  # nobs: parameter that receives a variable with the total number of observations. 
  # alphag: parameter that receives the percentage of contaminated observations for each class.
  # etag:   parameter that receives a vector when it is only 1 group and a 
  #         matrix when there are more than 1 group
  #        where each column corresponds to the elements of 
  #        the diagonal of the inflation factors for each group.
  
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
  sg <- array(0,dim = dim(sigmag))
  for(g in 1:G) 
    {
      if(length(dim(sigmag)) == 2){
           sg <- t(diag(sqrt(eta),p)) %*% sigmag %*% diag(sqrt(eta),p)
      } else if(length(dim(sigmag)) == 3) {
      sg[,,g] <- t(diag(sqrt(etag[,g]),p))  %*% sigmag[,,g] %*% diag(sqrt(etag[,g]),p)
      } else if(length(dim(sigmag)) > 3) stop("Sigma is an array of dimension 4")
  }
  

  for(i in 1:nobs)
    v[i] <- as.numeric(rbinom(1,1,alphag[l[i]]))
  if(length(dim(sigmag)) == 2){
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
          X[i,] <- rMVNorm(1,mug[,l[i]],sigmag)
        else  X[i,] <- rMVNorm(1,mug,sigmag)
        
      }else if(v[i]==0)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg)
        else  X[i,] <- rMVNorm(1,mug,sg)
      }  
      
    }
    
  }else if(length(dim(sg)) > 2)
  {
    
    for (i in 1:nobs)
      if(v[i] == 1)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sigmag[,,l[i]])
        else
          X[i,] <- rMVNorm(1,mug,sigmag[,,l[i]])
      }else if(v[i] == 0)
      {
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg[,,l[i]])
        else  X[i,] <- rMVNorm(1,mug,sg[,,l[i]])
        
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


SSFit_DifIF<- function(Xtrain, Xtest, ltrain, ltest,
                                  vtest, model = "EII",
                                  pnolabeled = 0.5,
                                  iterations = 10, 
                                 alpharef = 0.75, tol = 0.01)
  # Function SSFit_DifIF Semisupervised fitting for cases with different 
  #                       variables inflation factor within groups
  # Xtrain:       matrix with the observations used in training
  # Xtest:        matrix with the observations used in testing
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
  
  if(ncol(Xtrain) == 1) 
  {
    res <- CNmixt(Xtrain,G,  model = model, 
                  initialization = "random.post", alphamin = alpharef,
                  label = ltrain1,iter.max = iterations)
    
  }else if(ncol(Xtrain > 1))
  {
    res <- CNmixt(Xtrain,G,  model = model, 
                  initialization = "mixt", alphamin = alpharef,
                  label = ltrain1,iter.max = iterations)
  }
  
  #res1 <- ModelAccuracy2(Xtrain,Xtest,ltrain,ltest,"EEI")
  
  logl_nc <- res$models[[2]]$loglik
  obslll_nc <- res$models[[2]]$obslll
  
  logl_c <- res$models[[1]]$loglik
  obslll_c <- res$models[[1]]$obslll
  
  parameters_C$G <- res$models[[1]]$G
  parameters_C$pig <- res$models[[1]]$prior
  parameters_C$mu <- res$models[[1]]$mu
  parameters_C$Sigma <-res$models[[1]]$Sigma
  parameters_C$InvSigma <- res$models[[1]]$invSigma
  parameters_C$alpha <-res$models[[1]]$alpha
  parameters_C$eta <- res$models[[1]]$eta
  
  # estimate contaminated model  
  estimate$ztrain_hat <- res$models[[1]]$posterior
  # estimated class labels
  estimate$ltrain_hat <- res$models[[1]]$group
  estimate$vtrain_hat <- res$models[[1]]$v
  estimate$badPoints <- res$models[[1]]$detection
  
  parameters_Nc$pro <- res$models[[2]]$prior
  parameters_Nc$mean <- res$models[[2]]$mean
  parameters_Nc$variance <- res$models[[2]]$Sigma
  
  table(ltrain1,res$models[[1]]$group)
  
  if(ncol(Xtrain) == 1)
  {
    mstep_nc <- mclust::mstep( data = as.matrix(Xtrain), modelName = model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =as.matrix(Xtest), modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }else if(ncol(Xtrain) > 1)
  {
    mstep_nc <- mclust::mstep( data = Xtrain, modelName = model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =Xtest, modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }
  mstep_nc$modelName
  mstep_nc$parameters
  
  ltest_hat_nc <- apply(estep_nc$z,1,which.max)
  CCRTest_Nc <- sum((ltest_hat_nc == ltest)) / length(ltest)
  CCRTest_Nc
  
  ExpectedValues_C <- E_StepCMN(Xtest,ltest,parameters_C)
  if (length(ExpectedValues_C$lhat)==length(ltest)){
    CCRTest_C <- sum((ExpectedValues_C$lhat == ltest)) / length(ltest)
  }  else CCRTest_C = -1 
  
  if (length(ExpectedValues_C$vhat) == length(vtest))
  {
    AccTest_C <- sum((ExpectedValues_C$vhat == vtest)) / length(vtest)
  } else AccTest_C =  - 1
  
  res$models[[1]]$v
  
  res$models[[1]]$label
  res$models[[1]]$entropy
  res$models[[1]]$IC
  
  table(res$models[[1]]$label,res$models[[1]]$group)
  table(ltrain,res$models[[1]]$group)
  table(ltrain1,ltrain)
  length(res$models[[1]]$group)
  
  
  output <- list(CCRTestNc = CCRTest_Nc,CCRTestC = CCRTest_C,
                 ztest_hat_NC = estep_nc$z,
                 ltest_hat_NC =  ltest_hat_nc,
                 ztest_hat_C = ExpectedValues_C$z,
                 ltest_hat_C = ExpectedValues_C$lhat,
                 Expected_v = ExpectedValues_C$v,
                 vtest_hat = ExpectedValues_C$vhat,
                 niterations = iterations, par = parameters_C)
  return(output)  
}

