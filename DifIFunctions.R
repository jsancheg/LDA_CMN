Sim_DIF <- function(mug,sigmag,pig,nobs,ptraining,alphag,etag)
{
  # Function: Sim_DIF.
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

    if(ncol(X) == 1 & is.vector(sigmag) & length(sigmag)==1)
  {
    sg <- 0
  } else if(is.array(sigmag))
  {
    sg <- array(0,dim = dim(sigmag))
  }
  
  for(g in 1:G) 
    {
      if(ncol(X) == 1 & is.vector(sigmag) ) # only 1 column in X but different groups
      {
          sg <- etag * sigmag # sg could be a vector of length 1 if there is only one group 
                              # of dimension G if there are G groups
      }else if(length(dim(sigmag)) == 2){
           sg <- t(diag(sqrt(etag),p)) %*% sigmag %*% diag(sqrt(etag),p)
      } else if(length(dim(sigmag)) == 3) {
      sg[,,g] <- t(diag(sqrt(etag[,g]),p))  %*% sigmag[,,g] %*% diag(sqrt(etag[,g]),p)
      } else if(length(dim(sigmag)) > 3) stop("Sigma is an array of dimension 4")
    } # end-for g
  
  mg <- apply(unmap(l),2,sum)
  
  for(i in 1:nobs)
    v[i] <- as.numeric(rbinom(1,1,alphag[l[i]]))

  # initialize l
  if(ncol(X) == 1 & is.vector(sigmag) ) 
  {
    # same variance for all groups when X is composed by only 1 variable
    for (i in 1:nobs)
    {
      if(v[i] == 1) 
        # non-contaminated sample
      {  
        if(G > 1) # if there is more than 1 group 
          {
            # groups with different mean and equal variance
            X[i,] <- rmnorm(1,mug[l[i]],sigmag) 
          } else if(G > 1 & length(mug)== 1)  # more than 1 group with same mean
          { # groups with same mean and different variance
            X[i,] <- rmnorm(1,mug,sigmag[l[i]])
          } else if(G == 1) X[i,] <- rmnorm(1,mug,sigmag) # 1 group    
      }else if(v[i]==0)
        # contaminated sample
          {  
          if(is.matrix(mug) & G > 1 ) # if there is more than 1 group
          {
            # groups with different mean and equal contaminated variance
            X[i,] <- rmnorm(1,mug[,l[i]],sg)
          }else if (G>1 & length(mug)== 1) # more than 1 group with same mean
          { # groups with same mean and different variance
            X[i,] <- rnorm(1,mug,sg[l[i]])
          }else if (G==1) X[i,] <- rmnorm(1,mug,sg) # 1 group
          } # end if-else v  
      
    } # end for
  } # end if 
  
   if(length(dim(sigmag)) == 2){
#      Same covariance matrix for all groups
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
        } # end if-else v  
      
      } # end for
    
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

eCmn_DIF<-function(X,l,par)
{
  # eCmn_difIF E-step for contaminated mixture model with 
  #            different variable inflation factors within groups
  # X : matrix with the training data
  # l:  group information of the observations in training set
  # par:      parameters of the mixture of contaminated normal distribution with
  #           different variables inflation factor within groups
  
  m <- nrow(X)
  p <- ncol(X)
  alpha <- par$alpha
  mu <- par$mu
  sigma <- par$sigma
  G <- par$G
  pig <- par$pig
  eta <- par$eta #  eta contains a matrix of dimension p x G, where each column corresponds
                 #  the elements of the diagonal corresponding to variable inflation factor  
                 #  for the group g
  
  v <- matrix(0.0, ncol = G, nrow = m)
  z <- matrix(0.0, ncol = G, nrow = m)
  lhat <- rep(0,m)
  vhat <- rep(0,m)
  
  fxig <- matrix(0.0, ncol = G, nrow = m)
  thetaig <- matrix(0.0, ncol = G, nrow = m)
  
  
  numz <- matrix(0.0, ncol = G, nrow = m) # numerator for calculating zhat
  numv <- matrix(0.0, ncol = G, nrow = m) # numerator for calculating vhat
  denz <- rep(0,m)                        # denominator for calculating z hat
  denv <- matrix(0.0, ncol = G, nrow = m) # denominator for calculating v hat
  
  output <- list()
  
  if(ncol(X) == 1 & is.vector(sigma) & length(sigma)==1)
  {
    sg <- sigma
  } else if(is.array(sigma))
  {
    sg <- array(0,dim = dim(sigma))
  }
  
  for(g in 1:G) 
  {
    # different groups separated by only 1 variable with same variance or different variance
    if(ncol(X)==1 & is.vector(sigma) & length(sigma) >= 1)
    {
      sg <- eta*sigmag
    }
    if(length(dim(sigma)) == 2){
      sg <- t(diag(sqrt(eta),p)) %*% sigma %*% diag(sqrt(eta),p)
    } else if(length(dim(sigmag)) == 3) {
      sg[,,g] <- t(diag(sqrt(eta[g]),p))  %*% sigma[,,g] %*% diag(sqrt(eta[g]),p)
    } else if(length(dim(sigma)) > 3) stop("Sigma is an array of dimension 4")
  }
  
  
  
  
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
        fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dnorm(X[i,],mu[g],sg[g])
        
      } else if(length(dim(sigma))==2)
      {
        thetaig[i,g] <- dMVNorm(X[i,],mu[,g],sigma)
        fxig[i,g] <- alpha[g]*thethaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],sg[g])
        
      } else if(length(dim(sigma)) > 2)
      {
        if(ncol(X)>1)
        {
          thetaig[i,g] <-  dMVNorm(X[i,],mu[,g],sigma[,,g])
          fxig[i,g] <- alpha[g] * thetaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],sg[,,g])
          
        }else if(ncol(X)==1)
        {
          if(is.null(dim(sigma)))
          {
            thetaig[i,g] <- dnorm(X[i,],mu[g],sigma)
            fxig[i,g] <- alpha[g] * thetaig[i,g] + (1-alpha[g])*dnorm(X[i,],mu[g],eta[g]*sigma)
          } else if(!is.null(dim(sigma)) )
          {
            thetaig[i,g] <- dnorm(X[i,],mu[g],sigma[,,g])
            fxig[i,g] <- alpha[g] * thetaig[i,g] +  (1-alpha[g])*dnorm(X[i,],mu[g],sg[,,g])
            
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


#CNmixt_DifIF <- function(Xtrain,G,contamination,model,intialization,alphafix,
#                         alphamin,seed,start.z,start.v,start,label,AICcond,iter.max,
#                         threshold)

CNmixt_DifIF <- function(Xtrain,Xtest,ltrain,ltest,CE = "VVV",
                         niterations = 10,alpharef = 0.98, tol = 0.01)
{
  # Xtrain: dataset that contains the training set
  # Xtest:  dataset that contains the test set
  # ltrain: a vector containing the label/group information of observations in training set
  # ltest : a vector containing the label/group information of observations in test set
  # CE    : a covariance structure of the covariance matrices 
  # niterations: number of maximum iterations
  # alpharef: a vector of length G with the proportion of good observations in each group
  # tol:      tolerance value to use as a measure of improvement in log likelihood
  
  # Check parameters of the function
  
  if(!is.matrix(Xtrain)) Xtrain<-as.matrix(Xtrain)
  if(!is.matrix(Xtest)) Xtest<-as.matrix(Xtest)
  
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
  nvar <- ncol(Xtrain)
  nobs <- nrow(Xtrain)
  G <- length(unique(ltrain))
  if(is.null(alpharef)) alpharef <- rep(0.95,G)
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  if(ncol(Xtrain)==1) CE <- "E" else CE <- CE
  
  # Estimating initial parameters value for the model assuming
  # a non contaminated model using the function mstep and estep from 
  # library mclust
  mstep1 <-mclust::mstep(data = Xtrain,modelName = CE, z = unmap(ltrain))
  estep1 <- mclust::estep(data = Xtest, modelName = CE, 
                  parameters = mstep1$parameters)
  z <- estep1$z  
  lhat_nc <- apply(z,1,which.max)
  accTest_nc <- sum(lhat_nc == ltest)/length(ltest)
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma
  par$G <- mstep1$parameters$variance$G
  par$pig <- apply(unmap(ltrain),2,sum)/nrow(Xtrain)
  # give initial values for alpha
  par$alpha <- alpharef
  par$eta <- rep(1.011,G)
  
  
  #  cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
  
  estep2 <- eCmn_DIF(Xtrain,ltrain,par)
  estep2_test <- eCmn_DIF(Xtest,ltest,par)
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
  
  # Create the function loglikCMN for different variables inflation factors within group
  logc[[iter]] <- loglikCMN(X_train1, l_train,par) 
  vtrain_r[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  diflog[[iter]] <- NA
  #cat("\n","iter=",iter,";","diflog=",diflog[[iter]])
  
  
  
  
}


SSFit_DifIF<- function(Xtrain, Xtest, ltrain, ltest,
                                  vtest, model = "VVV",
                                  pnolabeled = 0,
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
    # fit Contaminated Mixture model with different variables inflation
    # factor within cluster
    res <- CNmixt(Xtrain,G,  model = model, 
                  initialization = "random.post", alphamin = alpharef,
                  label = ltrain1,iter.max = iterations)
    
  }else if(ncol(Xtrain > 1))
  {
    # fit Contaminated Mixture model with different variables inflation
    # factor within cluster
    
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

