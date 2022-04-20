# Create dataset


#work_path <- "E:/University of Glasgow/Literature review/R Code/"
#setwd(work_path)
source("utilities.R")


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


SimGClasses <- function(mug,sigmag,pig,nobs,ptraining,alphag,etag)
  #n : number of observations
{
  output <- list()
  G <- length(pig)
  if(is.matrix(mug)) p<-nrow(mug) else p <- length(mug)
  X <- matrix(0.0, ncol = p , nrow = nobs)
  v <- matrix(-1, ncol = G , nrow = nobs)
  #Validate parameters
  if(sum(pig)!=1) stop("proportions do not sum 1")
  if(any(pig<0) | any(pig>1)) stop("alpha is not a probability")
  if(any(ptraining<0) | any(ptraining>1)) stop("ptraining is not a probability")
  if(any(alphag<0) | any(alphag>1)) stop("alpha takes values in the interval (0,1)")
  if(any(etag < 1))stop("eta has to be greater than 1")  
  set.seed(123)
  aux <- (rmultinom(nobs,1,pig))
  l <- apply(aux,2,which.max)
  
  
  if(length(dim(sg)) == 2){
    # X[i,] <- unlist(gen(p, mu = 0, sigma = 1))
    l <- sample(1:G,nobs,replace = T, prob = pig)
    mg <- apply(unmap(l),2,sum)
    #     if (any(alphag!=1)){
    for(i in 1:nobs)
      v[i,l[i]] <- as.numeric(rbernoulli(1,alphag[l[i]]))  
    #      }
    for (i in 1:nobs)
    {
      if(v[i,l[i]] == 1)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg)
        else  X[i,] <- rMVNorm(1,mug,sg)
        
      }else if(v[i,l[i]]==0)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],etag[l[i]]*sg)
        else  X[i,] <- rMVNorm(1,mug,etag[l[i]]*sg)
      }  
      
    }
    
  }else if(length(dim(sg)) > 2)
  {
    
    for (i in 1:nobs)
      if(v[i,l[i]] == 1)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg[,,l[i]])
        else
          X[i,] <- rMVNorm(1,mug,sg[,,l[i]])
      }else if(v[i,l[i]] == 0)
      {
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],etag[l[i]]*sg[,,l[i]])
        else  X[i,] <- rMVNorm(1,mug,etag[l[i]]*sg[,,l[i]])
        
      }
  } #End-if
  
  ind <- sample(1:nobs, round(nobs* ptraining))
  Xtrain <- X[ind,]
  Xtest <- X[-ind,]
  ltrain <- l[ind]
  ltest <- l[-ind]
  vtrain <- v[ind,]
  vtest <- v[-ind,]
  output <- list(X = X,l =l, ind = ind,Xtrain = Xtrain,
                 Xtest = Xtest,ltrain = ltrain,ltest = ltest,
                 v = v, vtrain = vtrain, vtest = vtest)
  return(output)
}




loglikCMN<-function(X,l, par)
{
  
  mu <- par$mu
  sg <- par$sigma
  G <- par$G
  
  pig <- par$pig
  alpha <- par$alpha
  eta <- par$eta
  v <- par$v
  m <- nrow(X)
  l <- unmap(l)
  p <- ncol(X)
  M <- matrix(0.0, nrow = m, ncol = G)
  
  
  p1<-0
  p2<-0
  
  
  for (g in 1:G)
  {
    for(i in 1:m)
    {
      term1 <- log(pig[g])
      
      if(length(dim(sg)) > 2)
      {
        term2 <- v[i,g] * log(alpha[g]*dMVNorm(X[i,],mu[,g],sg[,,g])  )
        term3<-(1-v[i,g]) * log( (1-alphag[g]) * dMVNorm(X[i,],mu[,g],eta*sg[,,g]) )
        
      }else 
      {
        sg <- matrix(sg, ncol = p, nrow = p)
        term2 <- v[i,g] * log(alpha[g]*dMVNorm(X[i,],mu[,g],data.matrix(sg)) )
        term3<-(1-v[i,g]) * log( (1-alphag[g]) * dMVNorm(X[i,],mu[,g],eta*sg ) )
      }
      
      if(ncol(l)> 1)
        M[i,g] <- l[i,g]*(term1 + term2 + term3)   
      else  M[i,g] <- l[i]*(term1 + term2 + term3)   
      
      
    }
  }
  
  loglik <- sum(M)
  
  return(loglik)
}


ModelAccuracy3 <- function(X_train,X_test,l_train,l_test,CE,
                           alpharef=NULL, tol = 0.001)
{
  if(!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if(!is.matrix(X_test)) X_test <- as.matrix(X_test)
  accTest <- 0.0
  par <- list()
  nvar <- ncol(X_train)
  nobs <- nrow(X_train)
  G <- length(unique(l_train))
  if(is.null(alpharef)) alpharef <- rep(0.95,G)
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  
  mstep0 <- CNmixt(X = X_train, contamination = F, model = CE,
                   initialization = "mixt", start.z = unmap(l_train), G = 2 )
  estep0 <- mstep0$models
  # Estimate parameters assuming uncontaminated set
  mstep1 <-mstep(data = X_train,modelName = CE, z = unmap(l_train))
  estep1 <- estep(data = X_test, modelName = CE, 
                  parameters = mstep1$parameters)
  z <- estep1$z  
  ltest <- apply(z,1,which.max)
  accTest <- sum(ltest == l_test)/length(l_test)
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma/mstep1$parameters$variance$scale
  par$G <- mstep1$parameters$variance$G
  par$pig <- apply(unmap(l_train),2,sum)/nrow(X_train)
  # give initial values for alpha
  par$alpha <- alpharef
  par$eta <- rep(1.011,G)
  cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",eta,"\n")
  estep2 <- eCmn(X_train,par)
  vhat <- estep2$v
  cat("\n","vij = ", estep2$v, "\n")
  lhat <-estep2$lhat
  par$v <- vhat  
  # Estimate parameters assuming contaminated set
  iter <- 1
  vr <- list()
  
  vr[[iter]] <- vhat
  logc[[iter]] <-loglikCMN(X_train, l_train,par) 
  vr[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  
  while (iter <= 10)
  {
    mstep2 <- mCmn(X_train,l_train,par)
    par$mu <- mstep2$mu
    par$sigma <- mstep2$sigma
    par$eta <- mstep2$eta
    #  par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
    par$alpha <- mstep2$alpha
    estep3 <- eCmn(X_train,mstep2)
    iter <- iter + 1
    vr[[iter]] <- estep3$v
    par$v <- vr[[iter]]
    logc[[iter-1]] <- loglikCMN(X_train,l_train,par)
    cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",eta,"\n")
    cat("\n",logc[[iter-1]],"\n")
    cat(estep3$v, "\n")
    
    
  }
  
  par$alpha
  
  return(accTest)
}



ModelAccuracy2 <- function(X_train,X_test,l_train,l_test,CE,
                           alpharef=NULL, tol = 0.001)
{
  if(!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if(!is.matrix(X_test)) X_test <- as.matrix(X_test)
  accTest <- 0.0
  par <- list()
  nvar <- ncol(X_train)
  nobs <- nrow(X_train)
  G <- length(unique(l_train))
  if(is.null(alpharef)) alpharef <- rep(0.95,G)
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  
  mstep0 <- CNmixt(X = X_train, contamination = F, model = CE,
                   initialization = "mixt", start.z = unmap(l_train), G = 2 )
  estep0 <- mstep0$models
  # Estimate parameters assuming uncontaminated set
  mstep1 <-mstep(data = X_train,modelName = CE, z = unmap(l_train))
  estep1 <- estep(data = X_test, modelName = CE, 
                  parameters = mstep1$parameters)
  z <- estep1$z  
  ltest <- apply(z,1,which.max)
  accTest <- sum(ltest == l_test)/length(l_test)
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma/mstep1$parameters$variance$scale
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
  ar <- list()
  logc <- list()
  lrinf<- list()
  
  vr[[iter]] <- vhat
  logc[[iter]] <-loglikCMN(X_train, l_train,par) 
  vr[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  Stop1 <- F
  #  while (Stop1 == F)
  while (iter <= 300)   
  {
    mstep2 <- mCmn(X_train,l_train,par)
    par$mu <- mstep2$mu
    par$sigma <- mstep2$sigma
    par$eta <- mstep2$eta
    #    par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
    par$alpha <- mstep2$alpha
    estep3 <- eCmn(X_train,mstep2)
    if (iter >=4 )
    {
      ar[[iter-3]] <- ( logc[[iter - 1]] - logc[[iter - 2]] ) / ( logc[[iter-2]]-logc[[iter-3]])
      lrinf[[iter-2]]<-logc[[iter-3]] + (logc[[iter-2]]-logc[[iter-3]])/(1-ar[[iter-3]]) 
      
      if(!is.null(lrinf[[iter-2]]))
        if( abs(lrinf[[iter-2]] - logc[[iter-2]] )< tol )
          Stop1 <- T
      
    }
    
    iter <- iter + 1
    vr[[iter]] <- estep3$v
    par$v <- vr[[iter]]
    logc[[iter]] <- loglikCMN(X_train,l_train,par)
    
    
  }
  
  par$alpha
  
  return(accTest)
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
  par$sigma <- mstep1$parameters$variance$sigma/mstep1$parameters$variance$scale
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
  while (flag2 < 10)
  {
    mstep2 <- mCmn(X_train,l_train,par)
    par$mu <- mstep2$mu
    par$sigma <- mstep2$sigma
    par$eta <- mstep2$eta
    par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
    estep3 <- eCmn(X_train,mstep2)
    iter <- iter + 1
    vr[[iter]] <- estep3$v
    
    if (all(vr[[iter]] == vr[[iter-1]]))
      flag2 <- flag2 + 1
    
  }
  
  par$alpha
  
  return(accTest)
}


eCmn <- function(Xtrain,par)
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
        num1[i,g] <- pig[g]*den
        
      } else if(length(dim(par$sigma)) > 2)
      {
        num <- alpha[g] * dMVNorm(Xtrain[i,],mu[,g],sigma[,,g])
        den <- num + (1-alpha[g])*dMVNorm(Xtrain[i,],mu[,g],eta[g]*sigma[,,g])
        v[i,g] <- num/den 
        num1[i,g] <- pig[g]*den
        
      } #End-f
      
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

mCmn <- function(Xtrain,ltrain,par)
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
    eta1[g]<-max(1.001,b[g]/(p*a[g]))
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