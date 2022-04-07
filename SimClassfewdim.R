# Create dataset


work_path <- "E:/University of Glasgow/Literature review/R Code/"
setwd(work_path)
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

Sim2Classes4d <- function(n)
  #n : number of observations
{
  output <- list()
  mu1 <- c(0,0,0,0)
  mu2 <- c(0,4,0,4)
  X <- matrix(0, ncol = 4, nrow = n)
  p <- ncol(X)
  l <- rep(0,n)
  set.seed(123)
  for( i in 1:n)
  {
    u <- runif(1)
    if(u < 0.5)
    {
      X[i,] <- unlist(gen(p, mu = 0, sigma = 1))
      
      l[i] <-1
    }    else    {
      X[i,1] <- as.numeric(gen(1, mu = 0, sigma = 1))
      X[i,2] <- as.numeric(gen(1, mu = 4, sigma = 1))
      X[i,3] <- as.numeric(gen(1, mu = 0, sigma = 1))
      X[i,4] <- as.numeric(gen(1, mu = 4, sigma = 1))
      
      l[i] <- 2
      
    } # End-if
  } # End-for
  
  ind <- sample(1:n, round(n/2))
  Xtrain <- X[ind,]
  Xtest <- X[-ind,]
  ltrain <- l[ind]
  ltest <- l[-ind]
  
  output <- list(X = X,l =l, ind = ind,Xtrain = Xtrain,Xtest = Xtest,ltrain = ltrain,ltest = ltest)
  return(output)
}

nruns = 100


# Dataset A ---------------------------------------------------------------
selectedvariables <- list()
accuracy <- rep(0,nruns)

for(nrun in 1: nruns)
{
  
  mu1 <- c(0,1,0,2)
  mu2 <- c(0,2,0,1)
  n = 160
  X <- matrix(0, ncol = 4, nrow = n)
  p <- ncol(X)
  l <- rep(0,n)
  
  
  #set.seed(123)
  for( i in 1:n)
  {
    u <- runif(1)
    if(u < 0.5)
    {
      #        cat("\n",i)
      X[i,] <- unlist(gen(p, mu = 0, sigma = 1))
      
      l[i] <-1
    }    else    {
      X[i,1] <- as.numeric(gen(1, mu = 0, sigma = 1))
      X[i,2] <- as.numeric(gen(1, mu = 4, sigma = 1))
      X[i,3] <- as.numeric(gen(1, mu = 0, sigma = 1))
      X[i,4] <- as.numeric(gen(1, mu = 4, sigma = 1))
      
      l[i] <- 2
    } # End-if
  } # End-for
  X
  l
  
  ind <- sample(1:n, round(n/2))
  Xtrain <- X[ind,]
  Xtest <- X[-ind,]
  ltrain <- l[ind]
  ltest <- l[-ind]
  
  
  dfRW <- getOW(Xtrain,ltrain)
  RW <- dfRW$Var
  res <- fHLvarSearch(Xtrain,Xtest,RW,ltrain,ltest,"E")
  cat("\n", res$model,"-",res$Accuracy,"\n")
  accuracy[nrun] <- res$Accuracy
  selectedvariables[[nrun]] <- paste(res$model,sep="-")
  #  models[i] <- res$model
  #  accuracy[i] <- res$Accuracy
  
} # End-for



# Dataset B ---------------------------------------------------------------


selectedvariables <- list()
accuracy <- rep(0,nruns)

for(nrun in 1: nruns)
{
  
    mu1 <- c(0,0,0,0)
    mu2 <- c(0,4,0,4)
    n = 160
    X <- matrix(0, ncol = 4, nrow = n)
    p <- ncol(X)
    l <- rep(0,n)
  
  
    #set.seed(123)
  for( i in 1:n)
  {
    u <- runif(1)
    if(u < 0.5)
    {
#        cat("\n",i)
        X[i,] <- unlist(gen(p, mu = 0, sigma = 1))
      
        l[i] <-1
      }    else    {
        X[i,1] <- as.numeric(gen(1, mu = 0, sigma = 1))
        X[i,2] <- as.numeric(gen(1, mu = 4, sigma = 1))
        X[i,3] <- as.numeric(gen(1, mu = 0, sigma = 1))
        X[i,4] <- as.numeric(gen(1, mu = 4, sigma = 1))
      
        l[i] <- 2
      } # End-if
    } # End-for
    X
    l
    
    ind <- sample(1:n, round(n/2))
    Xtrain <- X[ind,]
    Xtest <- X[-ind,]
    ltrain <- l[ind]
    ltest <- l[-ind]


  dfRW <- getOW(Xtrain,ltrain)
  RW <- dfRW$Var
  res <- fHLvarSearch(Xtrain,Xtest,RW,ltrain,ltest,"E")
  cat("\n", res$model,"-",res$Accuracy,"\n")
  accuracy[nrun] <- res$Accuracy
  selectedvariables[[nrun]] <- paste(res$model,sep="-")
#  models[i] <- res$model
#  accuracy[i] <- res$Accuracy
    
} # End-for






ModelAccuracy1 <- function(X_train,X_test,l_train,l_test,CE,cont)
{
  if(!is.matrix(X_train)) X_train1 <- as.matrix(X_train)
  if(!is.matrix(X_test)) X_test1 <- as.matrix(X_test)
  accTest <- 0.0
  
  if(cont == 1)
    
  {
    
    
  }
  
}


EMContMN <- function(X_train,l_train,CE)
{
  if(length(dim(X_train)) == 2)
  {
    p <- ncol(X_train)
    n <- nrow(X_train)
  }
    z <- unmap(l_train)
    G <- ncol(z)
    ng <- rep(0,G)
    pig <- rep(0.0,G)
    Sg <- rep(0.0,G)
    etag <- rep(1.0, G)
    bg <- rep(0.0,G)
    mug <- matrix(0.0,ncol = p, nrow = G)
    W <- array(0.0, dim = c( p, p,G))
    
    weights <- matrix(0.0,ncol = G,nrow =n)
    Sigmag <- matrix(0.0, dim = (p,p,G) )
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
