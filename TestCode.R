getwd()
source("SimClassfewdim.R")



nruns = 10
selectedvariables <- list()
mu1 <- c(0,0,0,0)
mu2 <- c(0,6,0,6)
mu <- cbind(mu1,mu2)
sg <- diag(1,4)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <-c(1,1)
etag <- c(1,1)
GenData <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenData$ltrain
GenData$Xtrain


mg <- apply(unmap(GenData$ltrain),2,sum)
par <- list(mu = mu,sigma = sg, alpha = alphag, eta = etag,G = 2)

est1 <- eCmn(Xtrain = GenData$Xtrain,par = par)
par$v <- est1$v
est1$z

mCmn(Xtrain = GenData$Xtrain,ltrain=GenData$ltrain, par = par)

emCmn(GenData$Xtrain,GenData$ltrain,par)








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

