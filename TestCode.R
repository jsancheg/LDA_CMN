getwd()
source("SimClassfewdim.R")



nruns = 10
selectedvariables <- list()


# Dataset A.1 (uncontaminated) ---------------------------------------------------

mu1 <- c(0,0,0,0)
mu2 <- c(0,2,0,2)
mu <- cbind(mu1,mu2)
sg <- diag(1,4)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <-c(1,1)
etag <- c(1,1)
GenDataA.1 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
plot(GenDataA.1$Xtrain[,c(2,4)], col = GenDataA.1$ltrain, pch = 15+GenDataA$ltrain,
       xlab = "X2", ylab = "X4", main = "Dataset A.1")


# Dataset A.2 (uncontaminated) ---------------------------------------------------

mu1 <- c(0,0,0,0)
mu2 <- c(0,4,0,4)
mu <- cbind(mu1,mu2)
sg <- diag(1,4)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <-c(1,1)
etag <- c(1,1)
GenDataA.2 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)

plot(GenDataA.2$Xtrain[,c(2,4)], col = GenDataA.2$ltrain, 
     pch = 15+GenDataA.2$ltrain,
     xlab = "X2", ylab = "X4", main = "Dataset A.2")



# Dataset A.3 (uncontaminated) ----------------------------------------------

mu1 <- c(0,0,0,0)
mu2 <- c(0,6,0,6)
mu <- cbind(mu1,mu2)
sg <- diag(1,4)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <-c(1,1)
etag <- c(1,1)
GenDataA.3 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)

plot(GenDataA.3$Xtrain[,c(2,4)], col = GenDataA.3$ltrain, 
     pch = 15+GenDataA.3$ltrain,
     xlab = "X2", ylab = "X4", main = "Dataset A.3")


# Dataset A.4 (contaminated) ----------------------------------------------

mu1 <- c(0,0,0,0)
mu2 <- c(0,4,0,4)
mu <- cbind(mu1,mu2)
sg <- diag(1,4)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <-c(0.9,0.8)
etag <- c(2,3)
GenDataA.4 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)

plot(GenDataA.4$Xtrain[,c(2,4)], col = GenDataA.4$ltrain, 
     pch = 15+GenDataA.4$ltrain,
     xlab = "X2", ylab = "X4", main = "Dataset A.4")



# Dataset D.1 (contaminated) ----------------------------------------------

mu1 <- c(0,0,0,0)
mu <- mu1
sg <- diag(1,4)
pig<- c(1)
nobservations = 160
ptraining = 0.75
alphag < 0.9
etag <- 2
GenDataD.1 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.1$vtrain

GenDataD.1$vtrain

plot(GenDataD.1$Xtrain[,c(2,4)], col = GenDataD.1$vtrain+2, 
     pch = 15+GenDataD.1$ltrain,
     xlab = "X2", ylab = "X4", main = "Dataset D.4")









cbind(GenDataD$ltrain,GenDataD$vtrain)

GenDataD$Xtrain[GenDataD$vtrain[,1]==0,]
GenDataD$Xtrain[GenDataD$vtrain[,2]==0,]
GenDataD$Xtrain[95,]

GenDataD$Xtrain[GenDataD$ltrain == 2,]

GenDataD$vtrain[GenDataD$ltrain == 2,2]

GenDataD$Xtrain[GenDataD$vtrain[GenDataD$ltrain == 2,2]==0,]

 GenDataD$ltrain[119]
GenDataD$Xtrain[119,]

mod_par <- mclust::mstep(GenDataD$Xtrain,"EII",z = unmap(GenDataD$ltrain))
par <- list()
par$mu<-mod_par$parameters$mean
par$sigma <- mod_par$parameters$variance$sigma
par$alpha <- mod_par$parameters
par$eta <- c(2,3)
par$G <- 2
mg <- apply(unmap(GenDataD$ltrain),2,sum)
pig <- mg/nrow(Xtrain)

par <- list(mu = mu,sigma = sg, alpha = alphag, 
            eta = etag, G = 2, pig = pig)

est1 <- eCmn(Xtrain = GenDataD$Xtrain, par = par)
est1

par <- mCmn(Xtrain = GenDataD$Xtrain,ltrain=GenDataD$ltrain, par = est1)
par$G<-2
par$pig <- pig

est2 <- eCmn(Xtrain = GenDataD$Xtest,par = par)

est2$z
est2$lhat
GenDataD$ltest

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