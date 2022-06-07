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



# Dataset D.1 Non Contaminated dimension 2 --------------------------------


mu1 <- c(0,0)
mu <- mu1
sg <- diag(1,2)
pig<- 1
nobservations = 160
ptraining = 0.75
alphag <- 0.99
etag <- 10
GenDataD.1 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.1$vtrain

plot(GenDataD.1$Xtrain, col = GenDataD.1$vtrain+2, 
     pch = 15+GenDataD.1$ltrain,
     xlab = "X2", ylab = "X4", main = "Dataset D.1 Bivariate normal")

par_actual <- list()
par_actual$mu <-matrix(mu1,nrow = 2,ncol = 1) 
par_actual$sigma <- sg
par_actual$alpha <- alphag
par_actual$eta <- etag
par_actual$G <- 1
par_actual$pig <- 1
par_actual$v <- as.matrix(rep(0.99,nrow(GenDataD.1$Xtrain)),nrow = nrow(GenDataD.10$Xtrain), 
                          ncol = 1)

true.max <- loglikCMN(GenDataD.1$Xtrain, GenDataD.1$ltrain, par_actual)



resD.1 <- ModelAccuracy3(GenDataD.1$Xtrain,GenDataD.1$Xtest,
               GenDataD.1$ltrain,GenDataD.1$ltest,"EEI",
               alpharef = 0.95, tol = 0.001)

true.max

resD.1$mu[2]
resD.1$sigma[2]
resD.1$alpha[2]
resD.1$eta[2]
resD.1$v[2]

convergence.value <- resD.1$loglikelihood[length(resD.1$loglikelihood)]

max.ind <- which.max(resD.1$loglikelihood) 
max.value <- resD.1$loglikelihood[max.ind]
max.value

sum(unlist(resD.1$loglikelihood[max.value])  > resD.1$loglikelihood)

length(resD.1$loglikelihood)

plot(x = 1:length(resD.1$loglikelihood), resD.1$loglikelihood, type = "l",
     xlab = "number of iterations", ylab = "log-likelihood")


# Dataset D.10 (contaminated) ----------------------------------------------

mu1 <- c(0,0,0,0)
mu <- mu1
sg <- diag(1,4)
pig<- c(1)
nobservations = 160
ptraining = 0.75
alphag <- 0.9
etag <- 2
GenDataD.10 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.10$vtrain

GenDataD.10$vtrain

plot(GenDataD.10$Xtrain[,c(2,4)], col = GenDataD.10$vtrain+2, 
     pch = 15+GenDataD.10$ltrain,
     xlab = "X2", ylab = "X4", main = "Dataset D.10")





par_actual <- list()
par_actual$mu <-matrix(mu1,nrow = 4,ncol = 1) 
par_actual$sigma <- sg
par_actual$alpha <- alphag
par_actual$eta <- etag
par_actual$G <- 1
par_actual$pig <- 1
par_actual$v <- as.matrix(rep(0.99,nrow(GenDataD.10$Xtrain)),nrow = nrow(GenDataD.10$Xtrain), 
                          ncol = 1)

loglikCMN(GenDataD.10$Xtrain, GenDataD.10$ltrain, par_actual)








par$eta <- mstep2$eta
#  par$alpha <- sapply(mstep2$alpha,function(i) max(alpharef[i],i) ) 
par$alpha <- mstep2$alpha


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