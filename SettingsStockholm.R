getwd()
source("SimClassfewdim.R")
nruns = 10
selectedvariables <- list()


# Dataset D.1 (contaminated) ----------------------------------------------

mu1 <- c(0,0)
mu <- mu1
sg <- diag(1,2)
pig<- c(1)
nobservations = 160
ptraining = 0.75
alphag <- 0.99
etag <- 30
set.seed(123)
GenDataD.1 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.1$vtrain

GenDataD.1$vtrain

plot(GenDataD.1$Xtrain, col = GenDataD.1$vtrain+2, 
     pch = 15+GenDataD.1$ltrain,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Class A","Contaminated class A"), 
       col = c("green","red"),
       pch = c(16,16))

# Dataset D.2 (contaminated) ----------------------------------------------

mu1 <- c(0,0)
mu <- mu1
sg <- diag(1,2)
pig<- c(1)
nobservations = 160
ptraining = 0.75
alphag <- 0.95
etag <- 30
set.seed(123)
GenDataD.2 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.2$vtrain

plot(GenDataD.2$Xtrain, col = GenDataD.2$vtrain+2, 
     pch = 15+GenDataD.2$ltrain,
     xlab = "X2", ylab = "X4")
legend("bottomleft", legend = c("Class A","Contaminated class A"),
       col = c("green","red"),
       pch = c(16,16))


# Dataset D.3 (contaminated) ----------------------------------------------

mu1 <- c(0,0)
mu <- mu1
sg <- diag(1,2)
pig<- c(1)
nobservations = 160
ptraining = 0.75
alphag <- 0.85
etag <- 30
set.seed(123)
GenDataD.3 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.3$vtrain

plot(GenDataD.3$Xtrain, col = GenDataD.3$vtrain+2, 
     pch = 15+GenDataD.3$ltrain,
     xlab = "X1", ylab = "X2")

legend("bottomleft", legend = c("Class A","Contaminated class A"), 
       col = c("green","red"),
       pch = c(16,16))


# Dataset D.4 (2 contaminated classes)  ----------------------------------------------

mu1 <- c(0,0)
mu2 <- c(3,3)
mu <- cbind(mu1,mu2)
sg <- diag(1,2)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <- c(0.90,0.90)
etag <- c(30,30)
set.seed(123)
GenDataD.4 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.4$vtrain

v <- rep(0,nrow(GenDataD.4$Xtrain))

for (i in 1:nrow(GenDataD.4$Xtrain))
{
  if(GenDataD.4$ltrain[i] == 1)
  {
    if(GenDataD.4$vtrain[i,1] == 1)
      v[i]= 0
    else if(GenDataD.4$vtrain[i,1] == 0)
      v[i] = 1
  }else  if(GenDataD.4$ltrain[i] == 2)
  {
    if(GenDataD.4$vtrain[i,2] == 1)
      v[i]= 0
    else if(GenDataD.4$vtrain[i,2] == 0)
      v[i] = 1
  } 
  
}


plot(GenDataD.4$Xtrain, col = (1-v)+2, 
     pch = 15+GenDataD.4$ltrain,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Class A","Class B"), 
       col = c("green","red"),
       pch = c(16,17))


# Dataset D.5 (2 contaminated classes)  ----------------------------------------------

mu1 <- c(0,0)
mu2 <- c(6,6)
mu <- cbind(mu1,mu2)
sg <- diag(1,2)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <- c(0.85,0.85)
etag <- c(30,30)
set.seed(123)
GenDataD.5 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.5$vtrain
GenDataD.5$vtrain[,GenDataD.5$ltrain]

v <- rep(0,nrow(GenDataD.5$Xtrain))

for (i in 1:nrow(GenDataD.5$Xtrain))
{
  if(GenDataD.5$ltrain[i] == 1)
  {
    if(GenDataD.5$vtrain[i,1] == 1)
      v[i]= 0
    else if(GenDataD.5$vtrain[i,1] == 0)
      v[i] = 1
  }else  if(GenDataD.5$ltrain[i] == 2)
  {
      if(GenDataD.5$vtrain[i,2] == 1)
        v[i]= 0
    else if(GenDataD.5$vtrain[i,2] == 0)
      v[i] = 1
  } 
}
v

plot(GenDataD.5$Xtrain, col = (1-v)+2, 
     pch = 15+GenDataD.5$ltrain,
     xlab = "X1", ylab = "X2")

# Dataset D.7 (2 no contaminated classes)  ----------------------------------------------

mu1 <- c(0,0)
mu2 <- c(6,6)
mu <- cbind(mu1,mu2)
sg <- diag(1,2)
pig<- c(0.5,0.5)
nobservations = 160
ptraining = 0.75
alphag <- c(1,1)
etag <- c(1.011,1.011)
set.seed(123)
GenDataD.7 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.7$vtrain

v <- rep(0,nrow(GenDataD.7$Xtrain))

for (i in 1:nrow(GenDataD.7$Xtrain))
{
  if(GenDataD.7$ltrain[i] == 1)
  {
    if(GenDataD.7$vtrain[i,1] == 1)
      v[i]= 0
    else if(GenDataD.7$vtrain[i,1] == 0)
      v[i] = 1
  }else  if(GenDataD.7$ltrain[i] == 2)
  {
    if(GenDataD.7$vtrain[i,2] == 1)
      v[i]= 0
    else if(GenDataD.7$vtrain[i,2] == 0)
      v[i] = 1
  } 
}
v


plot(GenDataD.7$Xtrain, col = GenDataD.7$ltrain, 
     pch = 16,
     xlab = "X1", ylab = "X2")
legend("topleft", legend = c("Class A","Class B"), col = c("red","black"),
       pch = c(16,16))

plot(GenDataD.7$Xtrain, col = 1,
     pch = 16,
     xlab = "X1", ylab = "X2")

# fitting using mixture of contaminated normal 
library(ContaminatedMixt)

mod1
