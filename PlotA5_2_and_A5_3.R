# Simulation with correlation
setwd("E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN")
source("SimClassEM20Steps.R")



# Sim Data A5.2
mu1 <- rep(0,100)
mu2 <- c(0,6,0,6,rep(0,96))
mu <- cbind(mu1,mu2)
sg <- diag(1,100)
sg[2,4] <- 0.8
sg[4,2] <- 0.8
pig<- c(0.5,0.5)
nobservations = 320
ptraining = 0.75
alphag <-c(0.9,0.8)
etag <- c(20,30)

GenData <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)


v1 <- rep(0,nrow(GenData$Xtrain))

for (i in 1:nrow(GenData$Xtrain))
{
  if(GenData$ltrain[i] == 1)
  {
    if(GenData$vtrain[i,1] == 1)
      v1[i]= 0
    else if(GenData$vtrain[i,1] == 0)
      v1[i] = 1
  }else  if(GenData$ltrain[i] == 2)
  {
    if(GenData$vtrain[i,2] == 1)
      v1[i]= 0
    else if(GenData$vtrain[i,2] == 0)
      v1[i] = 1
  } 
}
v1


# plot training set
plot(GenData$Xtrain[,c(2,4)], col = (1-v1)*GenData$ltrain+2, 
     pch = 15+GenData$ltrain,
     xlab = "X2", ylab = "X4")
legend("topright", legend = c("Class A","Class B"), 
       col = c("green","steelblue"),
       pch = c(16,17))


# Sim Data A5.3
mu1 <- rep(0,100)
mu2 <- c(0,6,0,6,rep(0,96))
mu <- cbind(mu1,mu2)
sg <- diag(1,100)
sg[2,4] <- 0.1
sg[4,2] <- 0.1
pig<- c(0.5,0.5)
nobservations = 320
ptraining = 0.75
alphag <-c(0.9,0.8)
etag <- c(20,30)

GenData <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)


v1 <- rep(0,nrow(GenData$Xtrain))

for (i in 1:nrow(GenData$Xtrain))
{
  if(GenData$ltrain[i] == 1)
  {
    if(GenData$vtrain[i,1] == 1)
      v1[i]= 0
    else if(GenData$vtrain[i,1] == 0)
      v1[i] = 1
  }else  if(GenData$ltrain[i] == 2)
  {
    if(GenData$vtrain[i,2] == 1)
      v1[i]= 0
    else if(GenData$vtrain[i,2] == 0)
      v1[i] = 1
  } 
}
v1


# plot training set
plot(GenData$Xtrain[,c(2,4)], col = (1-v1)*GenData$ltrain+2, 
     pch = 15+GenData$ltrain,
     xlab = "X2", ylab = "X4")
legend("topright", legend = c("Class A","Class B"), 
       col = c("green","steelblue"),
       pch = c(16,17))
