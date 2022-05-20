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
mu2 <- c(0,6,0,6)
mu <- cbind(mu1,mu2)
sg <- diag(1,4)
pig<- c(0.5,0.5)
nobservations = 320
ptraining = 0.75
alphag <-c(0.9,0.8)
etag <- c(20,30)
GenDataA.4 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)

v1 <- rep(0,nrow(GenDataA.4$Xtrain))

for (i in 1:nrow(GenDataA.4$Xtrain))
{
  if(GenDataA.4$ltrain[i] == 1)
  {
    if(GenDataA.4$vtrain[i,1] == 1)
      v1[i]= 0
    else if(GenDataA.4$vtrain[i,1] == 0)
      v1[i] = 1
  }else  if(GenDataA.4$ltrain[i] == 2)
  {
    if(GenDataA.4$vtrain[i,2] == 1)
      v1[i]= 0
    else if(GenDataA.4$vtrain[i,2] == 0)
      v1[i] = 1
  } 
}
v1

# plot training set
plot(GenDataA.4$Xtrain[,c(2,4)], col = (1-v1)+2, 
     pch = 15+GenDataA.4$ltrain,
     xlab = "X2", ylab = "X4")
legend("topright", legend = c("Class A","Class B"), 
       col = c("green","green"),
       pch = c(16,17))

# plot testing set
plot(GenDataA.4$Xtest[,c(2,4)], col = (1-v1)+2, 
     pch = 15+GenDataA.4$ltest,
     xlab = "X1", ylab = "X2")
legend("topright", legend = c("Class A","Class B"), 
       col = c("green","green"),
       pch = c(16,17))



resA4<-ModelAccuracy3(GenDataA.4$Xtrain, GenDataA.4$Xtest,
                      GenDataA.4$ltrain, GenDataA.4$ltest,
                      CE = "EII", alpharef = 0.90, tol = 0.0001)

resA4$diflog

par <- list()
par$mu <- resA4$mu
par$sigma <- resA4$sigma
par$alpha <- resA4$alpha
par$eta <- resA4$eta
par$v <- resA4$v
par$G <- 1
par$pig <- 1


par_actual <- list()
par_actual$mu <-matrix(mu1,nrow = length(mu1),ncol = 1) 
par_actual$sigma <- sg
par_actual$alpha <- alphag
par_actual$eta <- etag
par_actual$G <- 1
par_actual$pig <- 1
par_actual$v <- as.matrix(rep(0.99,nrow(GenDataA.4$Xtrain)),
                          nrow = nrow(GenDataA.4$Xtrain), ncol = 1)

logLikActual <-loglikCMN(GenDataA.4$Xtrain, GenDataA.4$ltrain, par_actual)
logLikActual

modA4 <- CNmixt(GenDataA.4$Xtrain,contamination = T, model = "EII",
                initialization = "mixt", label = GenDataA.4$ltrain, G = 2)

modA4$models[[1]]$loglik

resA4$loglikelihod[[2]]
resA4$loglikelihod[[3]]
resA4$loglikelihod[[5]]
resA4$loglikelihod[[10]]
resA4$loglikelihod[[length(resA4$loglikelihod)]]
#convergence at e=0.01
#convergence at e=0.001
#convergence at e=0.0001
resA4$loglikelihod[[18]]



resA4$loglikelihod[[2]] - logLikActual
resA4$loglikelihod[[3]] - logLikActual
resA4$loglikelihod[[5]] - logLikActual
resA4$loglikelihod[[10]] - logLikActual
#convergence at e=0.0001
resA4$loglikelihod[[18]] - logLikActual



plot(1:length(resA4$loglikelihod),resA4$loglikelihod, type = "l",
     xlab = "Iteration", ylab = "log-likelihood")
#mu
resA4$mu[[2]]
resA4$mu[[3]]
resA4$mu[[5]]
resA4$mu[[10]]
# convergence at e=0.0001
resA4$mu[[18]]

#sigma
resA4$sigma[[3]]
resA4$sigma[[5]]
resA4$sigma[[10]]
# convergence at e=0.0001
resA4$sigma[[18]]


#alpha
resA4$alpha[[3]]
resA4$alpha[[5]]
resA4$alpha[[10]]
# convergence at e=0.0001
resA4$alpha[[18]]

#eta
resA4$eta[[3]]
resA4$eta[[5]]
resA4$eta[[10]]
# convergence at e=0.0001
resA4$eta[[18]]


# Compare 
# v Class1
vA4_18.1 <- ifelse(resA4$v[[18]][,1]<0.5,0,1)
# v Class2
vA4_18.2 <- ifelse(resA4$v[[18]][,2]<0.5,0,1)

indA4.1<-which(GenDataA.4$vtrain[,1]!=-1)
auxA4.1_18<-cbind(GenDataA.4$vtrain[,1],vA4_18.1)[indA4.1,]
tA4.1_18 <- table(GenDataA.4$vtrain[indA4.1,1],vA4_18.1[indA4.1])
tA4.1_18
indA4.2<-which(GenDataA.4$vtrain[,2]!=-1)
auxA4.2<-cbind(GenDataA.4$vtrain[,2],vA4_18.2)[indA4.2,]
tA4.2_18 <- table(GenDataA.4$vtrain[indA4.2,2],vA4_18.2[indA4.2])
tA4.2_18

# contamination cross classification table

table(GenDataD.1$vtrain,vD1_15)
table(GenDataD.1$vtrain,vD1_17)
cbind(GenDataD.1$vtrain,vD1_20)
GenDataD.1$Xtrain[56,]
tD1_20 <- table(GenDataD.1$vtrain,vD1_20)
sum(diag(tD1_20))/sum(tD1_20)

# convergence e = 0.01
parD1_15<-list()
parD1_15$G <- 1
parD1_15$pig <- 1
parD1_15$mu <- resD1$mu[[15]]
parD1_15$sigma <- resD1$sigma[[15]]
parD1_15$alpha <- resD1$alpha[[15]]
parD1_15$eta <- resD1$eta[[15]]

# convergence e = 0.001
parD1_17<-list()
parD1_17$G <- 1
parD1_17$pig <- 1
parD1_17$mu <- resD1$mu[[15]]
parD1_17$sigma <- resD1$sigma[[15]]
parD1_17$alpha <- resD1$alpha[[15]]
parD1_17$eta <- resD1$eta[[15]]

# convergence 0.0001
parD1_20<-list()
parD1_20$G <- 1
parD1_20$pig <- 1
parD1_20$mu <- resD1$mu[[20]]
parD1_20$sigma <- resD1$sigma[[20]]
parD1_20$alpha <- resD1$alpha[[20]]
parD1_20$eta <- resD1$eta[[20]]


aux15 <- eCmn(GenDataD.1$Xtest,parD1_15)
vD1_15t <- ifelse(aux15$v<0.5,0,1)
table(GenDataD.1$vtest,vD1_15t)



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

# plot training set
plot(GenDataD.1$Xtrain, col = GenDataD.1$vtrain+2, 
     pch = 15+GenDataD.1$ltrain,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Non Contaminated","Contaminated"), 
       col = c("green","red"),
       pch = c(16,16))
text(3.614436,-1.094842,"56",-0.5)

# plot testing set
plot(GenDataD.1$Xtest, col = GenDataD.1$vtest+2, 
     pch = 15+GenDataD.1$ltest,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Non Contaminated","Contaminated"), 
       col = c("green","red"),
       pch = c(16,16))
text(3.614436,-1.094842,"56",-0.5)




resD1<-ModelAccuracy3(GenDataD.1$Xtrain, GenDataD.1$Xtest,
                      GenDataD.1$ltrain, GenDataD.1$ltest,
                      CE = "EII", alpharef = 0.90, tol = 0.0001)

resD1$diflog

par <- list()
par$mu <- resD1$mu
par$sigma <- resD1$sigma
par$alpha <- resD1$alpha
par$eta <- resD1$eta
par$v <- resD1$v
par$G <- 1
par$pig <- 1


par_actual <- list()
par_actual$mu <-matrix(mu1,nrow = length(mu1),ncol = 1) 
par_actual$sigma <- sg
par_actual$alpha <- alphag
par_actual$eta <- etag
par_actual$G <- 1
par_actual$pig <- 1
par_actual$v <- matrix(GenDataD.1$vtrain,nrow = nrow(GenDataD.1$Xtrain), 
                       ncol = 1)

logLikActual <-loglikCMN(GenDataD.1$Xtrain, GenDataD.1$ltrain, par_actual)
logLikActual

modD1 <- CNmixt(GenDataD.1$Xtrain,contamination = T, model = "EII",
                initialization = "mixt", label = GenDataD.1$ltrain, G = 1)

modD1$models[[1]]$loglik

resD1$loglikelihod[[2]]
resD1$loglikelihod[[3]]
resD1$loglikelihod[[5]]
resD1$loglikelihod[[10]]
resD1$loglikelihod[[length(resD1$loglikelihod)]]

resD1$loglikelihod[[2]] - logLikActual
resD1$loglikelihod[[3]] - logLikActual
resD1$loglikelihod[[5]] - logLikActual
resD1$loglikelihod[[10]] - logLikActual
# convergence e = 0.01
resD1$loglikelihod[[15]] - logLikActual
# convergence e = 0.001
resD1$loglikelihod[[17]] - logLikActual
# convergence e = 0.0001
resD1$loglikelihod[[20]] - logLikActual

unlist(resD1$loglikelihod)
length(resD1$loglikelihod)

plot(1:length(resD1$loglikelihod),resD1$loglikelihod, type = "l",
     xlab = "Iteration", ylab = "log-likelihood")

#mu
resD1$mu[[2]]
resD1$mu[[3]]
resD1$mu[[5]]
resD1$mu[[10]]
# convergence e = 0.01
resD1$mu[[15]]
# convergence e = 0.001
resD1$mu[[17]]
# convergence e = 0.0001
resD1$mu[[20]]


#sigma
resD1$sigma[[3]]
resD1$sigma[[5]]
resD1$sigma[[10]]
# convergence e = 0.01
resD1$sigma[[15]]
# convergence e = 0.001
resD1$sigma[[17]]
# convergence e = 0.0001
resD1$sigma[[20]]


#alpha
resD1$alpha[[3]]
resD1$alpha[[5]]
resD1$alpha[[10]]
# convergence e = 0.01
resD1$alpha[[15]]
# convergence e = 0.001
resD1$alpha[[17]]
# convergence e = 0.0001
resD1$alpha[[20]]

#eta
resD1$eta[[3]]
resD1$eta[[5]]
resD1$eta[[10]]
# convergence e = 0.01
resD1$eta[[15]]
# convergence e = 0.001
resD1$eta[[17]]
# convergence e = 0.0001
resD1$eta[[20]]

# Compare 
vD1_15 <- ifelse(resD1$v[[15]]<0.5,0,1)
vD1_17 <- ifelse(resD1$v[[17]]<0.5,0,1)
vD1_20 <- ifelse(resD1$v[[20]]<0.5,0,1)

# contamination cross classification table

table(GenDataD.1$vtrain,vD1_15)
table(GenDataD.1$vtrain,vD1_17)
cbind(GenDataD.1$vtrain,vD1_20)
GenDataD.1$Xtrain[56,]
tD1_20 <- table(GenDataD.1$vtrain,vD1_20)
sum(diag(tD1_20))/sum(tD1_20)

# convergence e = 0.01
parD1_15<-list()
parD1_15$G <- 1
parD1_15$pig <- 1
parD1_15$mu <- resD1$mu[[15]]
parD1_15$sigma <- resD1$sigma[[15]]
parD1_15$alpha <- resD1$alpha[[15]]
parD1_15$eta <- resD1$eta[[15]]

# convergence e = 0.001
parD1_17<-list()
parD1_17$G <- 1
parD1_17$pig <- 1
parD1_17$mu <- resD1$mu[[15]]
parD1_17$sigma <- resD1$sigma[[15]]
parD1_17$alpha <- resD1$alpha[[15]]
parD1_17$eta <- resD1$eta[[15]]

# convergence 0.0001
parD1_20<-list()
parD1_20$G <- 1
parD1_20$pig <- 1
parD1_20$mu <- resD1$mu[[20]]
parD1_20$sigma <- resD1$sigma[[20]]
parD1_20$alpha <- resD1$alpha[[20]]
parD1_20$eta <- resD1$eta[[20]]


aux15 <- eCmn(GenDataD.1$Xtest,parD1_15)
vD1_15t <- ifelse(aux15$v<0.5,0,1)
table(GenDataD.1$vtest,vD1_15t)

# Dataset D.2 (contaminated) ----------------------------------------------

mu1 <- c(0,0)
mu <- mu1
sg <- diag(1,2)
pig<- c(1)
nobservations = 320
ptraining = 0.75
alphag <- 0.85
etag <- 30
set.seed(123)
GenDataD.2 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.2$vtrain

GenDataD.2$vtrain

# plot training set
plot(GenDataD.2$Xtrain, col = GenDataD.2$vtrain+2, 
     pch = 15+GenDataD.2$ltrain,
     xlab = "X1", ylab = "X2")
legend("bottomright", legend = c("Non Contaminated","Contaminated"), 
       col = c("green","red"),
       pch = c(16,16))
text(GenDataD.2$Xtrain[43,1],GenDataD.2$Xtrain[43,2],"43",c(0,0))
text(GenDataD.2$Xtrain[45,1],GenDataD.2$Xtrain[45,2],"45",c(0,0))
text(GenDataD.2$Xtrain[48,1],GenDataD.2$Xtrain[48,2],"48",c(0,0))
text(GenDataD.2$Xtrain[125,1],GenDataD.2$Xtrain[125,2],"125",c(0,0))

#text(3.614436,-1.094842,"56",-0.5)
GenDataD.2$Xtrain[45,]

# plot testing set
plot(GenDataD.2$Xtest, col = GenDataD.2$vtest+2, 
     pch = 15+GenDataD.2$ltest,
     xlab = "X1", ylab = "X2")
legend("topleft", legend = c("Non Contaminated","Contaminated"), 
       col = c("green","red"),
       pch = c(16,16))
text(GenDataD.2$Xtrain[26,1],GenDataD.2$Xtrain[26,2],"26",c(0,0))
text(GenDataD.2$Xtest[32,1],GenDataD.2$Xtest[32,2],"43",c(0,0))
text(GenDataD.2$Xtest[51,1],GenDataD.2$Xtest[51,2],"45",c(0,0))
text(GenDataD.2$Xtest[66,1],GenDataD.2$Xtest[66,2],"66",c(0,0))


resD2<-ModelAccuracy3(GenDataD.2$Xtrain, GenDataD.2$Xtest,
                      GenDataD.2$ltrain, GenDataD.2$ltest,
                      CE = "EII", alpharef = 0.90, tol = 0.0001)

resD2$diflog

par <- list()
par$mu <- resD2$mu
par$sigma <- resD2$sigma
par$alpha <- resD2$alpha
par$eta <- resD2$eta
par$v <- resD2$v
par$G <- 1
par$pig <- 1


par_actual <- list()
par_actual$mu <-matrix(mu1,nrow = length(mu1),ncol = 1) 
par_actual$sigma <- sg
par_actual$alpha <- alphag
par_actual$eta <- etag
par_actual$G <- 1
par_actual$pig <- 1
par_actual$v <- matrix(GenDataD.2$vtrain,nrow = nrow(GenDataD.2$Xtrain), 
                       ncol = 1)

logLikActual <-loglikCMN(GenDataD.2$Xtrain, GenDataD.2$ltrain, par_actual)
logLikActual

modD2 <- CNmixt(GenDataD.2$Xtrain,contamination = T, model = "EII",
                initialization = "mixt", label = GenDataD.2$ltrain, G = 1)

modD2$models[[1]]$loglik

resD2$loglikelihod[[3]]
resD2$loglikelihod[[5]]
resD2$loglikelihod[[10]]
resD2$loglikelihod[[length(resD2$loglikelihod)]]
# convergence at e = 0.01
resD2$loglikelihod[[14]]
# convergence at e = 0.001
resD2$loglikelihod[[16]]
# convergence at e = 0.0001
resD2$loglikelihod[[18]]



resD2$loglikelihod[[3]] - logLikActual
resD2$loglikelihod[[5]] - logLikActual
resD2$loglikelihod[[10]] - logLikActual
# convergence e=0.01
resD2$loglikelihod[[14]] - logLikActual
# convergence e=0.001
resD2$loglikelihod[[16]] - logLikActual
# convergence e=0.0001
resD2$loglikelihod[[18]] - logLikActual



unlist(resD2$loglikelihod)
length(resD2$loglikelihod)

plot(1:length(resD2$loglikelihod),resD2$loglikelihod, type = "l",
     xlab = "Iteration", ylab = "log-likelihood")
#mu
resD2$mu[[3]]
resD2$mu[[5]]
resD2$mu[[10]]
# convergence e=0.01
resD2$mu[[14]]
# convergence e=0.001
resD2$mu[[16]]
# convergence e=0.001
resD2$mu[[18]]


#sigma
resD2$sigma[[3]]
resD2$sigma[[5]]
resD2$sigma[[10]]
# convergence e=0.01
resD2$sigma[[14]]
# convergence e=0.001
resD2$sigma[[16]]
# convergence e=0.0001
resD2$sigma[[18]]



#alpha
resD2$alpha[[3]]
resD2$alpha[[5]]
resD2$alpha[[10]]
# convergence e=0.01
resD2$alpha[[14]]
# convergence e=0.001
resD2$alpha[[16]]
# convergence e=0.0001
resD2$alpha[[18]]


#eta
resD2$eta[[3]]
resD2$eta[[5]]
resD2$eta[[10]]
# convergence e=0.01
resD2$eta[[14]]
# convergence e=0.001
resD2$eta[[16]]
# convergence e=0.0001
resD2$eta[[18]]




# Compare 
vD2_10 <- ifelse(resD2$v[[10]]<0.5,0,1)
vD2_14 <- ifelse(resD2$v[[14]]<0.5,0,1)
vD2_16 <- ifelse(resD2$v[[16]]<0.5,0,1)
vD2_18 <- ifelse(resD2$v[[18]]<0.5,0,1)

# contamination cross classification table

tD2_14 <- table(GenDataD.2$vtrain,vD2_14)
tD2_16 <- table(GenDataD.2$vtrain,vD2_16)

cbind(GenDataD.2$vtrain,vD2_18)
GenDataD.1$Xtrain[56,]

tD2_10 <- table(GenDataD.2$vtrain,vD2_10)
tD2_10
sum(diag(tD2_10))/sum(tD2_10)

tD2_18 <- table(GenDataD.2$vtrain,vD2_18)
sum(diag(tD2_18))/sum(tD2_18)
tD2_18


# 10th iteration
parD2_10<-list()
parD2_10$G <- 1
parD2_10$pig <- 1
parD2_10$mu <- resD2$mu[[10]]
parD2_10$sigma <- resD2$sigma[[10]]
parD2_10$alpha <- resD2$alpha[[10]]
parD2_10$eta <- resD2$eta[[10]]


# convergence e = 0.01
parD2_14<-list()
parD2_14$G <- 1
parD2_14$pig <- 1
parD2_14$mu <- resD2$mu[[14]]
parD2_14$sigma <- resD2$sigma[[14]]
parD2_14$alpha <- resD2$alpha[[14]]
parD2_14$eta <- resD2$eta[[14]]

# convergence e = 0.001
parD2_16<-list()
parD2_16$G <- 1
parD2_16$pig <- 1
parD2_16$mu <- resD2$mu[[16]]
parD2_16$sigma <- resD2$sigma[[16]]
parD2_16$alpha <- resD2$alpha[[16]]
parD2_16$eta <- resD2$eta[[16]]

# convergence 0.0001
parD2_18<-list()
parD2_18$G <- 1
parD2_18$pig <- 1
parD2_18$mu <- resD2$mu[[18]]
parD2_18$sigma <- resD2$sigma[[18]]
parD2_18$alpha <- resD2$alpha[[18]]
parD2_18$eta <- resD2$eta[[18]]




auxD2_10 <- eCmn(GenDataD.2$Xtest,parD2_10)
vD2_10t <- ifelse(auxD2_10$v<0.5,0,1)
tD2_10t <- table(GenDataD.2$vtest,vD2_10t)
tD2_10t
sum(diag(tD2_10t))/sum(tD2_10t)


auxD2_18 <- eCmn(GenDataD.2$Xtest,parD2_18)
vD2_18t <- ifelse(auxD2_18$v<0.5,0,1)
tD2_18t <- table(GenDataD.2$vtest,vD2_18t)
tD2_18t
sum(diag(tD2_18t))/sum(tD2_18t)







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
}