library(MASS)
library(DIRECT)


getwd()
sys_info <- Sys.info()

if (sys_info["nodename"] != "WildFree") {
  ruta<- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
  setwd(ruta)
}
#source("SimClassfewdim.R")
source("SimClassEM20Steps.R")
library(utils)


nruns = 10
# Dataset D.1 (contaminated) ----------------------------------------------

mu1 <- c(0,0)
mu <- mu1
sg <- diag(1,2)
pig<- c(1)
nobservations = 700
ptraining = 0.5
alphag <- 0.95
etag <- 30
set.seed(123)
GenDataD.1 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)
GenDataD.1$vtrain

GenDataD.1 <- readRDS("DatasetD1.RDS")

GenDataD.1$Xtrain %>% apply(2,mean)
GenDataD.1$Xtrain[vtrain == 1, ] %>% apply(2,mean)
GenDataD.1$Xtrain[vtrain == 0, ] %>% apply(2,mean)

GenDataD.1$vtrain

# plot training set
plot(GenDataD.1$Xtrain, col = ifelse(GenDataD.1$vtrain == 1, "blue", "red"), 
     pch = 15+GenDataD.1$ltrain,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Non Contaminated","Contaminated"), 
       col = c("blue","red"),
       pch = c(16,16))
#text(3.614436,-1.094842,"56",-0.5)

# plot testing set
plot(GenDataD.1$Xtest, col = ifelse(GenDataD.1$vtest == 1, "blue", "red"), 
     pch = 15+GenDataD.1$ltest,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Non Contaminated","Contaminated"), 
       col = c("blue","red"),
       pch = c(16,16))
#text(3.614436,-1.094842,"56",-0.5)


# actual values for mu sigma and pi in the training set
#---------------------------------------------
Xtrain <- GenDataD.1$Xtrain
Xtest <- GenDataD.1$Xtest
ltrain <- GenDataD.1$ltrain
ltest <- GenDataD.1$ltest
vtrain <- GenDataD.1$vtrain
vtest <- GenDataD.1$vtest

actual_mean = apply(Xtrain[vtrain == 1,],2,mean)
actual_var = var(Xtrain[vtrain == 1,])
mg <- apply(unmap(ltrain),2,sum)
actual_var_cont = diag(var(Xtrain[vtrain == 0,]))

ntrain <- length(vtrain)

actual_pi = mg/ntrain
actual_alpha <- sum(vtrain == 1)/ntrain
actual_eta <- mean(actual_var_cont)
aux1<-0
aux2 <- 0
aux3 <- 0
a <-0
b<-0
for (i in 1:ntrain)
{
  aux1 <- GenDataD.1$ltrain[i]*(GenDataD.1$vtrain[i])
  actual_alpha <- actual_alpha  + aux1 
  aux2 <- GenDataD.1$ltrain[i]*(1-GenDataD.1$vtrain[i])

  a <- a + aux2
  aux3 <- aux2* mahalanobis(GenDataD.1$Xtrain[i,],actual_mean,actual_var)
  b <- b + aux3
}
#actual_alpha <- actual_alpha/ntrain
#actual_eta <- b/(2*a)
#---------------------------------------------
resD1 <- ModelAccuracy3(GenDataD.1$Xtrain, GenDataD.1$Xtest,
                      GenDataD.1$ltrain, GenDataD.1$ltest,
                      CE = "EII", alpharef = 0.90, tol = 0.0001)

resD1$diflog

unlist(resD1$loglikelihod)[10:20]

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

aux20 <- eCmn(GenDataD.1$Xtest,parD1_20)
vD1_20t <- ifelse(aux20$v<0.5,0,1)
table(GenDataD.1$vtest,vD1_20t)




