source("SimClassEM20steps")
#source("DifIFunctions.R")

loglikCMN<-function(X,l, par)
{
  
  mu <- as.matrix(par$mu)
  if(is.matrix(X) & ncol(X)==1) 
  { 
    if(length(dim(par$sigma))>2)
    {
      sg <- as.vector(par$sigma)
    }else if(!is.matrix(par$sigma))
    {
      sg <- rep(par$sigma,par$G)
    }
  }else  sg <- par$sigma
  
  G <- par$G
  pig <- par$pig
  alpha <- par$alpha
  eta <- par$eta
  v <- par$v
  
  m <- nrow(X)
  p <- ncol(X)
  
  l <- unmap(l)
  M <- matrix(0.0, nrow = m, ncol = G)
  
  
  
  for (g in 1:G)
  {
    for(i in 1:m)
    {
      term1 <- log(pig[g])
      if(length(dim(sg)) > 2)
      {
        term2 <- v[i,g] * (log(alpha[g]) + dMVNorm(X[i,],mu[,g],sg[,,g],log = TRUE) )
        term3<-(1-v[i,g]) * (log(1-alpha[g]) + dMVNorm(X[i,],mu[,g],eta[g]*sg[,,g], log = TRUE) )
        
      }else if(length(dim(sg))<=2)
      {
        if(ncol(X)>1)
        {
          s <- matrix(sg, ncol = p, nrow = p)
          term2 <- v[i,g] * ( log(alpha[g])+ dMVNorm(X[i,],mu[,g],data.matrix(s),log = TRUE ) )
          term3 <-(1-v[i,g]) * ( log (1-alpha[g]) + dMVNorm(X[i,],mu[,g],eta[g]*s,log = TRUE ) )
        } else if(ncol(X)==1)
        {
          mu <- as.vector(mu)
          term2 <- v[i,g] * log(alpha[g]*dnorm(X[i,],mu[g],sg[g] ) )
          term3<-(1-v[i,g]) * log( (1-alpha[g]) * dnorm(X[i,],mu[g],eta[g]*sg[g] ) )
        }
      }
      
      if(ncol(l)> 1)
        M[i,g] <- l[i,g]*(term1 + term2 + term3)   
      else  M[i,g] <- l[i]*(term1 + term2 + term3)   
      
      
    }
  }
  
  loglik <- sum(M)
  
  return(loglik)
}

CalculateMetrics <- function(Xtrain,Xtest,ltrain,ltest,
                             vtrain, vtest,
                             Model= c("EII","VII"),nsteps= 20)
{
  res1 <- vector("list",nsteps)
  ressd1 <-vector("list",nsteps)
  par1 <- vector("list",nsteps)
  logliked1 <- vector("list",nsteps)
  obsllld1 <- vector("list",nsteps)
  estmud1 <- vector("list",nsteps)
  estsigmad1 <-vector("list",nsteps)
  estalphad1 <- vector("list",nsteps)
  estetad1 <- vector("list",nsteps)
  
  par1s <- vector("list",nsteps)
  logliked1s <- vector("list",nsteps)
  obsllld1s <- vector("list",nsteps)
  estmud1s <- vector("list",nsteps)
  estsigmad1s <-vector("list",nsteps)
  Metricsd1 <- data.frame()
  estalphad1s <- vector("list",nsteps)
  estetad1s <-  vector("list",nsteps)

    
  cfd1_Train <- vector("list",nsteps)
  cfd1_Test <- vector("list",nsteps)
  
  for(i in 1:nsteps)
  {
    res1[[i]] <- CNmixt(Xtrain, G = 1, contamination = TRUE, model = Model ,iter.max = i)
    ressd1[[i]] <- CNmixt(Xtrain, G = 1, label = ltrain ,contamination = TRUE, model = Model ,iter.max = i)
    par1[[i]] <- ContaminatedMixt::getPar(res1[[i]])
    par1[[i]]$pig <- par1[[i]]$prior
    par1[[i]]$G <- length(unique(ltrain))
    
    
    logliked1[[i]] <- res1[[1]]$models[[1]]$loglik    
    
    obsllld1[[i]] <- res1[[1]]$models[[1]]$obslll
    estmud1[[i]] <-par1[[i]]$mu
    estsigmad1[[i]] <- par1[[i]]$Sigma
    estalphad1[[i]] <- par1[[i]]$alpha
    estetad1[[i]] <- par1[[i]]$eta
    
    par1s[[i]] <- ContaminatedMixt::getPar(ressd1[[i]])
    
    par_fittedModel <- list(pig = par1s[[i]]$prior, G = length(unique(ltrain)), mu = par1s[[i]]$mu,
                sigma = par1s[[i]]$Sigma, alpha = par1s[[i]]$alpha,
                eta = par1s[[i]]$eta, v = ressd1[[i]]$models[[1]]$v)
    
    
    logliked1s[[i]] <- loglikCMN(Xtrain,ltrain,par_fittedModel)
    
    
#    logliked1s[[i]] <- ressd1[[1]]$models[[1]]$loglik    
    obsllld1s[[i]] <- ressd1[[1]]$models[[1]]$obslll
    estmud1s[[i]] <-par1s[[i]]$mu
    estsigmad1s[[i]] <- par1s[[i]]$Sigma
    estalphad1s[[i]] <- par1s[[i]]$alpha
    estetad1s[[i]] <- par1s[[i]]$eta
    
    pred_class_train <- ressd1[[i]]$models[[1]]$group
    pred_v_train <- ifelse(ressd1[[i]]$models[[1]]$v>0.5,1,0)
    
    aux_train <- data.frame(Actual_v = vtrain, Pred_v = as.vector(pred_v_train) )
    
    Metricsd1[i,"Iteration"] <- i
    Metricsd1[i,"CCR_class_Train"]  <-  MLmetrics::Accuracy(pred_class_train,ltrain)
    Metricsd1[i,"TP_train"] <- nrow(aux_train[aux_train$Actual_v == 1 & aux_train$Pred_v == 1,])
    Metricsd1[i,"TN_train"] <- nrow(aux_train[aux_train$Actual_v == 0 & aux_train$Pred_v == 0,])
    Metricsd1[i,"FP_train"] <- nrow(aux_train[aux_train$Actual_v == 1 & aux_train$Pred_v == 0,])
    Metricsd1[i,"FN_train"] <- nrow(aux_train[aux_train$Actual_v == 0 & aux_train$Pred_v == 1,])
    
    Metricsd1[i,"Sensitivity_vTrain"] <- MLmetrics::Sensitivity(vtrain,pred_v_train,positive = 0 )
    Metricsd1[i,"Specificity_vTrain"] <- MLmetrics::Specificity( vtrain,pred_v_train, positive = 0)
    Metricsd1[i,"Precision_vTrain"] <- MLmetrics::Precision(vtrain,pred_v_train, positive = 0)
    Metricsd1[i,"F1_Train"] <- MLmetrics::F1_Score(vtrain,pred_v_train,positive = 0)
    
    cfd1_Train[[i]] <- MLmetrics::ConfusionMatrix(pred_v_train,vtrain)    
    
    par_fittedModel <- list(G = 1 ,pig = par1s[[i]]$prior,
                mu = par1s[[i]]$mu,
                sigma = par1s[[i]]$Sigma,
                alpha = par1s[[i]]$alpha,
                eta = par1s[[i]]$eta)
    
    output <- eCmn(Xtest, par_fittedModel)
    pred_class_test <- output$lhat
    
    
    pred_v_test <- ifelse(output$v>0.5,1,0)
    aux_test <- data.frame(Actual_v = vtest, Pred_v = as.vector(pred_v_test) )
    
    Metricsd1[i,"CCR_class_Test"]  <-  MLmetrics::Accuracy(pred_class_test,ltest)
    Metricsd1[i,"TP_Test"] <- nrow(aux_test[aux_test$Actual_v == 1 & aux_test$Pred_v == 1,])
    Metricsd1[i,"TN_Test"] <- nrow(aux_test[aux_test$Actual_v == 0 & aux_test$Pred_v == 0,])
    Metricsd1[i,"FP_Test"] <- nrow(aux_test[aux_test$Actual_v == 1 & aux_test$Pred_v == 0,])
    Metricsd1[i,"FN_Test"] <- nrow(aux_test[aux_test$Actual_v == 0 & aux_test$Pred_v == 1,])
    
    Metricsd1[i,"Sensitivity_vTest"] <- MLmetrics::Sensitivity(vtest,pred_v_test,positive = 0 )
    Metricsd1[i,"Specificity_vTest"] <- MLmetrics::Specificity( vtest,pred_v_test, positive = 0)
    Metricsd1[i,"Precision_vTest"] <- MLmetrics::Precision(vtest,pred_v_test, positive = 0)
    Metricsd1[i,"F1_Test"] <- MLmetrics::F1_Score(vtest,pred_v_test,positive = 0)
    cfd1_Test[[i]] <- MLmetrics::ConfusionMatrix(pred_v_test,vtest)    
    
  }
  
  Output <- list(Metrics = Metricsd1,cf_train = cfd1_Train,cf_test = cfd1_Test,
                 parameters = par1s, res_clustering = res1, res_da = ressd1,
                 loglikelihood_clustering = logliked1, loglikelihood_da = logliked1s,
                 muhat_da = estmud1s, Sigmahat_da = estsigmad1s,
                 alpha_da = estalphad1s, eta_da = estetad1s)
  
  return(Output)
}


# actual values for mu sigma and pi in the training set
#---------------------------------------------
actual_mean = apply(GenDataD.1$Xtrain,2,mean)
actual_var = var(GenDataD.1$Xtrain)
mg <- sum(GenDataD.1$vtrain)
ntrain <- length(GenDataD.1$vtrain)

actual_pi = mg/ntrain
actual_alpha <- 0
actual_eta <-0
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
actual_alpha <- actual_alpha/ntrain
actual_eta <- b/(2*a)


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
length(GenDataD.1$vtrain)

nsteps <- 20


metrics_d1 <- CalculateMetrics(GenDataD.1$Xtrain,GenDataD.1$Xtest,GenDataD.1$ltrain,
                               GenDataD.1$ltest,GenDataD.1$vtrain, GenDataD.1$vtest,
                               Model = c("EII","VII"), nsteps = 20 )
mu <- metrics_d1$muhat_da
sigma <- metrics_d1$Sigmahat_da
alpha <- metrics_d1$alpha_da
eta <- metrics_d1$eta_da
loglikelihood <- metrics_d1$loglikelihood_da

round(metrics_d1$Metrics[,c("Sensitivity_vTest","Specificity_vTest","Precision_vTest","F1_Test")],2)

# Unsupervised
# mu
lapply(list(mu[[3]],mu[[5]],mu[[10]],mu[[15]],mu[[17]],mu[[20]]),round,2)
# sigma
lapply(list(sigma[[3]],sigma[[5]],sigma[[10]],sigma[[15]],sigma[[17]],sigma[[20]]),round,2)
# alpha
round(c(alpha[[3]],alpha[[5]],alpha[[10]],alpha[[15]],alpha[[17]],alpha[[20]]),2)
# eta
round(c(eta[[3]],eta[[5]],eta[[10]],eta[[15]],eta[[17]],eta[[20]]),2)
# log-likelihood
round(c(loglikelihood[[3]],loglikelihood[[5]],loglikelihood[[10]],loglikelihood[[15]],loglikelihood[[17]],loglikelihood[[20]]),2)


actual_mean = apply(GenDataD.1$Xtrain,2,mean)
actual_var = var(GenDataD.1$Xtrain)
mg <- sum(GenDataD.1$vtrain)
ntrain <- length(GenDataD.1$vtrain)

actual_pi = apply(unmap(GenDataD.1$ltrain),2,sum)/ntrain
actual_alpha <- sum(GenDataD.1$vtrain==0)/ntrain

aux_eta <- optimize(f_eta,c(1,10000),tol=0.01, 
                    z = GenDataD.1$ltrain, v = GenDataD.1$v, X = GenDataD.1$Xtrain,
                    mu = mu1, sigma = sg, maximum = TRUE)


actual_eta <- aux_eta$maximum

actual_eta 


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


par_actual <- list()
par_actual$mu <- mu
par_actual$sigma <- sg
par_actual$alpha <- alphag
par_actual$eta <-etag
par_actual$z <- GenDataD.1$ltrain
par_actual$v <- GenDataD.1$vtrain
  
loglikCMN(GenDataD.1$Xtrain,l = GenDataD.1$ltrain,par_actual)

resD1<-ModelAccuracy3(GenDataD.1$Xtrain, GenDataD.1$Xtest,
                      GenDataD.1$ltrain, GenDataD.1$ltest,
                      CE = "EII", alpharef = 0.90, tol = 0.0001)

# mu at selected ECM steps
round(resD1$mu[[1]],2)
round(resD1$mu[[3]],2)
round(resD1$mu[[5]],2)
round(resD1$mu[[10]],2)
round(resD1$mu[[15]],2)
round(resD1$mu[[17]],2)
round(resD1$mu[[20]],2)

# sigma at selected ECM steps
round(resD1$sigma[[1]],2)
round(resD1$sigma[[3]],2)
round(resD1$sigma[[5]],2)
round(resD1$sigma[[10]],2)
round(resD1$sigma[[15]],2)
round(resD1$sigma[[17]],2)
round(resD1$sigma[[1]],2)


# alpha at selected ECM steps
round(resD1$alpha[[3]],2)
round(resD1$alpha[[5]],2)
round(resD1$alpha[[10]],2)
round(resD1$alpha[[15]],2)
round(resD1$alpha[[17]],2)
round(resD1$alpha[[20]],2)

# eta at selected ECM steps
round(resD1$eta[[3]],2)
round(resD1$eta[[5]],2)
round(resD1$eta[[10]],2)
round(resD1$eta[[17]],2)
round(resD1$eta[[20]],2)


# likelihood at selected ECM steps
resD1$loglikelihod[[1]]
resD1$loglikelihod[[3]]
resD1$loglikelihod[[5]]
resD1$loglikelihod[[10]]
resD1$loglikelihod[[15]]
resD1$loglikelihod[[17]]
resD1$loglikelihod[[20]]


# Prediction at iteration 5 10  15 20  Training set 
vD1_1 <- ifelse(resD1$v[[1]]<0.5,0,1)
vD1_2 <- ifelse(resD1$v[[1]]<0.5,0,2)
vD1_3 <- ifelse(resD1$v[[1]]<0.5,0,3)
vD1_4 <- ifelse(resD1$v[[1]]<0.5,0,4)
vD1_5 <- ifelse(resD1$v[[5]]<0.5,0,1)
vD1_10 <- ifelse(resD1$v[[10]]<0.5,0,1)
vD1_15 <- ifelse(resD1$v[[15]]<0.5,0,1)
vD1_20 <- ifelse(resD1$v[[20]]<0.5,0,1)



cond1_train_5iteracion <- paste0(GenDataD.1$vtrain,vD1_5)


# plot training set
plot(GenDataD.1$Xtrain, 
     col = ifelse(cond1_train_5iteracion == "11","blue",
                  ifelse(cond1_train_5iteracion == "00","blue",
                         ifelse(cond1_train_5iteracion=="10","red","red")) ),
     pch = ifelse(cond1_train_5iteracion == "11",19,
                  ifelse(cond1_train_5iteracion == "00",17,
                         ifelse(cond1_train_5iteracion=="10",3,4)) ),
     xlab = "X1", ylab = "X2")
legend("topleft",
       legend = c("TN",
                  "TP",
                  "FN",
                  "FP"),
       pch = c(19, 17, 3, 4),
       col = c("blue","blue","red","red"),
       bty = "n",
       pt.cex = 1,
       cex = 0.8)


# convergence e = 0.01
parD1_5<-list()
parD1_5$G <- 1
parD1_5$pig <- 1
parD1_5$mu <- resD1$mu[[5]]
parD1_5$sigma <- resD1$sigma[[5]]
parD1_5$alpha <- resD1$alpha[[5]]
parD1_5$eta <- resD1$eta[[5]]

# convergence e = 0.001
parD1_10<-list()
parD1_10$G <- 1
parD1_10$pig <- 1
parD1_10$mu <- resD1$mu[[10]]
parD1_10$sigma <- resD1$sigma[[10]]
parD1_10$alpha <- resD1$alpha[[10]]
parD1_10$eta <- resD1$eta[[10]]



est_v_5 <- eCmn(GenDataD.1$Xtest,parD1_5)
vD1_test_5 <- ifelse(est_v_5$v<0.5,0,1)

est_v_10 <- eCmn(GenDataD.1$Xtest,parD1_10)
vD1_test_10 <- ifelse(est_v_10$v<0.5,0,1)


cond1_test_5iteracion <- paste0(GenDataD.1$vtest,vD1_test_5)

# plot test set
plot(GenDataD.1$Xtest, 
     col = ifelse(cond1_test_5iteracion == "11","blue",
                  ifelse(cond1_test_5iteracion == "00","blue",
                         ifelse(cond1_test_5iteracion=="10","red","red")) ),
     pch = ifelse(cond1_test_5iteracion == "11",19,
                  ifelse(cond1_test_5iteracion == "00",17,
                         ifelse(cond1_test_5iteracion=="10",3,4)) ),
     xlab = "X1", ylab = "X2")
legend("topleft",
       legend = c("TN",
                  "TP",
                  "FN",
                  "FP"),
       pch = c(19, 17, 3, 4),
       col = c("blue","blue","red","red"),
       bty = "n",
       pt.cex = 1,
       cex = 0.8)



table(GenDataD.1$vtest,vD1_test_5)
table(GenDataD.1$vtest,vD1_test_10)




aux20 <- eCmn(GenDataD.1$Xtest,parD1_20)
vD1_20t <- ifelse(aux20$v<0.5,0,1)
table(GenDataD.1$vtest,vD1_20t)




# contamination cross classification table

table(GenDataD.1$vtrain,vD1_1)
table(GenDataD.1$vtrain,vD1_2)
table(GenDataD.1$vtrain,vD1_3)
table(GenDataD.1$vtrain,vD1_4)
table(GenDataD.1$vtrain,vD1_5)
table(GenDataD.1$vtrain,vD1_10)
table(GenDataD.1$vtrain,vD1_15)
cbind(GenDataD.1$vtrain,vD1_20)


GenDataD.1$Xtrain[56,]
tD1_20 <- table(GenDataD.1$vtrain,vD1_20)
sum(diag(tD1_20))/sum(tD1_20)

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


# actual values for mu sigma and pi in the training set
#---------------------------------------------
actual_mean = apply(GenDataD.1$Xtrain,2,mean)
actual_var = var(GenDataD.1$Xtrain)
mg <- sum(GenDataD.1$vtrain)
ntrain <- length(GenDataD.1$vtrain)

actual_pi = mg/ntrain
actual_alpha <- 0
actual_eta <-0
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
actual_alpha <- actual_alpha/ntrain
actual_eta <- b/(2*a)
#---------------------------------------------
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

aux20 <- eCmn(GenDataD.1$Xtest,parD1_20)
vD1_20t <- ifelse(aux20$v<0.5,0,1)
table(GenDataD.1$vtest,vD1_20t)


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
     pch = 15+GenDataD.2$ltrain,ylim = c(-15,10),
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
     pch = 15+GenDataD.2$ltest, ylim = c(-15,10),
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Non Contaminated","Contaminated"), 
       col = c("green","red"),
       pch = c(16,16))
text(GenDataD.2$Xtrain[26,1],GenDataD.2$Xtrain[26,2],"26",c(0,0))
text(GenDataD.2$Xtest[32,1],GenDataD.2$Xtest[32,2],"43",c(0,0))
text(GenDataD.2$Xtest[51,1],GenDataD.2$Xtest[51,2],"45",c(0,0))
text(GenDataD.2$Xtest[66,1],GenDataD.2$Xtest[66,2],"66",c(0,0))


# actual values for mu sigma and pi in the training set
#---------------------------------------------
actual_mean = apply(GenDataD.2$Xtrain,2,mean)
actual_var = var(GenDataD.2$Xtrain)
mg <- sum(GenDataD.2$vtrain)
ntrain <- length(GenDataD.2$vtrain)

actual_pi = mg/ntrain
actual_alpha <- 0
actual_eta <-0
aux1<-0
aux2 <- 0
aux3 <- 0
a <-0
b<-0
for (i in 1:ntrain)
{
  aux1 <- GenDataD.2$ltrain[i]*(GenDataD.2$vtrain[i])
  actual_alpha <- actual_alpha  + aux1 
  aux2 <- GenDataD.2$ltrain[i]*(1-GenDataD.2$vtrain[i])
  
  a <- a + aux2
  aux3 <- aux2* mahalanobis(GenDataD.2$Xtrain[i,],actual_mean,actual_var)
  b <- b + aux3
}
actual_alpha <- actual_alpha/ntrain
actual_eta <- b/(2*a)


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

tD2_10 <- table(GenDataD.2$vtrain,vD2_10)
tD2_14 <- table(GenDataD.2$vtrain,vD2_14)
tD2_16 <- table(GenDataD.2$vtrain,vD2_16)
tD2_18 <- table(GenDataD.2$vtrain,vD2_18)

cbind(GenDataD.2$vtrain,vD2_10)
cbind(GenDataD.2$vtrain,vD2_18)
GenDataD.1$Xtrain[56,]

tD2_10 <- table(GenDataD.2$vtrain,vD2_10)
tD2_10
sum(diag(tD2_10))/sum(tD2_10)

tD2_14 <- table(GenDataD.2$vtrain,vD2_14)
sum(diag(tD2_14))/sum(tD2_14)
tD2_14

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


auxD2_14 <- eCmn(GenDataD.2$Xtest,parD2_14)
vD2_14t <- ifelse(auxD2_14$v<0.5,0,1)
tD2_14t <- table(GenDataD.2$vtest,vD2_14t)
tD2_14t
sum(diag(tD2_14t))/sum(tD2_14t)


auxD2_18 <- eCmn(GenDataD.2$Xtest,parD2_18)
vD2_18t <- ifelse(auxD2_18$v<0.5,0,1)
tD2_18t <- table(GenDataD.2$vtest,vD2_18t)
tD2_18t
sum(diag(tD2_18t))/sum(tD2_18t)

