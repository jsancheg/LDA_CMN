library(mclust) # unmmap function
library(DIRECT) # 
source("DifIFunctions.R")

# simulate a data set with variable inflation factors witin class

# 2 groups with different mean and equal variance
G <- 2
p <- 5 # 5 variables
nobs <- 100
ptraining <- 0.75
pig <- c(0.5,0.5)
# separating variables X2 and X4
mug <- matrix(c(0,0,0,3,0,0,0,3,0,0), nrow = p, ncol = 2, byrow = TRUE)
sigmag <- array(0,dim = c(p,p,2))
sigmag[,,1] <- diag(1,p)
sigmag[,,2] <- diag(1,p)
alphag <- c(0.8,0.9)
etag <- matrix(c(1,1,5,1,1,5,1,1,1,1), nrow = p, ncol = 2, byrow = TRUE)

# Generate a dataset with different variables inflation factor within groups
DIF_df <- Sim_DIF(mug,sigmag,pig,nobs,ptraining,alphag,etag)

Xtrain <- DIF_df$Xtrain
Xtest <- DIF_df$Xtest
ltrain <- DIF_df$ltrain
ltest <- DIF_df$ltest
vtrain <- DIF_df$vtrain
vtest <- DIF_df$vtest

# visualizing train data
pairs(Xtrain, col = c("red","blue")[ltrain], 
      pch = ifelse(vtrain==0,24,19), cex = 2)

#visualizing test data
pairs(Xtest, col = c("red","blue")[ltest], 
      pch = ifelse(vtrain==0,24,19), cex = 2)

# Initial parameters supervised learning
# known class labels
zig <- ltrain
# initial random variables for unknown contaminated samples
vig <- matrix(runif(G*nrow(Xtrain)), nrow = nrow(Xtrain),ncol = G)
library(ContaminatedMixt)

fit_EIF_10 <- CNmixt(X= Xtrain, G = G , contamination = TRUE,
                     alphamin = 0.5,model = "EEI", 
                     initialization = "mixt", label = ltrain, iter.max = 10)


fit_EIF <- CNmixt(X= Xtrain, G = G , contamination = TRUE,
                 alphamin = 0.5,model = "EEI", 
                 initialization = "mixt", label = ltrain, iter.max = 1)

fit_EIF$models[[1]]$Sigma

par <- list()
par$G <- G
par$pig <- apply(unmap(ltrain),2,sum)/length(ltrain)
par$mu <- fit_EIF$models[[1]]$mu
par$sigma <- fit_EIF$models[[1]]$Sigma
par$alpha <- fit_EIF$models[[1]]$alpha
par$eta <- fit_EIF$models[[1]]$eta
par$v <- vig
estep_par <- eCmn_DIF(Xtrain,ltrain,par)

loglikCMN_DIF(Xtrain,ltrain,par)

# initialization parameters
# mean, sigma, eta, and proportions
par_real <- list()
par_real$G <- G
par_real$pig <- apply(l,2,sum)/sum(l)
par_real$mu <-mug
par_real$sigma <- sigmag
par_real$alpha <- alphag
par_real$etag <- etag

#par_real$etag <- list(diag(sqrt(etag[,1]),p ) , diag(sqrt(etag[,2]),p))

estep0 <- eCmn_DIF(Xtrain,ltrain,par_real)
# loglikelihood


library(MLmetrics)
AUC(estep0$vhat,vtrain)
Accuracy(estep0$vhat,vtrain)
Sensitivity(estep0$vhat,vtrain)
Specificity(estep0$vhat,vtrain)
Recall(estep0$vhat,vtrain)
Precision(estep0$vhat,vtrain)
F1_Score(estep0$vhat,vtrain,positive = 0)
ConfusionMatrix(estep0$vhat,vtrain)
ConfusionDF(estep0$vhat,vtrain)

estep_test <-eCmn_DIF(Xtest,ltest,par_real) 
AUC(estep_test$vhat,vtest)
Accuracy(estep_test$vhat,vtest)
Sensitivity(estep_test$vhat,vtest)
Specificity(estep_test$vhat,vtest)
Recall(estep_test$vhat,vtest)
Precision(estep_test$vhat,vtest)
F1_Score(estep_test$vhat,vtest,positive = 0)
ConfusionMatrix(estep_test$vhat,vtest)
ConfusionDF(estep_test$vhat,vtest)

mstep0 <- mCmn_DIF(Xtrain,ltrain,par)
mstep0$mu
mstep0$sigma
mstep0$eta
mstep0$alpha
mstep0$pig

# test eCmn_dif function


estep0 <- eCmn_DIF(Xtrain,ltrain,par)

# estep0$z
# update v
par$v <- estep0$v

est_v <- apply(par$v,1,which.max) 
table(ltrain,est_v)


# dataset 1 ---------------------------------------------------------------

# separating variables X2 and X4
mug <- matrix(c(0,0,0,3,0,0,0,3,0,0), nrow = p, ncol = 2, byrow = TRUE)
sigmag <- array(0,dim = c(p,p,2))
sigmag[,,1] <- diag(1,p)
sigmag[,,2] <- diag(1,p)
alphag <- c(0.8,0.9)
etag <- matrix(c(1,1,5,1,1,5,1,1,1,1), nrow = p, ncol = 2, byrow = TRUE)

# Generate a dataset with different variables inflation factor within groups
DIF_df <- Sim_DIF(mug,sigmag,pig,nobs,ptraining,alphag,etag)

Xtrain <- DIF_df$Xtrain
Xtest <- DIF_df$Xtest
ltrain <- DIF_df$ltrain
ltest <- DIF_df$ltest
vtrain <- DIF_df$vtrain
vtest <- DIF_df$vtest


# initiate from start
par0 <- list()
par0$G <- G
par0$pig <- apply(l,2,sum)/sum(l)
par0$mu <- fit_EIF$models[[1]]$mu
par0$sigma <- fit_EIF$models[[1]]$Sigma
par0$alpha <- fit_EIF$models[[1]]$alpha
par0$eta <- t(replicate(5,fit_EIF$models[[1]]$eta))
par0$v <- matrix(runif(nrow(Xtrain)*G), ncol = G, nrow = nrow(Xtrain) )


mstep0 <- mCmn_DIF(Xtrain,ltrain,par0)

mstep0$mu
mstep0$sigma
mstep0$alpha
mstep0$eta
mstep0$pig

esteps <- vector("list",10)
msteps <- vector("list",10)

estep0 <- eCmn_DIF(Xtrain,ltrain,par0)
estep0$v

esteps[[1]] <- estep0
msteps[[1]] <- mstep0
par1 <- list()
iter<-2
for(iter in 2:10)
{
  
  # update parameters previous step
  # pig, mu, sigma, alpha, eta
  par1$G <- msteps[[iter-1]]$G
  par1$pig <- msteps[[iter-1]]$pig
  par1$mu <- msteps[[iter-1]]$mu
  par1$sigma <- msteps[[iter-1]]$sigma
  par1$alpha <- msteps[[iter-1]]$alpha
  par1$eta <- msteps[[iter-1]]$eta
  
  esteps[[iter]] <- eCmn_DIF(Xtrain,ltrain,par1)
  par1$v <- esteps[[iter]]$v  
  msteps[[iter]] <- mCmn_DIF(Xtrain,ltrain,par1)
}

msteps[[10]]$pig
msteps[[10]]$mu
msteps[[10]]$sigma
msteps[[10]]$alpha
msteps[[10]]$eta

esteps[[10]]$lhat
esteps[[10]]$vhat

par10<- list()
par10$G <- msteps[[10]]$G
par10$pig <- msteps[[10]]$pig
par10$mu <- msteps[[10]]$mu
par10$sigma <- msteps[[10]]$sigma
par10$alpha <- msteps[[10]]$alpha
par10$eta <- msteps[[10]]$eta

test_mod <- eCmn_DIF(Xtest,ltest,par10)

AUC(test_mod$vhat,vtest)
Accuracy(test_mod$vhat,vtest)
Sensitivity(test_mod$vhat,vtest)
Specificity(test_mod$vhat,vtest)
Recall(test_mod$vhat,vtest)
Precision(test_mod$vhat,vtest)
F1_Score(test_mod$vhat,vtest,positive = 0)
ConfusionMatrix(test_mod$vhat,vtest)
ConfusionDF(test_mod$vhat,vtest)


AUC(test_mod$lhat,ltest)
Accuracy(test_mod$lhat,ltest)
Sensitivity(test_mod$lhat,ltest)
Specificity(test_mod$lhat,ltest)
Recall(test_mod$lhat,ltest)
Precision(test_mod$lhat,ltest)
F1_Score(test_mod$lhat,ltest,positive = 0)
ConfusionMatrix(test_mod$lhat,ltest)
ConfusionDF(test_mod$lhat,ltest)


eta10g <-matrix(c(10,1,1,10,1,1,1,1,1,1), nrow = 5, ncol =2, byrow = TRUE)
eta10g
