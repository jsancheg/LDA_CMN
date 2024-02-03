library(mclust) # unmmap function
library(DIRECT) #
library(MLmetrics)
library(xtable)
source("CMNFunctionsV2.R")
source("DifIFunctions.R")



# Dataset A ---------------------------------------------------------------

G_a <- 1
p_a <- 3 # 3 variables
nobs_a <- 100
ptraining <- 0.75
pig_a <- 1
mu_a <- matrix(c(0,0,0), nrow = p_a, ncol = G_a, byrow = TRUE)
sigma_a <- array(0,dim = c(p_a,p_a,G_a))
sigma_a[,,1] <- diag(1,p_a)
alpha_a <- 0.8
eta_a <- 5
set.seed(123)
DatasetA <- SimGClasses(mu_a,sigma_a,pig_a,nobs_a,ptraining,alpha_a,eta_a)
saveRDS(DatasetA,"DatasetA.RDS")


# Dataset B ---------------------------------------------------------------

G_b <- 1
p_b <- 3 # 3 variables
nobs_b <- 100
ptraining <- 0.75
pig_b <- 1
mu_b <- matrix(c(0,0,0), nrow = p_b, ncol = G_b, byrow = TRUE)
sigma_b <- array(0,dim = c(p_b,p_b,G_b))
sigma_b[,,1] <- diag(1,p_a)
alpha_b <- 0.8
eta_b <- c(1,5,1)
DatasetB <- Sim_DIF(mu_b,sigma_b,pig_b,nobs_b,ptraining,alpha_b,eta_b)
saveRDS(DatasetB,"DatasetB.RDS")


# Dataset C ---------------------------------------------------------------


