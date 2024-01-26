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
etag <- matrix(c(1,1,5,1,1,5,1,1,1,1), nrow = p, ncol = 2, byrow = TRUE)

# Generate a dataset with different variables inflation factor within groups
DIF_df <- Sim_DIF(mug,sigmag,pig,nobs,ptraining,alphag,etag)

Xtrain <- DIF_df$Xtrain
Xtest <- DIF_df$Xtest
ltrain <- DIF_df$ltrain
ltest <- DIF_df$ltest

# visualizing train data
pairs(Xtrain, col = c("red","blue")[ltrain], 
      pch = ifelse(vtrain==0,24,19), cex = 2)

#visualizing test data
pairs(Xtest, col = c("red","blue")[ltest], 
      pch = ifelse(vtrain==0,24,19), cex = 2)

eta10g <-matrix(c(10,1,1,10,1,1,1,1,1,1), nrow = 5, ncol =2, byrow = TRUE)
eta10g
