
# write matex -------------------------------------------------------------


write_matex <- function(x) {
  begin <- "$$\\begin{bmatrix}"
  end <- "\\end{bmatrix}$$"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  writeLines(c(begin, X, end))
}

# write matex 2 -----------------------------------------------------------


write_matex2 <- function(x) {
  begin <- "\\begin{bmatrix}"
  end <- "\\end{bmatrix}"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  paste(c(begin, X, end), collapse = "")
}


# loglikelihood CMN -------------------------------------------------------


loglikCMN_DIF1<-function(X,labels, par)
{
  G <- length(unique(labels))
  
  if(!is.matrix(X)) X <- as.matrix(X)
  mu <- as.matrix(par$mu)
  p <- ncol(X)
  m <- nrow(X)
  v <- par$v
  
  if(is.null(par$sigma)) stop("The variance covariance matrix of dimension pxp is required 
                              in the parameters")
  if(is.null(par$eta)) stop("The  matrix whose columns contains the inflation factor for classes of dimension pxG is required 
                              in the parameters")
  if(is.null(par$mu)) stop("The  matrix mu whose columns contains the mean for classes of dimension pxG is required 
                              in the parameters")
  
  if(!is.matrix(par$mu)) stop("Parameter mu should be a matrix")
  if(!is.matrix(par$sigma)) stop("Parameter mu should be a matrix")
  if(!is.matrix(par$eta)) stop("Parameter eta should be a matrix")
  
  
  if(!all(dim(par$mu) == c(p,G))  ) stop("The matrix mu should be of dimension pXG, where the i^{th} column contains the inflation factor for the i^{th} class")
  
  if(!all(dim(par$sigma ) == c(p,p))  ) stop(" The parameter sigma should be an matrix of dimension pxp")
  if(!all(dim(par$eta) == c(p,G)) ) stop("The matrix eta should be of dimension pXG, where the i^{th} column contains the inflation factor for the i^{th} class")
  if(length(par$alpha) != G ) stop("The vector alpha should be of the length of the number of groups G")
  
  if(is.null(par$v) ) stop("The matrix of dimension mx2 with probabilities whether 
                           the sample ith is contaminated or non-contaminated is 
                            missed in the parameters")
#  sigma <- array(0.0,dim = c(p,p,G))
#  sg1 <- array(0.0,dim = c(p,p,G))
  
#  inv_sigma <- array(0.0, dim = c(p,p,G) )
  
  
  # labels: class labels
  
  sigma <- matrix(0.0,ncol = p, nrow = p)
  sg1 <- matrix(0.0, ncol = p, nrow = p)
  inv_sigma <- matrix(0.0, ncol = p , nrow= p)
  mu <- matrix(0.0, ncol = G, nrow = p)
  eta <- matrix(0.0, ncol = G, nrow = p)
  inv_eta <- matrix(0.0, ncol = G, nrow = p)
  alpha <- numeric(G)
  
  
  pig <- par$pig
  alpha <- par$alpha
  eta <- par$eta
  sigma <- par$sigma
  
  l <- unmap(labels)
  M <- matrix(0.0, nrow = m, ncol = G)
  term1 <- 0
  term2 <- 0
  term3 <- 0
  
#  v0 <- matrix(runif(m*G), nrow = m, ncol = G, byrow = TRUE)
  exit <- 0
  loglik <- c(0.0,0.0,0.0)   
  
  
  
    sg1  <-t( diag( as.vector(sqrt(eta)) , p ) ) %*% sigma %*% diag( as.vector( sqrt(eta) ), p )
  
      term1 <- log(pig[1])
      term2 <- as.vector(sapply(1:m, function(i) {
              v*log(alpha) + dmvnorm(t(X[i,]), mu , sigma , log = TRUE)
      } ) )
      
      term3 <- as.vector(sapply(1:m, function(i) {
        term3 <- (1-v)* (log(1 - alpha) + dmvnorm(t(X[i,]), mu, sg1, log = TRUE ))
      }))

      M <- (term1 + term2 + term3)   
      
      

  
  
  loglik <- sum(M)
  
  return(loglik)
}


# E-step for ECM algorithm in a CMN with Sigma and Eta diagonal  ----------

e_step_dif_s_B_1 <- function(X,  mu, sg, alpha, etav)
{
  if(!is.matrix(sg)) stop("Sigma should be a matrix")
  if(!is.vector(etav)) stop("Eta should be a vector")
  # supervised e-step of a contaminated mixture of Gaussians distributions
    p <- ncol(X)
    m <- nrow(X)
    v <- numeric(m)
    
    sigma <- matrix(sg, nrow = p, ncol = p)  
    eta_sqrt <- diag(sqrt(etav),p)
    sigma_cont  <-  eta_sqrt %*% sigma %*% eta_sqrt
  
    pdf_nocont <- sapply(c(1:m), function(i) dmvnorm(X[i,],mean = as.vector(mu),sigma = sigma))
    pdf_cont <- sapply(c(1:m),function(i) dmvnorm(X[i,],mean = as.vector(mu), sigma = sigma_cont ))
    
    v <- alpha*pdf_nocont/(alpha*pdf_nocont + (1-alpha)*pdf_nocont )
    
    return(v)
}


# M-step for ECM algorithm in a CMN with Sigma and Eta diagonal ---------------------------------------------------------


cm_step_dif_s_B_1 <- function(Xtrain,ltrain, v_0, mu_0, sigma_0, alpha_0, eta_0)
{
  # supervised CM- steps of a contaminated mixture of Gaussian distributions
  Xtrain <- as.matrix(Xtrain)
  m <- nrow(Xtrain)
  p <- ncol(Xtrain)
  G <- length(unique(ltrain))
  l <- unmap(ltrain)
  
  if(!is.matrix(mu_0)) stop("Parameter mu should be a matrix")
  if(!is.matrix(sigma_0)) stop("Parameter mu should be a matrix")
  if(!is.vector(eta_0)) stop("When there is only 1 group, eta should be a vector")
  
  
#  if (is.null(v_0)) v_0 <- matrix(runif(m), ncol = G, nrow = p, byrow = TRUE) else v_0 <- v_0
  
  if(is.null(sigma_0)) stop("The variance covariance matrix of dimension pxp is required 
                              in the parameters")
  if(is.null(eta_0)) stop("The  matrix whose columns contains the inflation factor for classes of dimension pxG is required 
                              in the parameters")
  if(is.null(mu_0)) stop("The  matrix mu whose columns contains the mean for classes of dimension pxG is required 
                              in the parameters")
  
  if(!all(dim(mu_0) == c(p,G))  ) stop("The matrix mu should be of dimension pXG, where the i^{th} column contains the inflation factor for the i^{th} class")
  #  if(length(dim(par$sigma)) == 3)   if(!all(dim(par$sigma ) == c(p,p,G))  ) stop(" The parameter sigma should be an array of dimension pxpxG")
  if(!all(dim(sigma_0 ) == c(p,p))  ) stop(" The parameter sigma should be a matrix of dimension pxpxG")
  if(!all(dim(eta_0) == c(p,G)) ) stop("The matrix eta should be of dimension pXG, where the i^{th} column contains the inflation factor for the i^{th} class")
  if(length(alpha_0) != G ) stop("The vector alpha should be of the length of the number of groups G")

  mu_1 <- matrix(0.0, ncol = G, nrow = p)
  sigma_1 <- matrix(0.0,nrow = p, ncol = p)
  inv_sigma_1 <- matrix(0.0,nrow = p, ncol = p)
  eta_aux <- matrix(0.0, ncol = G, nrow = p)
  eta_1 <- matrix(0.0, ncol = G, nrow = p)
  alpha_1 <- numeric(G)
  inv_eta_1 <- matrix(0.0, ncol = G, nrow = p)
  
    
  s <- vector("list",m)
  sx <- vector("list",m)
  term1_s <- vector("list",m)
  term2_s <- vector("list",m)
  term1_e <- vector("list",m)

  sum_terms_s <- vector("list",m)

  sum_s <- matrix(0.0, ncol = p, nrow = p)
  sum_sx <- matrix(0.0, ncol = G, nrow = p)
  sum_sigma <- matrix(0.0, ncol = p, nrow = p)
  sum_eta <- matrix(0.0, ncol = p, nrow = p)
  
  
# CM1-step
  alpha_1 <- sum(v_0)/m
  
  for(i in 1:m)
  {
    s[[i]] <- ( v_0[i]*diag(1,p) + (1-v_0[i]) * solve(diag(eta_0,p))   )
    sx[[i]] <- s[[i]] %*% Xtrain[i,]
    sum_s <- sum_s + s[[i]]
    sum_sx <- sum_sx + sx[[i]]
    term1_s[[i]] <- v_0[i] * (Xtrain[i,] %*% t(Xtrain[i,]) )
    term2_s[[i]] <- (1-v_0[i])* ( solve(diag(eta_0,p)) %*% Xtrain[i,] %*%  t(Xtrain[i,] ) %*% solve(diag(eta_0,p))  ) 
    
    sum_terms_s[[i]] <- term1_s[[i]] + term2_s[[i]] 
    
    sum_sigma <- sum_sigma + sum_terms_s[[i]]
  }

  mu_1 <- mu_1 + (   solve( sum_s ) %*% sum_sx ) 
  sigma_1 <- diag(sum_sigma)/ ( ( det(sum_sigma) )^(1/p) )
  sigma_1

# CM2-step    
  for(i in 1:m)
  {
    term1_e[[i]] <- (1-v_0[i]) * (Xtrain[i,]   %*% t(Xtrain[i,]) )  
    sum_eta <- sum_eta + term1_e[[i]] 
    
  }

# ECM algotithm for a mixt of contaminated Gaussian for 1 group allowing di --------
# allowing different variable inflation factor within group
  
  eta_aux <- (diag(sum_eta) *  diag( solve( diag(sigma_1,p ) ) ) ) / ((  det(sum_eta) )^(1/p)   )
  for (j in 1:p) eta_1[j] <- max(1,eta_aux[j])
  eta_1
  inv_sigma <- solve(diag(sigma_1,p))
  
  output <- list(mu = mu_1, sigma = diag(sigma_1,p),
                 inv_sigma = inv_sigma,
                 alpha = alpha_1,
                 eta = eta_1 )
  
  return(output)
  
}


cem_dif_s_B_1 <- function(X, max_iter = 100, tol = 1e-3)
{
  p <- ncol(X)
  mu <- apply(X,2,mean)
  sigma <- diag(apply(X,2,var),p)
  alpha <- runif(1,min = 0.5,max = 1)
  eta <- runif(p,min=1,max = 1000)
  
  for( iter in 1:max_iter)
  {
    # E-step
    pdf <- e_step_dif_s_B_1(X,mu,sigma,alpha,eta)
    sum(pdf)
    # CM-steps
    new_params <- cm_step_dif_s_B_1(X,l,pdf,as.matrix(mu),sigma,alpha,eta)
    new_mu <- new_params$mu
    new_sigma <- new_params$sigma
    new_alpha <- new_params$alpha
    new_eta <- new_params$eta
    
    if(max(abs(mu-new_mu)) <tol & max(abs(sigma-new_sigma)) <tol & 
       max(abs(alpha-new_alpha)) <tol &  max(abs(eta-new_eta))  )
      break
    
    # update parameters
      mu <- new_mu
      sigma <- new_sigma
      alpha <- new_alpha
      eta <- new_eta
    
  }
  
      return(list(mu = mu, sigma = sigma, alpha = alpha, eta = eta))
}






# Simulation DIF ----------------------------------------------------------


Sim_DIF <- function(mug,sigmag,pig,nobs,ptraining,alphag,etag)
{
  # Function: Sim_DIF.
  # Description: This function generates datasets that allow.
  # different variable inflation factors within classes.
  # Parameters
  # mu: parameter that receives a matrix of dimension p rows and G columns
  #      that contains the mean of each class in each column.
  # sigmag: parameter that receives an array where each element of its 3rd dimension 
  #         is the covariance matrix of a group.
  # pig: parameter that receives a vector containing the proportion of observations for each
  #       group.
  # ptraining: parameter that receives a variable with the percentage of observations 
  #             that will be use in training.
  # nobs: parameter that receives a variable with the total number of observations. 
  # alphag: parameter that receives the percentage of contaminated observations for each class.
  # etag:   parameter that receives a vector when it is only 1 group and a 
  #         matrix when there are more than 1 group
  #        where each column corresponds to the elements of 
  #        the diagonal of the inflation factors for each group.
  
  output <- list()
  G <- length(pig)
  if(is.matrix(mug)) p<-nrow(mug) else p <- length(mug)
  X <- matrix(0.0, ncol = p , nrow = nobs)
  #  v <- matrix(-1,nrow = nobs)
  # 1: good observation
  # 0: contaminated observation
  #Validate parameters
  if(sum(pig)!=1) stop("proportions do not sum 1")
  if(any(pig<0) | any(pig>1)) stop("alpha is not a probability")
  if(any(ptraining<0) | any(ptraining>1)) stop("ptraining is not a probability")
  if(any(alphag<0) | any(alphag>1)) stop("alpha takes values in the interval (0,1)")
  if(any(etag < 1))stop("eta has to be greater than 1")  
  if(!is.matrix(etag)) etag <- as.matrix(etag)
  aux <- (rmultinom(nobs,1,pig))
  l <- apply(aux,2,which.max)
  v <- rep(0,nobs)
  
  # initialize sigmag
  if(p == 1 & is.vector(sigmag) & length(sigmag)==1)
  {
    sg <- 0
  }else if(p == 1 & is.vector(sigmag) & length(sigmag) > 1)
  {
    sg <- rep(0,length(sigmag))
  }else if(p > 1 & is.matrix(sigmag))
  {
    sg <- matrix(0,ncol = ncol(sigmag), nrow = nrow(sigmag)) 
  } else if(p >1 & is.array(sigmag))
  {
      sg <- array(0,dim = dim(sigmag))
      for(g in 1:G)
      {
        sg[,,g] <- t(diag(sqrt(etag[,g]),p))  %*% sigmag[,,g] %*% diag(sqrt(etag[,g]),p)
      }
      
    
    
  }
  
  if(p == 1 & is.vector(sigmag) ) # only 1 column in X but different groups
  {
    sg <- etag * sigmag # sg could be a vector of length 1 if there is only one group 
    # of dimension G if there are G groups
  }else if(p > 1 & length(dim(sigmag)) == 2) # equal variance across groups
  {
    sg <- t(diag(sqrt(etag),p)) %*% sigmag %*% diag(sqrt(etag),p)
  } else if(p > 1 & length(dim(sigmag)) == 3 & G>1) 
  {
    for(g in 1:G)
    {
      sg[,,g] <- t(diag(sqrt(etag[,g]),p))  %*% sigmag[,,g] %*% diag(sqrt(etag[,g]),p)
    }
  } else if(length(dim(sigmag)) > 3) stop("Sigma is an array of dimension 4")
  
  mg <- apply(unmap(l),2,sum)
  
  for(i in 1:nobs)
    v[i] <- as.numeric(rbinom(1,1,alphag[l[i]]))
  
  
  # Case where X contains only 1 variable
  if(p == 1 & is.vector(sigmag) ) 
  {
    for (i in 1:nobs)
    {
      if(v[i] == 1) 
        # non-contaminated sample
      {  
        if(G > 1) # if there is more than 1 group 
        {
          # groups with different mean and equal variance
          X[i,] <- rmnorm(1,mug[l[i]],sigmag) 
        } else if(G > 1 & length(mug)== 1)  
        { # groups with same mean and different variance
          X[i,] <- rmnorm(1,mug,sigmag[l[i]])
        } else if(G == 1) X[i,] <- rmnorm(1,mug,sigmag) # 1 group same mean and variance   
      }else if(v[i]==0)
        # contaminated sample
      {  
        if(is.matrix(mug) & G > 1 ) # if there is more than 1 group
        {
          # groups with different mean and equal contaminated variance
          X[i,] <- rmnorm(1,mug[,l[i]],sg)
        }else if (G>1 & length(mug)== 1) # more than 1 group with same mean
        { # groups with same mean and different variance
          X[i,] <- rmnorm(1,mug,sg[l[i]])
        }else if (G==1) X[i,] <- rmnorm(1,mug,sg) # 1 group
      } # end if-else v  
      
    } # end for
  } # end if 
  
  if(p > 1 & length(dim(sigmag)) == 2){
    #      Same covariance matrix for all groups
    for (i in 1:nobs)
    {
      if(v[i] == 1)
      {  
        if(is.matrix(mug))  
          X[i,] <- rMVNorm(1,mug[,l[i]],sigmag)  # different mean and same variance 
        else  X[i,] <- rMVNorm(1,mug,sigmag) # same mean and variance 
        
      }else if(v[i]==0)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg) # different mean and same variance
        else  X[i,] <- rMVNorm(1,mug,sg) # same mean and variance
      } # end if-else v  
      
    } # end for
    
  }else if(p >1 & length(dim(sg)) == 3) # different covariance matrix for groups
  {
    
    for (i in 1:nobs)
      if(v[i] == 1)
      {  
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sigmag[,,l[i]])
        else
          X[i,] <- rMVNorm(1,mug,sigmag[,,l[i]])
      }else if(v[i] == 0)
      {
        if(is.matrix(mug))
          X[i,] <- rMVNorm(1,mug[,l[i]],sg[,,l[i]])
        else  X[i,] <- rMVNorm(1,mug,sg[,,l[i]])
        
      }
  } #End-if
  
  colnames(X) <- paste("X",1:p,sep = "")
  ind <- sample(1:nobs, round(nobs* ptraining))
  Xtrain <- X[ind,]
  Xtest <- X[-ind,]
  ltrain <- l[ind]
  ltest <- l[-ind]
  vtrain <- v[ind]
  vtest <- v[-ind]
  
  output <- list(X = X,l =l, ind = ind,Xtrain = Xtrain,
                 Xtest = Xtest,ltrain = ltrain,ltest = ltest,
                 v = v, vtrain = vtrain, vtest = vtest)
  return(output)
  
}




# DatasetB
G_b <- 1
p_b <- 3 # 3 variables
nobs_b <- 3000
ptraining <- 0.5
percentage_trainning <-round(ptraining*100)
m <- nobs_b *ptraining
pig_b <- 1
mu_b <- matrix(c(0,0,0), nrow = p_b, ncol = G_b, byrow = TRUE)
sigma_b <- array(0,dim = c(p_b,p_b,G_b))
sigma_b[,,1] <- diag(c(2,6,3),p_b)
alpha_b <- 0.70
# for function Sim_DF
# ---------------------------------------------------------------------------------------------------
# eta is passed as a vector when there is one group where each element represent the variance inflation factor of a variable
# eta is passed as a matrix with each column correspond to the diagonal elements of the variance-covariance of a group

eta_b <- c(1,6,2)
# ---------------------------------------------------------------------------------------------------

DatasetB <- Sim_DIF(mu_b,sigma_b,pig_b,nobs_b,ptraining,alpha_b,eta_b)
#saveRDS(DatasetB,"DatasetB.RDS")
#DatasetB <- readRDS("DatasetB.RDS")


# Obtaining initial values for parameters
mu_0 <- as.matrix(apply(DatasetB$Xtrain,2,mean))
mu_0


apply(DatasetB$Xtrain[DatasetB$vtrain==1,],2,var)
sigma_0 <- diag( apply(DatasetB$Xtrain[DatasetB$vtrain==0,],2,var), p_b)

var(DatasetB$Xtrain)


table(DatasetB$vtrain)
prop.table(table(DatasetB$vtrain))

sum(DatasetB$X[DatasetB$vtrain == 1,])

sigma_b

v_0 <- runif(m)

sapply(1:p_b,function(i) { sum( (1-v_0) *(DatasetB$Xtrain[,i]-mu_0[i])^2 )/sum(1-v_0)  })



eta_b
eta_0 <- sqrt(apply(DatasetB$Xtrain[DatasetB$vtrain == 0,],2,var))

alpha_0 <- runif(1,0.7,0.99)
#  Running the M-step when the labels are the true and estimate the parameters


Xtrain <- DatasetB$Xtrain
ltrain <- DatasetB$ltrain
vtrain <- DatasetB$vtrain

# for function Sim_DF
# ---------------------------------------------------------------------------------------------------
# eta is passed as a vector when there is one group where each element represent the variance inflation factor of a variable
# eta is passed as a matrix with each column correspond to the diagonal elements of the variance-covariance of a group

# m-step
ms_BDif_0 <- cm_step_dif_s_B_1(X = Xtrain, l = ltrain, 
                                  v_0 = runif(m), mu_0 = as.matrix(mu_0), 
                                  sigma_0 = sigma_0, 
                                  alpha_0  = alpha_0,eta_0 = eta_0)


ms_BDif_0$mu
ms_BDif_0$sigma
ms_BDif_0$alpha
ms_BDif_0$eta

# e-step
es_BDifrealv1 <- e_step_dif_s_B_1(X = Xtrain, mu = ms_BDif_0$mu,
                                  sg = ms_BDif_0$sigma,
                                  alpha = ms_BDif_0$alpha,
                                  eta = ms_BDif_0$eta )


pred_BDif_train <-  ifelse(es_BDifrealv1>0.5,1,0)
table(pred_BDif_train)

# real parameters value
ms_BDifrealv <- cm_step_dif_s_B_1(X = Xtrain, l = ltrain, 
                                  v_0 = vtrain, mu_0 = mu_b, 
                                  sigma_0 =  matrix(as.vector(sigma_b),ncol = p_b, nrow = p_b, byrow = TRUE ), 
                                  alpha_0  =  alpha_b,eta_0 = eta_b)

round(ms_BDifrealv$mu,2)
round(ms_BDifrealv$sigma,2)
round(ms_BDifrealv$alpha,2)
round(ms_BDifrealv$eta,2)




mu_b
sigma_b
alpha_b
eta_b


es_BDifrealv1 <- e_step_dif_s_B_1(X = DatasetB$Xtrain, mu = ms_BDifrealv$mu,
                                  sg = ms_BDifrealv$sigma,
                                  alpha = ms_BDifrealv$alpha,
                                  eta = as.vector(ms_BDifrealv$eta))


pred_BDifreal_train <- ifelse(es_BDifrealv1>0.5,1,0)

table(pred_BDifreal_train)

Factpred_BDifTrainv <- factor(pred_BDifreal_train, levels = c("0","1"),
                              labels=c("Cont","Non-cont"))
Fact_BTrainv <- factor(vtrain, levels = c("0","1"),
                       labels = c("Cont","Non-cont"))

CM_BDifTrainv <- ConfusionMatrix(Factpred_BDifTrainv,Fact_BTrainv)

CompBDifTrainv <- cbind(True = DatasetB$vtrain, Pred = pred_BDifTrainv)


#loglikCMN_DIF1(X = DatasetB$Xtrain,labels = DatasetB$ltrain,par = par_02)  



fit_BDif <- cem_dif_s_B_1(X = DatasetB$Xtrain, max_iter = 3, tol = 0.01)


fit_BDif$mu
fit_BDif$sigma
fit_BDif$alpha
fit_BDif$eta



predBDifTrainv <- fit_BDif$v

predBDifTestv <- eCmn_DIF(X = DatasetB$Xtest,labels = DatasetB$ltest, par = par_BDif)


pred_BDifTrainv <- ifelse(predBDifTrainv>0.5,1,0)
Factpred_BDifTrainv <- factor(pred_BDifTrainv, levels = c("0","1"),
                              labels=c("Cont","Non-cont"))
Fact_BTrainv <- factor(DatasetB$vtrain, levels = c("0","1"),
                       labels = c("Cont","Non-cont"))

CM_BDifTrainv <- ConfusionMatrix(Factpred_BDifTrainv,Fact_BTrainv)

CompBDifTrainv <- cbind(True = DatasetB$vtrain, Pred = pred_BDifTrainv)


pred_BDifTestv <- ifelse(predBDifTestv$vhat>0.5,1,0)
Factpred_BDifTestv <- factor(pred_BDifTestv, levels = c("0","1"),
                             labels=c("Cont","Non-cont"))
Fact_BTestv <- factor(DatasetB$vtest, levels = c("0","1"),
                      labels = c("Cont","Non-cont"))

CM_BDifTestv <- ConfusionMatrix(Factpred_BDifTestv,Fact_BTestv)

CompBDifTestv <- cbind(True = DatasetB$vtest, Pred = pred_BDifTestv)

# condition 1 True contaminated samples

condTrueBDif_0Trainv <- CompBDifTrainv[,1] == 0
condPredBDif_1Trainv <- CompBDifTrainv[,2] == 1
condPredBDif_0Trainv <- CompBDifTrainv[,2] == 0

condBDif_01Trainv <- condTrueBDif_0Trainv & condPredBDif_1Trainv
condBDif_00Trainv <- condTrueBDif_0Trainv & condPredBDif_0Trainv

indBDif_T0_P1_Trainv <- which(condBDif_01Trainv == TRUE)
indBDif_T0_P0_Trainv <- which(condBDif_00Trainv == TRUE)

CompBDifTrainv[indBDif_T0_P1_Trainv,]
cond1BDif_trainv <- paste0(DatasetB$vtrain,pred_BDifTrainv)


condTrueBDif_0Testv <- CompBDifTestv[,1] == 0
condPredBDif_1Testv <- CompBDifTestv[,2] == 1
condPredBDif_0Testv <- CompBDifTestv[,2] == 0

condBDif_01Testv <- condTrueBDif_0Testv & condPredBDif_1Testv
condBDif_00Testv <- condTrueBDif_0Testv & condPredBDif_0Testv

indBDif_T0_P1_Testv <- which(condBDif_01Testv == TRUE)
indBDif_T0_P0_Testv <- which(condBDif_00Testv == TRUE)
CompBDifTestv[indBDif_T0_P1_Testv,]
cond1BDif_testv <- paste0(DatasetB$vtest,pred_BDifTestv)
table(cond1BDif_testv)

# round estimated parameters  
mu_BDifr <- round(fit_BDif$mu,2)
sigma_BDif <- fit_BDif$sigma
sigma_BDifr<- round(matrix(as.vector(fit_BDif$sigma),ncol = p_b,nrow= p_b,byrow = TRUE ),2 )
alpha_BDifr <- round(fit_BDif$alpha,2)
eta_BDifr <- round(fit_BDif$eta,2)


pred_BDifTrainv

Accuracy_BDifTrain <- Accuracy(pred_BDifTrainv,DatasetB$vtrain)
Sensitivity_BDifTrain <- Sensitivity(DatasetB$vtrain, pred_BDifTrainv,positive = 0)
Specificity_BDifTrain <- Specificity(DatasetB$vtrain,pred_BDifTrainv,positive = 0) 
Recall_BDifTrain <- Recall(DatasetB$vtrain, pred_BDifTrainv,positive = 0) 
Precision_BDifTrain <- Precision(DatasetB$vtrain,pred_BDifTrainv,positive = 0) 
ConfusionMatrix_BDifTrain <- ConfusionMatrix(pred_BDifTrainv,DatasetB$vtrain)
ConfusionDF_BDifTrain <- ConfusionDF(pred_BDifTrainv,DatasetB$vtrain)
F1_Score_BDifTrain <- F1_Score(DatasetB$vtrain,pred_BDifTrainv,positive = 0)

pred_BDifTest <-predBDifTestv 

Accuracy_BDifTest <- Accuracy(pred_BDifTest$vhat,DatasetB$vtest)
Sensitivity_BDifTest <- Sensitivity(DatasetB$vtest, pred_BDifTest$vhat,positive = 0)
Specificity_BDifTest <- Specificity(DatasetB$vtest,pred_BDifTest$vhat,positive = 0) 
Recall_BDifTest <- Recall(DatasetB$vtest, pred_BDifTest$vhat,positive = 0) 
Precision_BDifTest <- Precision(DatasetB$vtest,pred_BDifTest$vhat,positive = 0) 
ConfusionMatrix_BDifTest <- ConfusionMatrix(pred_BDifTest$vhat,DatasetB$vtest)
ConfusionDF_BDifTest <- ConfusionDF(pred_BDifTest$vhat,DatasetB$vtest)
F1_Score_BDifTest <- F1_Score(DatasetB$vtest,pred_BDifTest$vhat,positive = 0)


CM_BDifTrainv
CM_BDifTestv





df_BDifTrain <-  data.frame(
  Metric = c("Accuracy","Precision","Recall","Sensitivity","Specificity",
             "F1 score"),
  Different_IF = c(round(Accuracy_BDifTrain,2),round(Precision_BDifTrain,2),
                   round(Recall_BDifTrain,2),round(Sensitivity_BDifTrain,2),
                   round(Specificity_BDifTrain,2),
                   round(F1_Score_BDifTrain,2))
)



df_BDifTest <- data.frame(
  Metric = c("Accuracy","Precision","Recall","Sensitivity","Specificity",
             "F1 score"),
  Different_IF = c(round(Accuracy_BDifTest,2),round(Precision_BDifTest,2),
                   round(Recall_BDifTest,2),round(Sensitivity_BDifTest,2),
                   round(Specificity_BDifTest,2),
                   round(F1_Score_BDifTest,2))
)


df_BDifTrain
df_BDifTest

table(DatasetB$vtrain)
table(DatasetB$vtest)

table(DatasetB$vtrain, pred_BDifTrainv)

table(DatasetB$vtest, pred_BDifTestv)

