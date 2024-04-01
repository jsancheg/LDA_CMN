
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



# E-step for EM algorithm for CMN different eta  ----------
e_step <- function(X, mu, sigma, alpha, eta)
{
  m <- nrow(X)
  p <- ncol (X)
  aux <- matrix(0.0,ncol = p, nrow = m)
  aux1 <- matrix(0.0,ncol = p, nrow = m)
  v <- matrix(0.0,ncol = 1, nrow = m)
  
  
  
  sigma1 <- sigma*eta

  
  pdf_num <- sapply(1:m, function(i) {
              alpha*(dnorm(X[i,1], mu[1],sigma[1] ) *dnorm(X[i,2], mu[2],sigma[2] ) * dnorm(X[i,3], mu[3],sigma[3] ) )
  } )
  
  pdf_den <- sapply(1:m , function(i) 
  {
       (  alpha *(    dnorm(X[i,1], mu[1],sigma[1] )  + (1-alpha) * dnorm(X[i,1],mu[1],sigma1[1])   )  )* 
            (  alpha *(    dnorm(X[i,2], mu[2],sigma[2] )  + (1-alpha) * dnorm(X[i,2],mu[2],sigma1[2])   )  )*
                 (  alpha *(    dnorm(X[i,3], mu[3],sigma[3] )  + (1-alpha) * dnorm(X[i,3],mu[3],sigma1[3])   ) ) 
  })
  

      for(i in 1:m)
      {
        v[i] <- pdf_num[i]/pdf_den[i]
      }
      
    
  
  return(v)
}

# M-step for EM algorithm for CMN different eta ---------------------------------------------------------

m_step <- function(X,v, mu,sigma,alpha, eta)
{
  if(!is.vector(eta)) stop("Eta should be a vector of dimension p")
  m <- nrow(X)
  p <- ncol(X)
  
  alpha <- max(0.5,mean(v))

  den_mu <- sapply(1:p, function(j) {
        sum(v + (1-v)/(eta[j]^2))
  })

  num_mu <- sapply(1:p, function(j) {
    sum( X[,j] * (v + (1-v)/(eta[j]^2) )  )
  })
  

  mu <- sapply(1:p, function(j) { num_mu[j]/den_mu[j]
    })  
  
  num_sigma <- sapply(1:p, function(j) {
    sum( (  (X[,j] - mu[j])^2  )   * (v + (1-v)/(eta[j]^2)    ) )
  })
  

    sigma <- sapply(1:p, function(j) {
      num_sigma[j]/m
  })
  
  num_eta <- sapply(1:p, function(j) {
   sum ( (X[,j] - mu[j]) / sigma[j] )^2
  
    })
  
  den_eta <- sapply(1:p,function(j) {
    sum ( (1-v) )
  })

    eta <- sapply(1:p, function(j) {
            num_eta[j]/den_eta[j]
    })  
    
    return(list(mu = mu, sigma = sigma, alpha = alpha, eta = eta))
}  




# EM- algorithm for CMN withd different eta -------------------------------

em_algorithm <- function(X, max_iter = 100, tol = 1e-3)
{
  p <- ncol(X)
  m <- nrow(X)
  mu <- apply(X,2,mean)
  sigma <- apply(X,2, sd)
  alpha <- rep(0.9,1)
  eta <- rep(1.10,p)
  v <- sample(c(0,1),m,replace = TRUE,prob = c(0.2,0.8) )
  
  for(i in 1:max_iter)
  {
    # E-step
      v <- e_step(X, mu, sigma, alpha, eta)
      
      
      # M-step
      new_params <- m_step(X,v,mu,sigma,alpha,eta)
      new_mu <- new_params$mu
      new_sigma <- new_params$sigma
      new_alpha <- new_params$alpha
      new_eta <- new_params$eta
    
      if (max(abs(mu - new_mu)) < tol & max(abs(sigma - new_sigma)) < tol & 
          max(abs(alpha - new_alpha)) < tol & max(abs(eta - new_eta)) < tol)
        {
          break
      }
      
      # Update parameters
      mu <- new_mu
      sigma <- new_sigma
      alpha <- new_alpha
      eta <- new_eta
      
  }
  
      return(list(mu=mu, sigma = sigma, alpha = alpha, eta = eta))
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
nobs_b <- 20
ptraining <- 0.7
percentage_trainning <-round(ptraining*100)
m <- nobs_b *ptraining
pig_b <- 1
mu_b <- matrix(c(0,0,0), nrow = p_b, ncol = G_b, byrow = TRUE)
sg_b <- array(0,dim = c(p_b,p_b,G_b))
mu_b <- c(0,0,0)
sigma_b <- c(2,6,3)
alpha_b <- 0.70

# for function Sim_DF
# ---------------------------------------------------------------------------------------------------
# eta is passed as a vector when there is one group where each element represent the variance inflation factor of a variable
# eta is passed as a matrix with each column correspond to the diagonal elements of the variance-covariance of a group

eta_b <- c(1.5,6,2)
# ---------------------------------------------------------------------------------------------------

DatasetB <- Sim_DIF(as.matrix(mu_b),sg_b,pig_b,nobs_b,ptraining,alpha_b,eta_b)
#saveRDS(DatasetB,"DatasetB.RDS")
#DatasetB <- readRDS("DatasetB.RDS")

Xtrain <- DatasetB$Xtrain
Xtest <- DatasetB$Xtest
ltrain <- DatasetB$ltrain
ltest <- DatasetB$ltest
vtrain <- DatasetB$vtrain
vtest <- DatasetB$vtest


v_est_train <- e_step(DatasetB$Xtrain, mu = mu_b, 
                sigma = c(2,6,3),
                alpha = alpha_b,
                eta = eta_b)


table(vtrain,v_est_train)
  

v_est_test <- e_step(DatasetB$Xtest, mu = mu_b, 
       sigma = c(2,6,3),
       alpha = alpha_b,
       eta = eta_b)

table(vtest,v_est_test)


pred_v <- ifelse(v_est>0.5,1,0)
table(DatasetB$vtest,pred_v)


ms_BDif_0 <- em_algorithm(DatasetB$Xtrain,max_iter = 10,tol = 0.01)


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




modX12 <- ContaminatedMixt::CNmixt(as.matrix(DatasetB$Xtrain[,1:2]),G = 1, contamination = TRUE,label = DatasetB$ltrain)
modX13 <- ContaminatedMixt::CNmixt(as.matrix(DatasetB$Xtrain[,c(1,3) ]),G = 1, contamination = TRUE,label = DatasetB$ltrain)

ContaminatedMixt::getPar(modX12)
ContaminatedMixt::getPar(modX13)


# for function Sim_DF
# ---------------------------------------------------------------------------------------------------
# eta is passed as a vector when there is one group where each element represent the variance inflation factor of a variable
# eta is passed as a matrix with each column correspond to the diagonal elements of the variance-covariance of a group

# m-step

ms_BDif_0 <- em_algorithm(Xtrain,max_iter = 10,tol = 0.01)


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
                                  eta = as.vector(ms_BDif_0$eta ) )


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


es_BDifrealv1 <- e_step(X = Xtrain, mu = ms_BDifrealv$mu,
                                  sigma = ms_BDifrealv$sigma,
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


es_BDif <- e_step_dif_s_B_1(X = Xtrain, mu = fit_BDif$mu,
                                  sg = fit_BDif$sigma,
                                  alpha = fit_BDif$alpha,
                                  eta = as.vector(fit_BDif$eta))


table(es_BDif)

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

