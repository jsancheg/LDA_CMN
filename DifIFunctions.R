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
      }

      if(p == 1 & is.vector(sigmag) ) # only 1 column in X but different groups
      {
          sg <- etag * sigmag # sg could be a vector of length 1 if there is only one group 
                              # of dimension G if there are G groups
      }else if(p > 1 & length(dim(sigmag)) == 2) # equal variance across groups
        {
           sg <- t(diag(sqrt(etag),p)) %*% sigmag %*% diag(sqrt(etag),p)
      } else if(p > 1 & length(dim(sigmag)) == 3) 
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

loglikCMN_DIF<-function(X,labels, par)
{
  # labels: class labels
  if(!is.matrix(X)) X <- as.matrix(X)
  mu <- as.matrix(par$mu)
  p <- ncol(X)
  
  G <- length(unique(labels))
  pig <- par$pig
  alpha <- par$alpha
  eta <- par$eta
  if(is.null(par$v) ) stop("The matrix of dimension mx2 with probabilities whether 
                           the sample ith is contaminated or non-contaminated is 
                            missed in the parameters")
  v <- par$v
  m <- nrow(X)

  l <- unmap(labels)
  M <- matrix(0.0, nrow = m, ncol = G)
  term1 <- 0
  term2 <- 0
  term3 <- 0

#  initializing sg1
  if(p == 1)
  {
    sg <- rep(0.0,G)  
    sg1 <- rep(0.0,G)
  }else if(p>1 & G >= 1 & is.matrix(par$sigma) & length(dim(par$sigma))==2)
  {
      sg <- par$sigma
      sg1 <- matrix(0.0,ncol = ncol(sg), nrow = nrow(sg))
      
  } else if ( p>1 & G >= 1 & is.array(par$sigma) & length(dim(par$sigma))==3)
  {
    sg <- par$sigma
    sg1 <- array(0.0,dim = c(p,p,G))
    for(i_g in 1:G) {sg[,,i_g] <- par$sigma[,,i_g]}
  }else if (is.list(par$sigma))
  {
    if(length(par$sigma) != G) stop("The covariance matrix has been pass as a list but it does not have the list does not contain 
                                    the g covariance matrices where g is the number of groups ")
    sg <- array(0.0, dim = c(p,p,G))
    sg1 <- array(0.0,dim = c(p,p,G))
    for(i_g in 1:G) sg[,,i_g] <- par$sigma[[i_g]]
  }

 
   
#  # case X only is composed of 1 variable
#    compute  sigma1 
   if (p == 1 )
  {
      if(  G == 1 & length(par$sigma) == 1 ) # X contains 1 variable and 1 group
      {# one group 
        # variance of non contaminated samples
        if(is.vector(par$eta) )
        {
          if(length(par$eta)== 1)
            {
              sg <- as.vector(par$sigma)
              eta <- par$eta
              sg1 <- eta * sigmag
            } else if (length(par$eta) > 1)
              stop("The dimension of eta is higher than 1 and there is only 1 group.")
        } else if(is.matrix(par$eta))
          stop("The dimension of eta is higher than number of groups that is 1.")
      }else if( G > 1 & length(par$sigma) == 1) # X contains 1 variable and more than 1 group
                                                # with same variance for all groups
      { 
          # variance of non-contaminated samples
        if(is.vector(par$eta) & length(par$eta) == 1)
        {# eta is dimension 1 when X contains 1 variable and 
          # there are more than 1 group
          # replicate eta and sigma for contaminated and non-contaminated samples
          sg <- rep(par$sigma,G)
          eta <- par$eta
          sg1 <- rep(eta*sg,G)
        }else if(is.vector(par$eta) & length(par$eta) == G)
        { # eta is dimension G when X contains 1 variable and 
          # there are more than 1 group replicate eta and sigma for non-contaminated samples
          sg <- rep(par$sigma,G)
          eta <- par$eta
          sg1 <- eta*sg
        } else if (is.vector(par$eta) & length(par$eta)!= G & length(par$eta)!=1) 
          stop("The dimension of eta is not equal to the number of groups")
      }else if(G > 1 & length(par$sigma) == G)
      { # X contains 1 variable but there are more than 1 group with different variance
        # variance of non-contaminated samples
        if(is.vector(par$eta) & length(par$eta) == 1)
        { # # eta is dimension 1 when X contains 1 variable and 
          # there are G groups
          # replicate eta and sigma for contaminated and non-contaminated samples
          sg <- as.vector(par$sigma)
          eta <- rep(par$eta,G)
          sg1 <- eta * sg
        } else if(is.vector(par$eta) & length(par$eta) == G)
        { # eta is dimension G when X contains 1 variable and 
          # there are more than 1 group replicate eta and sigma for non-contaminated samples
          sg <- rep(par$sigma,G)
          eta <- par$eta
          sg1 <- eta*sg
        } else if (is.vector(par$eta) & (length(par$eta)!= G & length(par$eta)!=1) ) 
          stop("The dimension of eta is not equal to the number of groups")

    } # end different groups with its own variance for X containing 1 variable
  } # end if (p == 1)
  
  if (p > 1) # more than 1 variable
  {
    if(G == 1 & length(dim(par$sigma))==2) # 1 group with its covariance matrix
    {
      if(is.array(par$eta)){
        eta <- par$eta[,,1]
        sg1  <-t(eta) %*% sg %*% eta
      }else if(is.vector(par$eta) & length(par$eta) == 1)
      { # same variable inflation factor within the group
#        sg <- par$sigma
        eta <- par$eta
        sg1 <- eta * sg
      }else if (is.vector(par$eta) & length(par$eta) == p)
      {# different variables inflation factors within the group
 #       sg <- par$sigma
        eta <- par$eta
        sg1 <- t(diag(sqrt(par$eta),p)) %*% sg %*% diag(sqrt(par$eta),p)
      }else if (is.vector(par$eta) & (length(par$eta) != G & length(par$eta) != p ) )
        stop("The dimension of eta does not correspond to the number of variables")
    } else if(G > 1 & length(dim(par$sigma))==2) # same covariance matrix for all groups
      {     
        if(is.vector(par$eta) & length(par$eta) == 1)
          { # same variable inflation factor for all groups
   #           sg <- par$sigma
              eta <- par$eta
              sg1 <- eta * sg
          }
        if(is.vector(par$eta) & length(par$eta) == G)
          { # same variable inflation factor within group with
            # each group having different inflation factors
    #          sg <- par$sigma
              eta <-par$eta
#              sg1 <- array(0.0,dim = c(p,p,G))
              for(i_g in 1:G)   sg1 [,,i_g] <- eta[i_g]*sg
          }
        if(is.vector(par$eta) & length(par$eta) == p)
          {# different variables inflation factor within group with
           # all groups having the same variables inflation factor
          
     #         sg <- par$sigma
              eta <- diag(sqrt(par$eta),p)
 #             sg1 <- array(0.0,dim = c(p,p,G))
              for (i_g in 1:G) sg1[,,i_g] <- t(eta) %*% sg %*% eta 
            } 
        if(is.matrix(par$eta) & ncol(par$eta == 1 ) & nrow(par$eta) == p)
        {# different variables inflation factor within group with
         # all groups having the same variables inflation factor
      #      sg <- par$sigma
            eta <- diag(sqrt(as.vector(par$eta)),p)
            sg1 <- t(eta) %*% sg %*% eta
        }
        if(is.matrix(par$eta) & ncol(par$eta) == G & nrow(par$eta) == p )
            { # different variables inflation factor within group with
              # all groups having different variables inflation factor
      #        sg <- par$sigma
              eta <- array(0.0, dim = c(p,p,G) )
  #            sg1 <- array(0.0, dim = c(p,p,G))
              for (i_g in 1:G)
                {
                  eta[,,i_g] <- diag(sqrt(par$eta[i_g]),p)
                  sg1 [,,i_g] <-t(eta[,,i_g]) %*% sg %*% eta[,,i_g]
                }
            }  
          if(is.vector(par$eta) & length(par$eta)!=1 & 
             length(par$eta)!=p & length(par$eta) != G )
                stop("The dimension of eta does not correspond with either the number of variables or number of groups")
          if(is.matrix(par$eta) & ncol(par$eta)!= G &  nrow(par$eta)!=p &
             ncol(par$eta)!=1  & ncol(par$eta)!=G )
              stop("The dimension of eta does not correspond with either the number of variables or number of groups")
        }  else if(G >= 1 & length(dim(par$sigma))==3)
        {
            if(is.vector(par$eta) & length(par$eta) == 1)
            { # same variable inflation factor for all groups
              # different covariance matrix for each group
  #            sg <- par$sigma
              eta <- diag(sqrt(par$eta),p)
              for (i_g in 1:G) sg1[,,i_g] <- t(eta) %*% sg[,,i_g] %*% eta 
            }
            if(is.vector(par$eta) & length(par$eta) == G)
            { # same variable inflation factor within group with
            # each group having different inflation factors
 #             sg <- par$sigma
              eta <- array(0.0, dim = c(p,p,G))
  #            sg1 <- array(0.0,dim = c(p,p,G))
              for(i_g in 1:G) 
                {
                  eta[,,i_g] <- diag(sqrt(par$eta[i_g]),p)
                  sg1 [,,i_g] <- t(eta[,,i_g]) %*% sg[,,i_g] %*% eta[,,i_g] 
                }  
            }
          if(is.vector(par$eta) & length(par$eta) == p)
            {# different variables inflation factor within group with
            # all groups having the same variables inflation factor
#              sg <- par$sigma
              eta <- diag(sqrt(par$eta),p)
   #           sg1 <- array(0.0,dim = c(p,p,G))
              for (i_g in 1:G) sg1[,,i_g] <- t(eta) %*% sg[,,i_g] %*% eta 
            }
          if(is.array(par$eta))
          {
            for (i_g in 1:G)
            {
              eta[,,i_g] <- par$eta[,,g]
              sg1 [,,i_g] <-t(eta[,,i_g]) %*% sg[,,i_g] %*% eta[,,i_g]
            }
          }
          if(is.matrix(par$eta)  )
          {# different variables inflation factor within group with
            # all groups having the same variables inflation factor
   #         sg <- par$sigma
            if(ncol(par$eta) == 1 & nrow(par$eta) == p)
            {      eta <- diag(sqrt(as.vector(par$eta)),p)
    #               sg1 <- array(0.0,dim = c(p,p,G))
              for (i_g in 1:G) sg1[,,i_g] <- t(eta) %*% sg[,,i_g] %*% eta
            }
          
            if(ncol(par$eta) == G & nrow(par$eta) == p )
            { # different variables inflation factor within group with
            # all groups having different variables inflation factor
    #        sg <- par$sigma
              eta <- array(0.0, dim = c(p,p,G) )
     #        sg1 <- array(0.0, dim = c(p,p,G))
              for (i_g in 1:G)
                {
                eta[,,i_g] <- diag(sqrt(par$eta[,i_g]),p)
                sg1 [,,i_g] <-t(eta[,,i_g]) %*% sg[,,i_g] %*% eta[,,i_g]
                }
            } 
            if(nrow(par$eta)!=p &  (ncol(par$eta)!= G  |  ncol(par$eta)!=1 )  )
            {
              stop("The dimension of eta does not correspond 
                 with either the number of variables or number of groups")
            }
              
          }
          if(is.vector(par$eta) & length(par$eta)!=1 & 
             length(par$eta)!=p & length(par$eta) != G )
            stop("The dimension of eta does not correspond 
                 with either the number of variables or number of groups")

        }
  } # end if (p>1)

  for (g in 1:G)
  {
    for(i in 1:m)
    {
      term1 <- log(pig[g])
      if(length(dim(sg)) == 3)
      {
        term2 <- v[i,g] * (log(alpha[g]) + dMVNorm(X[i,],mu[,g],sg[,,g],log = TRUE) )
        term3<-(1-v[i,g]) * (log(1-alpha[g]) + dMVNorm(X[i,],mu[,g],sg1[,,g], log = TRUE) )
        
      }else if(length(dim(sg))==2)
      {
        if(p>1)
        {
          s <- matrix(sg, ncol = p, nrow = p)
          term2 <- v[i,g] * ( log(alpha[g])+ dMVNorm(X[i,],mu[,g],data.matrix(s),log = TRUE ) )
          term3 <-(1-v[i,g]) * ( log (1-alpha[g]) + dMVNorm(X[i,],mu[,g],sg1,log = TRUE ) )
        } else if(p==1)
        {
          mu <- as.vector(mu)
          term2 <- v[i,g] * log(alpha[g]) + dnorm(X[i,],mu[g],sg[g] , log = TRUE) 
          term3<-(1-v[i,g]) * log( (1-alpha[g])) + dnorm(X[i,],mu[g],sg1[g], log = TRUE ) 
        }
      }
      
      if(p > 1)
        M[i,g] <- l[i,g]*(term1 + term2 + term3)   
      else  M[i,g] <- l[i]*(term1 + term2 + term3)   
      
      
    }
  }
  
  loglik <- sum(M)
  
  return(loglik)
}



emCmn_DIF_EII <- function(X, labels,  Maxiterations= 10, threshold = 0.01)
{
  if(!is.matrix(X)) X <- as.matrix(X)
  
  p <- ncol(X)
  m <- nrow(X)
  G <- length(unique(labels))
  
  v0 <- matrix(runif(m*G), nrow = m, ncol = G, byrow = TRUE)
  lampda0 <- 1
  exit <- 0
  loglik <- c(0.0,0.0,0.0)   
  
  #if(is.null(par$G))  G <- length(unique(labels)) else  G <- par$G
    
    alpha0 <- rep(0.9,G)
    eta0 <- array(0.0, dim = c(p,p,G))
    pig <- numeric(G)
    mu <- matrix(0.0, nrow = p , ncol = G)
    sigma <- array(0.0, dim = c(p,p,G))

    l <- unmap(labels)
    mg <- apply(l,2,sum)
    pig0 <-mg/length(labels)
    
    mu0 <-sapply(1:G,function(g){
        apply(X[which(labels==g),],2,mean)
    })
    sigma0 <- array(0.0, dim = c(p,p,G))
    for(g in 1:G)
    {
      sigma0[,,g] <- diag(1,p)
      eta0[,,g] <- diag(1.10,p) 
    }
  
    par <- list(G = G, pig = pig0, mu = mu0,
                sigma = sigma0, alpha = alpha0,
                eta = eta0, z = l, v =  v0, lambda = lampda0)
    
    loglik[1] <- loglikCMN_DIF(X,labels,par)  
    loglik[1]
    
    estep0 <- eCmn_DIF(X,labels,par)
    par$v <- estep0$v
    iter <- 0
    llvalue <- 0
    a <- 0
    b <- 0
    
    while(exit == 0)
    {
          iter <- iter + 1  
          mstep1 <- mCmn_DIF_EII(X,labels,par)
          llvalue <- loglikCMN_DIF(X,labels,par)
          par$pig <- mstep1$pig
          par$mu <- mstep1$mu
          par$sigma <- mstep1$lambda * mstep1$sigma
          par$alpha <- mstep1$alpha
          par$eta <- mstep1$eta
          par$lambda <- mstep1$lambda
          
          
      if(iter >= Maxiterations) 
            { 
              exit == 1
            }else {
                loglik[3] = loglik[2]
                loglik[2] = loglik[1]
                loglik[1] = llvalue
                if(iter > 2)
                  {
                  if(abs(loglik[2]-loglik[3]) == 0) 
                    {
                      exit = 1
                    }else{
                      a = (loglik[1]-loglik[2])/(loglik[2]-loglik[3])
                      b = loglik[2] + (1/(1-a)*(loglik[1]-loglik[2]))
                      if(abs(b-loglik[1])< threshold) exit = 1
                    }
                  }
            }
          estep1 <- eCmn_DIF(X,labels,par)
          par$v <- estep1$v
          
    }
    
    output <- list(G = par$G, pig = par$pig, mu = par$mu,
                   sigma = par$sigma/par$lambda, alpha = par$alpha, 
                   eta = par$eta, z = par$z, v = par$v,
                   lambda = par$lambda, iterations = iter,
                   ll = llvalue)
    return(output)
}

eCmn_DIF<-function(X,labels,par)
{
  # eCmn_difIF E-step for contaminated mixture model with 
  #            different variable inflation factors within groups
  # X : matrix with the training data
  # l:  group information of the observations in training set
  # par:      parameters of the mixture of contaminated normal distribution with
  #           different variables inflation factor within groups
  
  m <- nrow(X)
  p <- ncol(X)
  if(is.null(par$alpha)) stop("A vector with the same length of number of groups 
                               that contains the percentage of non-contaminated 
                               samples is required")
  alpha <- par$alpha
  mu <- par$mu
  G <- length(unique(labels))
  if(is.null(par$pig)) stop("A vector of the same length of the number of groups
                            inidicating the percentage of observations in each class is missed
                            in the parameters")
  pig <- par$pig
  v <- matrix(0.0, ncol = G, nrow = m)
  z <- matrix(0.0, ncol = G, nrow = m)
  lhat <- rep(0,m)
  vhat <- rep(0,m)
  fxig <- matrix(0.0, ncol = G, nrow = m)
  thetaig <- matrix(0.0, ncol = G, nrow = m)
  numz <- matrix(0.0, ncol = G, nrow = m) # numerator for calculating zhat
  numv <- matrix(0.0, ncol = G, nrow = m) # numerator for calculating vhat
  denz <- rep(0,m)                        # denominator for calculating z hat
  denv <- matrix(0.0, ncol = G, nrow = m) # denominator for calculating v hat
  
 # sigma <- par$sigma
 # eta <- par$eta #  eta contains a matrix of dimension p x G, where each column corresponds
                 #  the elements of the diagonal corresponding to variable inflation factor  
                 #  for the group g
  output <- list()
  
  if(p == 1)
  {
    if (G == 1)
    {
      if (is.vector(par$sigma) & length(par$sigma) == 1)
      {
        if(is.vector(par$eta) & length(par$eta) == 1 )
        {
          sigma <- par$sigma
          eta <- par$eta
          sigma1 <- eta * sigma
        }else stop("The dimension of eta does not match")
      }else stop("The dimension of sigma does not match with the number of Groups")
    } else if ( G > 1)
    {
      if(is.vector(par$sigma) & length(par$sigma) == 1)
      {
        if(is.vector(par$eta) & length(par$eta) == 1)
        {
          eta <- rep(par$eta,G)
          sigma <- rep(par$sigma,G)
          sigma1 <- eta * sigma
        }else if(is.vector(par$eta) & length(par$eta) == G)
        {
          eta <- par$eta
          sigma <- rep(par$sigma,G)
          sigma1 <- eta * sigma
        }
        
      } else if ( is.vector(par$sigma) & length(par$sigma) == G)
      {
        if(is.vector(par$eta) & length(par$eta) == 1)
        {
          eta <- rep(par$eta,G)
          sigma <- par$sigma
          sigma1 <- eta * sigma
        }else if(is.vector(par$eta) & length(par$eta) == G)
        {
          eta <- par$eta
          sigma <- par$sigma
          sigma1 <- eta * sigma
        }
      }
    }
  }
  
  if(p > 1)
  {
    if(is.list(par$sigma) & G == 1)
    {
      if(is.vector(par$eta) & length(par$eta) == 1)
      {
        eta <- par$eta
        sigma <- matrix(unlist(par$sigma),ncol=p,nrow =p, byrow = TRUE)
        sigma1 <- eta * sigma
      }else if(is.vector(par$eta) & length(par$eta) == p)
      {
        eta <- diag(sqrt( par$eta),p)
        sigma <- matrix(unlist(par$sigma),ncol=p,nrow =p, byrow = TRUE)
        sigma1 <- t(eta) %*% sigma %*% eta
      }else if(is.matrix(par$eta))
      {
        sigma <- matrix(unlist(par$sigma),ncol=p,nrow =p, byrow = TRUE)
        sigma1 <- t(eta) %*% sigma %*% eta
        
      }else if(is.array(par$eta))
      {
        eta <- matrix(unlist(par$eta), ncol =p,nrow = p,byrow = TRUE)
        sigma <- matrix(unlist(par$sigma),ncol=p,nrow =p, byrow = TRUE)
        sigma1 <- t(eta) %*% sigma %*% eta
      }
      
    }
    if (is.matrix(par$sigma) & G == 1 )
    {   
        if(is.vector(par$eta) & length(par$eta) == 1)
        {
          eta <- par$eta
          sigma <- par$sigma
          sigma1 <- eta * sigma
        }else if(is.vector(par$eta) & length(par$eta) == p)
        {
          eta <- diag(sqrt( par$eta),p)
          sigma <- par$sigma
          sigma1 <- t(eta) %*% sigma %*% eta
        }else if(is.matrix(par$eta))
        {
          if(ncol(par$eta)!= p | nrow(par$eta)!= p) stop("The dimensions of the matrix eta are not pxp")
          if(ncol(par$eta) == p & nrow(par$eta)== p)
          {
            eta <- par$eta
            sigma <- par$sigma
            sigma1 <- t(eta) %*% sigma %*% eta
            
          }
        }else if(is.array(par$eta))
        {
            eta <- par$eta[,,1]
            sigma <- par$sigma
            sigma1 <-t(eta) %*% sigma %*% eta
        }
    }else if(is.matrix(par$sigma) & G > 1)
    {
      if(is.vector(par$eta) & length(par$eta) == 1)
      {
        eta <- par$eta
        sigma <- par$sigma
        sigma1 <- eta * sigma
      }else if(is.vector(par$eta) & length(par$eta) == p)
      {
        eta <- diag(sqrt(par$eta),p)
        sigma <- par$sigma
        sigma1 <- t(eta) %*% sigma %*% eta
      }else if(is.matrix(par$eta) & nrow(par$eta)== p & ncol(par$eta) == G)
      {
        sigma <- par$sigma
        eta <- array(0.0, dim =c(p,p,G) )
        sigma1 <- array(0.0, dim = c(p,p,G))
        for (i_g in 1:G)
        {
            eta[,,i_g] <- diag(sqrt(par$eta[,g]),p)
            sigma1[,,i_g] <- t(eta) %*% sigma %*% eta
        }
      }
      
    }else if(is.array(par$sigma) & length(dim(par$sigma))==3 )
    {
      if(is.vector(par$eta) & length(par$eta) == 1)
      {
        eta <- par$eta
        sigma <- par$sigma
        sigma1 <- array(0.0, dim =c(p,p,G))
        for(i_g in 1:G) sigma1[,,i_g] <- eta * sigma[,,i_g]
      }else if(is.vector(par$eta) & length(par$eta) == p)
      {
        eta <- diag(sqrt(par$eta),p)
        sigma <- par$sigma
        sigma1 <- array(0.0, dim = c(p,p,G))
        for(i_g in 1:G) sigma1[,,i_g] <- t(eta) %*% sigma[,,i_g] %*% eta
      }else if(is.vector(par$eta) & length(par$eta) == G)
      {
        sigma <- par$sigma
        eta <- par$eta
        sigma1 <- array(0.0, dim = c(p,p,G))
        for(i_g in 1:G) 
        {
          sigma1[,,i_g] <- eta[i_g] * sigma[,,i_g]
        }
      }else if(is.matrix(par$eta) & nrow(par$eta)== p & ncol(par$eta) == G)
      {
        sigma <- par$sigma
        eta <- array(0.0, dim = c(p,p,G))
        sigma1 <- array(0.0, dim = c(p,p,G))
        for(i_g in 1:G)
        {
          eta[,,i_g] <- diag(sqrt(par$eta[,i_g]),p)
          sigma1[,,i_g] <- t(eta[,,i_g]) %*% sigma[,,i_g] %*% eta[,,i_g]
        }
      }else if( is.array(par$eta) ) 
      {
        if( all( dim(par$eta) == c(p,p,G) ) == TRUE  ) 
        {
            sigma <- par$sigma
            eta <- par$eta
            sigma1 <- array(0.0, dim = c(p,p,G))
            
            for (i_g in 1:G) sigma1[,,i_g] <- t(eta[,,i_g]) %*% sigma[,,i_g] %*% eta[,,i_g]
        }
      }
    }else if(is.list(par$sigma) & length((par$sigma))==G & G > 1)
    {
      if(is.vector(par$eta) & length(par$eta) == 1)
      {
        eta <- par$eta
        sigma <- par$sigma
        sigma1 <- array(0.0, dim =c(p,p,G))
        for(i_g in 1:G) sigma1[,,i_g] <- eta * sigma[,,i_g]
      }else if(is.vector(par$eta) & length(par$eta) == p)
      {
        eta <- diag(sqrt(par$eta),p)
        sigma <- par$sigma
        sigma1 <- array(0.0, dim = c(p,p,G))
        for(i_g in 1:G) sigma1[,,i_g] <- t(eta) %*% sigma[,,i_g] %*% eta
      }else if(is.vector(par$eta) & length(par$eta) == G)
      {
        sigma <- par$sigma
        eta <- par$eta
        sigma1 <- array(0.0, dim = c(p,p,G))
        for(i_g in 1:G) 
        {
          sigma1[,,i_g] <- eta[i_g] * sigma[,,i_g]
        }
      }else if(is.matrix(par$eta) & nrow(par$eta)== p & ncol(par$eta) == G)
      {
        sigma <- par$sigma
        eta <- array(0.0, dim = c(p,p,G))
        sigma1 <- array(0.0, dim = c(p,p,G))
        for(i_g in 1:G)
        {
          eta[,,i_g] <- diag(sqrt(par$eta[,i_g]),p)
          sigma1[,,i_g] <- t(eta[,,i_g]) %*% sigma[,,i_g] %*% eta[,,i_g]
        }
      }else if( is.array(par$eta) ) 
      {
        if( all( dim(par$eta) == c(p,p,G) ) == TRUE  ) 
        {
          sigma <- array(0.0,dim = c(p,p,G))
          sigma1 <- array(0.0,dim = c(p,p,G))
          eta <- par$eta
          for (i_g in 1:G) 
            {
              sigma[,,i_g] <- par$sigma[[i_g]]
              sigma1[,,i_g] <- t(eta[,,i_g]) %*% sigma[,,i_g] %*% eta[,,i_g]
            }  
        }
      }
  }
  }
# Continuing here    
  # Calculating z's and v's
  
  for(g in 1:G)
  {
    for(i in 1:m)
    {
      if(p == 1 )
        {
          if( G == 1 & is.vector(par$sigma) & length(par$sigma)==1 &
              is.vector(par$eta) & length(par$eta) == 1)
          {
            # thetaig : matrix containing the probability of i-th observation in group g 
            # is not contaminated
            thetaig[i,g] <- dnorm(X[i,],mu,sigma)
            # fxig: matrix containing the probability of contaminated normal distribution for
            # observation i in group g
            fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dnorm(X[i,],mu,sigma1)
          }else if(G >1 & is.vector(par$sigma) )
          {
            thetaig[i,g] <- dnorm(X[i,],mu[,g],sigma[g])
            # fxig: matrix containing the probability of contaminated normal distribution for
            # observation i in group g
            fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dnorm(X[i,],mu[,g],sigma1[g])
            
          }
      } # end-if p == 1
      
      if(p > 1)
      {
        if(G == 1 & is.matrix(par$sigma) )
        {
          thetaig[i,g] <- dMVNorm(X[i,],mu,sigma)
          # fxig: matrix containing the probability of contaminated normal distribution for
          # observation i in group g
          fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu,sigma1)
          
        }else if(G > 1 & is.matrix(par$sigma) & is.vector(par$eta) )
        {
          thetaig[i,g] <- dMVNorm(X[i,],mu[,g],sigma)
          # fxig: matrix containing the probability of contaminated normal distribution for
          # observation i in group g
          fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],sigma1)
          
        }else if(G>1 & is.matrix(par$sigma) & is.matrix(par$eta))
        {
          thetaig[i,g] <- dMVNorm(X[i,],mu[,g],sigma)
          # fxig: matrix containing the probability of contaminated normal distribution for
          # observation i in group g
          fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],sigma1[,,g])
          
        }else if(G>=1 & (is.array(par$sigma)|is.list(par$sigma) ) )
        {
          thetaig[i,g] <- dMVNorm(X[i,],mu[,g],sigma[,,g])
          # fxig: matrix containing the probability of contaminated normal distribution for
          # observation i in group g
          fxig[i,g] <- alpha[g]*thetaig[i,g] + (1-alpha[g])*dMVNorm(X[i,],mu[,g],sigma1[,,g])
          
        }
      } # end-if p > 1
      


      # avoid division by zero
      numz[i,g] <- pig[g] * fxig[i,g]
      numv[i,g] <- alpha[g] * thetaig[i,g]
      denv[i,g] <- fxig[i,g]
      v[i,g] <- numv[i,g]/denv[i,g]
      
      
    }#End-for i
    
  }#End-for G
  
  # calculating zhat and lhat
  
  # calculating zhat and lhat
  denzi <- apply(numz,1,sum)
  for (i in 1:m)
  {
    z[i,] <- numz[i,]/denzi[i]
  }
  lhat<-apply(z,1,which.max)
  for (i in 1:m)
  {
    vhat[i] <- ifelse(v[i,labels[i]]>0.5,1,0)
  }
  
  output <- list(z = z, v = v, lhat = lhat, vhat = vhat )
  return(output)
  
  
}


f_etaDIF_EII <- function(eta_gj,z, v , X, mu, lambda)
{
  # eta_gj: contains the value of eta for the p-th variable
  # z : a vector of dimension nx1 containing either
  #     the labels of the observation for the g-th group where z = 1 if observation i belongs 
  #     to group g in the case of supervised learning or
  #     the estimated probabilities z of observation i belonging to group g in the case of
  #     semi-supervised learning
  # v : a vector of dimension nx1 containing either
  #     the labels of whether the i-th observation is contaminated v = 0 
  #     or non-contaminated v= 1  
  #     or the estimated probabilities v of observation i is contaminated or not
  #     where if the probability is higher than 0.5 the observation is non-contaminated
  #     and if the probability is lower than 0.5 is contaminated.
  # X : matrix of observations
  # mu: mu_gj it is the mean for the group g the variable j 
  # invSigma: invSigmajj it is the jth element of the diagonal of the estimated inverse 
  #           matrix of the covariance
  
  m <- length(X)
  #  G <- ncol(mu)
  output <- 0.0
  if(any(nrow(z),nrow(v))== m) stop("z and v has to be same length")
  sum1 <- 0
  sum2 <- 0
  product1 <- numeric(m)
  product2 <- numeric(m)
  
  product1 <- z*(1-v)
  
  for(i in 1:m)
  {
    product2[i]<- product1[i]* ( (X[i]-mu)^2 )
  } #end-if i
  sum1 <- (-1/(2*lambda*eta_gj)) * sum(product2)
  sum2 <- -(log(eta_gj)/2) *sum(product1)    
  
  output <- sum1 + sum2
  
  return(output)
  
}



f_etaDIF <- function(eta_gj,z, v , X, mu, invSigma_jj)
{
  # eta_gj: contains the value of eta for the p-th variable
  # z : a vector of dimension nx1 containing either
  #     the labels of the observation for the g-th group where z = 1 if observation i belongs 
  #     to group g in the case of supervised learning or
  #     the estimated probabilities z of observation i belonging to group g in the case of
  #     semi-supervised learning
  # v : a vector of dimension nx1 containing either
  #     the labels of whether the i-th observation is contaminated v = 0 
  #     or non-contaminated v= 1  
  #     or the estimated probabilities v of observation i is contaminated or not
  #     where if the probability is higher than 0.5 the observation is non-contaminated
  #     and if the probability is lower than 0.5 is contaminated.
  # X : matrix of observations
  # mu: mu_gj it is the mean for the group g the variable j 
  # invSigma: invSigmajj it is the jth element of the diagonal of the estimated inverse 
  #           matrix of the covariance

  m <- length(X)
  #  G <- ncol(mu)
  output <- 0.0
  if(any(nrow(z),nrow(v))== m) stop("z and v has to be same length")
  sum1 <- 0
  sum2 <- 0
  product1 <- numeric(m)
  product2 <- numeric(m)
  
  product1 <- z*(1-v)
  
  for(i in 1:m)
  {
    product2[i]<- product1[i]* ( (X[i]-mu)^2 )
  } #end-if i
  sum1 <- (-invSigma_jj/(2*eta_gj)) * sum(product2)
  sum2 <- -(log(eta_gj)/2) *sum(product1)    
  
  output <- sum1 + sum2
  
  return(output)
  
}


mCmn_DIF_VII <- function(Xtrain,ltrain, par, eta_max = 1000)
{
  Xtrain <- as.matrix(Xtrain)
  m <- nrow(Xtrain)
  p <- ncol(Xtrain)
  G <- length(unique(ltrain))
  l <- unmap(ltrain)
  
  mu <- matrix(0.0, nrow = p, ncol = G) 
  #sigma <- array(0.0, dim = c(p,p,G))
  #inv_sigma <-array(0.0, dim =c(p,p,G) )
  alpha <- numeric(G)
  eta <- array(0.0, dim = c(p,p,G))
  inv_eta<-array(0.0,dim = c(p,p,G))
  lambda <- numeric(G)
    
  est_mu <- matrix(0.0, nrow = p, ncol = G)
  est_lambda <- numeric(1)
  if(G == 1) 
    {
      est_sigma <- diag(1,p)
      sigma <- diag(1,p)
      inv_sigma <- diag(1,p)
    }  
  if(G > 1)
    {
      sigma <- diag(1,p)
      est_sigma<- array(0.0, dim = c(p,p,G))
      inv_sigma <- array(0.0, dim = c(p,p,G))
      for (g in 1:G) 
        {
          est_sigma[,,g] <- diag(1,p)
          inv_sigma[,,g] <- solve(est_sigma[,,g])
        }  
    }    
  est_alpha <- numeric(G)
  est_eta <- array(0.0, dim = c(p,p,G))
  
  # Estimate initial parameters ---------------------------------------------
  
  #  if(is.empty.list(par))
  #  {
  #    mu0 <- sapply(1:G, function(g)
  #    {
  #      apply(Xtrain[ltrain==g,],2,mean)
  #    })
  #  }
  
  if(is.null(par$v))
  {
    set.seed(123)
    v <- matrix(runif(m*G),nrow = m,ncol = G, byrow = TRUE)
  }else if(is.matrix(par$v))
           {
             if(all(dim(par$v) == c(m,G)) == TRUE )
                { v <- par$v}
            }else stop("The dimension of v has to be mXG")

  if(is.matrix(par$mu) & all(dim(par$mu)==c(p,G)) == TRUE )
  {
    mu<-par$mu
  }else stop("Parameter mu has to be a matrix of dimension pxG")
  
  if(is.numeric(par$lambda) & length(par$lambda) == 1 )
  {
    lambda <- par$lambda
  }else stop("Parameter lambda has to be a vector of dimension 1")
  
  if(is.vector(par$alpha) & length(par$alpha) == G) 
  {
    alpha <- par$alpha
  } else stop("Parameter alpha has to be a vector of length G")
  
  if(is.array(par$eta))
  {
    if(length(dim(par$eta))== 2 & G >= 1)
    {
      for (g in 1:G) eta[,,g] <- par$eta
    }else if(length(dim(par$eta)) == 3)
      if( all(dim(par$eta) == c(p,p,G)) == TRUE )  
        eta <- par$eta 
    } else if(is.matrix(par$eta))
      {
         if(all(dim(par$eta) == c(p,p)) == TRUE  )
           eta <- par$eta
      }else {stop("Parameter eta has to be a array of dimension pxpxG")}
    
  for (g in 1:G)  
    {
      
      if(G>1) inv_eta[,,g] <- solve(eta[,,g])
      if(G == 1) inv_eta[,,g] <- solve(eta[,,g])
    } 
  
  mg <-apply(unmap(ltrain),2,sum)
  pig <- mg/m
  sum_lig_vig <- numeric(G)
  sig <- vector("list",m)
  sum_sig <- vector("list",G)
  sum_sig_xi <- vector("list",G)
  term1 <- vector("list",m)
  term2 <- vector("list",m)
  sumTerms<- vector("list",m)
  sumSigma <-vector("list",G)
  est_alpha <- numeric(length(alpha))

  # CM-step1 ----------------------------------------------------------------
  
  # numerator for calculating alphag
  if(is.matrix(v) & is.matrix(l))
  {
    
    sum_lig_vig <- sapply(1:G, function(g) {sum(l[,g]*v[,g]) })
    est_alpha <- sum_lig_vig/mg
    
    for(g in 1:G)
    {
      
      for(i in 1:m)
      {
        if(p==1 & G == 1)
        {
          sig[[i]] <- (l[i]) *(   v[i,g] * diag(1,p) + 
                                  (1-v[i,g]) * inv_eta )       
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu) * (Xtrain[i,]-mu) )
          term2[[i]] <- (1-v[i,g]) * inv_eta* (Xtrain[i,]-mu) * (Xtrain[i,]-mu) * inv_eta
          sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
        }else if (p ==1 & G>1)
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * diag(1,p) + 
                                    (1-v[i,g]) * inv_eta[g] )
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g]) )
          term2[[i]] <- (1-v[i,g]) * inv_eta[g]* (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g])* inv_eta[g]
          sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }else if(p> 1 & G == 1)
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * diag(1,p) + 
                                    (1-v[i,g]) * inv_eta[,,g]  )  
          term1[[i]] <-v[i,g]* t(as.matrix(Xtrain[i,]-mu[,g])) %*% as.matrix(Xtrain[i,]-mu[,g]) 
          term2[[i]] <- (1-v[i,g]) * t(as.matrix(Xtrain[i,]-mu[,g])) %*% inv_eta[,,g] %*% t(inv_eta[,,g]) %*% as.matrix(Xtrain[i,]-mu[,g])
          if(G == 1)  sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
          if(G > 1)   sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
          
        } else if (p > 1 & G > 1 )
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * diag(1,p) + 
                                    (1-v[i,g]) * inv_eta[,,g]  )  
          term1[[i]] <-v[i,g]* ( t(Xtrain[i,]-mu[,g]) %*% (Xtrain[i,]-mu[,g]) )
          term2[[i]] <- (1-v[i,g]) * ( t(as.matrix(Xtrain[i,]-mu[,g]) ) %*% inv_eta[,,g] %*% t(inv_eta[,,g]) %*% as.matrix(Xtrain[i,]-mu[,g] ) )
          if(G == 1)  sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
          if(G > 1)   sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }
        if(i==1) 
        {
          sum_sig[[g]] <- 0
          sum_sig_xi[[g]] <-0
          sumSigma[[g]] <-0
        }  
        sum_sig[[g]]  <- sum_sig[[g]] + sig[[i]]
        sum_sig_xi[[g]] <-sum_sig_xi[[g]] + sig[[i]] %*% Xtrain[i,]
        sumSigma[[g]] <- sumSigma[[g]] + sumTerms[[i]]
      }# end-for i
      est_mu[,g] <- t(sum_sig_xi[[g]]) %*% solve(sum_sig[[g]])
      if(G==1) est_lambda <- sumSigma[[g]]/(p*m)
      if(G>1) est_lambda[g] <- sumSigma[[g]]/(p*m)
    }# end-for g          
    
  }# end-if
  
  
  # CM-step2 ----------------------------------------------------------------
  factor1 <- 0
  eta_max <- 1000


  for(g in 1:G)
  {
    for(j in 1:p)
    {
      if(G == 1)
      {
        factor1 <- optimize(f_etaDIF_EII,c(1,eta_max), tol = 0.001,
                            z = l[,g], v = v[,g], X = Xtrain[,j],
                            mu = est_mu[j,g], lambda = est_lambda, 
                            maximum = TRUE)
        est_eta[j,j,g] <- factor1$maximum
        
      }else(G > 1)
      {
        factor1 <- optimize(f_etaDIF_EII,c(1,eta_max), tol = 0.001,
                            z = l[,g], v = v[,g], X = Xtrain[,j],
                            mu = est_mu[j,g], lambda = est_lambda[G], 
                            maximum = TRUE)
        est_eta[j,j,g] <- factor1$maximum
        
      }
    }
  }
  
  
  
  
  output <- list(mu = est_mu, sigma = est_sigma,lambda = est_lambda,
                 alpha = est_alpha ,eta = est_eta, pig = pig, G = G)
  
  return(output)
  
  
}

mCmn_DIF_EII <- function(Xtrain,ltrain, par, eta_max = 1000)
{
  Xtrain <- as.matrix(Xtrain)
  m <- nrow(Xtrain)
  p <- ncol(Xtrain)
  G <- length(unique(ltrain))
  l <- unmap(ltrain)
  
  mu <- matrix(0.0, nrow = p, ncol = G) 
  #sigma <- array(0.0, dim = c(p,p,G))
  #inv_sigma <-array(0.0, dim =c(p,p,G) )
  alpha <- numeric(G)
  eta <- array(0.0, dim = c(p,p,G))
  inv_eta<-array(0.0,dim = c(p,p,G))
  lambda <- numeric(1)
  
  est_mu <- matrix(0.0, nrow = p, ncol = G)
  est_lambda <- numeric(1)
  if(G == 1) 
  {
    est_sigma <- diag(1,p)
    sigma <- diag(1,p)
    inv_sigma <- diag(1,p)
  }  
  if(G > 1)
  {
    sigma <- diag(1,p)
    est_sigma<- array(0.0, dim = c(p,p,G))
    inv_sigma <- array(0.0, dim = c(p,p,G))
    for (g in 1:G) 
    {
      est_sigma[,,g] <- diag(1,p)
      inv_sigma[,,g] <- solve(est_sigma[,,g])
    }  
  }    
  est_alpha <- numeric(G)
  est_eta <- array(0.0, dim = c(p,p,G))
  
  # Estimate initial parameters ---------------------------------------------
  
  #  if(is.empty.list(par))
  #  {
  #    mu0 <- sapply(1:G, function(g)
  #    {
  #      apply(Xtrain[ltrain==g,],2,mean)
  #    })
  #  }
  
  if(is.null(par$v))
  {
    set.seed(123)
    v <- matrix(runif(m*G),nrow = m,ncol = G, byrow = TRUE)
  }else if(is.matrix(par$v))
  {
    if(all(dim(par$v) == c(m,G)) == TRUE )
    { v <- par$v}
  }else stop("The dimension of v has to be mXG")
  
  #if(is.matrix(par$mu) & all(dim(par$mu)==c(p,G)) == TRUE )
  #{
  #  mu<-par$mu
  #}else stop("Parameter mu has to be a matrix of dimension pxG")
  
  #if(is.numeric(par$lambda) & length(par$lambda) == 1 )
  #{
    lambda <- par$lambda
  #}else stop("Parameter lambda has to be a vector of dimension 1")
  
  #if(is.vector(par$alpha) & length(par$alpha) == G) 
  #{
  #  alpha <- par$alpha
  #} else stop("Parameter alpha has to be a vector of length G")
  
  #if(is.array(par$eta))
  #{
  #  if(length(dim(par$eta))== 2 & G >= 1)
  #  {
  #    for (g in 1:G) eta[,,g] <- par$eta
  #  }else if(length(dim(par$eta)) == 3)
  #    if( all(dim(par$eta) == c(p,p,G)) == TRUE )  
  #      eta <- par$eta 
  #} else if(is.matrix(par$eta))
  #{
  #  if(all(dim(par$eta) == c(p,p)) == TRUE  )
  #    eta <- par$eta
  #}else {stop("Parameter eta has to be a array of dimension pxpxG")}
  
  #for (g in 1:G)  
  #{
    
  #  if(G>1) inv_eta[,,g] <- solve(eta[,,g])
  #  if(G == 1) inv_eta[,,g] <- solve(eta[,,g])
  #} 
  
  mg <-apply(unmap(ltrain),2,sum)
  pig <- mg/m
  sum_lig_vig <- numeric(G)
  sig <- vector("list",m)
  sum_sig <- vector("list",G)
  sum_sig_xi <- vector("list",G)
  term1 <- vector("list",m)
  term2 <- vector("list",m)
  sumTerms<- vector("list",m)
  sumSigma <-vector("list",G)
  est_alpha <- numeric(length(alpha))
  
  # CM-step1 ----------------------------------------------------------------
  
  # numerator for calculating alphag
  if(is.matrix(v) & is.matrix(l))
  {
    
    sum_lig_vig <- sapply(1:G, function(g) {sum(l[,g]*v[,g]) })
    est_alpha <- sum_lig_vig/mg
    
    for(g in 1:G)
    {
      
      for(i in 1:m)
      {
        if(p==1 & G == 1)
        {
          sig[[i]] <- (l[i]) *(   v[i,g] * diag(1,p) + 
                                    (1-v[i,g]) * inv_eta )       
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu) * (Xtrain[i,]-mu) )
          term2[[i]] <- (1-v[i,g]) * inv_eta* (Xtrain[i,]-mu) * (Xtrain[i,]-mu) * inv_eta
          sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
        }else if (p ==1 & G>1)
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * diag(1,p) + 
                                    (1-v[i,g]) * inv_eta[g] )
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g]) )
          term2[[i]] <- (1-v[i,g]) * inv_eta[g]* (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g])* inv_eta[g]
          sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }else if(p> 1 & G == 1)
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * diag(1,p) + 
                                    (1-v[i,g]) * inv_eta[,,g]  )  
          term1[[i]] <-v[i,g]* t(as.matrix(Xtrain[i,]-mu[,g])) %*% as.matrix(Xtrain[i,]-mu[,g]) 
          term2[[i]] <- (1-v[i,g]) * t(as.matrix(Xtrain[i,]-mu[,g])) %*% inv_eta[,,g] %*% t(inv_eta[,,g]) %*% as.matrix(Xtrain[i,]-mu[,g])
          if(G == 1)  sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
          if(G > 1)   sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
          
        } else if (p > 1 & G > 1 )
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * diag(1,p) + 
                                    (1-v[i,g]) * inv_eta[,,g]  )  
          term1[[i]] <-v[i,g]* ( t(Xtrain[i,]-mu[,g]) %*% (Xtrain[i,]-mu[,g]) )
          term2[[i]] <- (1-v[i,g]) * ( t(as.matrix(Xtrain[i,]-mu[,g]) ) %*% inv_eta[,,g] %*% t(inv_eta[,,g]) %*% as.matrix(Xtrain[i,]-mu[,g] ) )
          if(G == 1)  sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
          if(G > 1)   sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }
        if(i==1) 
        {
          sum_sig[[g]] <- 0
          sum_sig_xi[[g]] <-0
          sumSigma[[g]] <-0
        }  
        sum_sig[[g]]  <- sum_sig[[g]] + sig[[i]]
        sum_sig_xi[[g]] <-sum_sig_xi[[g]] + sig[[i]] %*% Xtrain[i,]
        sumSigma[[g]] <- sumSigma[[g]] + sumTerms[[i]]
      }# end-for i
      est_mu[,g] <- t(sum_sig_xi[[g]]) %*% solve(sum_sig[[g]])
    }# end-for g          
    est_lambda <- sum(unlist(sumSigma))/(p*m)
                                 
  }# end-if
  
  
  # CM-step2 ----------------------------------------------------------------
  factor1 <- 0
  eta_max <- 1000
  
  
  for(g in 1:G)
  {
    for(j in 1:p)
    {
        factor1 <- optimize(f_etaDIF_EII,c(1,eta_max), tol = 0.001,
                            z = l[,g], v = v[,g], X = Xtrain[,j],
                            mu = est_mu[j,g], lambda = est_lambda, 
                            maximum = TRUE)
        est_eta[j,j,g] <- factor1$maximum
        
    }
  }
  
  
  
  
  output <- list(mu = est_mu, sigma = est_sigma,lambda = est_lambda,
                 alpha = est_alpha ,eta = est_eta, pig = pig, G = G)
  
  return(output)
  
  
}


mCmn_DIF_VVV <- function(Xtrain,ltrain,par,eta_max = 1000)
{
  Xtrain <- as.matrix(Xtrain)
  m <- nrow(Xtrain)
  p <- ncol(Xtrain)
  G <- length(unique(ltrain))
  l <- unmap(ltrain)
  if(is.null(par$sigma)) stop("The variance covariance matrix of dimension pxp is required 
                              in the parameters")
  sigma <- par$sigma
  inv_sigma <- array(0.0, dim = c(p,p,G) )
  
  if(!is.array(par$sigma) & !is.list(par$sigma) )
  {
    inv_sigma <- solve(sigma)
  }else if(is.array(par$sigma) )
  {
    for(g in 1:G) inv_sigma[,,g] <- solve(par$sigma[,,g])
  }else if(is.list(par$sigma)) 
  {
    if(length(par$sigma)!=G) stop("The list should contain the same number of covariance matrices as number of groups")
    for(g in 1:G) inv_sigma[,,g] <- solve(par$sigma[[g]])
  }
  
  
  if (any(par$alpha>1) | any(par$alpha<0) ) stop("The parameter of level of contamination alpha should be between 0 and 1")
  
  # initial values
  mu <- par$mu
  
  
  # sigma and inv_sigma initialization --------------------------------------
  
  
  # alpha initialization  
  #  if(is.null(par$alpha) & G == 1) 
  #  {
  #    alpha <- 1.011
  #  }else if(!is.null(par$alpha) & G == 1 & length(par$alpha) == 1)
  #  {
  #    alpha <- par$alpha 
  #  }else if(is.null(par$alpha) & G >1)
  #  {
  #    alpha <- rep(1.011,G)
  #  }else if(!is.null(par$alpha) & G >1 & length(par$alpha) == 1)
  #  {
  #    alpha <- rep(par$alpha,G)
  #  }else if(!is.null(par$alpha) & G>1 & length(par$alpha) == G)
  #  {
  #    alpha <- par$alpha
  #  }
  #  
  
  #  if(is.vector(par$alpha) & length(par$alpha) == 1)
  #  {
  #    alpha <- rep(par$alpha,G)
  #  }else if(is.vector(par$alpha) & length(par$alpha)== G)
  #  {
  #    alpha <- par$alpha
  #  }else if(is.vector(par$alpha) & (length(par$alpha)!= 1 & length(par$alpha)!=G ) )
  #    stop("The dimension of alpha does not correspond to the groups number")
  
  
  # Eta initialization and inverse of Eta -------------------------------------------
  
  
  if( p == 1 )
  {
    eta <- numeric()
    inv_eta <- numeric()
    if(G ==1)
    {
      if(is.vector(par$eta) & length(par$eta) == 1)
      {
        eta <- par$eta
        inv_eta <- 1/eta
      }else if(is.vector(par$eta) & length(par$eta)!= 1 ) 
        stop("Dimension of parameter eta does not match having 1 group")
    }else if(G>1)
    {
      if(is.vector(par$eta) & length(par$eta) == 1)
      {
        eta <- rep(par$eta,G)
        inv_eta <-rep(1/eta,G)
      }else if(is.vector(par$eta) & length(par$eta) == G)
      { 
        eta <- par$eta
        inv_eta<- 1/eta
      }
    }
  }
  
  if(p > 1)
  {
    eta <- array(0.0,dim = c(p,p,G))
    inv_eta <- array(0.0, dim = c(p,p,G))
    if(G == 1)
    {
      if(is.vector(par$eta) & (length(par$eta) == 1 | length(par$eta) == G ) ) 
      {
        eta[,,G] <- diag(sqrt(par$eta),p)
        inv_eta[,,G] <- solve(eta[,,G])
      }else if(is.vector(par$eta) & length(par$eta) != 1 & length(par$eta) != p)
        stop("The dimension of eta is incorrect for having 1 group")
      
    }else if(G > 1)
    {
      if(is.vector(par$eta) & (length(par$eta) == 1 | length(par$eta) ==p ) )
      {
        for(g in 1:G)
        {
          eta[,,g] <- diag(sqrt(par$eta),p)
          inv_eta[,,g] <- solve(eta[,,g])
        } #end -for
      } else if (is.vector(par$eta) & length(par$eta) == G)
      {
        for(g in 1:G)
        {
          eta[,,g] <- diag(sqrt(par$eta[g]),p)
          inv_eta[,,g] <- solve(eta[,,g])
        } #end -for
        
      } else if (is.matrix(par$eta) & nrow(par$eta)== p & ncol(par$eta) == G )
      {
        for(g in 1:G)
        {
          eta[,,g] <- diag(sqrt(par$eta[,g]),p)
          inv_eta[,,g]<- solve(eta[,,g])
        }
      } else if (is.matrix(par$eta) & nrow(par$eta) == p & ncol(par$eta) == p)
      {
        for(g in 1:G)
        {
          eta[,,g] <- diag(sqrt(diag(par$eta)),p)
          inv_eta[,,g] <- solve(eta[,,g])
        }
        
      }else if(is.array(par$eta) )
      {       
        if( all(dim(par$eta) == c(p,p,G) ) == TRUE )
          for(g in 1:G)
          {
            eta[,,g] <- diag( sqrt(diag(par$eta[,,g])) , p)
            inv_eta[,,g] <- solve(eta[,,g])
          }
      }
    }   # end-if G  
  }# end-if p
  
  
  
  # v initialization --------------------------------------------------------
  
  if(is.null(par$v))
  {
    v<- matrix(c(runif(m*G)), ncol = G, nrow = nrow(X))
  }else  v <- par$v
  
  
  # Calculations ------------------------------------------------------------
  
  
  mg <-apply(unmap(ltrain),2,sum)
  pig <- mg/m
  sum_lig_vig <- numeric(G)
  sig <- vector("list",m)
  sum_sig <- vector("list",G)
  sum_sig_xi <- vector("list",G)
  term1 <- vector("list",m)
  term2 <- vector("list",m)
  sumTerms<- vector("list",m)
  sumSigma <-vector("list",G)
  est_sigma <- vector("list",G)
  est_alpha <- numeric(length(alpha))
  est_mu <- matrix(0.0,nrow=p,ncol=G) 
  
  # CM-step1 ----------------------------------------------------------------
  
  # numerator for calculating alphag
  if(is.matrix(v) & is.matrix(l))
  {
    
    sum_lig_vig <- sapply(1:G, function(g) {sum(l[,g]*v[,g]) })
    est_alpha <- sum_lig_vig/mg
    
    for(g in 1:G)
    {
      
      for(i in 1:m)
      {
        if(p==1 & G == 1)
        {
          sig[[i]] <- l[i] *(   v[i,g] * inv_sigma + 
                                  (1-v[i,g]) * inv_eta * inv_sigma * inv_eta  )       
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu) * (Xtrain[i,]-mu) )
          term2[[i]] <- (1-v[i,g]) * inv_eta* (Xtrain[i,]-mu) * (Xtrain[i,]-mu) * inv_eta
          sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
        }else if (p ==1 & G>1)
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * inv_sigma[g] + 
                                    (1-v[i,g]) * inv_eta[g] * inv_sigma[g] * inv_eta[g]  )
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g]) )
          term2[[i]] <- (1-v[i,g]) * inv_eta[g]* (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g])* inv_eta[g]
          sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }else if (p > 1 )
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * inv_sigma[,,g] + 
                                    (1-v[i,g]) * inv_eta[,,g] %*% inv_sigma[,,g] %*% inv_eta[,,g]  )  
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu[,g]) %*% t(Xtrain[i,]-mu[,g]) )
          term2[[i]] <- (1-v[i,g]) * (inv_eta[,,g] %*% (Xtrain[i,]-mu[,g]) %*% t(Xtrain[i,]-mu[,g]) %*% inv_eta[,,g])
          if(G == 1)  sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
          if(G > 1)   sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }
        if(i==1) 
        {
          sum_sig[[g]] <- 0
          sum_sig_xi[[g]] <-0
          sumSigma[[g]] <-0
        }  
        sum_sig[[g]]  <- sum_sig[[g]] + sig[[i]]
        sum_sig_xi[[g]] <-sum_sig_xi[[g]] + sig[[i]] %*% Xtrain[i,]
        sumSigma[[g]] <- sumSigma[[g]] + sumTerms[[i]]
      }# end-for i
      est_mu[,g] <- t(sum_sig_xi[[g]]) %*% solve(sum_sig[[g]])
      if(G==1) est_sigma[[g]] <- sumSigma[[g]]/mg
      if(G>1) est_sigma[[g]] <- sumSigma[[g]]/mg[g]
    }# end-for g          
    
  }# end-if
  
  
  # CM-step2 ----------------------------------------------------------------
  factor1 <- 0
  eta_max <- 1000
  est_invSigma <- array(0.0,dim = c(p,p,G))
  est_eta <- array(0.0, dim = c(p,p,G))
  
  for( g in 1:G)
  {
    est_invSigma[,,g] <- solve(est_sigma[[g]])
  }
  
  for(g in 1:G)
  {
    for(j in 1:p)
    {
      factor1 <- optimize(f_etaDIF,c(1,eta_max), tol = 0.001,
                          z = l[,g], v = v[,g], X = Xtrain[,j],
                          mu = est_mu[j,g], invSigma = est_invSigma[j,j,g], 
                          maximum = TRUE)
      est_eta[j,j,g] <- factor1$maximum
    }
  }
  
  
  
  
  output <- list(mu = est_mu, sigma = est_sigma,inv_Sigma = est_invSigma,
                 alpha = est_alpha ,eta = est_eta, pig = pig, G = G)
  
  return(output)
}


mCmn_DIF <- function(Xtrain,ltrain,par)
{
  Xtrain <- as.matrix(Xtrain)
  m <- nrow(Xtrain)
  p <- ncol(Xtrain)
  G <- length(unique(ltrain))
  l <- unmap(ltrain)
  if(is.null(par$sigma)) stop("The variance covariance matrix of dimension pxp is required 
                              in the parameters")
  sigma <- par$sigma
  inv_sigma <- array(0.0, dim = c(p,p,G) )
  
  if(!is.array(par$sigma) & !is.list(par$sigma) )
  {
    inv_sigma <- solve(sigma)
  }else if(is.array(par$sigma) )
  {
    for(g in 1:G) inv_sigma[,,g] <- solve(par$sigma[,,g])
  }else if(is.list(par$sigma)) 
  {
    if(length(par$sigma)!=G) stop("The list should contain the same number of covariance matrices as number of groups")
    for(g in 1:G) inv_sigma[,,g] <- solve(par$sigma[[g]])
  }

  
  if (any(par$alpha>1) | any(par$alpha<0) ) stop("The parameter of level of contamination alpha should be between 0 and 1")

  # initial values
  mu <- par$mu
  

# sigma and inv_sigma initialization --------------------------------------

  
# alpha initialization  
#  if(is.null(par$alpha) & G == 1) 
#  {
#    alpha <- 1.011
#  }else if(!is.null(par$alpha) & G == 1 & length(par$alpha) == 1)
#  {
#    alpha <- par$alpha 
#  }else if(is.null(par$alpha) & G >1)
#  {
#    alpha <- rep(1.011,G)
#  }else if(!is.null(par$alpha) & G >1 & length(par$alpha) == 1)
#  {
#    alpha <- rep(par$alpha,G)
#  }else if(!is.null(par$alpha) & G>1 & length(par$alpha) == G)
#  {
#    alpha <- par$alpha
#  }
#  
  
#  if(is.vector(par$alpha) & length(par$alpha) == 1)
#  {
#    alpha <- rep(par$alpha,G)
#  }else if(is.vector(par$alpha) & length(par$alpha)== G)
#  {
#    alpha <- par$alpha
#  }else if(is.vector(par$alpha) & (length(par$alpha)!= 1 & length(par$alpha)!=G ) )
#    stop("The dimension of alpha does not correspond to the groups number")
  

# Eta initialization and inverse of Eta -------------------------------------------


  if( p == 1 )
  {
    eta <- numeric()
    inv_eta <- numeric()
    if(G ==1)
      {
        if(is.vector(par$eta) & length(par$eta) == 1)
          {
            eta <- par$eta
            inv_eta <- 1/eta
        }else if(is.vector(par$eta) & length(par$eta)!= 1 ) 
              stop("Dimension of parameter eta does not match having 1 group")
      }else if(G>1)
      {
        if(is.vector(par$eta) & length(par$eta) == 1)
        {
            eta <- rep(par$eta,G)
            inv_eta <-rep(1/eta,G)
        }else if(is.vector(par$eta) & length(par$eta) == G)
        { 
            eta <- par$eta
            inv_eta<- 1/eta
        }
      }
    }
  
  if(p > 1)
    {
      eta <- array(0.0,dim = c(p,p,G))
      inv_eta <- array(0.0, dim = c(p,p,G))
      if(G == 1)
      {
          if(is.vector(par$eta) & (length(par$eta) == 1 | length(par$eta) == G ) ) 
          {
            eta[,,G] <- diag(sqrt(par$eta),p)
            inv_eta[,,G] <- solve(eta[,,G])
          }else if(is.vector(par$eta) & length(par$eta) != 1 & length(par$eta) != p)
            stop("The dimension of eta is incorrect for having 1 group")
      
      }else if(G > 1)
        {
          if(is.vector(par$eta) & (length(par$eta) == 1 | length(par$eta) ==p ) )
            {
              for(g in 1:G)
              {
                eta[,,g] <- diag(sqrt(par$eta),p)
                inv_eta[,,g] <- solve(eta[,,g])
              } #end -for
            } else if (is.vector(par$eta) & length(par$eta) == G)
              {
                  for(g in 1:G)
                  {
                    eta[,,g] <- diag(sqrt(par$eta[g]),p)
                    inv_eta[,,g] <- solve(eta[,,g])
                  } #end -for
        
              } else if (is.matrix(par$eta) & nrow(par$eta)== p & ncol(par$eta) == G )
                  {
                        for(g in 1:G)
                            {
                                  eta[,,g] <- diag(sqrt(par$eta[,g]),p)
                                  inv_eta[,,g]<- solve(eta[,,g])
                            }
              } else if (is.matrix(par$eta) & nrow(par$eta) == p & ncol(par$eta) == p)
                  {
                        for(g in 1:G)
                        {
                            eta[,,g] <- diag(sqrt(diag(par$eta)),p)
                            inv_eta[,,g] <- solve(eta[,,g])
                        }
        
                  }else if(is.array(par$eta) )
                    {       
                           if( all(dim(par$eta) == c(p,p,G) ) == TRUE )
                                for(g in 1:G)
                                {
                                  eta[,,g] <- diag( sqrt(diag(par$eta[,,g])) , p)
                                  inv_eta[,,g] <- solve(eta[,,g])
                                }
                    }
        }   # end-if G  
    }# end-if p



# v initialization --------------------------------------------------------

  if(is.null(par$v))
  {
    v<- matrix(c(runif(m*G)), ncol = G, nrow = nrow(X))
  }else  v <- par$v


# Calculations ------------------------------------------------------------

  
  mg <-apply(unmap(ltrain),2,sum)
  pig <- mg/m
  sum_lig_vig <- numeric(G)
  sig <- vector("list",m)
  sum_sig <- vector("list",G)
  sum_sig_xi <- vector("list",G)
  term1 <- vector("list",m)
  term2 <- vector("list",m)
  sumTerms<- vector("list",m)
  sumSigma <-vector("list",G)
  est_sigma <- vector("list",G)
  est_alpha <- numeric(length(alpha))
  est_mu <- matrix(0.0,nrow=p,ncol=G) 

# CM-step1 ----------------------------------------------------------------

  # numerator for calculating alphag
  if(is.matrix(v) & is.matrix(l))
  {
    
    sum_lig_vig <- sapply(1:G, function(g) {sum(l[,g]*v[,g]) })
    est_alpha <- sum_lig_vig/mg

    for(g in 1:G)
    {
      
      for(i in 1:m)
      {
        if(p==1 & G == 1)
        {
          sig[[i]] <- l[i] *(   v[i,g] * inv_sigma + 
                                    (1-v[i,g]) * inv_eta * inv_sigma * inv_eta  )       
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu) * (Xtrain[i,]-mu) )
          term2[[i]] <- (1-v[i,g]) * inv_eta* (Xtrain[i,]-mu) * (Xtrain[i,]-mu) * inv_eta
          sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
        }else if (p ==1 & G>1)
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * inv_sigma[g] + 
                                    (1-v[i,g]) * inv_eta[g] * inv_sigma[g] * inv_eta[g]  )
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g]) )
          term2[[i]] <- (1-v[i,g]) * inv_eta[g]* (Xtrain[i,]-mu[g]) * (Xtrain[i,]-mu[g])* inv_eta[g]
          sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }else if (p > 1 )
        {
          sig[[i]] <- l[i,g] *(   v[i,g] * inv_sigma[,,g] + 
                                    (1-v[i,g]) * inv_eta[,,g] %*% inv_sigma[,,g] %*% inv_eta[,,g]  )  
          term1[[i]] <-v[i,g]* ( (Xtrain[i,]-mu[,g]) %*% t(Xtrain[i,]-mu[,g]) )
          term2[[i]] <- (1-v[i,g]) * (inv_eta[,,g] %*% (Xtrain[i,]-mu[,g]) %*% t(Xtrain[i,]-mu[,g]) %*% inv_eta[,,g])
          if(G == 1)  sumTerms[[i]] <- l[i] *(term1[[i]] + term2[[i]])  
          if(G > 1)   sumTerms[[i]] <- l[i,g] *(term1[[i]] + term2[[i]])  
        }
        if(i==1) 
          {
            sum_sig[[g]] <- 0
            sum_sig_xi[[g]] <-0
            sumSigma[[g]] <-0
          }  
        sum_sig[[g]]  <- sum_sig[[g]] + sig[[i]]
        sum_sig_xi[[g]] <-sum_sig_xi[[g]] + sig[[i]] %*% Xtrain[i,]
        sumSigma[[g]] <- sumSigma[[g]] + sumTerms[[i]]
      }# end-for i
      est_mu[,g] <- t(sum_sig_xi[[g]]) %*% solve(sum_sig[[g]])
      if(G==1) est_sigma[[g]] <- sumSigma[[g]]/mg
      if(G>1) est_sigma[[g]] <- sumSigma[[g]]/mg[g]
    }# end-for g          
    
  }# end-if


# CM-step2 ----------------------------------------------------------------
  factor1 <- 0
  eta_max <- 1000
  est_invSigma <- array(0.0,dim = c(p,p,G))
  est_eta <- array(0.0, dim = c(p,p,G))

   for( g in 1:G)
    {
      est_invSigma[,,g] <- solve(est_sigma[[g]])
    }
  
  for(g in 1:G)
  {
    for(j in 1:p)
    {
      factor1 <- optimize(f_etaDIF,c(1,eta_max), tol = 0.001,
                          z = l[,g], v = v[,g], X = Xtrain[,j],
                          mu = est_mu[j,g], invSigma = est_invSigma[j,j,g], 
                          maximum = TRUE)
      est_eta[j,j,g] <- factor1$maximum
    }
  }
    
  
              
  
  output <- list(mu = est_mu, sigma = est_sigma,inv_Sigma = est_invSigma,
                 alpha = est_alpha ,eta = est_eta, pig = pig, G = G)
  
  return(output)
}



#CNmixt_DifIF <- function(Xtrain,G,contamination,model,intialization,alphafix,
#                         alphamin,seed,start.z,start.v,start,label,AICcond,iter.max,
#                         threshold)

#Cnmixt_DIF <- function(X, G, model)

CNmixt_DifIF <- function(Xtrain,Xtest,ltrain,ltest,CE = "VVV",
                         niterations = 10,alpharef = 0.98, tol = 0.01)
{  
    
  # Xtrain: dataset that contains the training set
  # Xtest:  dataset that contains the test set
  # ltrain: a vector containing the label/group information of observations in training set
  # ltest : a vector containing the label/group information of observations in test set
  # CE    : a covariance structure of the covariance matrices 
  # niterations: number of maximum iterations
  # alpharef: a vector of length G with the proportion of good observations in each group
  # tol:      tolerance value to use as a measure of improvement in log likelihood
  
  # Check parameters of the function
  
  if(!is.matrix(Xtrain)) Xtrain<-as.matrix(Xtrain)
  if(!is.matrix(Xtest)) Xtest<-as.matrix(Xtest)
  
  accTest_nc <- 0.0
  accTest_c <- 0.0
  laccTest_c <- list()
  ltest_r <- list()
  output <- list()
  lmu <- list()
  lsigma <- list()
  lalpha <- list()
  leta <- list()
  diflog <- list()
  par <- list()
  p <- ncol(Xtrain)
  nobs <- nrow(Xtrain)
  G <- length(unique(ltrain))
  if(is.null(alpharef)) alpharef <- rep(0.95,G)
  if(length(alpharef)>G) stop("alpharef must be of dimension G")
  if(length(alpharef) == 1) alpharef <- rep(alpharef,G)
  if(ncol(Xtrain)==1) CE <- "E" else CE <- CE
  
  # Estimating initial parameters value for the model assuming
  # a non contaminated model using the function mstep and estep from 
  # library mclust
  mstep1 <-mclust::mstep(data = Xtrain,modelName = CE, z = unmap(ltrain))
  estep1 <- mclust::estep(data = Xtest, modelName = CE, 
                  parameters = mstep1$parameters)
  z <- estep1$z  
  lhat_nc <- apply(z,1,which.max)
  accTest_nc <- sum(lhat_nc == ltest)/length(ltest)
  
  # we are not doing vaiable selection an allow to fit the model with different 
  # inflation factors stars with p = 5 and see how it goes Supervised
  
  # Estimated initial parameters
  par$mu <- mstep1$parameters$mean
  par$sigma <- mstep1$parameters$variance$sigma
  par$G <- mstep1$parameters$variance$G
  par$pig <- apply(unmap(ltrain),2,sum)/nrow(Xtrain)
  # give initial values for alpha
  par$alpha <- alpharef
  if (p == 1 & is.vector(par$eta)) 
    { 
      par$eta <- 1.011
    }else if (p > 1 & is.vector(par$eta))
    {
      par$eta <- rep(1.011,G)
    }

  
  
  #  cat("\n","mu=",par$mu,"-","alpha=",par$alpha,"- eta=",par$eta,"\n")
  
  estep2 <- eCmn_DIF(Xtrain,ltrain,par)
  estep2_test <- eCmn_DIF(Xtest,ltest,par)
  vhat <- estep2$v
  #cat("\n","vij = ", estep2$v, "\n")
  lhat <-estep2$lhat
  par$v <- vhat  
  # Estimate parameters assuming contaminated set
  iter <- 1
  vtrain_r <- list()
  vtest_r <- list()
  logc <- list()
  lmu[[iter]] <- par$mu
  lsigma[[iter]] <- par$sigma
  leta[[iter]] <- par$eta
  vtrain_r[[iter]] <- vhat
  vtest_r[[iter]]<-estep2_test$v
  ltest_r[[iter]] <- lhat 
  
  # Create the function loglikCMN for different variables inflation factors within group
  logc[[iter]] <- loglikCMN_DIF(Xtrain, ltrain,par) 
  vtrain_r[[2]] <- matrix(-1.0, ncol = ncol(vhat), nrow(vhat))
  diflog[[iter]] <- NA
  #cat("\n","iter=",iter,";","diflog=",diflog[[iter]])
  
  
  
  
}


SSFit_DifIF<- function(Xtrain, Xtest, ltrain, ltest,
                                  vtest, model = "VVV",
                                  pnolabeled = 0,
                                  iterations = 10, 
                                 alpharef = 0.75, tol = 0.01)
  # Function SSFit_DifIF Semisupervised fitting for cases with different 
  #                       variables inflation factor within groups
  # Xtrain:       matrix with the observations used in training
  # Xtest:        matrix with the observations used in testing
  # ltrain:       labels used in training
  # ltest:        labels used in test
  # pnolabeled:   percentage of no labeled observations in ltrain 
  # model:        model used
  # iterations:   maximum number of iterations
  # alpharef:     reference for alpha
  # tol:          tolerance
{
  p <- ncol(Xtrain)
  parameters_C <- list()
  parameters_Nc <- list()
  estimate <- list()
  # logl_c: log likelihood for contaminated model
  logl_c <- 0
  obslll_c <- 0
  accTest_C <- 0
  CCRTest_C <- 0
  
  # logl_nc: log likelihood for non-contaminated model
  logl_nc <- 0
  obslll_nc <- 0
  accTest_Nc <- 0
  
  G <- length(unique(ltrain))
  
  ntrain <- length(ltrain)
  ltrain1 <- ltrain
  # nolebeled: contains the number of unlabeled
  nolabeled <- floor(pnolabeled*ntrain)
  ind_nolabeled <- sample(1:ntrain,nolabeled,replace = FALSE)
  ltrain1[ind_nolabeled] <- 0
  #table(ltrain1)
  
  if(p == 1) 
  {
    # fit Contaminated Mixture model with different variables inflation
    # factor within cluster
    res <- CNmixt(Xtrain,G,  model = model, 
                  initialization = "random.post", alphamin = alpharef,
                  label = ltrain1,iter.max = iterations)
    
  }else if(p > 1)
  {
    # fit Contaminated Mixture model with different variables inflation
    # factor within cluster
    
    res <- CNmixt(Xtrain,G,  model = model, 
                  initialization = "mixt", alphamin = alpharef,
                  label = ltrain1,iter.max = iterations)
  }
  
  #res1 <- ModelAccuracy2(Xtrain,Xtest,ltrain,ltest,"EEI")
  
  logl_nc <- res$models[[2]]$loglik
  obslll_nc <- res$models[[2]]$obslll
  
  logl_c <- res$models[[1]]$loglik
  obslll_c <- res$models[[1]]$obslll
  
  parameters_C$G <- res$models[[1]]$G
  parameters_C$pig <- res$models[[1]]$prior
  parameters_C$mu <- res$models[[1]]$mu
  parameters_C$Sigma <-res$models[[1]]$Sigma
  parameters_C$InvSigma <- res$models[[1]]$invSigma
  parameters_C$alpha <-res$models[[1]]$alpha
  parameters_C$eta <- res$models[[1]]$eta
  
  # estimate contaminated model  
  estimate$ztrain_hat <- res$models[[1]]$posterior
  # estimated class labels
  estimate$ltrain_hat <- res$models[[1]]$group
  estimate$vtrain_hat <- res$models[[1]]$v
  estimate$badPoints <- res$models[[1]]$detection
  
  parameters_Nc$pro <- res$models[[2]]$prior
  parameters_Nc$mean <- res$models[[2]]$mean
  parameters_Nc$variance <- res$models[[2]]$Sigma
  
  table(ltrain1,res$models[[1]]$group)
  
  if(ncol(Xtrain) == 1)
  {
    mstep_nc <- mclust::mstep( data = as.matrix(Xtrain), modelName = model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =as.matrix(Xtest), modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }else if(ncol(Xtrain) > 1)
  {
    mstep_nc <- mclust::mstep( data = Xtrain, modelName = model, z = unmap(estimate$ltrain_hat) )
    estep_nc <- mclust::estep(data =Xtest, modelName =  mstep_nc$modelName, 
                              parameters = mstep_nc$parameters)
    
  }
  mstep_nc$modelName
  mstep_nc$parameters
  
  ltest_hat_nc <- apply(estep_nc$z,1,which.max)
  CCRTest_Nc <- sum((ltest_hat_nc == ltest)) / length(ltest)
  CCRTest_Nc
  
  ExpectedValues_C <- eCmn_DIF(Xtest,ltest,parameters_C)
  
  if (length(ExpectedValues_C$lhat)==length(ltest)){
    CCRTest_C <- sum((ExpectedValues_C$lhat == ltest)) / length(ltest)
  }  else CCRTest_C = -1 
  
  if (length(ExpectedValues_C$vhat) == length(vtest))
  {
    AccTest_C <- sum((ExpectedValues_C$vhat == vtest)) / length(vtest)
  } else AccTest_C =  - 1
  
  res$models[[1]]$v
  
  res$models[[1]]$label
  res$models[[1]]$entropy
  res$models[[1]]$IC
  
  table(res$models[[1]]$label,res$models[[1]]$group)
  table(ltrain,res$models[[1]]$group)
  table(ltrain1,ltrain)
  length(res$models[[1]]$group)
  
  
  output <- list(CCRTestNc = CCRTest_Nc,CCRTestC = CCRTest_C,
                 ztest_hat_NC = estep_nc$z,
                 ltest_hat_NC =  ltest_hat_nc,
                 ztest_hat_C = ExpectedValues_C$z,
                 ltest_hat_C = ExpectedValues_C$lhat,
                 Expected_v = ExpectedValues_C$v,
                 vtest_hat = ExpectedValues_C$vhat,
                 niterations = iterations, par = parameters_C)
  return(output)  
}

