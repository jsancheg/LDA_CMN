# Contain SimScenario function which simulate 1 scenario per time 
# and it can be used in parallelization process


# Simulate scenarios in a pool --------------------------------------------
library(tools)


SimScenario <- function(Sets, nruns,pathOutput)
{
  #-----------------------------------------------------------------------------
  # Function: SimScenario
  # Goal: Simulate n runs of a particular scenario/set.
  #-----------------------------------------------------------------------------
  # Sets: a matrix of 1 row with contains all the parameters for the simulations in 
  # the following order (1) number of classes, (2) number of separating variables,
  # (3) number of variables,  (4) number of observations to be generated, 
  # (5) class proportion, (6) mean distance, (7) Covariance structure,
  # (8) proportion of training samples, (9) alpha class 1, (10) alpha class 2,
  # (11) alpha class 3, (12) eta class 1, (13) eta class 2, 
  # (14) eta class 3. 
  # example c("2","2","100","3000","BAL","MD","SCBSV","0.75","0.8","0.8","0","20","20","0")
  # nruns: number of simulations per scenario to carry out.
  # PathOutput: the path where the files will be stored.
  
  GenData <- vector("list",nruns)
  if (!is.list(Sets)) Sets <- as.list(Sets)
  generatefiles <- dir(pathOutput)
  
  f3 <- numeric() # class proportion
  f4 <- numeric() # number of observations to be generated
  f6 <- numeric() # proportion of training samples
  f8 <- numeric() # alpha's
  f9 <- numeric() # eta's
  
  
  i <- 1
  
  if( as.numeric(Sets[1]) == 2) # Number of classes
  {
    dfname <- paste0("S_",as.numeric(Sets[1]),"_",as.numeric(Sets[2]),"_",
                     as.numeric(Sets[3]),"_",
                     as.numeric(Sets[4]),"_",as.numeric(Sets[8])*100,"_",
                     Sets[5],"_",Sets[7],"_",
                     Sets[6],"_A",as.numeric(Sets[9])*100,"_", 
                     as.numeric(Sets[10])*100,"_E",
                     as.numeric(Sets[12]), "_",as.numeric(Sets[13]))
    
  }else if (as.numeric(Sets[1]) == 3)
  {
    dfname <- paste0("S_",as.numeric(Sets[1]),"_",as.numeric(Sets[2]),"_",
                     as.numeric(Sets[3]),"_",
                     as.numeric(Sets[4]),"_",as.numeric(Sets[8])*100,"_",
                     Sets[5],"_",Sets[7],"_",
                     Sets[6],"_A",as.numeric(Sets[9])*100, "_",
                     as.numeric(Sets[10])*100, "_", as.numeric(Sets[11])*100,
                     "_E",as.numeric(Sets[12]), "_", as.numeric(Sets[13]),"_",
                     as.numeric(Sets[14]))
  }
  
  if(length(generatefiles) == 0 | 
     (length(generatefiles)>0 & 
      !any(str_detect(generatefiles,paste0(dfname,"_",nruns,".RDS")))==TRUE  ))
    # Run scenarios if the scenario is not already in a file
  {
    for(i_run in 1: nruns)
    {
      cat("\nScenario: ",dfname, "-","Run: ", i_run,"\n" )
      sg <- array(0,dim= c(as.numeric(Sets[3]),as.numeric(Sets[3]),as.numeric(Sets[1]) ))
      mu <- matrix(0,nrow = as.numeric(Sets[3]), ncol = as.numeric(Sets[1]))
      if(as.numeric(Sets[1]) == 2) # Begin Number of classes
      {  
        f2 = 2
        sg[,,1] <- diag(1,as.numeric(Sets[3]))
        sg[,,2] <- diag(1,as.numeric(Sets[3]))
        
        if(as.numeric(Sets[2]) == 2) # Number of separating variables
        {
          f10 = 2
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,05))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          }  # End-if number of variables
          
        } else if(as.numeric(Sets[2]) == 3)
        {
          f10 = 3
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,05))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
          }  # End-if number of variables
          
        }# End-if number of separating variables
        
      } else if(as.numeric(Sets[1]) == 3)  # 3 Classes
      {
        
        f2 = 3
        sg[,,1] <- diag(1,as.numeric(Sets[3]))
        sg[,,2] <- diag(1,as.numeric(Sets[3]))
        
        if(as.numeric(Sets[2]) == 2) # Number of separating variables
        {
          f10 = 2
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                mu[,3] <- c(0,-6,0,-6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                mu[,3] <- c(0,-3,0,-3,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                mu[,3] <- c(0,-1.5,0,-1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                mu[,3] <- c(0,-6,0,-6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                mu[,3] <- c(0,-3,0,-3,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                mu[,3] <- c(0,-1.5,0,-1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,95))
                mu[,3 ]<- c(0,-6,0,-6,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95))
                mu[,3] <- c(0,-3,0,-3,0,rep(0,95)) 
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,0,rep(0,95))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,05))
                mu[,3] <- c(0,-6,0,-6,0,rep(0,05))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                mu[,3] <- c(0,-3,0,-3,0,rep(0,95)) 
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,0,rep(0,95))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          }  # End-if number of variables
          
        } else if(as.numeric(Sets[2]) == 3) # begin number of separating variables
        {
          f10 = 3
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                mu[,2] <- c(0,-6,0,-6,-6)
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                mu[,3] <- c(0,-3,0,-3,-3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                mu[,3] <- c(0,-6,0,-6,-6)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                mu[,3] <- c(0,-3,0,-3,-3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,95))
                mu[,3] <- c(0,-6,0,-6,-6,rep(0,95))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                mu[,3] <- c(0,-3,0,-3,-3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,05))
                mu[,3] <- c(0,-6,0,-6,-6,rep(0,05))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                mu[,3] <- c(0,-3,0,-3,-3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
          }  # End-if number of variables
          
        }# End-if number of separating variables
        
        
      }# Number of classes
      
      
      f4 <- as.numeric(Sets[4]) # number of observations to be generated
      f6 <- as.numeric(Sets[8]) # proportion of training samples
      if (f2 == 2)
      {
        f8 <- c(as.numeric(Sets[9]),as.numeric(Sets[10])) # alpha's
        f9 <- c(as.numeric(Sets[12]),as.numeric(Sets[13])) # eta's
      }else if(f2 == 3)
      {
        f8 <- c(as.numeric(Sets[9]),as.numeric(Sets[10]),as.numeric(Sets[11])) # alpha's
        f9 <- c(as.numeric(Sets[12]),as.numeric(Sets[13]),as.numeric(Sets[14])) # eta's
        
      }    
      GenData[[i_run]] <- SimGClasses(mu,sg,f3,f4,f6,f8,f9)
      
    } # End-for replicates
    parameters <- list(G = f2, mu = mu, sigma = sg, 
                       ClassProportion = f3,
                       alpha = f8,
                       eta = f9,
                       NumberObservations = f4,
                       ProportionTraining = f6)
    output <- list(GenData = GenData, par = parameters)
    saveRDS(output,paste0(pathOutput,dfname,"_",nruns,".RDS"))
    cat("\n writing file: ",paste0(dfname,"_",nruns,".RDS") ,"\n")
    return(paste0(dfname,"_",nruns,".RDS"))
    #return(1);    
  }else
  {
#    cat("\n The file already exists in the directory \n")
#    return("File already exists in the directory")
    #    return(0);
  } # end-if generate files
  return(c("File no generated"));
}



# Simulate Individual Scenarios -------------------------------------------

# Contain SimScenario function which simulate 1 scenario per time 
# and it can be used in parallelization process

SimScenario_Individual <- function(Sets, nruns,pathOutput)
{
  #-----------------------------------------------------------------------------
  # Function: SimScenario
  # Goal: Simulate n runs of a particular scenario/set.
  #-----------------------------------------------------------------------------
  # Sets: a matrix of 1 row with contains all the parameters for the simulations in 
  # the following order (1) number of classes, (2) number of separating variables,
  # (3) number of variables,  (4) number of observations to be generated, 
  # (5) class proportion, (6) mean distance, (7) Covariance structure,
  # (8) proportion of training samples, (9) alpha class 1, (10) alpha class 2,
  # (11) alpha class 3, (12) eta class 1, (13) eta class 2, 
  # (14) eta class 3. 
  # example c("2","2","100","3000","BAL","MD","SCBSV","0.75","0.8","0.8","0","20","20","0")
  # nruns: number of simulations per scenario to carry out.
  # PathOutput: the path where the files will be stored.
  
  GenData <- vector("list",1)
  if (!is.list(Sets)) Sets <- as.list(Sets)
  generatefiles <- dir(pathOutput)
  
  f3 <- numeric() # class proportion
  f4 <- numeric() # number of observations to be generated
  f6 <- numeric() # proportion of training samples
  f8 <- numeric() # alpha's
  f9 <- numeric() # eta's
  
  
  i <- 1
  
  if( as.numeric(Sets[1]) == 2) # Number of classes
  {
    dfname <- paste0("S_",as.numeric(Sets[1]),"_",as.numeric(Sets[2]),"_",
                     as.numeric(Sets[3]),"_",
                     as.numeric(Sets[4]),"_",as.numeric(Sets[8])*100,"_",
                     Sets[5],"_",Sets[7],"_",
                     Sets[6],"_A",as.numeric(Sets[9])*100, 
                     as.numeric(Sets[10])*100,"_E",
                     as.numeric(Sets[12]), as.numeric(Sets[13]))
    
  }else if (as.numeric(Sets[1]) == 3)
  {
    dfname <- paste0("S_",as.numeric(Sets[1]),"_",as.numeric(Sets[2]),"_",
                     as.numeric(Sets[3]),"_",
                     as.numeric(Sets[4]),"_",as.numeric(Sets[8])*100,"_",
                     Sets[5],"_",Sets[7],"_",
                     Sets[6],"_A",as.numeric(Sets[9])*100, 
                     as.numeric(Sets[10])*100,as.numeric(Sets[11])*100,
                     "_E",as.numeric(Sets[12]), as.numeric(Sets[13]),
                     as.numeric(Sets[14]))
  }
  
  if(length(generatefiles) == 0 | 
     (length(generatefiles)>0 & 
      !any(str_detect(generatefiles,paste0(dfname,"_",nruns,".RDS")))==TRUE  ))
    # Run scenarios if the scenario is not already in a file
  {
    for(i_run in 1: nruns)
    {
      cat("\nScenario: ",dfname, "-","Run: ", i_run,"\n" )
      sg <- array(0,dim= c(as.numeric(Sets[3]),as.numeric(Sets[3]),as.numeric(Sets[1]) ))
      mu <- matrix(0,nrow = as.numeric(Sets[3]), ncol = as.numeric(Sets[1]))
      if(as.numeric(Sets[1]) == 2) # Begin Number of classes
      {  
        f2 = 2
        sg[,,1] <- diag(1,as.numeric(Sets[3]))
        sg[,,2] <- diag(1,as.numeric(Sets[3]))
        
        if(as.numeric(Sets[2]) == 2) # Number of separating variables
        {
          f10 = 2
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,05))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          }  # End-if number of variables
          
        } else if(as.numeric(Sets[2]) == 3)
        {
          f10 = 3
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,05))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
          }  # End-if number of variables
          
        }# End-if number of separating variables
        
      } else if(as.numeric(Sets[1]) == 3)  # 3 Classes
      {
        
        f2 = 3
        sg[,,1] <- diag(1,as.numeric(Sets[3]))
        sg[,,2] <- diag(1,as.numeric(Sets[3]))
        
        if(as.numeric(Sets[2]) == 2) # Number of separating variables
        {
          f10 = 2
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                mu[,3] <- c(0,-6,0,-6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                mu[,3] <- c(0,-3,0,-3,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                mu[,3] <- c(0,-1.5,0,-1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0)
                mu[,3] <- c(0,-6,0,-6,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0) 
                mu[,3] <- c(0,-3,0,-3,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0)
                mu[,3] <- c(0,-1.5,0,-1.5,0)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,95))
                mu[,3 ]<- c(0,-6,0,-6,0,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95))
                mu[,3] <- c(0,-3,0,-3,0,rep(0,95)) 
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,0,rep(0,95))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,0,rep(0,05))
                mu[,3] <- c(0,-6,0,-6,0,rep(0,05))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                mu[,3] <- c(0,-3,0,-3,0,rep(0,95)) 
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,0,rep(0,95))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          }  # End-if number of variables
          
        } else if(as.numeric(Sets[2]) == 3) # begin number of separating variables
        {
          f10 = 3
          if(as.numeric(Sets[3]) == 5) # Number of variables
          {
            f5 = 5
            mu[,1] <- rep(0,f5)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                mu[,2] <- c(0,-6,0,-6,-6)
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                mu[,3] <- c(0,-3,0,-3,-3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6)
                mu[,3] <- c(0,-6,0,-6,-6)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3) 
                mu[,3] <- c(0,-3,0,-3,-3) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5)
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5)
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
            
            
          } else if(as.numeric(Sets[3]) == 100) # Number of variables is 100
          {
            f5 = 100
            mu[,1] <- rep(0,100)
            if(Sets[5] == "BAL") # Class proportion
            {
              f3 = c(0.5,0.5)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,95))
                mu[,3] <- c(0,-6,0,-6,-6,rep(0,95))
                
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                } # End-if Covariance structure
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                mu[,3] <- c(0,-3,0,-3,-3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance VO
              
            }else if(Sets[5] == "INB")
            {
              f3 = c(0.9,0.1)
              if (Sets[6] == "VD") # Mean distance
              {
                mu[,2] <- c(0,6,0,6,6,rep(0,05))
                mu[,3] <- c(0,-6,0,-6,-6,rep(0,05))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,1]) = 2
                  
                }
                
              }else if(Sets[6] == "MD") 
              {
                mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                mu[,3] <- c(0,-3,0,-3,-3,rep(0,95)) 
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
                
                
              }else if(Sets[6] == "VO")
              {
                mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                mu[,3] <- c(0,-1.5,0,-1.5,-1.5,rep(0,95))
                if (Sets[7] == "SCBSV")
                {
                  sg[2,4,1] = 0.8
                  sg[4,2,1] = 0.8
                  
                  sg[2,4,2] = 0.8
                  sg[4,2,2] = 0.8
                }else if (Sets[7] == "SCBNSV")
                {
                  sg[1,3,1] = 0.8
                  sg[3,1,1] = 0.8
                  
                  sg[1,3,2] = 0.8
                  sg[3,1,2] = 0.8
                  
                }else if (Sets[7] == "SCBSNSV")
                {
                  sg[1,2,1] = 0.8
                  sg[2,1,1] = 0.8
                  
                  sg[1,2,2] = 0.8
                  sg[2,1,2] = 0.8
                  
                }else if(Sets[7]== "IND")
                {
                  diag(sg[,,1]) = 1
                  diag(sg[,,2]) = 1
                  
                } # End-if Covariance structure IND
              }  # End-if Mean distance
            } # End-if Class proportion 
          }  # End-if number of variables
          
        }# End-if number of separating variables
        
        
      }# Number of classes
      
      
      f4 <- as.numeric(Sets[4]) # number of observations to be generated
      f6 <- as.numeric(Sets[8]) # proportion of training samples
      if (f2 == 2)
      {
        f8 <- c(as.numeric(Sets[9]),as.numeric(Sets[10])) # alpha's
        f9 <- c(as.numeric(Sets[12]),as.numeric(Sets[13])) # eta's
      }else if(f2 == 3)
      {
        f8 <- c(as.numeric(Sets[9]),as.numeric(Sets[10]),as.numeric(Sets[11])) # alpha's
        f9 <- c(as.numeric(Sets[12]),as.numeric(Sets[13]),as.numeric(Sets[14])) # eta's
        
      }    
      GenData[[1]] <- SimGClasses(mu,sg,f3,f4,f6,f8,f9)
      parameters <- list(G = f2, mu = mu, sigma = sg, 
                         ClassProportion = f3,
                         alpha = f8,
                         eta = f9,
                         NumberObservations = f4,
                         ProportionTraining = f6)
      
      output <- list(GenData = GenData, par = parameters)
      saveRDS(output,paste0(pathOutput,dfname,"_",i_run,".RDS"))
      cat("\n writing file: ",paste0(dfname,"_",nruns,".RDS") ,"\n")
      
    } # End-for replicates
#    output <- list(GenData = GenData, par = parameters)
#    saveRDS(output,paste0(pathOutput,dfname,"_",nruns,".RDS"))
    return(paste0(dfname,"_",nruns,".RDS"))
    #return(1);    
  }else
  {
    #    cat("\n The file already exists in the directory \n")
    #    return("File already exists in the directory")
    #    return(0);
  } # end-if generate files
  return(c("File no generated"));
}


get_factors_from_file_name <- function(file_name )
{
  aux_Factors <- str_split(file_name,"_",simplify = TRUE)
  aux_Factors1 <- as.character(14)
  Scenarios_Factors <- as.character()
  temp <- as.character()
  
  if(length(aux_Factors)<11)
    {
      stop("\n Check the name of the file because it does not fill the expected structure \n")
    }else  aux_Factors1[1:8] <- aux_Factors[2:9]

    aux_Factors1
    
    if(str_length(aux_Factors[10]) == 5)
    {
      aux_Factors1[9] <- as.numeric(str_sub(aux_Factors[10],2,3))/100 
      aux_Factors1[10] <- as.numeric(str_sub(aux_Factors[10],4,5))/100
      aux_Factors1[11] <- 0
    } else if(str_length(aux_Factors[10]) == 7)
    {
      aux_Factors1[9] <- as.numeric(str_sub(aux_Factors[10],2,3))/100 
      aux_Factors1[10] <- as.numeric(str_sub(aux_Factors[10],4,5))/100
      aux_Factors1[11] <- as.numeric(str_sub(aux_Factors[10],6,7))/100
      
    }
      
    if(str_length(aux_Factors[11]) == 4)
    {
      aux_Factors1[12] <- str_sub(aux_Factors[11],2,2) 
      aux_Factors1[13] <- str_sub(aux_Factors[11],3,4)
      aux_Factors1[14] <- 0
    } else if(str_length(aux_Factors[11]) == 5)
    {
      aux_Factors1[12] <- str_sub(aux_Factors[11],2,2) 
      aux_Factors1[13] <- str_sub(aux_Factors[11],3,3)
      aux_Factors1[14] <- str_sub(aux_Factors[11],4,5)
    }
      
      Scenarios_Factors[1:4] <- aux_Factors1[1:4]  
      
      
      Scenarios_Factors[5] <- aux_Factors1[6]
      Scenarios_Factors[6] <- aux_Factors1[8]
      Scenarios_Factors[7] <- aux_Factors1[7]
      Scenarios_Factors[8] <- as.numeric(aux_Factors1[5])/100
      Scenarios_Factors[9:14] <- aux_Factors1[9:14]
      
      return(Scenarios_Factors)  
}

Deconstruct_Pool_Scenarios <- function(file_name,pathFile,pathOutput)
{
  # The goal of the Deconstruc_Pool_Scenarios is to create a separated file 
  # for each simulation in file_name
  # file_name: File with RDS extension that contains the simulations
  # pathFile:  Path where this file has been saved
  
  # read the file
  
  fileRDS <- readRDS(paste0(pathFile,file_name))
  
  # count number of simulations 
  n_simulations <- length(fileRDS$GenData)
  
  # Save simulation parameters
  simulation_parameters <- fileRDS$par

  output_file_name <- file_path_sans_ext(file_name)
  
  output_file_name
  
  create_individual_files <- function(simulation_number,pooled_data,simulation_parameters,output_file_name,pathOutput)
    {
        data_simulated <- vector("list",1)
        data_simulated <- pooled_data[[simulation_number]]
        output <- list(GenData = list(data_simulated), par  = simulation_parameters)
        saveRDS(output,paste0(pathOutput,output_file_name,"_",simulation_number,".RDS") )
        return(1)
    }
  
    status <- sapply(1:n_simulations,function(i) 
           { 
                            create_individual_files(i,fileRDS$GenData,simulation_parameters,output_file_name,pathOutput)
                            
             } )
    
    return(unlist(status))                                                         
}
  