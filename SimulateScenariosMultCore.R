
SimScenario <- function(Sets, nruns,pathOutput)
{
  #-----------------------------------------------------------------------------
  # Function: SimScenario
  # Goal: Simulate n runs of a particular scenario/set.
  #-----------------------------------------------------------------------------
  # Sets: a matrix of 1 row with contains all the parameters for the simulations.  
  # nruns: number of simulations per scenario to carry out.
  # PathOutput: the path where the files will be stored.
  
  GenData <- vector("list",10)
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
    cat("\n The file already exists in the directory \n")
    return("File already exists in the directory")
#    return(0);
  } # end-if generate files
   return(c("File no generated"));
}




CreateSimFilesMultCore <- function(pathOutput,nruns)
  # Create all possible combinations of simulated datasets
  # F1 : Mean distance
  # F2 : Number of classes
  # F3 : class proportion matrix with the number of columns equals to F2
  # F4 : Number of observations
  # F5 : Number of variables
  # F6 : Percentage of samples used as training
  # F7 : Correlation structure
  # F8 : Percentage of non contaminated samples (alpha)
  # F9 : Variance inflation factor
  # F10: number of separating variables
# pathOutput: Location where the simulated files will be saved
# nruns: Number of runs for each datasets
{
  
  F1 <- c("VD","MD","VO") # Mean distance
  F2 <- c(2,3) # Number of classes
  #  F3 <- list(matrix(c(0.5,0.5,0.9,0.1),nrow = 2, ncol = 2, byrow = TRUE),
  #             matrix(c(0.33,0.33,0.33,0.70,0.15,0.15), nrow = 2, ncol = 3, byrow = TRUE ))
  F3 <- c("BAL","INB") #class proportion matrix with the number of columns equals to F2
  F4 <- c(3000,4000) # Number of observations
  F5 <- c(5,100) # Number of variables
  F6 <- c(0.75,0.85) # Percentage of samples used as training
  F7 <- c("SCBSV","SCBNSV","SCBSNSV","IND") # Correlation structure
  F8 <- c(0.8,0.9) # Percentage of non contaminated samples (alpha)
  F9 <- c(20,30) # Variance inflation factor
  F10 <- c(2,3) # number of separating variables
  # posibilities
  # possibilities for 2 classes
  Sets2 <- as.matrix(expand.grid(2,F10,F5,F4,F3,F1,F7,F6,F8,F8,0,F9,F9,0))
  Sets3 <- as.matrix(expand.grid(3,F10,F5,F4,F3,F1,F7,F6,F8,F8,F8,F9,F9,F9))
  Scenarios <- rbind(Sets2,Sets3)
  nrow(Sets2)
  #  Sets
  n <- nrow(Scenarios)
  i <- 1
  #  Sets[1,]
  nruns<-10
  GenData <- vector("list",nruns)
  
  aux <- vector("list",n)
  
  for(i in 1:n) {aux[[i]] <- as.list(Scenarios[i,])}
  aux
  sapply(1:n,function(i) aux[[i]] <- as.list(Scenarios[i,]))
  
  aux[[1]]
  
  SimScenario(Scenarios[27,],10,pathOutput)
  
  SimStatus <- mclapply(1:3, function(i) 
    {
    SimProgress <- SimScenario(aux[[i]],10,pathOutput)
    },mc.cores = 2)
  
  
    for(i in 1:n) 
  {
    generatefiles <- dir(pathOutput)
    if( as.numeric(Sets[i,1]) == 2) # Number of classes
    {
      dfname <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                       as.numeric(Sets[i,3]),"_",
                       as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                       Sets[i,5],"_",Sets[i,7],"_",
                       Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                       as.numeric(Sets[i,10])*100,"_E",
                       as.numeric(Sets[i,12]), as.numeric(Sets[i,13]))
      
    }else if (as.numeric(Sets[i,1]) == 3)
    {
      dfname <- paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
                       as.numeric(Sets[i,3]),"_",
                       as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
                       Sets[i,5],"_",Sets[i,7],"_",
                       Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
                       as.numeric(Sets[i,10])*100,as.numeric(Sets[i,11])*100,
                       "_E",as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),
                       as.numeric(Sets[i,14]))
    }
    
    if(length(generatefiles) == 0 | 
       (length(generatefiles)>0 & 
        !any(str_detect(generatefiles,paste0(dfname,"_",nruns,".RDS")))==TRUE  ))
    { # Run scenarios if the scenario is not already in a file
      
      for(i_run in 1: nruns)
      {
        cat("\nScenario: ",i, "-","Run: ", i_run,"\n" )
        sg <- array(0,dim= c(as.numeric(Sets[i,3]),as.numeric(Sets[i,3]),as.numeric(Sets[i,1]) ))
        mu <- matrix(0,nrow = as.numeric(Sets[i,3]), ncol = as.numeric(Sets[i,1]))
        if(as.numeric(Sets[i,1]) == 2) # Begin Number of classes
        {  
          f2 = 2
          sg[,,1] <- diag(1,as.numeric(Sets[i,3]))
          sg[,,2] <- diag(1,as.numeric(Sets[i,3]))
          
          if(as.numeric(Sets[i,2]) == 2) # Number of separating variables
          {
            f10 = 2
            if(as.numeric(Sets[i,3]) == 5) # Number of variables
            {
              f5 = 5
              mu[,1] <- rep(0,f5)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
              
              
            } else if(as.numeric(Sets[i,3]) == 100) # Number of variables is 100
            {
              f5 = 100
              mu[,1] <- rep(0,100)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0,rep(0,05))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
              
              
            }  # End-if number of variables
            
          } else if(as.numeric(Sets[i,2]) == 3)
          {
            f10 = 3
            if(as.numeric(Sets[i,3]) == 5) # Number of variables
            {
              f5 = 5
              mu[,1] <- rep(0,f5)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
              
              
            } else if(as.numeric(Sets[i,3]) == 100) # Number of variables is 100
            {
              f5 = 100
              mu[,1] <- rep(0,100)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6,rep(0,05))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
            }  # End-if number of variables
            
          }# End-if number of separating variables
          
        } else if(as.numeric(Sets[i,1]) == 3)  # 3 Classes
        {
          
          f2 = 3
          sg[,,1] <- diag(1,as.numeric(Sets[i,3]))
          sg[,,2] <- diag(1,as.numeric(Sets[i,3]))
          
          if(as.numeric(Sets[i,2]) == 2) # Number of separating variables
          {
            f10 = 2
            if(as.numeric(Sets[i,3]) == 5) # Number of variables
            {
              f5 = 5
              mu[,1] <- rep(0,f5)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0)
                  mu[,3] <- c(0,-6,0,-6,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0) 
                  mu[,3] <- c(0,-3,0,-3,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0)
                  mu[,3] <- c(0,-1.5,0,-1.5,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0)
                  mu[,3] <- c(0,-6,0,-6,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0) 
                  mu[,3] <- c(0,-3,0,-3,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0)
                  mu[,3] <- c(0,-1.5,0,-1.5,0)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
              
              
            } else if(as.numeric(Sets[i,3]) == 100) # Number of variables is 100
            {
              f5 = 100
              mu[,1] <- rep(0,100)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0,rep(0,95))
                  mu[,3 ]<- c(0,-6,0,-6,0,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0,rep(0,95))
                  mu[,3] <- c(0,-3,0,-3,0,rep(0,95)) 
                  
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                  mu[,3] <- c(0,-1.5,0,-1.5,0,rep(0,95))
                  
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,0,rep(0,05))
                  mu[,3] <- c(0,-6,0,-6,0,rep(0,05))
                  
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,0,rep(0,95)) 
                  mu[,3] <- c(0,-3,0,-3,0,rep(0,95)) 
                  
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,0,rep(0,95))
                  mu[,3] <- c(0,-1.5,0,-1.5,0,rep(0,95))
                  
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
              
              
            }  # End-if number of variables
            
          } else if(as.numeric(Sets[i,2]) == 3) # begin number of separating variables
          {
            f10 = 3
            if(as.numeric(Sets[i,3]) == 5) # Number of variables
            {
              f5 = 5
              mu[,1] <- rep(0,f5)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6)
                  mu[,2] <- c(0,-6,0,-6,-6)
                  
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3) 
                  mu[,3] <- c(0,-3,0,-3,-3) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5)
                  mu[,3] <- c(0,-1.5,0,-1.5,-1.5)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6)
                  mu[,3] <- c(0,-6,0,-6,-6)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3) 
                  mu[,3] <- c(0,-3,0,-3,-3) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5)
                  mu[,3] <- c(0,-1.5,0,-1.5,-1.5)
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
              
              
            } else if(as.numeric(Sets[i,3]) == 100) # Number of variables is 100
            {
              f5 = 100
              mu[,1] <- rep(0,100)
              if(Sets[i,5] == "BAL") # Class proportion
              {
                f3 = c(0.5,0.5)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6,rep(0,95))
                  mu[,3] <- c(0,-6,0,-6,-6,rep(0,95))
                  
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  } # End-if Covariance structure
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                  mu[,3] <- c(0,-3,0,-3,-3,rep(0,95)) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                  mu[,3] <- c(0,-1.5,0,-1.5,-1.5,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance VO
                
              }else if(Sets[i,5] == "INB")
              {
                f3 = c(0.9,0.1)
                if (Sets[i,6] == "VD") # Mean distance
                {
                  mu[,2] <- c(0,6,0,6,6,rep(0,05))
                  mu[,3] <- c(0,-6,0,-6,-6,rep(0,05))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,1]) = 2
                    
                  }
                  
                }else if(Sets[i,6] == "MD") 
                {
                  mu[,2] <- c(0,3,0,3,3,rep(0,95)) 
                  mu[,3] <- c(0,-3,0,-3,-3,rep(0,95)) 
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                  
                  
                }else if(Sets[i,6] == "VO")
                {
                  mu[,2] <- c(0,1.5,0,1.5,1.5,rep(0,95))
                  mu[,3] <- c(0,-1.5,0,-1.5,-1.5,rep(0,95))
                  if (Sets[i,7] == "SCBSV")
                  {
                    sg[2,4,1] = 0.8
                    sg[4,2,1] = 0.8
                    
                    sg[2,4,2] = 0.8
                    sg[4,2,2] = 0.8
                  }else if (Sets[i,7] == "SCBNSV")
                  {
                    sg[1,3,1] = 0.8
                    sg[3,1,1] = 0.8
                    
                    sg[1,3,2] = 0.8
                    sg[3,1,2] = 0.8
                    
                  }else if (Sets[i,7] == "SCBSNSV")
                  {
                    sg[1,2,1] = 0.8
                    sg[2,1,1] = 0.8
                    
                    sg[1,2,2] = 0.8
                    sg[2,1,2] = 0.8
                    
                  }else if(Sets[i,7]== "IND")
                  {
                    diag(sg[,,1]) = 1
                    diag(sg[,,2]) = 1
                    
                  } # End-if Covariance structure IND
                }  # End-if Mean distance
              } # End-if Class proportion 
            }  # End-if number of variables
            
          }# End-if number of separating variables
          
          
        }# Number of classes
        
        
        f4 <- as.numeric(Sets[i,4]) # number of observations to be generated
        f6 <- as.numeric(Sets[i,8]) # proportion of training samples
        if (f2 == 2)
        {
          f8 <- c(as.numeric(Sets[i,9]),as.numeric(Sets[i,10])) # alpha's
          f9 <- c(as.numeric(Sets[i,12]),as.numeric(Sets[i,13])) # eta's
        }else if(f2 == 3)
        {
          f8 <- c(as.numeric(Sets[i,9]),as.numeric(Sets[i,10]),as.numeric(Sets[i,11])) # alpha's
          f9 <- c(as.numeric(Sets[i,12]),as.numeric(Sets[i,13]),as.numeric(Sets[i,14])) # eta's
          
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
    }else
    {
      cat("\n The file already exists in the directory \n")
    } # End-if generate files    
    
  }# End-for scenarios 
  
} # End function

