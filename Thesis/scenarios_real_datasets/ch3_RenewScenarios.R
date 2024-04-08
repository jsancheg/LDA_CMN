
# 
getwd()
source(paste0(getwd(),"/Thesis/create_paths.R"))
library(fs)
library(tools)
library(stringr)

pathOrigen <- paste0(getwd(),"/Proc_CrabsNew/") 
files <- dir(pathOrigen)
head(files,5)

pathOutput <- paste0(getwd(),"/Thesis/Plasmode/crabs/")

i <- 1
for (namefile in files)
{
    
                name_scenario <- tools::file_path_sans_ext(paste0(pathOrigen,namefile))
                name_archivo <- basename(name_scenario)
          if(name_archivo != "MetricsContDf")
          {
                output <- list()
                name_scenario  
                df <- vector("list",1)
                
                  file_data <- load(paste0(name_scenario,".Rdata"))
                  nsim <- length(auxSim$Train)
                  #datos <- vector(nsim,"list")
                  
                  for (i in 1: nsim)
                  {
                    
                  
                             auxX <- auxSim$Train[[i]]
                            
                              
                             Xtrain <- as.matrix(auxSim$Train[[i]][,-c(1,2,8)])
                             Xtest <- as.matrix(auxSim$Test[[i]][,-c(1,2,8)])
                             ltrain <- auxSim$Train[[i]]$class
                             vtrain <- auxSim$Train[[i]]$Cont
                             
                             ltest <- auxSim$Test[[i]]$class
                             vtest <- auxSim$Test[[i]]$Cont
                             
                             X <- rbind(Xtrain,Xtest)
                             l <- c(ltrain,ltest)
                             v <- c(vtrain,vtest)
                             
                             ind <- as.numeric( rownames(Xtrain))
                            
                             G = length(unique(l))
                             p <- ncol(X)
                             NumberObservations <- nrow(X)
                             ProportionTraining <- length(ltrain)/length(l)
                             ClassProportion <- apply(unmap(ltrain),2,sum )/length(ltrain) 
                             aux_mu <- sapply(1:G, function(g) matrix(apply(X[l==g,], 2, mean ), nrow = p, ncol = G , byrow = TRUE)  )
                             aux_sigma <- sapply(1:G, function(g) matrix(apply(X[l==g,], 2, mean ), nrow = p, ncol = p,  byrow = TRUE )  )
                             aux_var <- lapply(1:G, function(g) matrix(var(X[l==g,]),nrow = p, ncol = p, byrow = TRUE  ))
                             
                             aux_var <- array(0.0, dim = c(p,p,G))
                             
                             
                             
                             for( g in 1:G) aux_var[,,g] <- sd(X[l==g],)
                             

                             aux_name <- str_split(files[i],"_",simplify = TRUE)
                             aux_alpha <- numeric(G)
                             aux_eta <- numeric(G)
                             if(G == 2)
                             {
                                aux_alpha[1]  = as.numeric(gsub("A","",aux_name[,1])) / 100
                                aux_alpha[2]  = as.numeric(aux_name[,2]) / 100
                                aux_eta[1]  = as.numeric(gsub("E","",aux_name[,3])) 
                                aux_eta[2]  = as.numeric(aux_name[,4]) 
                             }else if(G == 3)
                             {
                               aux_alpha[1]  = as.numeric(gsub("A","",aux_name[,1])) / 100
                               aux_alpha[2]  = as.numeric(aux_name[,2]) / 100
                               aux_alpha[3]  = as.numeric(aux_name[,3]) / 100
                               
                               aux_eta[1]  = as.numeric(gsub("E","",aux_name[,4])) 
                               aux_eta[2]  = as.numeric(aux_name[,5]) 
                               aux_eta[3]  = as.numeric(aux_name[,6]) 
                             }
                             
                             rownames(Xtrain)
                             n <- nrow(X)
                             
                          #   datos[[1]]$X <- X
                          #   datos[[1]]$l <- l
                          #   datos[[1]]$Xtrain <- Xtrain
                          #   datos[[1]]$Xtest <- Xtest
                          #   datos[[1]]$ltrain <- ltrain
                          #   datos[[1]]$ltest <- ltest
                          #   datos[[1]]$v <- v
                          #   datos[[1]]$vtrain <- vtrain
                          #   datos[[1]]$vtest <- vtest 
                             
                            datos <- list(X = X, l = l,
                                         Xtrain = Xtrain, Xtest = Xtest,
                                         ltrain = ltrain, ltest = ltest,
                                         v = v,
                                        vtrain = vtrain, vtest = vtest)
                           
                               par <- list(G = G, mu = aux_mu, sigma = aux_sigma,
                                         ClassProportion , alpha = aux_alpha,
                                         eta  = aux_eta, NumberObservations = NumberObservations,
                                         ProportionTraining = ProportionTraining)
                               
                              Output <- list(GenData = datos,
                                             par = par)
                              
                              saveRDS(Output, paste0(pathOutput,name_archivo,"_",i,".RDS") )
                  }      
          }    
}                              

