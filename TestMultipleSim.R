getwd()
ruta <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(ruta)
# to select multiple lines to align
# alt + shift and use mouse 

source("SimClassfewdim.R")
source("SimClassEM20Steps.R")


for (i_batch in 5:7)
{
  nruns = 10
  selectedvariables <- list()
  mu1 <- rep(0,100)
  mu2 <- c(0,6,0,6,rep(0,96))
  mu <- cbind(mu1,mu2)
  sg <- diag(1,100)
  pig<- c(0.5,0.5)
  nobservations = 320
  ptraining = 0.75
  alphag <-c(0.9,0.8)
  etag <- c(20,30)

  CM <- list()
  Accuracy <- list()
  ModelSize <- list()
  Inclusion_correctness <- list()
  Exclusion_correctness <- list()
  Accuracy <- list()
  Specificity_Class1 <- list()
  Sensitivity_Class2 <- list()
  Specificity_Class2 <- list()
  #muC1 <- matrix(0, nrow = nruns, ncol = 2)
  #muC2 <- matrix(0, nrow = nruns, ncol = 2)
  # sigma for class 1 and 2 expressed as a vector
  # the dimension of the matrix might change depending of the
  # model size of the chosen model
  # sC1v <- matrix(0, nrow = nruns, ncol = ncol(muC1)*2)
  # sC2v <- matrix(0, nrow = nruns, ncol = ncol(muC1)*2)
  #alphav <- matrix(0, nrow = nruns, ncol = 2)
  #etav  <- matrix(0, nrow = nruns, ncol = 2)
  real_model <- c("X2","X4")
  for (i_runs in 1:nruns)
      {
        cat("\n ---- Run: ", i_runs,"-----\n")
        set.seed(123)
        aux <- MultSimSetting(mu, sg, pig, nobservations, ptraining, alphag, etag)
        CM[[i_runs]] <- paste(unlist(aux$CM),collapse="-")
        Accuracy[[i_runs]]<-aux$Accuracy
        ModelSize[[i_runs]] <- aux$nVarSel
        Inclusion_correctness[[i_runs]] <- length(intersect(aux$CM,real_model))/length(real_model)
        Exclusion_correctness[[i_runs]] <- length(setdiff(aux$CM,real_model))/nrow(mu)
        Sensitivity_Class1[[i_runs]]<-aux$sensitivity_Class1
        Specificity_Class1[[i_runs]]<- aux$specificity_Class1
        Sensitivity_Class2[[i_runs]]<- aux$sensitivity_Class2
        Specificity_Class2[[i_runs]]<- aux$specificity_Class2
        #  muC1[i_runs,] <- aux$Parameters$mu[1,]    
        #  muC2[i_runs,] <- aux$Parameters$mu[2,]
        #  sC1v[i_runs,] <- as.vector(aux$Parameters$Sgnc[,,1])
        #  sC2v[i_runs,] <- as.vector(aux$Parameters$Sgnc[,,2])
        #  alphav[i_runs,] <- aux$Parameters$alpha
        #  etav[i_runs,] <- aux$Parameters$eta
      
        res <- data.frame(AvgAccuracy = mean(unlist(Accuracy)),
                          AvgModelSize = mean(unlist(ModelSize)),
                          MinModelSize = min(unlist(ModelSize)),
                          MaxModelSize = max(unlist(ModelSize)),
                          AvgInclusion_correctness = mean(unlist(Inclusion_correctness)),
                          AvgExclusion_correctness = mean(unlist(Exclusion_correctness)),
                          AvgSensitivity_Class1 = mean(unlist(Sensitivity_Class1)),
                          AvgSpecificity_Class1 = mean(unlist(Specificity_Class1)),
                          AvgSensitivity_Class2 = mean(unlist(Sensitivity_Class2)),
                          AvgSpecificity_Class2 = mean(unlist(Specificity_Class2)))
        #25:54
    }
#      res
#      unlist(CM)
#      table(unlist(CM))

      ruta <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"

      Output <- list(CM = CM, Accuracy = Accuracy,ModelSize = ModelSize, 
               Inclusion_correctness = Inclusion_correctness, 
               Exclusion_correctness = Exclusion_correctness,
               Sensitivity_Class1 = Sensitivity_Class1,
               Sensitivity_Class2 = Sensitivity_Class2,
               Specificity_Class1 = Specificity_Class1,
               Specificity_Class2 = Specificity_Class2)

      saveRDS(Output,paste0(ruta,"Output/Sim_Batch",i_batch,".RDS") )
      cat("\n Batch ",i_batch, " from 10 \n")
}
#save(CM, Accuracy,ModelSize, Inclusion_correctness, Exclusion_correctness,
#     Sensitivity_Class1,Sensitivity_Class2,Specificity_Class1,Specificity_Class2,
#     file = paste0(ruta,"Output/Sim_Batch1.RData"))

saveRDS(Output,paste0(ruta,"Output/Sim_Batch5",".RDS") )


