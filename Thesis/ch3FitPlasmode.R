# 
sys_info <- Sys.info()
if(sys_info["nodename"] == "WildFree")
  setwd("/home/jsancheg/git_environment/LDA_CMN/")

source("Semisupervised.R")
source("ListScenariosAlpha_Eta.R")
source("GSFile.R")
source("GSSFile.R")
library(purrr)
library(ContaminatedMixt)
library(ssh)


# Windows path
system_info <- Sys.info()
#OS_name <- system_info("")
pc_name <- system_info['nodename'] 

if(pc_name == "LAPTOP-ADR3M911")
{
  
  pathwd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
  path_plasmode <- paste0(pathwd,"Thesis//Plasmode/")
  path_plasmode_crabs <- paste0(path_plasmode,"crabs/")
  path_plasmode_wdbc <- paste0(path_plasmode,"wdbc/")
  path_plasmode_wine <- paste0(path_plasmode,"wine/")
  path_scrabs <- paste0(path_plasmode,"Scrabs/")
  
}else if(pc_name == "WildFree")
{
  pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
  pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
  pathSFiles <- "/home/jsancheg/Documents/SFiles/"
  
}


list_files <- dir(path_plasmode_crabs)
list_files


ini <- 1
fin <- length(list_files)
#fin <- n2.5
fin-ini +1

tic("File 5 variables")

Model <- c("EII","VII","VEI","EEI","EVI","VVI","EEE","VVV")
Model <- c("EII","VII","EEI","VEI","EEE","VVV")


tic("SFiles 1 to n5")

pathOutput <- path_scrabs


status<-mclapply(list_files[ini:fin], function(x){

#  location_file <- tools::file_path_sans_ext(paste0(path_plasmode_crabs,x))
#  aux_name <- basename(location_f)
  SFilename <- str_replace(x,"A","SV_A")
  FilesProcessed <- dir(path_scrabs)
  tryCatch(
    {
#      if(is_empty(intersect(FilesProcessed,SSFilename)))
 #     {
      
        #CE <- Model
        #variables_True_Model <- c("CL","RW")
        #Output <- SemiSupervised_GS(x,path_plasmode_crabs,CE,variables_True_Model,
        #                            pnolabeled = 0, niterations = 10,
        #                            alpharef = 0.99, tol = 0.01, epsilon = 0)
        
        
      
      
      
      
      #    Metrics <- data.frame(Nsim = rep(1:nsimulations,each = 3),
      #                          )
#
      Metrics = data.frame(Nsim = numeric(),
                           FittedContModel_SM = character(),
                           FittedNoContModel_SM = character(),
                           Iterations_SM = numeric(),
                           Model_SM = character(),
                           Nvars_SM = numeric(),
                           CCR_SM = numeric(),Precision_SM = numeric(),
                           Recall_SM = numeric(), F1_SM = numeric(),
                           CCRCont_SM = numeric(), CCRNoCont_SM = numeric(),
                           PrecisionV_SM = numeric(), RecallV_SM = numeric(),
                           Specificity_SM = numeric(), 
                           F1V_SM = numeric(),
                           FittedContModel_TM = character(),
                           FittedNoContModel_TM = character(),
                           Iterations_TM = numeric(),
                           Model_TM = character(), Nvars_TM = numeric(), 
                           CCR_TM  = numeric(), Precision_TM = numeric(),
                           Recall_TM = numeric(), F1_TM = numeric(),
                           CCRCont_TM = numeric(), CCRNoCont_TM = numeric(),
                           PrecisionV_TM = numeric(), RecallV_TM = numeric(),
                           Specificiy_TM = numeric(),
                           F1V_TM = numeric(), 
                           FittedContModel_SaturatedM = character(),
                           FittedNoContModel_SaturatedM = character(),
                           Iterations_SaturatedM = numeric(),
                           Model_SaturatedM = character(),
                           Nvars_SaturatedM = numeric(),
                           CCR_SaturatedM = numeric(),Precision_SaturatedM = numeric(),
                           Recall_SaturatedM = numeric(), Specificity_SaturatedM = numeric(), 
                           F1_SaturatedM = numeric(),
                           CCRCont_SaturatedM = numeric(), CCRNoCont_SaturatedM = numeric(),
                           PrecisionV_SaturatedM = numeric(), RecallV_SaturatedM = numeric(),
                           F1V_SaturatedM = numeric(), stringsAsFactors = FALSE)
      #                          ltesthat_SM = numeric(), ltesthat_TM = numeric(), 
      #                          ltesthat_SaturatedM = numeric(),
      #                          vtesthat_SM = numeric(), vtesthat_TM = numeric(),
      
        
        
        fileRDS <- readRDS(paste0(path_plasmode_crabs,x))
      
        nsimulations <- length(fileRDS$GenData)
        
        MmetricsSaturatedM <- vector("list",nsimulations)
        MmetricsTM <- vector("list",nsimulations)
        MmetricsSM <- vector("list",nsimulations)
        GenData <- vector("list",nsimulations)
        dfRW <- vector("list",nsimulations)
        estimates <- vector("list",nsimulations)
        
        dfRW <- vector("list",nsimulations)
        
        i_sim <- 1
        
        
        for(i_sim in 1:nsimulations)
        {
            GenData[[i_sim]] <-fileRDS$GenData[[i_sim]]
            
              
            Xtrain <-GenData[[i_sim]]$Xtrain
            Xtest <- GenData[[i_sim]]$Xtest
            ltrain <- GenData[[i_sim]]$ltrain
            ltest <- GenData[[i_sim]]$ltest
            vtrain <- GenData[[i_sim]]$vtrain
            vtest <- GenData[[i_sim]]$vtest
            G <- length(unique(c(ltrain,ltest)) )
            CE <- Model
            pnolabeled = 0
            ind_nolabeled = NULL
            niterations = 10
            
            
            MmetricsSM[[i_sim]] <- data.frame(Group = 1:G, Precision = rep(0,G), 
                                              Recall = rep(0,G), Specificity = rep(0,G),
                                              F1 = rep(0,G)) 
            
            
            dfRW[[i_sim]] <- getOW(Xtrain,ltrain)
            RW <- dfRW[[i_sim]]$Var
            variables_saturated_model <- RW
            
            selectedVar_mod <- GreedySearch(Xtrain,Xtest,RW,ltrain,ltest, vtrain, vtest, 
                                 CE = CE, pnolabeled = pnolabeled, ind_nolabeled, iterations = niterations,
                                 alpharef = 0.75, tol = 0.01, epsilon = 0)
          
          
            
            #saturated_mod$pseudo_label_info$vtrain <- GenData[[i_sim]]$vtrain
            #saturated_mod$pseudo_label_info$True_Labels_V_train <- GenData[[i_sim]]$vtrain[saturated_mod$pseudo_label_info$Unlabelled_index]
            selectedVar_mod$pseudo_label_info$vtrain <- GenData[[i_sim]]$vtrain
            selectedVar_mod$pseudo_label_info$True_Labels_V_Train <- GenData[[i_sim]]$vtrain[selectedVar_mod$pseudo_label_info$Unlabelled_index]
            #TrueModel$pseudo_label_info$vtrain <- GenData[i_sim]$vtrain
            #TrueModel$pseudo_label_info$True_Labels_V_Train <- GenData[[i_sim]]$vtrain[TrueModel$pseudo_label_info$Unlabelled_index]
            
            # pos: position model obtained by variable selection
            pos <- selectedVar_mod$posCM
            nVarSel <- length(selectedVar_mod$Selectedmodel)
            
            PM <-selectedVar_mod$Selectedmodel
            Xsubset <- data.frame(Xtrain) %>% dplyr::select(all_of(PM))
            

            cat("\n", "selected model", "test set ",selectedVar_mod$Selectedmodel,"-",CCRltesthatSM,"\n")
            
            # Filter contaminated vs non-contaminated samples
            lind_nocont_class <- list()
            lind_cont_class <- list()
            
            
            for (i_g in 1:G)
            {
              # non contaminated samples in classes
              lind_nocont_class[[i_g]] <- which(ltest == i_g & vtest == 1)
              # contaminated samples in classes
              lind_cont_class[[i_g]] <- which(ltest== i_g & vtest== 0)
            #  MmetricsSaturatedM[[i_sim]][i_g,2] <- Precision(ltest,saturated_mod$ltest_hat_C,positive = i_g)
            #  MmetricsTM[[i_sim]][i_g,2] <- Precision(ltest,TrueModel$ltest_hat_C,positive = i_g)
              MmetricsSM[[i_sim]][i_g,2] <-MLmetrics::Precision(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
             # MmetricsSaturatedM[[i_sim]][i_g,3] <- Recall(ltest,saturated_mod$ltest_hat_C,positive = i_g)
            #  MmetricsTM[[i_sim]][i_g,3] <- Recall(ltest,TrueModel$ltest_hat_C,positive = i_g)
              MmetricsSM[[i_sim]][i_g,3] <-Recall(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
            #  MmetricsSaturatedM[[i_sim]][i_g,4] <- Specificity(ltest,saturated_mod$ltest_hat_C,positive = i_g)
            #  MmetricsTM[[i_sim]][i_g,4] <- Specificity(ltest,TrueModel$ltest_hat_C,positive = i_g)
              MmetricsSM[[i_sim]][i_g,4] <-Specificity(ltest,selectedVar_mod$models[[pos]]$ltest_hat_C,positive = i_g)
              
            }
            
            MmetricsSaturatedM[[i_sim]]$F1 <- 2*(MmetricsSaturatedM[[i_sim]]$Precision*MmetricsSaturatedM[[i_sim]]$Recall)/(MmetricsSaturatedM[[i_sim]]$Precision+MmetricsSaturatedM[[i_sim]]$Recall)
            #MmetricsTM[[i_sim]]$F1 <- 2*(MmetricsTM[[i_sim]]$Precision*MmetricsTM[[i_sim]]$Recall)/(MmetricsTM[[i_sim]]$Precision+MmetricsTM[[i_sim]]$Recall)
            MmetricsSM[[i_sim]]$F1 <- 2*(MmetricsSM[[i_sim]]$Precision*MmetricsSM[[i_sim]]$Recall)/(MmetricsSM[[i_sim]]$Precision+MmetricsSM[[i_sim]]$Recall)
            
            ind_nocont_samples <- unlist(lind_nocont_class)
            ind_cont_samples <- unlist(lind_cont_class)
            
            no_cont_samples <- ltest[ind_nocont_samples]
            cont_samples <- ltest[ind_cont_samples]
            
            length(TrueModel$ltest_hat_C)
            
            # Calculating precision, recall, and F1 metrics
            
          #  saturated_vtest <- saturated_mod$vtest_hat
          #  TM_vtest <- TrueModel$vtest_hat
            SM_vtest <- selectedVar_mod$models[[pos]]$vtest_hat
            
            
            # Calculating accuracy of detecting whether sample is contaminated
            # or not, precision, recall, and F1
            
          #  CCRSaturated_vtest <- 100*sum(vtest == saturated_vtest)/length(vtest)
          #  CCRTM_vtest <- 100*sum(vtest == TM_vtest)/length(vtest)
            CCRSM_vtest <- 100*sum(vtest == SM_vtest)/length(vtest)
            
            
          #  precision_saturated_V <- Precision(vtest,saturated_vtest,positive = 0)
          #  precision_TM_V <- Precision(vtest,TM_vtest,positive = 0)
            precision_SM_V <- Precision(vtest,SM_vtest,positive = 0)
            
        #    recall_saturated_V <- Recall(vtest,saturated_vtest,positive = 0)
        #    recall_TM_V <- Recall(vtest,TM_vtest,positive = 0)
            recall_SM_V <- Recall(vtest,SM_vtest,positive = 0)
            
          #  specificity_saturated_V <- Specificity(vtest,saturated_vtest,positive = 0)
          #  specificity_TM_V <- Specificity(vtest,TM_vtest,positive = 0)
            specificity_SM_V <- Specificity(vtest,SM_vtest,positive = 0)
            
          #  F1_Saturated_V <- 2*(precision_saturated_V * recall_saturated_V)/(precision_saturated_V+recall_saturated_V) 
          #  F1_TM_V <- 2*(precision_TM_V * recall_TM_V)/(precision_TM_V + recall_TM_V)
            F1_SM_V <- 2*(precision_SM_V * recall_SM_V)/(precision_SM_V + recall_SM_V)
            
            
            CCRSM_Identifying_cont_samples <- 0
            CCRSM_Identifying_no_cont_samples <- 0
            #CCRTM_Identifying_cont_samples <- 0
            #CCRTM_Identifying_no_cont_samples <- 0
            #CCRSaturated_Identifying_cont_samples <- 0
            #CCRSaturated_Identifying_no_cont_samples <- 0
            
            
            # predicted class for contaminated and non-contaminated samples for the true model
            #TM_nocont_lhat <- TrueModel$ltest_hat_C[ind_nocont_samples]
            #TM_cont_lhat <- TrueModel$ltest_hat_C[ind_cont_samples]
            
            # predicted class for contaminated and non-contaminated samples for selected model
            SM_nocont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_nocont_samples]
            SM_cont_lhat <- selectedVar_mod$models[[pos]]$ltest_hat_C[ind_cont_samples]  
            
            # predicted class for contaminated and non-contaminated samples for saturated model 
            #Saturated_nocont_lhat <- saturated_mod$ltest_hat_C[ind_nocont_samples]
            #Saturated_cont_lhat <- saturated_mod$ltest_hat_C[ind_cont_samples]
            
          #  CCRTM_no_cont_samples <- 0
          #  CCRTM_cont_samples <- 0
            CCRSM_no_cont_samples <- 0
            CCRSM_cont_samples <- 0
          #  CCRSaturated_no_cont_samples <- 0
          #  CCRSaturated_cont_samples <- 0
            
            # Accuracy no-contaminated True Model
          #  CCRTM_no_cont_samples <- (sum(no_cont_samples == TM_nocont_lhat)/length(no_cont_samples))
          #  CCRTM_cont_samples <- (sum(cont_samples == TM_cont_lhat)/length(cont_samples))
            CCRSM_no_cont_samples <- (sum(no_cont_samples == SM_nocont_lhat)/length(no_cont_samples))
            CCRSM_cont_samples <- (sum(cont_samples == SM_cont_lhat)/length(cont_samples))
          #  CCRSaturated_no_cont_samples <- (sum(no_cont_samples == Saturated_nocont_lhat)/length(no_cont_samples))
          #  CCRSaturated_cont_samples <-     (sum(cont_samples == Saturated_cont_lhat)/length(cont_samples))
            Metrics[i_sim,"Nsim"] <- i_sim
            Metrics[i_sim,"FittedContModel_SM"] <- selectedVar_mod$fitted_C_model
            Metrics[i_sim,"FittedNoContModel_SM"] <- selectedVar_mod$fitted_NC_model
            Metrics[i_sim,"Iterations_SM"] <- selectedVar_mod$iterations
            Metrics[i_sim,"Model_SM"] <- paste(PM,collapse = "-")
            Metrics[i_sim,"Nvars_SM"] <- nVarSel
            Metrics[i_sim,"CCR_SM"] <-CCRltesthatSM
            Metrics[i_sim,"Precision_SM"] <- mean(MmetricsSM[[i_sim]]$Precision)
            Metrics[i_sim,"Recall_SM"] <- mean(MmetricsSM[[i_sim]]$Recall)
            Metrics[i_sim,"Specificity_SM"] <- mean(MmetricsSM[[i_sim]]$Specificity)
            Metrics[i_sim,"F1_SM"] <- mean(MmetricsSM[[i_sim]]$F1)
            Metrics[i_sim,"CCRCont_SM"] <- CCRSM_cont_samples
            Metrics[i_sim,"CCRNoCont_SM"] <- CCRSM_no_cont_samples
            Metrics[i_sim,"PrecisionV_SM"] <- precision_SM_V
            Metrics[i_sim,"RecallV_SM"] <- recall_SM_V
            Metrics[i_sim,"SpecificityV_SM"] <- specificity_SM_V
            Metrics[i_sim,"F1V_SM"] <- F1_SM_V
            #Metrics[i_sim,"FittedContModel_TM"] <- TrueModel$fitted_C_model
            #Metrics[i_sim,"FittedNoContModel_TM"] <- TrueModel$fitted_NC_model
            #Metrics[i_sim,"Iterations_TM"] <- TrueModel$niterations
            #Metrics[i_sim,"Model_TM"] <- paste(TrueModel$PM,collapse = "-")
            #Metrics[i_sim,"Nvars_TM"] <-length(TrueModel$PM)
            #Metrics[i_sim,"CCR_TM"] <- TrueModel$CCRTestC
            #Metrics[i_sim,"Precision_TM"] <- mean(MmetricsTM[[i_sim]]$Precision)
            #Metrics[i_sim,"Recall_TM"] <- mean(MmetricsTM[[i_sim]]$Recall)
            #Metrics[i_sim,"Specificity_TM"] <- mean(MmetricsTM[[i_sim]]$Specificity)
            #Metrics[i_sim,"F1_TM"] <- mean(MmetricsTM[[i_sim]]$F1)
            #Metrics[i_sim,"CCRCont_TM"] <-CCRTM_cont_samples
            #Metrics[i_sim,"CCRNoCont_TM"] <- CCRTM_no_cont_samples
            #Metrics[i_sim,"PrecisionV_TM"] <- precision_TM_V
            #Metrics[i_sim,"RecallV_TM"] <- recall_TM_V
            #Metrics[i_sim,"SpecificityV_TM"] <- specificity_TM_V
            #Metrics[i_sim,"F1V_TM"] <- F1_TM_V
          #  Metrics[i_sim,"FittedContModel_SaturatedM"] <- saturated_mod$fitted_C_model
          #  Metrics[i_sim,"FittedNoContModel_SaturatedM"] <- saturated_mod$fitted_NC_model
          #  Metrics[i_sim,"Iterations_SaturatedM"] <- saturated_mod$niterations
          #  Metrics[i_sim,"Model_SaturatedM"] <- paste(RW,collapse = "-")
          #  Metrics[i_sim,"Nvars_SaturatedM"] <- length(RW)
           # Metrics[i_sim,"CCR_SaturatedM"] <- saturated_mod$CCRTestC
          #  Metrics[i_sim,"Precision_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Precision)
          #  Metrics[i_sim,"Recall_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$Recall)
           # Metrics[i_sim,"Specificity_SaturatedM"]<-mean(MmetricsSaturatedM[[i_sim]]$Specificity)
          #  Metrics[i_sim,"F1_SaturatedM"] <- mean(MmetricsSaturatedM[[i_sim]]$F1)
          #  Metrics[i_sim,"CCRCont_SaturatedM"] <- CCRSaturated_cont_samples
          #  Metrics[i_sim,"CCRNoCont_SaturatedM"] <- CCRSaturated_no_cont_samples
          #  Metrics[i_sim,"PrecisionV_SaturatedM"] <- precision_saturated_V
          #  Metrics[i_sim,"RecallV_SaturatedM"] <- recall_saturated_V
          #  Metrics[i_sim,"SpecificityV_SaturatedM"] <- specificity_saturated_V
          #  Metrics[i_sim,"F1V_SaturatedM"] <- F1_Saturated_V
          #  estimates[[i_sim]]$models <- selectedVar_mod$models
            estimates[[i_sim]]$posSM <- pos
            estimates[[i_sim]]$SM <- selectedVar_mod$models[[pos]]$PM
            estimates[[i_sim]]$par <- selectedVar_mod$models[[pos]]$par
            estimates[[i_sim]]$par$Fitted_Model <- CE
            estimates[[i_sim]]$lTestHat_SM <- selectedVar_mod$models[[pos]]$ltest_hat_C
            #estimates[[i_sim]]$lTestHat_TM <- TrueModel$ltest_hat_C
            #estimates[[i_sim]]$lTestHat_SaturatedM <- saturated_mod$ltest_hat_C
            estimates[[i_sim]]$vTestHat_SM <- SM_vtest
          #  estimates[[i_sim]]$vTestHat_TM <- TM_vtest
          #  estimates[[i_sim]]$vTestHat_SaturatedM <- saturated_vtest
          #  estimates[[i_sim]]$PseudoInformation_TM <- TrueModel$pseudo_label_info
            estimates[[i_sim]]$PseudoInformation_SM <- selectedVar_mod$models[[pos]]$pseudo_label_info
        #    estimates[[i_sim]]$PseudoInformation_SaturatedM <- saturated_mod$pseudo_label_info
            
            
            
            Output <-  list(Metrics = Metrics , 
                            # Matrix of metrics
                            Metrics_SaturatedM = MmetricsSaturatedM,
                            Metrics_SM = MmetricsSM,
                            Metrics_TM = MmetricsTM,
                            # Generated Data
                            GenData = GenData,
                            # Estimates
                            Estimates = estimates)
            
            
            saveRDS(Output,paste0(pathOutput,SFilename))
          
        }
        
        
#        GenerateSFile(x,path_plasmode_crabs,path_scrabs, Model) 
#      }else cat("\n The file ",SFilename, " already exists in the directory. \n")
      
 #     return(1)
    }, error = function(e){
      cat("Error fitting scenario: ",x, "\n")
      return(NULL)
      
    }
  )
  
  
}, mc.cores = 1)
toc()

