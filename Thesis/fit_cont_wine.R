source("Semisupervised.R")


# Order scenarios ---------------------------------------------------------


pathwd<-getwd()

path_scenarios_to_run <- paste0(pathwd,"/scenarios_wine/")

path_output <- paste0(pathwd,"/s_wine")

path_real_data_metrics <- paste0(pathwd,"/real_data_metrics")




path_scenarios_to_run <- paste0(pathwd,"/scenarios_wdbc/")

nameDf<-"Wdbc"

pathOutput <- paste0(pathwd,"/s_wdbc")

pathOutput <- paste0(pathwd,"/ss_wdbc")


fit_cont_datasets <- function(name_metrics_file = "real_data_metrics.RDS",nameDf,path_real_data_metrics, pathOutput)

{
  
  
  
#if(is_empty(str_detect("real_data_metrics.RDS",dir(path_real_data_metrics))) )
#{
  metrics <-data.frame(file = character(), dataset = character(),
                          time_execution = numeric(), 
                          steps = numeric(), iterations = numeric() ,
                          fitted_model_nc_sm = character(),
                          fitted_model_c_sm = character(),
                          selected_model = character(),
                          ccr_class_c_sm = numeric(), tp_sm = numeric(),
                          tn_sm = numeric(), fp_sm = numeric(),
                          fn_sm = numeric(),  ccr_class_c_all = numeric(), 
                          tp_all = numeric(), tn_all = numeric(),
                          fp_all = numeric(), fn_all = numeric(),
                          sensitivity_cont_sm = numeric(), sensitivy_cont_all = numeric())
#  saveRDS(metrics_rd,paste0(path_real_data_metrics,"/real_data_metrics.RDS") )
#}



scenarios_to_run <- dir(path_scenarios_to_run)
scenarios_to_run 

Model <- c("EII","VII","EEI","VEI","EEE","VVV")
CE <- Model
#statusModelstatus<-mclapply(scenarios_to_run, function(x){

x <- scenarios_to_run[1] 
for(x in scenarios_to_run)
{
    SFilename <- paste0("S_",x)
    FilesProcessed <- dir(path_output)
    niterations <- 10
  
    tryCatch(
    {
      #  if(is_empty(intersect(FilesProcessed,SFilename)))
      #  {
      
      
      #  }else cat("\n The file ",SFilename, " already exists in the directory. \n")
      
      auxRDS <- readRDS(paste0(path_scenarios_to_run,x))
      colnames(auxRDS$GenData$Xtrain)
      
#      Xtrain <- auxRDS$GenData$Xtrain [,-c(15,20)]
#      Xtest <- auxRDS$GenData$Xtest   [,-c(15,20)]
    
      Xtrain <- scale(auxRDS$GenData$Xtrain) 
      Xtest <- scale(auxRDS$GenData$Xtest)
      ltrain <- auxRDS$GenData$ltrain
      ltest <-   auxRDS$GenData$ltest 
      vtrain <- auxRDS$GenData$vtrain
      vtest <- auxRDS$GenData$vtest
      
      dfRW <- getOW(Xtrain,ltrain)
      RW <- dfRW$Var
      
      tic("greeddy_search")
      gs_model <- HeadLongSearch(Xtrain,Xtest,RW,ltrain,ltest,vtest, 
                                 CE = CE, pnolabeled = 0.1, iterations = niterations,
                                 alpharef = 0.75, tol = 0.01, epsilon = 0)
      elapsed_time_gs <- toc()
      time_sm <- elapsed_time_gs$toc - elapsed_time_gs$tic
      
      tic("full_model")
      
      saturated_mod <- SemiSupervisedFitting(Xtrain,Xtest,ltrain,ltest,vtest,CE,pnolabeled = 0)
      
      toc()
      elapsed_time_full <- toc()
      
      output_models <- list(gs_model = gs_model, full_model = saturated_mod)
      
      saveRDS(output_models, paste0(pathOutput,"/",SFilename,".RDS"))
      


      if(is.null (nregisters)) fila <- 1 else fila <- nregisters+1  
      
      
      pred_ltest  <- gs_model$Classprediction
      pred_vtest  <- gs_model$ContaminatedSamplesprediction

      cond_sm <- paste0(vtest,pred_vtest)
      # model predict correctly that the observation was contaminated
      tp_sm <- sum(cond_sm == "00")
      # model predict correctly that the observation was non-contaminated
      tn_sm <- sum(cond_sm == "11")
      # model predict that the observation was contaminated 0 but it wasn't 
      fp_sm <- sum(cond_sm == "10")
      # model predict that the observation was non-contaminated but it was
      fn_sm <- sum(cond_sm == "01")
      
      pred_vtest_all <- saturated_mod$vtest_hat
      
      cond_all <- paste0(vtest,pred_vtest_all)
      
      tp_all <- sum(cond_all == "00")
      tn_all <- sum(cond_all == "11")
      fp_all <- sum(cond_all == "10")
      fn_all <- sum(cond_all == "01")
      
      
      metrics[fila,"file"] <- x
      metrics[fila,"dataset"] <- nameDf
      metrics[fila,"time_execution"] <- time_sm
      metrics[fila,"steps"] <- length(gs_model$models[[gs_model$posCM]]$PM)
      metrics[fila,"iterations"] <- gs_model$iterations
      metrics[fila,"fitted_model_nc_sm"] <- gs_model$fitted_NC_model
      metrics[fila,"fitted_model_c_sm"] <- gs_model$fitted_C_model
      metrics[fila,"selected_model"] <- paste(gs_model$Selectedmodel,collapse = "-")
      metrics[fila,"ccr_class_c_sm"] <- gs_model$CCRCM
      metrics[fila,"sensitivity_cont_sm"] <- MLmetrics::Sensitivity(vtest,pred_vtest,positive = 0)
      metrics[fila,"tp_sm"] <- tp_sm
      metrics[fila,"tn_sm"] <- tn_sm
      metrics[fila,"fp_sm"] <- fp_sm
      metrics[fila,"fn_sm"] <- fn_sm
      metrics[fila,"ccr_class_c_all"]  <- saturated_mod$CCRTestC
      metrics[fila,"sensitivity_cont_all"] <- MLmetrics::Sensitivity(vtest,pred_vtest_all,positive = 0)
      metrics[fila,"tp_all"] <- tp_all
      metrics[fila,"tn_all"] <- tn_all
      metrics[fila,"fp_all"] <- fp_all
      metrics[fila,"fn_all"] <- fn_all
      
      
      fila = fila + 1

      return(1)
      }, error = function(e){
      cat("Error fitting scenario: ",x, "\n")
      return(NULL)
      
          }
  )
} 
#}, mc.cores = 1)
            toc()
}




nameDf
saveRDS(metrics,paste0(path_real_data_metrics,"/real_data_metrics",nameDf,".RDS") )



