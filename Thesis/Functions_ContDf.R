



contamina_dataset <- function(X,y,lab,vpi,alphaM,etaM,ptrain,run_begin,run_end,nameDf,pathOutput)
{
  gen_data <- vector("list",1)
  gen_data
  
  for(i_a in 1:nrow(alphaM))
  {
    
    for(i_eta in run_begin:run_end)
    {
      time_df <- data.frame(File = character(), Simulation = numeric(),Time_Execution = numeric())
      for(i in 1:nruns)
      {
        name_alpha <- paste(alphaM[i_a,],collapse = "_")
        name_eta <- paste(etaM[i_eta,],collapse="_")
        name_file <- paste0(pathOutput,"A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_",nameDf,"_",i,".RDS")
        list_processed_files <- dir(pathOutput)
        flag_existing_file <- str_detect(list_processed_files,paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Crabs_",i_sim,".RDS"))
        cat("\n File: ",name_file, " processed status ", any(flag_existing_file == TRUE), "\n" )
        if (all(flag_existing_file == FALSE))
        {
          cat ("Simulation ", i_a,"-",i_eta)
          tic("Simulation ")
          
          gen_data[[1]] <- cont_df (X,y,lab,vpi,alphaM[i_a,],etaM[i_eta,],ptrain)
          
          elapsed_time <- toc()
          time_sim = elapsed_time$toc - elapsed_time$tic
          
          time_df[i_sim, "File"] <- name_file
          time_df[i_sim,"Simulation"] <- i_sim
          time_df[i_sim, "Time_Execution"] <- time_sim
          time_df_global[i_sim,"File"] <- name_file
          time_df_global[i_sim,"Simulation"] <- i_sim
          time_df_global[i_sim,"Time_Execution"] <- time_sim
          
          parameters <- list(G = length(unique(y)),
                             mu = apply(X,2,mean),
                             sigma = aux_sigma,
                             ClassProportion = vpi,
                             alpha = alphaM[i_a,],
                             eta = etaM[i_eta,],
                             NumberObservations = length(y),
                             ProportionTraining = ptrain
          )
          output <- list(GenData = gen_data[[1]], par = parameters, execution_time_df <- time_df)
          saveRDS(output,name_file )
        }
      }  
    }
  }
  
  name <- "times_for_wine"
  
  saveRDS(time_df_global,paste0(pathwd,"/time_cont_files/",name,".RDS"))
  
  
}
