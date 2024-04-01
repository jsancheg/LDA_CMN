    # Generate Semi-supervised metric files

#pathWd <- "/home/jsancheg/git_environment/LDA_CMN/"
#setwd(pathWd)
#source("Semisupervised.R")
#pathScenarios <- "/home/jsancheg/git_environment/LDA_CMN/Scenarios/"
#pathSSFiles <- "/home/jsancheg/git_environment/LDA_CMN/SSFiles/"
library(stringr)

#file_name <- dir(pathScenarios)[[1]]
                                                                                                                       
GenerateSSFile <- function(file_name,pathScenarios, pathOutput,Model = "VVV",pnolabeled = 0.5)
{
  #fileScenario <- readRDS(paste0(pathScenarios,file_name))  
  
  Number_Separating_Variables <-str_split_1(file_name, "_")[[3]]
  
  if(Number_Separating_Variables == 2)
  {
    variables_True_Model <- c("X2","X4")
  }else if(Number_Separating_Variables == 3)
  {
    variables_True_Model <- c("X2","X4","X5")
  }
  
  CE <- Model
  Output <- SemiSupervised_HLS(file_name,pathScenarios,CE,variables_True_Model,
                           pnolabeled = pnolabeled, niterations = 10,
                           alpharef = 0.99, tol = 0.01, epsilon = 0)
    
  SSfile_name <- str_replace(file_name,"S_","SSV_")
  saveRDS(Output,paste0(pathOutput,SSfile_name))
  
  return(1);  
}


GenerateSSFile_vectorize <- function(file_name,pathScenarios, pathOutput,Model = "VVV", Search = "GS",pnolabeled = 0.5)
{
  #fileScenario <- readRDS(paste0(pathScenarios,file_name))  
  
  Number_Separating_Variables <-str_split_1(file_name, "_")[[3]]
  
  if(Number_Separating_Variables == 2)
  {
    variables_True_Model <- c("X2","X4")
  }else if(Number_Separating_Variables == 3)
  {
    variables_True_Model <- c("X2","X4","X5")
  }
  
  CE <- Model
  
  if(Search == "GS")
  {
    Output <- SemiSupervised_GS(file_name,pathScenarios,CE,variables_True_Model,
                                           pnolabeled = pnolabeled, niterations = 10,
                                           alpharef = 0.99, tol = 0.01, epsilon = 0)
    
  }else{
    Output <- SemiSupervised_HLS_vectorize(file_name,pathScenarios,CE,variables_True_Model,
                                           pnolabeled = pnolabeled, niterations = 10,
                                           alpharef = 0.99, tol = 0.01, epsilon = 0)
    
  }
  
  
  
  if(Search == "GS")
  {
    SSfile_name <- str_replace(file_path_sans_ext(file_name),"S_","SSV_")
    
    saveRDS(Output,paste0(pathOutput,SSfile_name,"_",as.character(pnolabeled*100),".RDS"  ) )
    
  }
  else{
    SSfile_name <- str_replace(file_name,"S_","SSV_")
    
    saveRDS(Output,paste0(pathOutput,SSfile_name))
  }
  return(1);  
}


GenerateSSFile_SSH <- function(file_name,pathScenarios)
{
  #filePathScenario <- paste0(pathScenarios,file_name)
  
  #fileScenario <- readRDS(filePathScenario)
  
  
  Number_Separating_Variables <-str_split_1(file_name, "_")[[3]]
  
  if(Number_Separating_Variables == 2)
  {
    variables_TrueModel <- c("X2","X4")
  }else if(Number_Separating_Variables == 3)
  {
    variables_TrueModel <- c("X2","X4","X5")
  }
  
  CE <- "EEI"
  Output <- SemiSupervised_HLS_SSH(file_name,pathScenarios,CE,variables_TrueModel,
                               pnolabeled = 0.5, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0)
  
  
  return(Output)  
}




#GenerateSSFile(file_name,pathScenarios,pathSSFiles  )

