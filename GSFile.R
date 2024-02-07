# Generate Semi-supervised metric files

#pathWd <- "/home/jsancheg/git_environment/LDA_CMN/"
#setwd(pathWd)
#source("Semisupervised.R")
#pathScenarios <- "/home/jsancheg/git_environment/LDA_CMN/Scenarios/"
#pathSSFiles <- "/home/jsancheg/git_environment/LDA_CMN/SSFiles/"


#file_name <- dir(pathScenarios)[[1]]

GenerateSFile <- function(file_name,pathScenarios, pathOutput)
{

  fileScenario <- readRDS(paste0(pathScenarios,file_name))  

  Number_Separating_Variables <-str_split_1(file_name, "_")[[3]]
  
  if(Number_Separating_Variables == 2)
  {
    variables_True_Model <- c("X2","X4")
  }else if(Number_Separating_Variables == 3)
  {
    variables_True_Model <- c("X2","X4","X5")
  }
  
  CE <- "EEI"
  Output <- SemiSupervised_HLS(file_name,pathScenarios,CE,variables_True_Model,
                               pnolabeled = 0, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0)
  
  Sfile_name <- str_replace(file_name,"S_","SV_")
  saveRDS(Output,paste0(pathOutput,Sfile_name))
  
  return(1);  
}



#GenerateSSFile(file_name,pathScenarios,pathSSFiles  )

GenerateSFile_SSH <- function(file_name,pathScenarios)
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
  
  CE <- "EII"
  Output <- SemiSupervised_HLS_SSH(file_name,pathScenarios,CE,variables_TrueModel,
                                   pnolabeled = 0, niterations = 10,
                                   alpharef = 0.99, tol = 0.01, epsilon = 0)
  
  
  return(Output)  
}


