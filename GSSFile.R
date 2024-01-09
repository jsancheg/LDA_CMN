    # Generate Semi-supervised metric files

#pathWd <- "/home/jsancheg/git_environment/LDA_CMN/"
#setwd(pathWd)
#source("Semisupervised.R")
#pathScenarios <- "/home/jsancheg/git_environment/LDA_CMN/Scenarios/"
#pathSSFiles <- "/home/jsancheg/git_environment/LDA_CMN/SSFiles/"


#file_name <- dir(pathScenarios)[[1]]
                                                                                                                       
GenerateSSFile <- function(file_name,pathScenarios, pathOutput)
{
  fileScenario <- readRDS(paste0(pathScenarios,file_name))  
  
  Number_Variables <-str_split_1(file_name, "_")[[3]]
  
  if(Number_Variables == 2)
  {
    variables_TrueModel <- c("X2","X4")
  }else if(Number_Variables == 3)
  {
    variables_TrueModel <- c("X2","X4","X5")
  }
  
  CE <- "VVV"
  Output <- SemiSupervised_HLS(paste0(pathScenarios,file_name),CE,variables_TrueModel,
                           pnolabeled = 0.5, niterations = 10,
                           alpharef = 0.99, tol = 0.01, epsilon = 0)
    
  SSfile_name <- str_replace(file_name,"S_","SSV_")
  saveRDS(Output,paste0(pathOutput,SSfile_name))
  
  return(1);  
}

#GenerateSSFile(file_name,pathScenarios,pathSSFiles  )

