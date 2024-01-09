# Generate Semi-supervised metric files

pathWd <- "/home/jsancheg/git_environment/LDA_CMN/"
setwd(pathWd)
source("Semisupervised.R")
pathScenarios <- "/home/jsancheg/git_environment/LDA_CMN/Scenarios/"
pathSSFiles <- "/home/jsancheg/git_environment/LDA_CMN/SSFiles/"

GenerateSSFiles <- function(pathScenarios,pathSSFiles)
{

  ScenariosFiles <- dir(pathScenarios)
  SSFilesProcessed <- dir(pathSSFiles)
  nSSFilesProcessed <- 0
  nssFilesToProcessed <- 0
  nFiles <- 0
  timeFile <- list()
  class(ScenariosFiles)
  
  nFiles <- length(ScenariosFiles)
  nFiles
  
  
  nSSFilesProcessed <- length(SSFilesProcessed)
  
  if(nSSFilesProcessed == 0 )
  {
    SSFilesToProcess <- paste0(pathScenarios,ScenariosFiles)
    
    nSSFilesToProcess <- nFiles
    
  }else {
    nssFilesToProcess <- nFiles - nSSFilesProcessed
    SSFilesToProcess <- paste0(pathScenarios,setdiff(ScenariosFiles,SSFilesProcessed))
  }
  
  
  CE <- "VVV"
  
  
  
  variables_True_Model <- c("X2","X4")

  file_name <- str_split_1(SSFilesToProcess[[1]],"/")[7]
  str_split_1(file_name,"_")[3]
  #SemiSupervised_HLS(SSFilesToProcessed[[1]],CE,variables_True_Model)
  
   system.time( simFiles <- mclapply(SSFilesToProcess, function(x){
          nameFile <- unlist(str_split(x,"/"))[7]  
          Number_Variables <- str_split_1(nameFile,"_")[[3]]
          if(Number_Variables == 2)
          {
            variables_True_Model <- c("X2","X4")
          }else if(Number_Variables == 3)
          {
            variables_True_Model <- c("X2","X4","X%")
          }
          Output <- SemiSupervised_HLS(x,CE,variables_True_Model, 
                                   pnolabeled = 0.5, niterations = 10,
                                   alpharef = 0.99, tol = 0.01, epsilon = 0)
     SSfile_name <- str_replace(str_split_1(x,"/")[7],"S_","SSV_")
     paste0(pathSSFiles,SSfile_name)
     saveRDS(output,paste0(pathSSFiles,SSfile_name))
     
  },mc.cores = 1) )
  
}


