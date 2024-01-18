# Generate Semi-supervised metric files

pathWd <- "/home/jsancheg/git_environment/LDA_CMN/"
setwd(pathWd)
source("Semisupervised.R")
pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"

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
    SSFilesToProcess <- ScenariosFiles
    nSSFilesToProcess <- nFiles
    
  }else {
    nssFilesToProcess <- nFiles - nSSFilesProcessed
    SSFilesToProcess <- setdiff(ScenariosFiles,str_replace(SSFilesProcessed,"SSV_","S_"))
  }
  
  # index for data files having 2 separating variables
  ind2 <- str_split(SSFilesToProcess,"_",simplify = TRUE)[,3] == 2
  # index for data files having 3 separating variables
  ind3 <- str_split(SSFilesToProcess,"_",simplify = TRUE)[,3] == 3
  
  Number_separating_variables <-   str_split(SSFilesProcessed,"_",simplify = TRUE)[,3]
  Number_separating_variables
  
  CE <- "VVV"
  
  if(any(ind2 == TRUE))
  {
    variables_True_Model <- c("X2","X4")
    simFiles <- mclapply(SSFilesToProcess[ind2], function(x) {
      Output <- SemiSupervised_HLS(x, pathScenarios,CE,variables_True_Model ,
                                   pnolabeled = 0.5, niterations = 10,
                                   alpharef = 0.99, tol = 0.01, epsilon = 0)
      SSfile_name <- str_replace(x,"S_","SSV_")
      saveRDS(Output, paste0(pathSSFiles,SSfile_name))
    }, mc.cores = 2)
  }

  if(any(ind2 == TRUE))
  {
    variables_True_Model <- c("X2","X4")
    simFiles <- mclapply(SSFilesToProcess[ind2], function(x) {
      Output <- SemiSupervised_HLS(x, pathScenarios,CE,variables_True_Model ,
                                   pnolabeled = 0.5, niterations = 10,
                                   alpharef = 0.99, tol = 0.01, epsilon = 0)
      SSfile_name <- str_replace(x,"S_","SSV_")
      saveRDS(Output, paste0(pathSSFiles,SSfile_name))
    }, mc.cores = 2)
  }
  
  if(any(ind3 == TRUE))
  {
    variables_True_Model <- c("X2","X4","X5")
    simFiles <- mclapply(SSFilesToProcess[ind2], function(x) {
      Output <- SemiSupervised_HLS(x, pathScenarios,CE,variables_True_Model ,
                                   pnolabeled = 0.5, niterations = 10,
                                   alpharef = 0.99, tol = 0.01, epsilon = 0)
      SSfile_name <- str_replace(x,"S_","SSV_")
      saveRDS(Output, paste0(pathSSFiles,SSfile_name))
    }, mc.cores = 2)
  }
  
  
  file_name <- str_split_1(SSFilesToProcess[[1]],"/")[6]
  str_split_1(file_name,"_")[3]
  aux <- str_split_1(FileNameToProcess[[1]],"_")
  as.numeric(aux[4])
  ind5vars <- sapply(FileNameToProcess,function(x) {
    
      aux <- str_split_1(x,"_")  
      ind5 <- as.numeric(aux[4]) == 5
      return(ind5)
    }
    )
  ind100vars <- !ind5vars  
  
# Process sets with only 5 variables  
  str(ind5vars)
  names(ind5vars)
  length(FileNameToProcess[ind5vars])
  
  SSFilesToProcess5 <- paste0(pathScenarios,FileNameToProcess[ind5vars])
  n5 <- length(SSFilesToProcess5)
  n5
  n5.1 <- n5* 1/10
  n5.2 <- n5* 2/10
  n5.3 <- n5* 3/10
  n5.4 <- n5* 4/10
  n5.5 <- n5* 5/10
  n5.6 <- n5* 6/10
  n5.7 <- n5* 7/10
  n5.8 <- n5* 8/10
  n5.9 <- n5* 9/10
  
    
  system.time( simFiles <- mclapply(SSFilesToProcess5[1], function(x){
    nameFile <- unlist(str_split(x,"/"))[6]  
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
    saveRDS(Output,paste0(pathSSFiles,SSfile_name))
    
  },mc.cores = 1) )
  
  
  
  length(FileNameToProcess[ind100vars])
  
  
  #str_split(FileNameToProcess,"_")[4]
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


