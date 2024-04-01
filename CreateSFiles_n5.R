# 
source("utilities.R")
source("Semisupervised_Vectorize.R")
source("ListScenarios.R")
source("GSSFile.R")
source("GSFile.R")

library(parallel)
library(purrr)
library(ContaminatedMixt)
library(tictoc)

if (!requireNamespace("googledrive", quietly = TRUE)) {
  install.packages("googledrive")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(googledrive)
library(gargle)
library(readr)



ini <- n100.4
fin <- n100.5+1

fin-ini+1
Model <- c("EII","VII","VEI","EEI","EVI","VVI","EEE","VVV")
Model <- c("EII","VII","EEI","VEI","EEE","VVV")

Scenarios100[ini:fin]

tic("vectorize function")
status <-mclapply(Scenarios5[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSFiles)
    tryCatch(
      {

        if(is_empty(intersect(FilesProcessed,SFilename))) 
        {
#          GenerateSFile_vectorize(x,pathScenarios,pathSFiles,Model) 
          GenerateSFile(x,pathScenarios,pathSFiles,Model) 
          
        }else cat("\n The file ",SFilename, " already exists in the directory. \n")
        
        return(1)
      }, error = function(e){
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
        
      }
    )
    
  
}, mc.cores = 1)
toc()
