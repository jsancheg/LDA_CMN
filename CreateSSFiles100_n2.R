# 
source("Semisupervised.R")
source("ListScenarios.R")
source("GSSFile.R")
source("GSFile.R")

library(purrr)
library(ContaminatedMixt)

if (!requireNamespace("googledrive", quietly = TRUE)) {
  install.packages("googledrive")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(googledrive)
library(gargle)
library(readr)

# Ubunto paths
# pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
# pathFiles <- "/home/jsancheg/Documents/SSFiles/"



ini <- n100.1+1
fin <- n100.2

fin-ini+1
Model <- c("EII","VII","VEI","EEI","EVI","VVI","EEE","VVV")

status <- mclapply(Scenarios100[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
  tryCatch(
    {
     
      if(is_empty(intersect(FilesProcessed,SSFilename))) 
      {
      
        GenerateSSFile(x,pathScenarios1,pathSSFiles1,Model) 
      }  else cat("\n The file ",SSFilename, " already exists in the directory. \n")
      
      return(1)
    }, error = function(e){
      cat("Error fitting scenario: ",x, "\n")
      return(NULL)
    }
    )
  
  
}, mc.cores = 1)



