# 
source("Semisupervised.R")
source("ListScenarios.R")
source("GSFile.R")
source("GSSFile.R")

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



dir(pathScenarios)
ini <- n100.9 + 1
fin <- n100
fin-ini + 1

tic("SFiles n100.9+1 to n100")
status <- mclapply(Scenarios100[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSFiles)
#  if(is_empty(intersect(FilesProcessed,SFilename)))
#  {
    tryCatch(
      {
        GenerateSFile(x,pathScenarios,pathSFiles) 
        return(1)
      }, error = function(e){
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
        
      }
    )
    
#  }else cat("\n The file ",SFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)
toc()
