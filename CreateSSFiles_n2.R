# 


source("Semisupervised.R")
source("ListScenarios.R")
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

# Windows path
#pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
#pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"


ini <- n100.1
fin <- n100.2

fin-ini+1

status <- mclapply(Scenarios100[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
#  if(is_empty(intersect(FilesProcessed,SSFilename))) 
#    {
    tryCatch(
      {
        
      GenerateSSFile(x,pathScenarios,pathSSFiles) 
        return(1)
      }, error = function(e){
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
        
      }
    )
    
 #   }else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)

