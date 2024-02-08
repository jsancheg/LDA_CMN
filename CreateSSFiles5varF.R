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

# Windows path
# pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
# pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
# pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"


dir(pathScenarios)
ini <- 1
fin <- n5
fin-ini + 1

tic("SFiles 5 variables 192 files")

status <- mclapply(Scenarios5[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSSFiles)
  
#  if(is_empty(intersect(FilesProcessed,SFilename))) 
#  {
  
    tryCatch(
      {
        GenerateSSFile(x,pathScenarios,pathSSFiles) 
        return(1)
       }, error = function(e)
        {
          cat("Error fitting scenario: ",x, "\n")
          return(NULL)
        }
      )

#  } else cat("\n The file ",SFilename, " already exists in the directory. \n")

    
}, mc.cores = 1)
toc()
