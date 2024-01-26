# 


source("Semisupervised.R")
source("ListScenariosFiles.R")
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
pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"


dir(pathScenarios)
ini <- n5.4+1
fin <- n5.5

LastFileProcessed <- NULL
countFilesProcessed <- 0

mclapply(Scenarios5[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
  LastFileProcessed <- x
  
  if(is_empty(intersect(FilesProcessed,SSFilename))) 
    {
        GenerateSSFile(x,pathScenarios,pathSSFiles) 
        countFilesProcessed <- countFilesProcessed + 1
  } else cat("\n The file ",SSFilename, " already exists in the directory. \n")
}, mc.cores = 1)
