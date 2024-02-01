# 


source("Semisupervised.R")
source("ListScenariosFiles1.R")
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

# Windows path
system_info <- Sys.info()
#OS_name <- system_info("")
pc_name <- system_info['modename'] 

if(pc_name == "LAPTOP-ADR3M911")
{
  pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
  pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
  pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"
  
}else if(pc_name == "WildFree")
{
  pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
  pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
  pathSFiles <- "/home/jsancheg/Documents/SFiles/"
  
}


dir(pathScenarios)
ini <- 1
fin <- n5.1
#fin <- n2.5
fin

tic("File 5 variables")

LastFilename <- NULL
CountFilesProcessed <- 0


mclapply(Scenarios2.5[ini:fin], function(x){
  LastFilename <- x  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSSFiles)
  if(is_empty(intersect(FilesProcessed,SFilename))) 
  {
    GenerateSFile(x,pathScenarios,pathSFiles) 
    CountFilesProcessed <- CountFilesProcessed + 1
  } else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1) 

toc()
