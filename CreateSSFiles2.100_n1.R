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
pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"


dir(pathScenarios)

#ini <- n2.100.1 +1
#fin <- n5.100.2
ini <- 2 
fin <- 10

n2.100.1

tic("SS Files 100") # 2 hours per file
mclapply(Scenarios2.100[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
  if(is_empty(intersect(FilesProcessed,SSFilename))) GenerateSSFile(x,pathScenarios,pathSSFiles) else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1) 

toc()