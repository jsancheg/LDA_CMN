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

# Ubunto paths
# pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
# pathFiles <- "/home/jsancheg/Documents/SSFiles/"

# Windows path
pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"


dir(pathScenarios)
ini <- 1
fin <- 1
#fin <- n2.5
fin


Sys.time(mclapply(Scenarios2.5[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSSFiles)
  if(is_empty(intersect(FilesProcessed,SFilename))) GenerateSFile(x,pathScenarios,pathSFiles) else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1) )

