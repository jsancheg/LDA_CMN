# 
source("Semisupervised.R")
source("ListScenariosFiles.R")
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
pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"


dir(pathScenarios)
ini <- n2.5.5+1
fin <- n2.5.6
fin-ini

tic("SFiles 5 variables 306 files")
mclapply(Scenarios5[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSFiles)
  if(is_empty(intersect(FilesProcessed,SFilename))) GenerateSFile(x,pathScenarios,pathSFiles) else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)
toc()
