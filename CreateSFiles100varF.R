# 
source("Semisupervised.R")
source("ListScenarios.R")
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
ini <- n100.6+7
fin <- n100.7
fin-ini + 1

tic("SFiles 5 variables 306 files")
mclapply(Scenarios100[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSFiles)
#  write.table(SFilename,"LastFile.csv",sep = ",",col.names = "FileName",row.names = 1,append = TRUE )
  if(is_empty(intersect(FilesProcessed,SFilename))) GenerateSFile(x,pathScenarios,pathSFiles) 
  else cat("\n The file ",SFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)
toc()
