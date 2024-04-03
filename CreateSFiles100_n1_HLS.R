# 
sys_info <- Sys.info()
if(sys_info["nodename"] == "WildFree")
  setwd("/home/jsancheg/git_environment/LDA_CMN/")

source("Semisupervised.R")
source("ListScenarios.R")
source("GSFile.R")
source("GSSFile.R")
library(purrr)
library(ContaminatedMixt)
library(ssh)

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
pc_name <- system_info['nodename'] 

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

Scenarios100_to_Process <- dir(pathScenarios1)
length(Scenarios100_to_Process)

ini <- 1
fin <- n100.4 * 10

fin-ini +1

tic("Individual Scenarios")
Model <- c("EII","VII","EEI","VEI","EEE","VVV")

status <- mclapply(Scenarios100_to_Process[ini:fin], function(x)
    {

    tryCatch(
      {
        SFilename <- str_replace(x,"S_","SV_")
        FilesProcessed <- dir(pathSFiles)
        if(is_empty(intersect(FilesProcessed,SFilename))) 
        {
          GenerateSFile_HLS(x,pathScenarios1,pathSFiles_HLS_ALL,Model) 
        } else cat("\n The file ",SFilename, " already exists in the directory. \n")
        return(1)
      }, error = function(e)
      {
        cat("Error processing scenario: ",x , " \n")
        return(NULL)
      }
    )
    
  }, mc.cores = 1) 
  

toc()
