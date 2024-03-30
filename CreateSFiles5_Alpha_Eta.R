# 
sys_info <- Sys.info()
if(sys_info["nodename"] == "WildFree")
  setwd("/home/jsancheg/git_environment/LDA_CMN/")

source("Semisupervised.R")
source("ListScenariosAlpha_Eta.R")
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

#ScenariosTBF <- readRDS("Scenarios100varTBFit.RDS")
#Scenarios100S <- ScenariosTBF$SFiles_Scenarios
#Scenarios100SS <-ScenariosTBF$SSFiles_Scenarios
  
dir(pathScenarios)
ini <- 1
fin <- n5
#fin <- n2.5
fin-ini +1

tic("File 5 variables")

Model <- c("EII","VII","VEI","EEI","EVI","VVI","EEE","VVV")
Model <- c("EII","VII","EEI","VEI","EEE","VVV")


tic("SFiles 1 to n5")
status<-mclapply(Scenarios5[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSFiles)
  tryCatch(
    {
        if(is_empty(intersect(FilesProcessed,SFilename)))
        {
      
      GenerateSFile(x,pathScenarios,pathSFiles, Model) 
        }else cat("\n The file ",SFilename, " already exists in the directory. \n")
      
      return(1)
    }, error = function(e){
      cat("Error fitting scenario: ",x, "\n")
      return(NULL)
      
    }
  )
  
  
}, mc.cores = 1)
toc()

