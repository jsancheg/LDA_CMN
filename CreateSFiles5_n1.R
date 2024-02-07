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

#ScenariosTBF <- readRDS("Scenarios100varTBFit.RDS")
#Scenarios100S <- ScenariosTBF$SFiles_Scenarios
#Scenarios100SS <-ScenariosTBF$SSFiles_Scenarios
  
dir(pathScenarios)
ini <- n100.8
fin <- n100.9
#fin <- n2.5
fin-ini +1

tic("File 5 variables")
my_ssh_session <- ssh_connect("2201449s@130.209.66.82")

  mclapply(Scenarios100[ini:(ini+3)], function(x)
    {
    if(pc_name == "WildFree")
    {
      if(is_empty(intersect(x,dir(pathScenarios))) )
          scp_download(my_ssh_session, paste0("/home/pgrad1/2201449s/R/CMN/Scenarios3/",x),                             pathScenarios )
    }
    
    tryCatch(
      {
        SFilename <- str_replace(x,"S_","SV_")
        FilesProcessed <- dir(pathSFiles)
        if(is_empty(intersect(FilesProcessed,SFilename))) 
        {
          GenerateSFile(x,pathScenarios,pathSFiles) 
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
