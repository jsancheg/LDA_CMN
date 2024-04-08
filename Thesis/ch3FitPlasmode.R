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


# Windows path
system_info <- Sys.info()
#OS_name <- system_info("")
pc_name <- system_info['nodename'] 

if(pc_name == "LAPTOP-ADR3M911")
{
  
  pathwd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
  path_plasmode <- paste0(pathwd,"Thesis//Plasmode/")
  path_plasmode_crabs <- paste0(path_plasmode,"crabs/")
  path_plasmode_wdbc <- paste0(path_plasmode,"wdbc/")
  path_plasmode_wine <- paste0(path_palsmode,"wine/")
  path_scrabs <- paste0(path_plasmode,"Scrabs/")
  
}else if(pc_name == "WildFree")
{
  pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
  pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
  pathSFiles <- "/home/jsancheg/Documents/SFiles/"
  
}


list_files <- dir(path_plasmode_crabs)
list_files


ini <- 1
fin <- length(list_files)
#fin <- n2.5
fin-ini +1

tic("File 5 variables")

Model <- c("EII","VII","VEI","EEI","EVI","VVI","EEE","VVV")
Model <- c("EII","VII","EEI","VEI","EEE","VVV")


tic("SFiles 1 to n5")
status<-mclapply(list_files[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(path_scrabs)
  tryCatch(
    {
      if(is_empty(intersect(FilesProcessed,SSFilename)))
      {
        
        GenerateSFile_vectorize(x,path_plasmode_crabs,path_scrabs, Model, pnolabeled = 0) 
      }else cat("\n The file ",SFilename, " already exists in the directory. \n")
      
      return(1)
    }, error = function(e){
      cat("Error fitting scenario: ",x, "\n")
      return(NULL)
      
    }
  )
  
  
}, mc.cores = 1)
toc()
