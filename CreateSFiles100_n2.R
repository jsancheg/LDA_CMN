# 
source("utilities.R")
source("Semisupervised.R")
source("ListScenarios.R")
source("GSSFile.R")
source("GSFile.R")
library(tictoc)
library(ssh)
#if (!requireNamespace("googledrive", quietly = TRUE)) {
#  install.packages("googledrive")
#}
#if (!requireNamespace("readr", quietly = TRUE)) {
#  install.packages("readr")
#}
#library(googledrive)
#library(gargle)
#library(readr)

# Ubunto paths
# Windows path
#pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
#pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"

sys_info <- Sys.info()

if(!sys_info["nodename"] == "LAPTOP-ADR3M911")
{
  my_ssh_session <- ssh_connect("2201449s@130.209.66.80:22")
  local_SSFile_path <- "/home/jsancheg/Documents/SSFiles/"
  local_SFile_path <- "/home/jsancheg/Documents/SFiles/"
  
  remote_username <- "2201449s"
  remote_server_ip <- "130.209.66.139"
  remote_SSFile_path <- "/home/pgrad1/2201449s/R/CMN/SSFiles"
  remote_SFile_path <- "/home/pgrad1/2201449s/R/CMN/SFiles"
  remote_Scenarios_path <- "/home/pgrad1/2201449s/R/CMN/Scenarios3"
  dir(local_SSFile_path)
  command1 <- "ls /home/pgrad1/2201449s/R/CMN/Scenarios3"
  result1 <- ssh_exec_wait(my_ssh_session,command1)
  
}




ini <- n2.100.2+1 
fin <- n2.100.3
fin-ini + 1

tic(paste0("Supervised files fitted :"),as.character(fin-ini))
mclapply(Scenarios100[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSFiles)
  
      if(!sys_info["nodename"] == "LAPTOP-ADR3M911")
      {
          command2 <- "ls /home/pgrad1/2201449s/R/CMN/SFiles"
          FilesProcessed <- capture.output(ssh_exec_wait(my_ssh_session,command2))
          scp_download(my_ssh_session, paste0("/home/pgrad1/2201449s/R/CMN/Scenarios3/",x),pathScenarios )
        }
  
  if(is_empty(intersect(FilesProcessed,SFilename))) 
    {
      Output <- GenerateSFile(x,pathScenarios,pathSFiles)
      saveRDS(Output,paste0(pathSFiles,SFilename))
  }else {
    cat("\n The file ",SFilename, " already exists in the directory. \n")
  }
  
      
      if(!sys_info["nodename"] == "LAPTOP-ADR3M911")
      {
        scp_upload(my_ssh_session,paste0(pathSFiles,SFilename),"/home/pgrad1/2201449s/R/CMN/SFiles/")
        filePathScenario <- paste0(pathScenarios,x)
        filePathSFile <- paste0(pathSFiles,SFilename)
        
        # remote the file containing the scenario
        
        if (file.exists(filePathScenario)) 
        {
          # Remove the file
          file.remove(filePathScenario)
          cat("File deleted successfully.\n")
        } else {
          cat("File does not exist.\n")
        }
        
        # remote the file containing the generate SFile
        
        if (file.exists(filePathSFile)) 
        {
          # Remove the file
          file.remove(filePathSFile)
          cat("File deleted successfully.\n")
        } else {
          cat("File does not exist.\n")
        }
        
      }
      
            
  
  
  
  
}, mc.cores = 1)

toc()
