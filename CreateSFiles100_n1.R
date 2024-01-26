# 
source("Semisupervised.R")
source("ListScenariosFiles.R")
source("GSSFile.R")
source("GSFile.R")
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


ini <- 1
fin <- n2.100.1
fin-ini + 1

tic(paste0("Supervised files fitted :"),as.character(fin-ini))
mclapply(Scenarios2.100[ini:fin], function(x){
  
  command2 <- "ls /home/pgrad1/2201449s/R/CMN/SFiles"
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- capture.output(ssh_exec_wait(my_ssh_session,command2))
  scp_download(my_ssh_session, paste0("/home/pgrad1/2201449s/R/CMN/Scenarios3/",x),pathScenarios )
  if(is_empty(intersect(FilesProcessed,SFilename))) 
    {
      Output <- GenerateSFile_SSH(x,pathScenarios)
      saveRDS(Output,paste0(pathSFiles,SFilename))
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
      
            
    }else {
      cat("\n The file ",SSFilename, " already exists in the directory. \n")
      }
  
  
  
  
}, mc.cores = 1)
toc()
ssh_disconnect()