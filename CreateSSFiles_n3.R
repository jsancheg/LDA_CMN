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

system.info <- Sys.info()
operative_system <- system.info['sysname']
if(operative_system == "Linux" )
{
  # Ubuntu paths
   pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
   pathFiles <- "/home/jsancheg/Documents/SSFiles/"
} else {
  # Windows path
   pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
   pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
  
}



# Function to authenticate with google drive using service account json
#authenticate_service_account <- function(json_key_path){
  #token <- gargle::service_token(json = json_key_path, 
  #                               scopes = "https://www.googleapis.com/auth/drive" )
  #drive_auth(path = json_key_path, service_account = TRUE, token = token)
#  googledrive::drive_auth(path = json_key_path, cache = TRUE)
#}

# Function to list files in a specific folder in Google Drive
#list_files_in_folder <- function(folder_id){
#  files <- drive_ls(folder_id)
#  return(files)
#}

#json_path <- "/home/jsancheg/Descargas/my-demo-project-1541388437111-0c22b3f48973.json"
#folder_id <- "https://drive.google.com/drive/folders/1uoMrPEdqzH2-_vJgM4Kog78WoMP5ePy6/"
#authenticate_service_account(json_path)
#list_files_in_folder()
#drive_ls(folder_id)

#list_files_in_folder(pattern = "1uoMrPEdqzH2-_vJgM4Kog78WoMP5ePy6", type = "folder")

#drive_find()
#drive_find(pattern = "S_")



dir(pathScenarios)
ini <- n5.2 + 1
fin <- n5.3

fin - ini

FilesProcessed <- dir(pathSSFiles)


tic("SSFiles")
mclapply(Scenarios5[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
  if(is_empty(intersect(FilesProcessed,SSFilename))) GenerateSSFile(x,pathScenarios,pathSSFiles) else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)

toc()
