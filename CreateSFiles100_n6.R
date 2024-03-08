# 
source("Semisupervised.R")
source("ListScenarios.R")
source("GSFile.R")
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


# Ubunto paths
# pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
# pathFiles <- "/home/jsancheg/Documents/SSFiles/"

# Windows path
#pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
#pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
#pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"


dir(pathScenarios)
ini <- n100.5 + 1 
fin <- n100.6
fin-ini + 1

Model <- c("EII","VII","VEI","EEI","EVI","VVI","EEE","VVV")
Model <- c("EII","VII","EEI","VEI","EEE","VVV")


tic("SFiles n100.3 to n100.4")
status<-mclapply(Scenarios100[ini:fin], function(x){
  
  SFilename <- str_replace(x,"S_","SV_")
  FilesProcessed <- dir(pathSFiles)
    tryCatch(
      {
        #  if(is_empty(intersect(FilesProcessed,SFilename)))
        #  {
        
        GenerateSFile(x,pathScenarios,pathSFiles, Model) 
        #  }else cat("\n The file ",SFilename, " already exists in the directory. \n")
        
        return(1)
      }, error = function(e){
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
        
      }
    )
    
  
}, mc.cores = 1)
toc()
