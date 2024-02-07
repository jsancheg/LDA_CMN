# 


source("Semisupervised.R")
source("ListScenarios.R")
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

dir(pathScenarios)
ini <- n100.2+1 
fin <- n100.3
fin-ini + 1


tic("SSFiles scenarios with 100 variables from n3 to n4")
status <- mclapply(Scenarios100[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
#  write.table(SSFilename,"LastFile.csv",sep = ",",col.names = "FileName",row.names = 1,append = TRUE )
#  if(is_empty(intersect(FilesProcessed,SSFilename))) 
 # {
    tryCatch(
      {
        GenerateSSFile(x,pathScenarios,pathSSFiles)
        return(1)
      }, error = function(e)
      {
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
      }
    )
  #} else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)
toc()

