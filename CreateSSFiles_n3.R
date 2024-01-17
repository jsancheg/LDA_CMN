source("Semisupervised.R")
source("ListScenariosFiles.R")
source("GSSFile.R")
library(purrr)
pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
pathFiles <- "/home/jsancheg/Documents/SSFiles/"
dir(pathScenarios)
ini <- n5.2 + 1
fin <- n5.3


Scenarios5[n5.2:(n5.2+3)]
mclapply(Scenarios5[ini:fin], function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(pathSSFiles)
  if(is_empty(intersect(FilesProcessed,SSFilename))) GenerateSSFile(x,pathScenarios,pathSSFiles) else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 2)

