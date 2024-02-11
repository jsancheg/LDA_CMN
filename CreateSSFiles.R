source("Semisupervised.R")
source("ListScenarios.R")
source("GSFile.R")
source("GSSFile.R")

#pathScenarios <- "/home/pgrad1/2201449s/R/CMN/ScenariosNew/"
#pathSSFiles <- "/home/pgrad1/2201449s/R/CMN/ScenariosNew/SSFiles/"


ScenariosFiles <- dir(pathScenarios)
SSFilesProcessed <- dir(pathSSFiles)
nSSFilesProcessed <- 0
nssFilesToProcessed <- 0
nFiles <- 0
timeFile <- list()

nFiles <- length(ScenariosFiles)
nFiles


nSSFilesProcessed <- length(SSFilesProcessed)

if(nSSFilesProcessed == 0 )
{
  SSFilesToProcess <- paste0(pathScenarios,ScenariosFiles)
  
  nSSFilesToProcess <- nFiles
  
}else {
  nSSFilesToProcess <- nFiles - nSSFilesProcessed
  SSFilesToProcess <- paste0(pathScenarios,setdiff(ScenariosFiles,str_replace(SSFilesProcessed,"SSV_","S_") ) )

}

SimFiles <- mclapply(1:nSSFilesToProcess, function(i){
  nameFile <- str_split(SSFilesToProcess[i],"/")[[1]][7]  
  Number_Variables <- str_split_1(nameFile,"_")[[3]]
  if(Number_Variables == 2)
  {
    variables_True_Model <- c("X2","X4")
  }else if(Number_Variables == 3)
  {
    variables_True_Model <- c("X2","X4","X5")
  }
  Output <- SemiSupervised_HLS(SSFilesToProcess[[i]],CE,variables_True_Model, 
                               pnolabeled = 0.5, niterations = 10,
                               alpharef = 0.99, tol = 0.01, epsilon = 0)
  SSfile_name <- str_replace(str_split_1(SSFilesToProcess[[i]],"/")[7],"S_","SSV_")
  paste0(pathSSFiles,SSfile_name)
  saveRDS(Output,paste0(pathSSFiles,SSfile_name))
  
},mc.cores = 2) 

