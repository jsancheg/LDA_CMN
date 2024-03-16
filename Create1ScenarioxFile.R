# The following code generate the scenarios in the cases that
# one of the simulated scenarios of the pool of 10 stop the fitting at the others
# scenarios
source("utilities.R")
source("Semisupervised.R")
source("ListScenarios.R")
source("SimulateScenario.R")
source("GSFile.R")
source("GSSFile.R")


path_SSFilesOld <- "E:/University of Glasgow/Thesis/SSFiles_Old_PNolabeled50/"
path_SFilesOld <- "E:/University of Glasgow/Thesis/SFiles_Old/"
path_Scenarios_Hard_To_Fit <- "E:/University of Glasgow/Thesis/Scenarios_Hard_To_Fit/"

path_SSFiles_Hard_To_Fit <- "E:/University of Glasgow/Thesis/SSFiles_Hard_To_Fit/"
path_SFiles_Hard_To_Fit <- "E:/University of Glasgow/Thesis/SFiles_Hard_To_Fit/"


files_no_generated_SSV <- setdiff(str_replace(dir(pathScenarios),"S_","SSV_"),dir(path_SSFilesOld) ) 
files_no_generated_SSV

files_no_generated_SV <- setdiff(str_replace(dir(pathScenarios),"S_","SV_"),dir(path_SFilesOld) ) 

files_no_generated_SV

ScenariosSSV_To_Be_Generated <-paste0("S",str_sub(files_no_generated_SSV,4,str_length(files_no_generated_SSV)))

ScenariosSV_To_Be_Generated <-paste0("S",str_sub(files_no_generated_SV,3,str_length(files_no_generated_SV)))

intersect(ScenariosSSV_To_Be_Generated,ScenariosSV_To_Be_Generated)

setdiff(ScenariosSSV_To_Be_Generated,ScenariosSV_To_Be_Generated)


Scenarios_to_be_generated <- unique(ScenariosSV_To_Be_Generated,ScenariosSSV_To_Be_Generated)

scenarios_to_be_decomposed <- ScenariosSSV_To_Be_Generated


# Decompose these scenarios
n_scenarios_to_decompose <-length(scenarios_to_be_decomposed)

tic("Decompose")
sapply(1:n_scenarios_to_decompose,function(i)
{
  Deconstruct_Pool_Scenarios(Scenarios100[i],pathScenarios,pathScenarios1)
})
toc()


scenarios_hard_to_fit <- dir(path_Scenarios_Hard_To_Fit)

Model <- c("EII","VII","VEI","EEI","EVI","VVI","EEE","VVV")
Model <- c("EII","VII","EEI","VEI","EEE","VVV")

# Generate Supervised files

tic("SFiles_hard_to_fit")
status <-mclapply(scenarios_hard_to_fit, function(x){
  
  SFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(path_SFiles_Hard_To_Fit)
  if(is_empty(intersect(FilesProcessed,SFilename))) 
  {
    tryCatch(
      {
        
        GenerateSFile(x,path_Scenarios_Hard_To_Fit,path_SFiles_Hard_To_Fit,Model) 
        return(1)
      }, error = function(e){
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
        
      }
    )
    
  }else cat("\n The file ",SFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)
toc()



tic("SSFiles_hard_to_fit")
status <-mclapply(scenarios_hard_to_fit, function(x){
  
  SSFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(path_SSFiles_Hard_To_Fit)
  if(is_empty(intersect(FilesProcessed,SSFilename))) 
  {
    tryCatch(
      {
        
        GenerateSSFile(x,path_Scenarios_Hard_To_Fit,path_SSFiles_Hard_To_Fit,Model,pnolabeled = 0.50) 
        return(1)
      }, error = function(e){
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
        
      }
    )
    
  }else cat("\n The file ",SSFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)
toc()


# Update 


files_no_generated_SSV_1 <- setdiff(str_replace(scenarios_hard_to_fit,"S_","SSV_"),dir(path_SSFiles_Hard_To_Fit) ) 
files_no_generated_SSV

files_no_generated_SV <- setdiff(str_replace(dir(pathScenarios),"S_","SV_"),dir(path_SFilesOld) ) 

files_no_generated_SV

ScenariosSSV_To_Be_Generated <-paste0("S",str_sub(files_no_generated_SSV,4,str_length(files_no_generated_SSV)))

ScenariosSV_To_Be_Generated <-paste0("S",str_sub(files_no_generated_SV,3,str_length(files_no_generated_SV)))

intersect(ScenariosSSV_To_Be_Generated,ScenariosSV_To_Be_Generated)

setdiff(ScenariosSSV_To_Be_Generated,ScenariosSV_To_Be_Generated)


Scenarios_to_be_generated <- unique(ScenariosSV_To_Be_Generated,ScenariosSSV_To_Be_Generated)


# Simulate new 20 simulations per Scenario

nSimulations <- 20

x <- Scenarios_to_be_generated[1]
x


      Simulation_Parameters<- get_factors_from_file_name(x)
      SimScenario_Individual(Simulation_Parameters,20,path_Scenarios_Hard_To_Fit)

sapply( 2:length(Scenarios_to_be_generated), function (i) {
  Simulation_Parameters<- get_factors_from_file_name(Scenarios_to_be_generated[i])
  SimScenario_Individual(Simulation_Parameters,20,path_Scenarios_Hard_To_Fit)
  
})      


tic("SSFiles_hard_to_fit")
status <-mclapply(scenarios_hard_to_fit, function(x){
  
  SFilename <- str_replace(x,"S_","SSV_")
  FilesProcessed <- dir(path_SFiles_Hard_To_Fit)
  if(is_empty(intersect(FilesProcessed,SFilename))) 
  {
    tryCatch(
      {
        
        GenerateSFile(x,path_Scenarios_Hard_To_Fit,path_SFiles_Hard_To_Fit,Model,pnolabeled = 0) 
        return(1)
      }, error = function(e){
        cat("Error fitting scenario: ",x, "\n")
        return(NULL)
        
      }
    )
    
  }else cat("\n The file ",SFilename, " already exists in the directory. \n")
  
}, mc.cores = 1)
toc()
