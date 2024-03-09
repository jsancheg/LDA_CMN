# The following code generate the scenarios in the cases that
# one of the simulated scenarios of the pool of 10 stop the fitting at the others
# scenarios

source("ListScenarios.R")
source("SimulateScenario.R")

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



# Simulate 20 simulations per Scenario

nSimulations <- 20

x <- Scenarios_to_be_generated[1]
x


      Simulation_Parameters<- get_factors_from_file_name(x)
      SimScenario_Individual(Simulation_Parameters,20,path_Scenarios_Hard_To_Fit)

sapply( 2:length(Scenarios_to_be_generated), function (i) {
  Simulation_Parameters<- get_factors_from_file_name(Scenarios_to_be_generated[i])
  SimScenario_Individual(Simulation_Parameters,20,path_Scenarios_Hard_To_Fit)
  
})      
