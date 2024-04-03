library(stringr)
source("Semisupervised.R")
source("Semisupervised_Vectorize.R")
source("SimulateScenario.R")
source("GSSFile.R")
source("ListScenarios.R")

ssmetrics_21_03_2024 <- readRDS("SSMetrics_2024_03_21.RDS")

ssmetrics <- ssmetrics_21_03_2024
colnames(ssmetrics)

head(ssmetrics$File)
options(scipen = 999)

missing_values <- colSums(is.na(ssmetrics))
missing_values
total_rows <- nrow(ssmetrics)/3
total_rows

ssdf <-ssmetrics

# select files of interest

ssdf <- ssdf %>% mutate(Precision_Cont = Precicison_Cont)


Files_for_SS_Scenario <- ssdf %>%filter(Training_Proportion=="75",Class_Proportion=="BAL",Number_Separating_Variables==3,Number_Classes == 2,
               Number_Variables == 5) 

Files_for_SS_Scenario
length(unique(Files_for_SS_Scenario$File))

x <- unlist(Files_for_SS_Scenario$File)[1]

x

parameters_for_simulation <- get_factors_from_file_name(x)

class(parameters_for_simulation)
as.matrix(parameters_for_simulation)

SimScenario(as.matrix(parameters_for_simulation),10,pathScenarios)

nFiles <- length(unique(Files_for_SS_Scenario$File))

sapply(2:nFiles, function(i) {
        length_name <-str_length(unlist(Files_for_SS_Scenario$File)[i]) 
        
        aux_x <- substr(Files_for_SS_Scenario$File[i],4,length_name)

        x <- paste0("S",aux_x)
        parameters_for_simulation <- get_factors_from_file_name(x)
        
        SimScenario(as.matrix(parameters_for_simulation),10,pathScenarios)
})

unlabelled <- rep(1:5)/10
unlabelled 

# semi-supervised with 10%, 20%, 30%, 40% of labelled data

for(i in 0.5:0.9)
{
  sapply(str_replace(unique(Files_for_SS_Scenario$File),"SSV_","S_"),function(x) {
    
    Model <- c("EII","VII","EEI","VEI","EEE","VVV")
    
    if(! length(intersect(x,dir(pathSSFiles) ) ) >0 )
      GenerateSSFile_vectorize(file_name = x,pathScenarios = pathScenarios,pathOutput = pathSSFiles,Model = Model,
                               Search = "GS",pnolabeled = i)
    
  })
  
  
}



# supervised
sapply(str_replace(unique(Files_for_SS_Scenario$File),"SSV_","S_"),function(x) {
  
  Model <- c("EII","VII","EEI","VEI","EEE","VVV")
  
  if(! length(intersect(x,dir(pathSSFiles) ) ) >0 )
    GenerateSSFile_vectorize(file_name = x,pathScenarios = pathScenarios,pathOutput = pathSSFiles,Model = Model,
                             Search = "GS",pnolabeled = 0)
  
})


x <- unlist(Files_for_SS_Scenario$File)[2]


Model <- c("EII","VII","EEI","VEI","EEE","VVV")



