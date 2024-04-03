library(dbplyr)
library(MLmetrics)
getwd()
source("Semisupervised.R")
source("ListScenarios.R")

path_input <- pathSFiles_Old
path_output <- getwd()
name_output_file <- sMetrics_OLd_2024_03_18_wider

wider_metrics <- create_metrics_wider_format(path_input,path_output,name_output_file)

