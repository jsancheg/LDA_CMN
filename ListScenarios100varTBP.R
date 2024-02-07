
# List scenarios of 100 variables to be processed

source("ListScenarios.R")

ListSFilesTBP <- str_replace_all(setdiff(str_replace_all(Scenarios100,"S_","SV_"),dir(pathSFiles)),"SV_","S_")
ListSSFilesTBP <- str_replace_all(setdiff(str_replace_all(Scenarios100,"S_","SSV_"),dir(pathSSFiles)),"SSV_","S_")

length(ListSFilesTBP)
length(ListSSFilesTBP)

ListScenarios100varTBP <- list(SFiles_Scenarios = ListSFilesTBP, SSFiles_Scenarios = ListSSFilesTBP)

saveRDS(ListScenarios100varTBP, "Scenarios100varTBFit.RDS")

dfListScenarios100 <- readRDS("Scenarios100varTBFit.RDS")

dfListScenarios100$SFiles_Scenarios

rm(sfListScenarios100)
