
pathwd <- getwd()
pathSFiles <- paste0(pathwd,"/s_wine/")
dir(pathSFiles)

aux <-readRDS(paste0(pathSFiles,"s_wineS_A07_07_07_E5_30_30_Wine_1.RDS")) 

head(aux)
