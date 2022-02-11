
pathProcDf <- paste0(WorkPath,"/Processed data version 1/")
rutaRaw <- paste0(WorkPath,"/Raw Data/")


dfCoffee <- read.csv(paste0(pathProcDf,"FTIR_Spectra_instant_coffee_processed.csv"))
dfMeat <- read.csv(paste0(pathProcDf,"MIRFreshMeats_processed.csv"))
