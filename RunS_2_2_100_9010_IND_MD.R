ruta <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN"
setwd(ruta)
source("S_2_2_100_9010_IND_MD.R")
#ruta <- "/home/pgrad1/2201449s/R/CMN"
ruta <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Output"
library(tictoc)
tic("One Simulation")
for(i_sim in 4:10){
  sim.A5 <- mclapply(1:10, function(x) {
    sim.progress <- MultSimPar3(1)
  }, mc.cores = 1)
  cat("\nWriting file ",i_sim,"\n")
  filename <- paste0(ruta,"/OutputS_2_2_100_9010_IND_MD/Output.txt")
  #  cat("done run ", i_sim, file = filename, sep = "\n", append=TRUE)
  save(sim.A5,file = paste0(ruta,"/OutputS_2_2_100_9010_IND_MD/S_2_2_100_9010_IND_MD_",i_sim,".Rdata") )
  cat(paste0(i_sim*10," of a 100 at ", Sys.time()), file = filename, sep = "\n", append = TRUE)
}
toc()
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            