source("Sim_2_2_4_9010_IND_VO.R")
pathOutput <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Output/"

dir(paste0(pathOutput,"OutputS_2_2_4_9010_IND_VO"))

load(paste0(pathOutput,"OutputS_2_2_4_9010_IND_VO/","S_2_2_4_9010_IND_VO_1.Rdata"))
ls()

sim.A5[[10]]$resumen

table(sim.A5[[10]]$GenData[[1]]$ltest)

table(sim.A5[[10]]$Label_prediction$Labels,sim.A5[[10]]$Label_prediction$Variables)

table(sim.A5[[10]]$Label_prediction$Contamination,sim.A5[[10]]$Label_prediction$Variables)

sim1 <- MultSimPar3(1)
