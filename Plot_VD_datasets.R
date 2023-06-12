pathPro <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Output/"


library(stringr)
library(stringi)
library(ggplot2)
library(lattice)


files <- dir(pathPro)

check_file_numbers <- function(path)
{
  auxfiles <- dir(path)
  return(length(auxfiles))
  
}

VO_path_aux <- files[str_detect(files,"VO")]



VO_path <- VO_path_aux[sapply(VO_path_aux,function(x) check_file_numbers(paste0(pathPro,x)) ) > 2]

dir(paste0(pathPro,VO_path[2]))

load(paste0(pathPro,VO_path[2],"/S_2_2_100_5050_SCBNSV_VO_4.Rdata"))



ls()

XTrain <- sim.A5[[1]]$GenData[[1]]$Xtrain
ltrain <- sim.A5[[1]]$GenData[[1]]$ltrain



plot(XTrain[,2],XTrain[,4], col = ltrain)

sim.A5[[1]]$resumen


?plot
