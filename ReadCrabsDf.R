# Read crabs dataset
pathProcessDf <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathProcessDf)

pathProcessDf <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathProcessDf)
pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data"
pathProCrabs <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_Crabs/"

#work_path <- "E:/University of Glasgow/Literature review/R Code/"
#setwd(work_path)

# read the file 
CrabsDf <- read.csv(paste0(pathFile,"/","Crabs.csv"))
CrabsDf$sex <- as.factor(CrabsDf$sex)
head(CrabsDf)
