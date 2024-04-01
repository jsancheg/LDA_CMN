library(dplyr)
library(tidyr)
library(tidyverse)


source("FunctionsConsolidate.R")
source("VSCMN.R")
# Create dataset
ruta <-  "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN"
pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data"
setwd(ruta)

sort_labels <- function(model_string)
{
  sorted_model <- sort(unlist(str_split(model_string,"-")))
  return(paste(sorted_model,collapse="-"))
}



contCrabs <- function(mug,sg,lab,alpha,eta, ptrain, ns = 100)
{
  # Function that contaminated the data set blue crabs for both sex
  # mug : matrix where each column is the mean of a group
  # sg : matrix or array that contains the variance-covariance matrix for a group
  # lab: vector containing label for the corresponding groups
  # alpha: vector containing the percentage of non contaminated observations for each group
  # eta:   vector containing the inflation factor for each group
  # ptrain: vector containing the percentage of samples included in the training set for groups
  # ns:    number of data set simulated
  
  set.seed(123)
  SVmodel <- list() 
  AccuracyClassSV <- rep(0,ns)
  AccuracyContSV <-rep(0,ns)
  AccuracyClassSatM_C <- rep(0,ns)
  AccuracyContSatM_C <- rep(0,ns)
  AccuracyClassSatM_Nc <- rep(0,ns)
  BlueCrabs$Sex <- BlueCrabs$sex
  G <- length(unique(BlueCrabs$Sex))
  ncont <- rep (0,G)
  nocont <- rep(0,G)
  nocont_train <- rep(0,G)
  nocont_test <- rep(0,G)
  ncont_train <- rep(0,G)
  ncont_test <- rep(0,G)
  
  ntrain <- rep(0,G)
  ntest <- rep(0,G)
  #BlueCrabsCont$sex <- ifelse(BlueCrabsCont$sex == "F",1,2)
  colnames(BlueCrabs)
  i<-1
  nocont_train[1] = round(50 * ptrain[1],0) 
  nocont_train[2] = round(50 * ptrain[2],0) 
  nocont_test[1] = 50 - nocont_train[1]
  nocont_test[2] = 50 - nocont_train[2]

  # number of contaminated samples in the train set
  ncont_train[1] = round(nocont_train[1]/alpha[1],0) - nocont_train[1]
  ncont_train[2] = round(nocont_train[2]/alpha[2],0) - nocont_train[2]
  
  # number of contaminated samples in the test set
  ncont_test[1] = round(nocont_test[1]/alpha[1],0) - nocont_test[1]
  ncont_test[2] = round(nocont_test[2]/alpha[2],0) - nocont_test[2]
  
  
  nocont[1] = nocont_train[1] + nocont_test[1]
  nocont[2] = nocont_train[2] + nocont_test[2]
  
  
  # number of contaminated observations to be simulated
  ncont[1] = ncont_train[1] + ncont_test[1]
  ncont[2] = ncont_train[2] + ncont_test[2]
  # train size for each sex
  ntrain[1] = nocont_train[1] + ncont_train[1]
  ntrain[2] = nocont_train[2] + ncont_train[2] 
  
  # test set for each sex
  ntest[1]= nocont_test[1] + ncont_test[1]
  ntest[2] = nocont_test[2] + ncont_test[2]
  
  
  
  
i<-1    
  for (i in 1:ns)
  {
    GenContSamples <- SimCont(mug,sg,lab,ncont,eta)
    GenContSamples$index <- ns+1:nrow(GenContSamples) 
    colnames(GenContSamples) <-  c("sex","FL","RW","CL","CW","BD","index")
    GenContSamples <- GenContSamples %>% dplyr::select(index,sex,everything()) 
    GenContSamples$Cont <- 1
    colnames(GenContSamples)
    head(GenContSamples)
    
    # no contaminated observations in the training set
    indMnc <- sample(1:ntrain[1],nocont_train[1],replace =  FALSE)
    indFnc <- sample((ntrain[1]+1):sum(ntrain),nocont_train[2],replace =  FALSE)
    
    #    tail(GenContSamples,17)
    
    indMc <- sample(1:ncont[1],ncont_train[1],replace = FALSE)
    indFc <- sample((ncont[1]+1):sum(ncont),ncont_train[2],replace = FALSE)
    
    BlueCrabs$Cont <- 0
    
    colnames(GenContSamples)
    colnames(BlueCrabs)
    colnames(BlueCrabs[,-c(1:3,11)])
    
    BlueCrabsCont <- rbind.data.frame(    BlueCrabs %>% dplyr::select(colnames(GenContSamples))
,GenContSamples)
    
    # sex: "F" - 1
    #      "M" - 2
    
    GenContSamples$Sex <- GenContSamples$sex
    #  GenContSamples$sex <- ifelse(GenContSamples$Sex == "F",1,2)
    #  GenContSamples$sex <- as.factor(GenContSamples$sex)
    levels(GenContSamples$sex)
    levels(GenContSamples$Sex)
    
    colnames(BlueCrabsCont)
    head(BlueCrabs)
    head(GenContSamples)
    colnames(GenContSamples)
    colnames(BlueCrabs)
    colnames(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,11)])
    colnames(GenContSamples[c(indMc,indFc),-c(2,9)])
    str(GenContSamples)
    str(BlueCrabs[c(indMnc,indFnc),-c(1:3,5,11)])
    str(GenContSamples[c(indMc,indFc),-c(2,9)])
    
    
    BlueCrabsTrain <- rbind.data.frame(BlueCrabs[c(indMnc,indFnc),]%>% dplyr::select(colnames(GenContSamples)),
                                       GenContSamples[c(indMc,indFc),])
  #  BlueCrabsTrain <- BlueCrabsTrain %>% dplyr::
    
    BlueCrabsTrain <- BlueCrabsTrain[sample(1:nrow(BlueCrabsTrain)),]
    
    BlueCrabsTest <- rbind.data.frame(BlueCrabs[-c(indMnc,indFnc),-c(1:3,5,11)], 
                                      GenContSamples[-c(indMc,indFc),-c(2,9)])
    
    BlueCrabsTest <- BlueCrabsTest[sample(1:nrow(BlueCrabsTest)),]
    
    
    BlueCrabsTrainX <- BlueCrabsTrain[,-c(1,7)]
    BlueCrabsTrainl <- BlueCrabsTrain$sex
    BlueCrabsTestX <- BlueCrabsTest[,-c(1,7)]
    BlueCrabsTestl <- BlueCrabsTest$sex
    #  SexTrain <- ifelse(BlueCrabsTrain$sex == 1, "F","M")
    #  SexTrain <- factor(SexTrain)
    ContTrain <- ifelse(BlueCrabsTrain$Cont == 0, "NC","C")
    ContTrain <- factor(ContTrain)
    
    #SexTest <- ifelse(BlueCrabsTest$sex == 1, "F", "M")
    ContTest <- ifelse(BlueCrabsTest$Cont == 0, "NC","C")
    
    
    colnames(BlueCrabsTrain)
    colnames(BlueCrabsTrainX)
    
    dfRW <- getOW(BlueCrabsTrainX,BlueCrabsTrainl)
    RW <- dfRW$Var
    variables_saturated_model <- RW
    
    # model including all variables
    saturated_mod  <- ModelAccuracy2(BlueCrabsTrainX,
                                     BlueCrabsTestX,
                                     as.numeric(BlueCrabsTrainl),
                                     as.numeric(BlueCrabsTestl),"EII",
                                     alpharef = 0.98, 
                                     tol = 0.01)
    
    saturated_mod
    AccuracyClassSatM_Nc[i] <- saturated_mod$accTestNc
    AccuracyClassSatM_C[i] <- saturated_mod$accTestC
    
    saturated_mod$accTestNc
    
    auxTestCont <- rep(0,length(BlueCrabsTestl))
    
    for (j in 1:length(auxTestCont))
    {
      auxTestCont[j] <- 1-saturated_mod$predv[j,BlueCrabsTestl[j]]
    }
    
    AccuracyContSatM_C[i] <-sum(auxTestCont == BlueCrabsTest$Cont)/length(BlueCrabsTest$Cont)
    
    # model including selected variables
    modSV <-fHLvarSearch2(BlueCrabsTrainX
                          ,BlueCrabsTestX,RW,
                          as.numeric(BlueCrabsTrainl),
                          as.numeric(BlueCrabsTestl),"E",
                          alpharef =0.99,tol=0.01,epsilon = 0)
    
    SVmodel[[i]] <- modSV$Selectedmodel
    AccuracyClassSV[i] <-  modSV$Accuracy
    
    modSV$posCM
    TestContSV <- rep(0,length(BlueCrabsTestl))
    for (j in 1:length(TestContSV))
    {
      TestContSV[j] <- 1- modSV$models[[modSV$posCM]]$predv[j,BlueCrabsTestl[j]]
    }
    AccuracyContSV[i] <- sum(TestContSV == BlueCrabsTest$Cont)/ length(BlueCrabsTest$Cont)
    
    
  }

SVmodel1 <- lapply(SVmodel, function(i) paste(unlist(i),collapse = "-"))
SVmodel1 <- ldply(SVmodel1)
colnames(SVmodel1) <- "Model"
SVmodel1

aux_df <- data.frame(CR_SatMNc = AccuracyClassSatM_Nc,
                     CR_SatMC = AccuracyClassSatM_C,
                     Accuracy_SatCont = AccuracyContSatM_C,
                     CR_SV = AccuracyClassSV,
                     Accuracy_SVCont = AccuracyContSV)

metrics_res <- aux_df %>% 
  summarise(Accuracy_SatMNc = mean(CR_SatMNc),
            Accuracy_SatMC = mean(CR_SatMC),
            Accuracy_SatCont = mean(Accuracy_SatCont),
            Accuracy_SV = mean(CR_SV),
            Accuracy_SVCont = mean(Accuracy_SVCont))


  df_resumen <- cbind.data.frame(SVmodel1,aux_df)

  output <-list ( Metrics_res = metrics_res, 
                 Metrics_models = df_resumen)

  return(output)
  
}
