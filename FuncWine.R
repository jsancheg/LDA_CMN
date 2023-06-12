
funcSample <- function(X,y,vpi)
{
  # X: variables 
  # y: vector of response variables with labels
  # vpi: vector of the percentage contribution for each class
  
  if (sum(vpi)!=1) stop("The vector of contribution percentage for groups should sum 1")
  
  G = length(unique(y))
  
  if (length(vpi)!= G) stop("The vector must be the same length as the number of groups")
  
  # ng  number of observations in each group
  ng <- summary(factor(y))
  aux <- matrix(0.0,ncol = 3, nrow = 3)
  val <- rep(0,G)
  bolval <- NA
  # mg <- number of samples taken from each group
  for (g in 1:G)
  {
      total = round(ng[g]/vpi[1],0)
      aux[g,1] = floor(total * vpi[1])
      aux[g,2] = floor(total *vpi[2])
      aux[g,3] = floor(total * (1-sum(vpi[1:2])))
      bolval[g] = all(aux[g,]<ng)
  }

  
  aux
  ng
  bolval
  if (all(bolval == FALSE)) stop("Change the proportions one of the group does not have enough observations")
  
  val <- apply(aux,1,sum)
  
  maximo <- max(val[bolval])
  
  indmax <- which(val == maximo)
  
  mg <- aux[indmax,]
  
  mg_samples <- vector("list",G)
  ind_samples <- vector("list",G)

  for (g in 1:G)
  {

    mg_samples[[g]] <- sample(1:ng[g], mg[g] ,replace = FALSE )
  }
  
    return(mg_samples)
}


contWine <- function(X,y,lab,vpi,alpha,eta,ptrain,ns = 100)
{
   # Function that contaminate the wine data set
  # mug : matrix where each column is the mean of a group
  # sg : matrix or array that contains the variance-covariance matrix for a group
  # X  : matrix or array containing the covariates
  # y  : vector containing the response (group information)
  # lab: vector containing label for the corresponding groups
  # vpi: vector containing the proportion of the sample for each group
  # alpha: vector containing the percentage of non contaminated observations for each group
  # eta:   vector containing the inflation factor for each group
  # ptrain: vector containing the percentage of samples included in the training set for groups
  # ns:    number of data set simulated
  
  SVmodel <- list() 
  AccuracyClassSV <- rep(0,ns)
  AccuracyContSV <-rep(0,ns)
  AccuracyClassSatM_C <- rep(0,ns)
  AccuracyContSatM_C <- rep(0,ns)
  AccuracyClassSatM_Nc <- rep(0,ns)
  G <- length(unique(y))
  ncont <- rep (0,G)
  nocont <- rep(0,G)
  nocont_train <- rep(0,G)
  nocont_test <- rep(0,G)
  ncont_train <- rep(0,G)
  ncont_test <- rep(0,G)
  ntrain <- rep(0,G)
  ntest <- rep(0,G)
  ng <- rep(0,G)
  indsamples <- funcSample(X,y,vpi)
  p <- ncol(X)
  mug <- matrix(0.0,nrow = p, ncol = G)
  sg <- array(0.0, dim = c(p,p,G))
  
  
  for (g in 1:G) 
    {
      mug[,g] <- X[y==g,] %>% apply(2,mean)
      sg[,,g] <- X[y == g,] %>% var
      ng[g] <- length(indsamples[[g]])
    }

  #BlueCrabsCont$sex <- ifelse(BlueCrabsCont$sex == "F",1,2)
  for (g in 1:G)
  {
    nocont_train[g] = round(ng[g] * ptrain[g],0) 
    nocont_test[g] = ng[g] - nocont_train[g]
    
    # number of contaminated samples in the train set
    ncont_train[g] = round(nocont_train[g]/alpha[g],0) - nocont_train[g]
    
    # number of contaminated samples in the test set
    ncont_test[g] = round(nocont_test[g]/alpha[g],0) - nocont_test[g]

    nocont[g] = nocont_train[g] + nocont_test[g]
    
    # number of contaminated observations to be simulated
    ncont[g] = ncont_train[g] + ncont_test[g]
    
    # train size
    ntrain[g] = nocont_train[g] + ncont_train[g]

    # test set for each sex
    ntest[g]= nocont_test[g] + ncont_test[g]

  }
  
  indnc <- vector("list",G)
  indc <- vector("list",G)
  Xgnc_train <- vector("list",G)
  Xgnc_test <- vector("list",G)
  Xgc_train <- vector("list",G)
  Xgc_test <- vector("list",G)
  i<-1    
  for (i in 1:ns)
  {
    GenContSamples <- SimCont(mug,sg,unique(y),ncont,eta)
    GenContSamples$index <- (nrow(X)+1):(nrow(X) +nrow(GenContSamples) )
    GenContSamples <- GenContSamples %>% dplyr::select(index, everything())
    colnames(GenContSamples)
    ncolumns <- ncol(GenContSamples)
    colnames(GenContSamples)[3:ncolumns] <-  colnames(X)
    GenContSamples$Cont <- 1
    colnames(GenContSamples)
    head(GenContSamples)
  
    # Generate a set with the composition required for each group  
    auxindnc <- funcSample(X,y,vpi)
    winedf <- data.frame(X)
    winedf$Cont <- 0
    winedf$class <- y
    winedf$index <- 1:nrow(X)
    winedf <- winedf %>% dplyr::select(index,class, everything())
    colnames(winedf)
    
    # Generate contaminated observation with the sane group composition used 
    # in non contaminated set
    auxindc <- funcSample(GenContSamples[,-1], GenContSamples$class, vpi)
#    indsampleTrain_nc <- vector("list",G)
    GenContSamples$class
    
    for (g in 1:G)
    {
      # non contaminated set
      auxDfg <- winedf %>% filter(class == g)
      subsetg <- auxDfg[auxindnc[[g]],]
      Xgnc_train[[g]] <- subsetg %>% slice_sample(n=nocont_train[g], replace = FALSE)

      indsampleTrain_nc <- Xgnc_train[[g]]$index
      indsampleTest_nc <- setdiff(subsetg$index,indsampleTrain_nc)
      
      Xgnc_test[[g]] <-   subsetg %>% filter(index %in% indsampleTest_nc)

      
      # contaminated
      auxDfcont <- GenContSamples %>% filter (class == g)
      subsetgcont <- auxDfcont[auxindc[[g]],]
      
      Xgc_train[[g]] <- subsetgcont %>% slice_sample(n = ncont_train[g], replace = FALSE)
      
      indsampleTrain_c <- Xgc_train[[g]]$index
      indsampleTest_c <- setdiff(subsetgcont$index,indsampleTrain_c)
      
      Xgc_test[[g]] <- subsetgcont %>% filter(index %in% indsampleTest_c)
  
    }
    nrow(winedf)
    
    colnames(GenContSamples)
    table(GenContSamples$Cont)
    table(winedf$Cont)
    
    table(GenContSamples$class)
    table(winedf$class)
    
    # getting rid off index column    
    WineCont <- rbind.data.frame(winedf %>% dplyr::select(-index),
                                 GenContSamples %>% dplyr::select(-index)
    )
    colnames(WineCont)
    nrow(WineCont)
    
    auxTrain_nc <- ldply(Xgnc_train)
    auxTest_nc <- ldply(Xgnc_test)
    indnc_train <- auxTrain_nc$index
    
    table(auxTrain_nc$class)
    table(auxTest_nc$class)
    
    auxTrain_c <- ldply(Xgc_train)
    auxTest_c <- ldply(Xgc_test)
    indc_train <- auxTrain_c$index
    
    auxTrain_c$class

      #  DfTrain <- rbind.data.frame(winedf[indnc_train,]%>% dplyr::select(-index),
      #                                 GenContSamples[unlist(indc_train),] %>% dplyr::select(-index))
        
        
        # apply(Xgnc_train[[3]],2,function(x) any(is.na(x)))
        # apply(Xgc_train[[3]],2,function(x) any(is.na(x)))
        
        # apply(ldply(Xgnc_train),2,function(x) any(is.na(x)))
        
        # apply(ldply(Xgc_train),2,function(x) any(is.na(x)))
        Xgc_train[[2]]$class  
        
        dfTrain <- rbind.data.frame(ldply(Xgnc_train),ldply(Xgc_train))

        apply(dfTrain,2,function(x) any(is.na(x)))
        dfTrain$class
        dfTrain$Cont
        
        colnames(dfTrain)
        DfTrain <- dfTrain[sample(1:nrow(dfTrain)),]
        apply(dfTrain,2,function(x) any(is.na(x)))
        
        table(DfTrain$Cont)
        table(DfTrain$class)
        DfTrain$class
        
        apply(DfTrain,2,function(x) any(is.na(x)))

        dfTest <- rbind.data.frame(ldply(Xgnc_test),ldply(Xgc_test))
        
        # DfTest <- rbind.data.frame(winedf[-indnc_train,-2], 
        #                              GenContSamples[-indc_train,-2])
        
         colnames(dfTest)
         
        DfTest <- dfTest[sample(1:nrow(dfTest)),]
        table(DfTest$Cont)
        table(DfTest$class)    
        
        colnames(DfTrain)
        DfTrainX <- DfTrain %>% dplyr::select(-c(class,index,Cont))
        DfTrainl <- DfTrain$class
        DfTestX <- DfTest %>% dplyr::select(-c(class,index,Cont))
        DfTestl <- DfTest$class
    #  SexTrain <- ifelse(BlueCrabsTrain$sex == 1, "F","M")
    #  SexTrain <- factor(SexTrain)
    ContTrain <- ifelse(DfTrain$Cont == 0, "NC","C")
    ContTrain <- factor(ContTrain)
    
    #SexTest <- ifelse(BlueCrabsTest$sex == 1, "F", "M")
    ContTest <- ifelse(DfTest$Cont == 0, "NC","C")
    
    
    colnames(DfTrain)
    colnames(DfTrainX)
    
    dfRW <- getOW(DfTrainX,DfTrainl)
    RW <- dfRW$Var
    variables_saturated_model <- RW
    
    # model including all variables
    saturated_mod  <- ModelAccuracy2(DfTrainX,
                                     DfTestX,
                                     as.numeric(DfTrainl),
                                     as.numeric(DfTestl),"EII",
                                     alpharef = 0.98, 
                                     tol = 0.01)
    
    saturated_mod
    AccuracyClassSatM_Nc[i] <- saturated_mod$accTestNc
    AccuracyClassSatM_C[i] <- saturated_mod$accTestC
    
    saturated_mod$accTestNc
    
    auxTestCont <- rep(0,length(DfTestl))
    
    for (j in 1:length(auxTestCont))
    {
      auxTestCont[j] <- 1-saturated_mod$predv[j,DfTestl[j]]
    }
    
    AccuracyContSatM_C[i] <-sum(auxTestCont == DfTest$Cont)/length(DfTest$Cont)
    
    # model including selected variables
    modSV <-fHLvarSearch2(DfTrainX
                          ,DfTestX,RW,
                          as.numeric(DfTrainl),
                          as.numeric(DfTestl),"E",
                          alpharef =0.99,tol=0.01,epsilon = 0)
    
    SVmodel[[i]] <- modSV$Selectedmodel
    AccuracyClassSV[i] <-  modSV$Accuracy
    
    modSV$posCM
    TestContSV <- rep(0,length(DfTestl))
    for (j in 1:length(TestContSV))
    {
      TestContSV[j] <- 1- modSV$models[[modSV$posCM]]$predv[j,DfTestl[j]]
    }
    AccuracyContSV[i] <- sum(TestContSV == DfTest$Cont)/ length(DfTest$Cont)
    
    
  }
  
  
  
}
  