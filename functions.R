
# F-test 
# ftest function(df_train, Class)
# df_train: contanins the complete datasets including the response and explanatory variables
# L: contains the name of the variable which contains the label information

source("E:/University of Glasgow/Literature review/R Code/Food Analysis/F-test.R")

gen <- function(nsamples,mu, sigma)
{
  # Objective: This function generate spectra samples using the
  #             rnorm function for each wavelength.
  # mu : vector containing the mean spectra of the class.
  # sigma: vector containing the standard deviation at each wavelength.  
  
  if(!is.vector(sigma)) stop("sigma has to be a vector")
  p <- length(mu)
  output <- array(0, c(nsamples,p))
  for (j in 1:p)
    output[,j] <- rnorm(nsamples,mu[j],sigma[j] )
  
  output <- as.data.frame(output)
  colnames(output) <- names(mu)
  
  return(output)  
}



modMean <- function(mu, peaks, ws, c, shape)
{
  # mu :      vector containing the mean spectra of reference to be changed
  # peaks:    vector that contains the peaks where the mu would be modified around
  # ws:       number of wavelength around the peak that would be modified creating a window 
  # c:        vector of same length than peaks and contains the constant for each peak to make separation
  # nsamples: number of samples to be generated
  # shape:    shape 1 c * exp^|t-s|
  #           shape 2  exp^(c*|t-s|)
  #           shape 3  c * |t-s|  
  #           shape 4 exp^(c*(t-s)^2)
  
  npeaks <- length(peaks)
  wavelengths <- list()
  output <- numeric()
  mean.2 <- mu
  
  if(!is.vector(mu)) stop("mu has to be a vector")
  if(!is.vector(peaks)) stop("peaks has to be a vector")
  if(!is.vector(ws)) stop("windows size has to be a vector of same length as vector for peaks")
  if(!is.vector(c)) stop("windows size has to be a vector of same length as vector for peaks")
  if(!is.vector(shape)) stop("shape has to be a vector of same length as vector for peaks")
  
  if(!is.numeric(mu)) stop("mu has to be a numeric vector")
  if(!is.numeric(peaks)) stop("peaks has to be a numeric vector")
  if(!is.numeric(ws)) stop("ws has to be a numeric vector")
  if(!is.numeric(c)) stop("c has to be a numeric vector")
  if(!is.numeric(shape)) stop("shape has to be a numeric vector")
  
  if(length(ws) == 1) ws <- rep(ws,npeaks)
  if(length(c) == 1) c <- rep(c,npeaks)
  if(length(shape) == 1) shape <- rep(shape,npeaks)
  
  getWavelengths <- function(peak,w,step = 1)
  {
    cells <- numeric()
    getcells <- function(pos,w,step=1) 
    {
      output <- numeric()
      if(!length(pos) == 1) stop("pos has to be only one numeric variable")
      output <- as.vector(seq(pos-w,pos+w,step))
      return(output)
    }
    cells <- sapply(peak,getcells,w,step)
    return(cells)
  }
  
  newMean <- function(wavelengths,mu,peak,coeficient,sh)
  {
    mean2 <-mu
    
    for(j in wavelengths)
    {
      if (sh == 1)
        mean2[j ] <- mu[j ] + coeficient*exp(-abs(j-peak))
      else if(sh == 2)
        mean2[j ]<- mu[j ] + exp(-coeficient*abs(j-peak))
      else if(sh == 3)
        mean2[wavelengths[j] ]<- mu[j ] + coeficient*abs(j-peak)
      else if(sh == 4)
        mean2[j ]<- mu[j ]+ exp(-coeficient* ((j-peak)^2) )
      else if(sh == 5)
        mean2[j ]<- mu[j ]+ exp(- (((j-peak)/coeficient)^2)/2 )
      
    }
    
    return(mean2)
  }
  
  
  for(indpeak in 1:npeaks)
  {
    wavelengths[[indpeak]] <- getWavelengths(peaks[indpeak],ws[indpeak])  
    mean.2 <- newMean(wavelengths[[indpeak]], mean.2, peaks[indpeak], c[indpeak], shape[indpeak])
  }
  
  return(list(Mean2 = mean.2, Peaks = peaks, Wavelengths = wavelengths))
}


correctSingularity <- function(x, gridvalues){
  # The function return the minimum value of a  constant that correct singularity
  # from the grid of values given
  
  # x: is the vector that contains the standard deviation from each wavelength
  # gridvalues: grid of constant to sum in order to correct singularity
  
  
  ngrid <- length(gridvalues)
  p <- length(x)
  
  if(!is.vector(x)) stop ("x has to be a vector of standard deviations")
  
  for(ind1 in 1:ngrid) 
  {
    temp <- det(diag(x,p) + diag(gridvalues[ind1],p))
    if(! temp== 0) {
      return(gridvalues[ind1])
      break}
  }
  return(0)
}


DAArcade <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,type,classes,models,components)
{
  
  output <- array(NA, c(length(ws),length(c),length(models)) )
  sequence <- function(x,w)
  {
    # cat(x,"-",w,"\n")    
    return(seq(x-w,x+w,1))
  }
  
  for(ind1 in 1:length(ws))
  {
    
    if (all(is.list(peaks), length(peaks) == 1)){
      vars <- as.vector(sapply(peaks[[1]],sequence, w = ws[ind1]))
    } else if (all(is.list(peaks), length(peaks) == length(classes))) {
      vars <- as.vector(sapply(peaks[[ind1]],sequence, w = ws[ind1]))
    } else if (!is.list(peaks)) { 
      stop("peaks has to be a list")
    }
    
    for(ind2 in 1:length(c))
    {
      cat(ws[ind1],"-",c[ind2],"\n")
      CompleteDf <- DfSamples(mu,sigma,peaks,nsamples,ws[ind1], c[ind2],type, classes)
      samplesDf <- CompleteDf %>% select(c("Class","Sample",colnames(CompleteDf)[vars]))
      
      n <- nrow(SamplesDf)
      ind <- sample(1:n, ntrain)  
      train <- SamplesDf[ind,-c(1:2)]
      test <- SamplesDf[-ind,-c(1:2)]
      trainl <- SamplesDf[ind,]$Class
      testl  <- SamplesDf[-ind,]$Class
      
      res <- EDDA(train,test,trainl,testl,models,components)
      cat(res, "\n")
      output[ind1,ind2,] <- res[,3]
    }
  }
  dimnames(output) <- list(ws,c,models)
  return(output)
}

CNMArcadeCont <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,type,classes,models,
                          component,initialization = "random.post", 
                          contamination = T, parallel = F,
                          ng = NULL,ncont = NULL,labelCont = NULL)
{
  output <- array(NA, c(length(ws),length(c),length(models),2 ) )
  
  sequence <- function(x,w)
  {
    # cat(x,"-",w,"\n")    
    return(seq(x-w,x+w,1))
  }
  
  for(ind1 in 1:length(ws))
  {
    
    if (all(is.list(peaks), length(peaks) == 1)){
      vars <- as.vector(sapply(peaks[[1]],sequence, w = ws[ind1]))
    } else if (all(is.list(peaks), length(peaks) == length(classes))) {
      vars <- as.vector(sapply(peaks[[ind1]],sequence, w = ws[ind1]))
    } else if (!is.list(peaks)) { 
      stop("peaks has to be a list")
    }
    
    for(ind2 in 1:length(c))
    {
      cat("ws=",ws[ind1],"-","c=",c[ind2],"\n")
      CompleteDf <- DfSamples(mu,sigma,peaks,nsamples,ws[ind1], c[ind2],type, classes)
      CompleteDf <- CompleteDf %>% mutate(Class1 = Class) %>% relocate(Class1, .after = Class)
      ContSamples <- CompleteDf[1,]
      
      
      if(!all(length(ng) == length(ncont),length(ncont) == length(labelCont))) stop("Contaminated parameters have different lenght")
      if(length(ng)>0)
      {  
        for(indcont in 1:length(ng))
        {
          temp <- newclassSamplefrom(mu,ng[indcont]*sigma,unlist(peaks), 
                                     unlist(ws), unlist(c),ncont[indcont]
                                     ,unlist(type) )
          
          tempWide <- SampleToDf(temp,labelCont[indcont])
          tempWide <- tempWide %>% mutate(Class1 = paste0(Class,"-Bad")) %>%
            relocate(Class1, .after = Class)
          tempWide$Class <- as.factor(tempWide$Class) 
          tempWide$Sample <- as.factor(tempWide$Sample)
          tempWide$Class1 <- as.factor(tempWide$Class1)
          #samples[((clase-1)*K+1):((clase*K)+1),] <- sampleWide
          ContSamples <- rbind(ContSamples,tempWide)
        }
        CompleteDf <- rbind(CompleteDf,ContSamples[-1,])
        CompleteDf$Class <- as.factor(CompleteDf$Class)
        CompleteDf$Sample <- as.factor(CompleteDf$Sample)
      }
      #tempDf <- CompleteDf %>% mutate(Class = Class1) %>% select(!Class1)
      
      #plotMeanSamples(tempDf %>% filter(Class %in% c("A","Bad-B")))[[1]]
      
      SamplesDf <- CompleteDf %>% select(c("Class","Sample",colnames(CompleteDf)[vars]))
      set.seed(123)
      n <- nrow(SamplesDf)
      ind <- sample(1:n, ntrain)  
      train <- SamplesDf[ind,-c(1:2)]
      test <- SamplesDf[-ind,-c(1:2)]
      trainl <- SamplesDf[ind,]$Class
      testl  <- SamplesDf[-ind,]$Class
      
      cat("\n","dim Train Set = " , dim(train),"\n")
      
      res <- MixCont(train, test, trainl, testl, models, component,
                     initialization = initialization,
                     contamination = contamination, 
                     parallel = parallel)
      
      #res <- EDDA(train,test,trainl,testl,models,components)
      #cat(res, "\n")
      output[ind1,ind2, ,1] <- res[[1]][,1]
      output[ind1,ind2, ,2] <- res[[1]][,2]
      
    }
  }
  dimnames(output) <- list(ws,c,models,c("Train Error","Test Error"))
  return(list(output, CompleteDf) )
}

CNMArcade <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,type,classes,models,component,
                      initialization = "random.post", contamination = T, parallel = F)
{
  output <- array(NA, c(length(ws),length(c),length(models),2) )
  
  
  sequence <- function(x,w)
  {
    # cat(x,"-",w,"\n")    
    return(seq(x-w,x+w,1))
  }
  
  for(ind1 in 1:length(ws))
  {
    
    if (all(is.list(peaks), length(peaks) == 1)){
      vars <- as.vector(sapply(peaks[[1]],sequence, w = ws[ind1]))
    } else if (all(is.list(peaks), length(peaks) == length(classes))) {
      vars <- as.vector(sapply(peaks[[ind1]],sequence, w = ws[ind1]))
    } else if (!is.list(peaks)) { 
      stop("peaks has to be a list")
    }
    
    for(ind2 in 1:length(c))
    {
      cat("ws =",ws[ind1]," - ","c =",c[ind2],"\n")
      CompleteDf <- DfSamples(mu,sigma,peaks,nsamples,ws[ind1], c[ind2],type, classes)
      SamplesDf <- CompleteDf %>% select(c("Class","Sample",colnames(CompleteDf)[vars]))
      
      set.seed(123)
      n <- nrow(SamplesDf)
      ind <- sample(1:n, ntrain)  
      train <- SamplesDf[ind,-c(1:2)]
      test <- SamplesDf[-ind,-c(1:2)]
      trainl <- SamplesDf[ind,]$Class
      testl  <- SamplesDf[-ind,]$Class
      
      cat("\n","dim Train Set = " , dim(train),"\n")
      
      res <- MixCont(train, test, trainl, testl, models, component,
                     initialization = initialization,
                     contamination = contamination, 
                     parallel = parallel)
      
      #res <- EDDA(train,test,trainl,testl,models,components)
      #cat(res, "\n")
      #output[ind1,ind2,] <- res[[1]]
      output[ind1,ind2, ,1] <- res[[1]][,1]
      output[ind1,ind2, ,2] <- res[[1]][,2]
      
    }
  }
  dimnames(output) <- list(ws,c,models,c("Train Error","Test Error"))
  return(list(output,CompleteDf) )
}


EMM <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,type,classes,models,component,
                initialization = "random.post", contamination = F, parallel = F, 
                Arcade = 0)
{
  
  output <- array(NA, c(length(ws),length(c),length(models),2) )
  samMatrix <- list()  

  for(ind1 in 1:length(ws))
  {
    for(ind2 in 1:length(c))
    {
      cat("ws =",ws[ind1]," - ","c =",c[ind2],"\n")
      temp <- DfSamples2(mu,sigma,peaks,nsamples,ws[ind1], c[ind2],type, classes)
      CompleteDf <- temp[[1]]
      vars <- unlist(temp$Wavelengths)
      SamplesDf <- CompleteDf 
      
      set.seed(123)
      n <- nrow(SamplesDf)
      ind <- sample(1:n, ntrain)  
      
      if(Arcade) 
      {
        SamplestDf <- CompleteDf %>% select(c("Class","Sample",colnames(CompleteDf)[vars]))
        
      }
      
      train <- SamplesDf[ind,-c(1:2)]
      test <- SamplesDf[-ind,-c(1:2)]
      trainl <- SamplesDf[ind,]$Class
      testl  <- SamplesDf[-ind,]$Class
      
      samMatrix[[ind2]] <- list(SamplesDf, ind) 
      cat("\n","dim Train Set = " , dim(train), " Arcade mode = ",Arcade ,"\n")
      
      res <- EM(models,train, trainl, test, testl)
      
      #res <- EDDA(train,test,trainl,testl,models,components)
      #cat(res, "\n")
      #output[ind1,ind2,] <- res[[1]]
      output[ind1,ind2, ,1] <- res[[1]][,1]
      output[ind1,ind2, ,2] <- res[[1]][,2]
      
    }
  }
  dimnames(output) <- list(ws ,c , models , c("Train Error","Test Error"))
  return(list(metrics = output, samples = samMatrix))
}





EM <- function(models, train, trainl,test,testl)
{
  # modelName : String vector providing the different models to fit
  
  tab <- matrix(NA, nrow = length(models), ncol = 2)
  rownames(tab) <- models
  colnames(tab) <- c("Train error","Test error")
  ccmatrix <- list()

  for (j in seq(models) )
  {
    Mtrain <- mstep(train,modelName =  models[j],
                    z= unmap(trainl))
    Etrain <- estep(Mtrain$modelName,
                    data = train, parameters = Mtrain$parameters)
    
    tabTrain <- table(max.col(Etrain$z,"first"),trainl)
    
    Etest <- estep(Mtrain$modelName,
                    data = test, parameters = Mtrain$parameters)
    
    tabTest <- table(max.col(Etest$z, "first"), testl)
    
      
    tab[j,1] <- sum( max.col(Etrain$z) != as.numeric(trainl) ) / length(trainl)
    tab[j,2] <- sum( max.col(Etest$z) != as.numeric(testl)   ) / length(testl)
    
    ccmatrix[[j]] <- list(tabTrain,tabTest)
    
  }
  return( list(tab, tabTrain, tabTest) )
}
  

  



CMN <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,shape,classes,models,component,
                initialization = "random.post", contamination = T, parallel = F, 
                vars = NULL)
{
# CMN: contaminated mixture of normals
# vars <- variable to be selected in the arcade mode

  np <- length(peaks) # number of peaks
  p <- length(mu)
  tam <- nrow(ws)
  output <- array(NA, c(length(ws),length(c),length(models),2) )
  
  if(!is.vector(mu)) stop("parameter mu has to be a vector")
  if(!is.vector(sigma)) stop("parameter sigma has to be a vector")
  if(!is.vector(peaks)) stop("parameter peaks has to be a vector") 
  if(!is.vector(ws)) stop("parameter ws has to be a matrix of number of columns equal number of peaks") 
  if(!is.vector(c)) stop("parameter c has to be a matrix of number of columns equal number of peaks") 
  if(!is.vector(shape)) stop("parameter shape has to be a matrix of number of columns equal number of peaks") 
  if ( dim(ws) != dim(c) )  stop ("dimension of parameters ws and c should be the same")
  if ( dim(ws) != dim(shape) )  stop ("dimension of parameters ws and shape should be the same")
  if ( dim(c) != dim(shape) )  stop ("dimension of parameters c and shape should be the same")
  
  
  sample1 <- gen(nsamples,mu,sigma)
  sample1df <- SampleToDf(sample1,classes[1])
  
  
  
  for(ind1 in 1:tam)
  {
    mu2 <- modMean(mu,peaks,ws[ind1,],c[ind1,],shape[ind1,])
    sample2 <- gen(nsamples,mu2,sigma)
    sample2df <- SampleToDf(sample2,classes[2])
    samples <- rbind(sample1df,sample2df)
    set.seed(123)
    n <- nrow(SamplesDf)
    if(!is.null(vars)) SamplesDf <- samples[,vars] else SamplesDf <- samples
    ind <- sample(1:n, ntrain)  
    train <- SamplesDf[ind,-c(1:2)]
    test <- SamplesDf[-ind,-c(1:2)]
    trainl <- SamplesDf[ind,]$Class
    testl  <- SamplesDf[-ind,]$Class
    
    rownames(tab) <- modelName
    colnames(tab) <- c("Train error","Test error")
    outmatrix <- list()
    tab <- matrix(NA, nrow = length(modelName), ncol = 2)
    
    for (m in seq(modelName))
    {
        mod <- CNmixt(X = as.matrix(train), G = component,
                    contamination = contamination,
                    model = modelName[m],
                    label = as.numeric(trainl),
                    initialization = initialization,
                    seed = 12, parallel = parallel  )                         
      
        tabTrain <- agree(mod, givgroup = as.numeric(trainl),criterion = "BIC")

      
        predTrain<- CNpredict(as.matrix(train), prior= mod$models[[1]]$prior,
                            mu = mod$models[[1]]$mu,
                            invSigma = mod$models[[1]]$invSigma)
        tab[m,1] <- sum(predTrain != as.numeric(trainl))/length(predTrain)
      
        pred <- CNpredict(as.matrix(test), prior = mod$models[[1]]$prior, 
                        mu = mod$models[[1]]$mu, 
                        invSigma = mod$models[[1]]$invSigma)
      
        tabTest<- table(pred, as.numeric(testl))
        tab [m,2] <- sum(pred != as.numeric(testl))/length(pred)
      }
      outmatrix <- (list(tab, tabTrain, tabTest) )
      output[ws[ind1],c[ind1], ,1] <- outmatrix[[1]][,1]
      output[ws[ind1],c[ind2], ,2] <- outmatrix[[1]][,2]
      
    }
  
        
}

CNM <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,type,classes,models,component,
                initialization = "random.post", contamination = T, parallel = F, 
                vars = NULL, alpha = 0.8)
{
  results <- array(NA, c(length(ws),length(c),length(models),2) )
  output <- list()
  par <- list()
  sets <- list()
  status <- list()
  cont <- numeric()
  cont <- 1
#  sequence <- function(x,w)
#  {
    # cat(x,"-",w,"\n")    
#    return(seq(x-w,x+w,1))
#  }
  
  for(ind1 in 1:length(ws))
  {
    
#   if (all(is.list(peaks), length(peaks) == 1)){
#      vars <- as.vector(sapply(peaks[[1]],sequence, w = ws[ind1]))
#    } else if (all(is.list(peaks), length(peaks) == length(classes))) {
#      vars <- as.vector(sapply(peaks[[ind1]],sequence, w = ws[ind1]))
#    } else if (!is.list(peaks)) { 
#      stop("peaks has to be a list")
#    }
    
    for(ind2 in 1:length(c))
    {
      cat("ws =",ws[ind1]," - ","c =",c[ind2],"\n")
      CompleteDf <- DfSamples(mu,sigma,peaks,nsamples,ws[ind1], c[ind2],type, classes)
      if(!is.null(vars))  SamplesDf <- CompleteDf[,c(1,2,vars)] else  SamplesDf <- CompleteDf 

      
      set.seed(123)
      n <- nrow(SamplesDf)
      ind <- sample(1:n, ntrain)  
      train <- SamplesDf[ind,-c(1:2)]
      test <- SamplesDf[-ind,-c(1:2)]
      trainl <- SamplesDf[ind,]$Class
      testl  <- SamplesDf[-ind,]$Class
      
      cat("\n","dim Train Set = " , dim(train),"\n")
      sets[[cont]] <- list(train = train, train.labels = trainl,
                           test = test, test.labels = testl)
      
      res <- MixCont(train, test, trainl, testl, models, component,
                     initialization = initialization,
                     contamination = contamination, 
                     parallel = parallel, alpha = alpha)
      
        par[[cont]] <- res$parameters
        results[ind1,ind2, ,1] <- res[[1]][,1]
        results[ind1,ind2, ,2] <- res[[1]][,2]
      
        cat("\n error ", res$errors)
      if(res$errors == 1){
        status[[cont]] <- list(ws = ws[ind1], c = c[ind2], status = "error")  
        results[ind1,ind2, ,1] <- -1
        results[ind1,ind2, ,2] <- -1
        
      }
      #res <- EDDA(train,test,trainl,testl,models,components)
      #cat(res, "\n")
      #output[ind1,ind2,] <- res[[1]]
      cont <- cont + 1
      
    }
  }
  dimnames(results) <- list(ws,c,models,c("Train Error","Test Error"))
  output <- list(performance = results, parameters = par, 
                 variables = colnames(SamplesDf), sets = sets,
                 status = status)
  return(output)
}




DAcont <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,type,classes,models,
                   components,ng = NULL,ncont = NULL,labelCont = NULL)
{
  output <- array(NA, c(length(ws),length(c),length(models)))
  for(ind1 in 1:length(ws))
  {
    for(ind2 in 1:length(c))
    {
      cat(ind1,"-",ind2,"\n")
      SamplesDf <- DfSamples(mu,sigma,peaks,nsamples,ws[ind1], c[ind2],type, classes)
      ContSamples <- SamplesDf[1,]
      if(!all(length(ng) == length(ncont),length(ncont) == length(labelCont))) stop("Contaminated parameters have different lenght")
      if(length(ng)>0)
      {  
        for(indcont in 1:length(ng))
        {
          temp <- newclassSamplefrom(mu,ng[indcont]*sigma,unlist(peaks), 
                                     unlist(ws), unlist(c),ncont[indcont]
                                     ,unlist(type) )
          
          tempWide <- SampleToDf(temp,labelCont[indcont])
          tempWide$Class <- as.factor(tempWide$Class) 
          tempWide$Sample <- as.factor(tempWide$Sample)
          #samples[((clase-1)*K+1):((clase*K)+1),] <- sampleWide
          ContSamples <- rbind(ContSamples,tempWide)
        }
        
        SamplesDf <- rbind(SamplesDf,ContSamples[-1,])
        SamplesDf$Class <- as.factor(SamplesDf$Class)
        SamplesDf$Sample <- as.factor(SamplesDf$Sample)
      }
      n <- nrow(SamplesDf)
      ind <- sample(1:n, ntrain)  
      train <- SamplesDf[ind,-c(1:2)]
      test <- SamplesDf[-ind,-c(1:2)]
      trainl <- SamplesDf[ind,]$Class
      testl  <- SamplesDf[-ind,]$Class
      
      res <- EDDA(train,test,trainl,testl,models,components)
      cat(res, "\n")
      output[ind1,ind2,] <- res[,3]
    }
  }
  dimnames(output) <- list(ws,c,models)
  return(output)
}


DA <- function(mu,sigma,peaks,nsamples,ntrain,ws,c,type,classes,models,components)
{
  
  output <- array(NA, c(length(ws),length(c),length(models)) )
  
  for(ind1 in 1:length(ws))
  {
    for(ind2 in 1:length(c))
    {
      cat(ind1,"-",ind2,"\n")
      SamplesDf <- DfSamples(mu,sigma,peaks,nsamples,ws[ind1], c[ind2],type, classes)
      n <- nrow(SamplesDf)
      ind <- sample(1:n, ntrain)  
      train <- SamplesDf[ind,-c(1:2)]
      test <- SamplesDf[-ind,-c(1:2)]
      trainl <- SamplesDf[ind,]$Class
      testl  <- SamplesDf[-ind,]$Class
      
      res <- EDDA(train,test,trainl,testl,models,components)
      cat(res, "\n")
      output[ind1,ind2,] <- res[,3]
    }
  }
  dimnames(output) <- list(ws,c,models)
  return(output)
}

DfSamples2<- function(mu,sigma, peaks, nsamples, ws, c, sh, classes)
{
  # Objective : It is to simulate a second class based with same covariance,
  #             but with different mean which is calculated using the rest of parameters
  #             and returning the samples of both classes in a unify dataset
  
  # mu: vector which contains the mean for the base class at each wavelength
  # sigma: vector which contains the standard deviation for the base class at each wavelength
  # peaks: a vector that contains the different peaks for the new class
  # nsamples: a vector which contains the number of samples for each class
  # ws: a list that contains the window size around peaks for each class. The
  #     same windows size for all peaks is assumed in this implementation
  # c : constant to be used in creating different shapes
  # shape: integer that provide the type of shape that we would apply around peaks
  # classes : vector of label for classes
  
  K <- length(classes)
  p <- length(mu)
  np <- length(peaks)
  
if(length(nsamples) == 1) nsamples <- rep(nsamples,K) 

    
if(! (is.numeric(mu) & is.vector(mu)) ) stop("mu has to be a numeric vector")
if(! (is.numeric(sigma) & is.vector(sigma)) ) stop("sigma has to be a numeric vector")
if(! (is.numeric(peaks) & is.vector(peaks)) ) stop("peaks has to be a numeric vector")
if(! (is.numeric(nsamples) & is.vector(nsamples)) ) stop("number of samples has to be a numeric vector")
if( (!is.numeric(ws) | length(ws) > 1) ) stop("Windows size has to be a numeric containing only 1 value")
if( (!is.numeric(c)  | length(c)) > 1 ) stop("Constant has to be a numeric containing only 1 value")
if( (!is.numeric(sh) | length(sh)) > 1 ) stop("Constant has to be a numeric containing only 1 value")
  
if(! (is.character(classes) & is.vector(classes)) ) stop("Label for the two classes to be generated")
  
  
#  peaksm <- list()
#  wsm <- list()
#  cm <- list()
#  typem <- list()
  


  sample <- matrix(NA, nrow = sum(nsamples), ncol = p)
  sample1 <- gen(nsamples[1],mu,sigma)
  sample1Wide <- SampleToDf(sample1, classes[1])
  sample1Wide$Class <- as.factor(sample1Wide$Class) 
  sample1Wide$Sample <- as.factor(sample1Wide$Sample)
  mu2 <- modMean(mu,peaks,ws,c,sh)
  sample2 <- gen(nsamples[2],mu2[[1]],sigma)
  sample2Wide <- SampleToDf(sample2, classes[2])
  sample2Wide$Class <- as.factor(sample2Wide$Class) 
  sample2Wide$Sample <- as.factor(sample2Wide$Sample)
  samples <- rbind(sample1Wide,sample2Wide)  
  
  return(list(samples, Peaks = mu2$Peaks, Wavelengths = mu2$Wavelengths) )
}


DfSamples<- function(mu,sigma, peaks, nsamples, ws, c, type, classes)
{
  # mu: vector which contains the mean for the base class at each wavelength
  # sigma: vector which contains the variance for the base class at each wavelength
  # peaks: a list that contains the different peaks for each class
  # nsamples: a vector which contains the number of samples for each class
  # ws: a list that contains the window size around peaks for each class
  # classes : vector of label for classes
  
  K <- length(classes)
  p <- length(mu)
  np <- length(peaks)
  if(length(nsamples) == 1) m <- rep(nsamples,K) else m <- nsamples
  if(length(type) == 1) typem <- rep(type,np) else typem <- type
  if(length(ws) == 1) wsm <- rep(ws,np) else wsm <- ws
  if(length(c) == 1) cm <- rep(c,np) else cm <- c
  
#  peaksm <- list()
#  wsm <- list()
#  cm <- list()
#  typem <- list()
  
#  if(is.list(peaks) & length(peaks) == 1) 
#  {
#    peaksm[[1]] <- peaks
#  }else if(is.list(peaks) & length(peaks) > 1)
# {
#    peaksm[[1]] <- peaks[[1]]
#  }  
  
#  if(length(c) == 1) cm[[1]] <- rep(c,np) else cm[[1]] <- c
#  if(length(ws) == 1) wsm[[1]] <- rep(ws,np) else wsm[[1]] <- ws
#  if(length(type) == 1) typem[[1]] <- rep(type,np) else typem[[1]] <- type
  
#  for(clase in 2:K)
#  {
#    if(is.list(peaks) & length(peaks) == 1 ) peaksm[clase]] <- peaks
#    if(length(c) == 1) cm[[clase]] <- rep(c,np) else cm[[clase]] <-c      
#    if(length(ws) == 1) wsm[[clase]] <- rep(ws,np) else wsm[[clase]] <-ws
#    if(length(type) == 1) typem[[clase]] <- rep(type,np) else typem[[clase]] <- type
    
 # }
  
  sample <- matrix(NA, nrow = m[2], ncol = p)
  sample1 <- gen(m[1],mu,sigma)
  sample1Wide <- SampleToDf(sample1, classes[1])
  sample1Wide$Class <- as.factor(sample1Wide$Class) 
  sample1Wide$Sample <- as.factor(sample1Wide$Sample)
  samples <- sample1Wide
  
#  for (clase in 2: K)
 # {
    mu.clase <- modMean(mu,peaks,wsm,cm, typem)
    sample <- gen(m[2],mu.clase[[1]],sigma)
#    sample <- newclassSamplefrom(mu,sigma,peaks[clase], ws[clase], c[clase], m[clase],type[clase] )
    sampleWide <- SampleToDf(sample,classes[2])
    sampleWide$Class <- as.factor(sampleWide$Class) 
    sampleWide$Sample <- as.factor(sampleWide$Sample)
    #samples[((clase-1)*K+1):((clase*K)+1),] <- sampleWide
    samples <- rbind(samples,sampleWide)
#  }
  
  return(samples)
}


EDDA <- function(train, test, trainl, testl, modelName, components)
{
  # train: train set without labels
  # test:  test set without test
  # trainl: labels for train set
  # testl:  labels for test set
  # modelName: models to try
  # component: number of components of mixture normals
  
  
  tab <- matrix(NA, nrow = length(modelName), ncol = 3)
  rownames(tab) <- modelName
  colnames(tab) <- c("BIC", "10-fold CV", "Test error")
  
  for (j in seq(modelName))
  {
    mod <- MclustDA(train, trainl, modelType = "EDDA",
                    G = 1:components,modelNames = modelName[j])
    tab[j,1] <- mod$bic
    tab[j,2] <- cvMclustDA(mod, nfold = 10, verbose = F)$error
    pred <- predict(mod, test)
    tab[j,3] <- classError(pred$classification, testl)$errorRate
    
  }
  return(tab)
}

MixCont <- function(train, test, trainl, testl, modelName, component,
                    initialization = "random.post",contamination = T, 
                    parallel = F, alpha = 0.8)
{
  # train: train set without labels
  # test:  test set without test
  # trainl: labels for train set
  # testl:  labels for test set
  # modelName: models to try
  # components: number of components of mixture normals
  output <- list()
  tab <- matrix(NA, nrow = length(modelName), ncol = 2)
  rownames(tab) <- modelName
  colnames(tab) <- c("Train error","Test error")
  parameters <- list()

  for (j in seq(modelName))
  {

    tryCatch(mod <- CNmixt(X = as.matrix(train), G = component,
                  contamination = contamination,
                  model = modelName[[j]],
                  label = as.numeric(trainl),
                  initialization = initialization,
                  seed = 12, parallel = parallel, 
                  alphamin =  alpha ), 
             error = function(e){return(list(metrics = tab, train.matrix = NULL, 
                                             test.matrix = NULL, parameters = NULL,
                                             errors = 1)  
             )}
     )
            
    # tabTrain <- agree(mod, givgroup = as.numeric(trainl),criterion = "BIC")
    # ccmatrix[[j]] <- tabTrain
    parameters[[j]] <- mod$models
    
    predTrain<- CNpredict(as.matrix(train), prior= mod$models[[1]]$prior,
                          mu = mod$models[[1]]$mu,
                          invSigma = mod$models[[1]]$invSigma)
    tabTrain <- table(predTrain,as.numeric(trainl))
    tab[j,1] <- sum(predTrain != as.numeric(trainl))/length(predTrain)
    
    pred <- CNpredict(as.matrix(test), prior = mod$models[[1]]$prior, 
                      mu = mod$models[[1]]$mu, 
                      invSigma = mod$models[[1]]$invSigma)
    
    tabTest<- table(pred, as.numeric(testl))
    tab [j,2] <- sum(pred != as.numeric(testl))/length(pred)
  }
  
  output <-list(metrics = tab, train.matrix = tabTrain, 
                test.matrix = tabTest, parameters = parameters,
                errors = 0)  
  return(output)
  
}

which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(TRUE,diff(x)<=0,FALSE))>0)
    }else {
      which(diff(diff(x)<=0)>0)
    }    
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)
    }
    
  }
}

plot2Samples <- function(sample1, sample2, rotulos) 
{
  
  dftype <- data.frame(Class = c("A","B"),
                       Sample = c(1,2),
                       rbind(sample1,sample2))
  
  dftype$Class <- as.factor(dftype$Class)
  dftype$Sample <- as.factor(dftype$Sample)
  colnames(dftype) <- rotulos
  
  dftypeLong <- pivot_longer(dftype, cols = starts_with("X"),names_to = "Wavelength",
                             values_to = "Intensitive")
  dftypeLong <- dftypeLong %>% mutate(Wavelength1 = Wavelength,.after = "Wavelength")
  
  dftypeLong <- dftypeLong %>% mutate(Wavelength = substr(Wavelength,2,5))
  dftypeLong$Wavelength <- as.numeric(dftypeLong$Wavelength)
  
  ggplot(dftypeLong%>%filter(Sample %in% c(1,2)),aes(x =Wavelength,y = Intensitive,col = Class)) + 
    geom_line() + theme_minimal() 
  #+ ggtitle("Sample A vs Sample B, type = 2, c = 0.0001, ws = 30")
  
}

plot2Classes <- function(sample1, mean,sigma,peaks, ws,c,type,rotulos) 
{
  
  sample2 <- newClassSample(mean,sigma,peaks,ws, c, 1,type )
  
  dftype <- data.frame(Class = c("A","B"),
                       Sample = c(1,2),
                       rbind(sample1,sample2))
  
  dftype$Class <- as.factor(dftype$Class)
  dftype$Sample <- as.factor(dftype$Sample)
  colnames(dftype) <- rotulos
  
  dftypeLong <- pivot_longer(dftype, cols = starts_with("X"),names_to = "Wavelength",
                             values_to = "Intensitive")
  dftypeLong <- dftypeLong %>% mutate(Wavelength1 = Wavelength,.after = "Wavelength")
  
  dftypeLong <- dftypeLong %>% mutate(Wavelength = substr(Wavelength,2,5))
  dftypeLong$Wavelength <- as.numeric(dftypeLong$Wavelength)
  
  legenda <- paste0("Sample A vs Sample B, type =", as.character(type), " c = ",c[1], " ws = ",ws )
  
  ggplot(dftypeLong%>%filter(Sample %in% c(1,2)),aes(x =Wavelength,y = Intensitive,col = Class)) + 
    geom_line() + theme_minimal() + ggtitle(legenda)
  
}

plotContSamples2Classes <- function(sample1, sample2, sampleCont, labelClasses,labelCont) 
{
  n1<-nrow(sample1)
  n2<-nrow(sample2)
  nCont <- nrow(sampleCont)
  
  
  dftype <- data.frame(Class = rep(c(labelClasses),c(n1,n2+nCont)),
                       Class1 = rep(c(labelClasses,labelCont),c(n1,n2,nCont)),
                       Sample = c(1:n1,1:(n2+nCont)),
                       Sample1 = c(1:n1,1:n2,1:nCont),
                       rbind(sample1,sample2,sampleCont))
  
  dftype$Class <- as.factor(dftype$Class)
  dftype$Class1 <- as.factor(dftype$Class1)
  
  dftype$Sample <- as.factor(dftype$Sample)
  dftype$Sample <- as.factor(dftype$Sample1)
  # 
  #  colnames(dftype) <- rotulos
  
  dftypeLong <- pivot_longer(dftype, cols = starts_with("X"),names_to = "Wavelength",
                             values_to = "Intensitive")
  dftypeLong <- dftypeLong %>% mutate(Wavelength1 = Wavelength,.after = "Wavelength")
  
  dftypeLong <- dftypeLong %>% mutate(Wavelength = str_extract(Wavelength,"\\d+"))
  dftypeLong$Wavelength <- as.numeric(dftypeLong$Wavelength)
  
  ggplot(dftypeLong%>%filter(Class %in% c("A","B") ),
         aes(x =Wavelength,y = Intensitive,group = Class,col = Class)) + 
    geom_line() + theme_minimal() + scale_color_manual(values = c("#E69F00", "#56B4E9"))
  #+ ggtitle("Sample A vs Sample B, type = 2, c = 0.0001, ws = 30")
  
  ggplot(dftypeLong%>%filter(Class1 %in% c("A","B",labelCont) ),
         aes(x =Wavelength,y = Intensitive,group = Class,col = Class1)) + 
    geom_line() + theme_minimal() + 
    scale_color_manual(values = c("#E69F00", "#56B4E9","#CC6666"))
  
  
}




plot5 <- function(C){
  # C has to contain columns Wavelength and Intensitive and Sample
  ggplot(C, aes(x = Wavelength, y = Intensitive,group = 1)) +
    geom_line(aes(col = Sample) ) +
    theme(legend.position = "bottom") + 
    theme_minimal()
}

SampleToDf <- function(SampleSet, class)
{
  n <- dim(SampleSet)[1]

  Class <- rep(class,n)
  Sample <- as.character(1:n)
  
  df <- as.data.frame(cbind(Class , Sample, SampleSet))

  df <- df %>% mutate_at(-c(1,2), as.numeric)
  
  df <- df %>% mutate_if(is.character, as.factor)
  
  SampleToDf <- df
}

SampleLong <- function(dfwide)
{
  # df:  contain the dataframe wide format with measurements at different wavelengths for different samples 
  # name: the new name of the new column for the variables that are going to be gather
  # value: the name of the new column that contains the value of interest
  name <- "Wavelength"
  value <- "Intensitive"
    
    xbar <- dfwide %>% select_if(is.numeric) %>% apply(2,mean) %>% t %>% data.frame 

    numeric.variables <- colnames(select_if(dfwide,is.numeric))
    
    dftypeLong <- pivot_longer(dfwide, cols = numeric.variables,names_to = name,
                               values_to = value)
    dftypeLong <- dftypeLong %>% mutate(Wavelength1 = Wavelength)
    dftypeLong <- dftypeLong %>% relocate(Wavelength1, .after = Sample)
    
    dftypeLong <- dftypeLong %>% mutate(Wavelength = str_extract(Wavelength,"(\\d)+") )
    #  dftypeLong$Wavelength <- as.numeric(dftypeLong$Wavelength)
    
    dftypeLong <- dftypeLong %>% mutate(Wavelength = as.numeric(Wavelength))
    SampleLong <- dftypeLong
    
}

addMeantoDfWide <- function(dfWide,values = NULL)
{
  # function add the sample mean of the set dfWide and return it in long format with
  # the sample mean in the last row
  # The function assumes that the first variable is Class and the second one is Sample
  # if it is different then the values of the summary row should be provided through variable
  # values
  
  # SampleLong :  dataset in wide format that contain all samples of interest
  # values:       values of non numerical variables in case that they are more than 2 variables
  
  
  # Calculate the mean

  dfWide <- dfWide %>% mutate_if(is.factor, as.character)
  xbar <- as.numeric(dfWide %>% select_if(is.numeric) %>% apply(2,mean))
  
  if (is.null(values))
  {
    m <- nrow(dfWide) + 1
    dfWide <- dfWide %>% rbind(c(m,m, xbar))
    dfWide[m,1] <- "mean"
    dfWide <- dfWide %>% mutate_if(is.character, as.factor)
  }  
  
  if(!is.null(values))
  {
    nc <- length(values)
    vars <- colnames(dfWide)[1:nc]
    dfWide<- dfWide %>% rbind(c(values, xbar))
    dfWide <- dfWide %>% mutate_at(vars,as.factor)

  }

    
    addMeantoDfWide  <- dfWide 
}
  
  
plotSample <- function(dfLong)
{
  # dfLong : contain the dataset with the measurements at wavelengths in
  #          long form data set. 
  
  # It is assumed that Wavelength and Intensitive are the columns that contains
  # the wavelength measured and the respective values
  
  plotSample <- ggplot(dfLong,aes(x =Wavelength,y = Intensitive)) + 
    geom_line() + theme_minimal() 
  
}

plotMeanSamples <- function(samples)
{
  # return two graphs
  # fig.1 sample mean of classes
  # fig.2 samples of all classes
  
  var.num <- samples %>% select_if(is.numeric) %>% colnames
  var.no.num <- samples %>% select_if(negate(is.numeric)) %>% colnames
  xbar <- samples %>% group_by(Class) %>% filter(n()>1)%>%
    summarize(across(where(is.numeric), ~ mean(.x) ) ) 
    

  dfLong <- pivot_longer(xbar,cols = all_of(var.num), 
                           names_to = "Wavelength", 
                           values_to = "Mean")
    
  dfLong <- dfLong %>% mutate(Wavelength1 = Wavelength) %>%
    relocate(Wavelength1, .before = Wavelength)
  dfLong <- dfLong %>% 
    mutate(Wavelength = as.numeric(str_extract(Wavelength1,"(\\d)+")))
     

  dfLong <- dfLong %>% mutate_at(c("Wavelength1","Class"), as.factor)
  dfLong <- dfLong %>% mutate_at(c("Wavelength1","Class"), as.factor)
  
  
  #dfLong <- arrange(dfLong, Class, Wavelength)
  
  g1 <- ggplot(dfLong, aes(x = Wavelength, y = Mean) ) +
    geom_line(aes(color = Class)) + 
    theme_minimal()  
  
#  ggplot(dfLong %>% filter(Class %in% c("B","B-bad")), aes(x = Wavelength, y = Mean) ) +
#    geom_line(aes(color = Class)) + 
#    theme_minimal()  
  
  
  samplesLong <- pivot_longer(samples, all_of(var.num),
                              names_to = "Wavelength",
                              values_to = "Intensitive")
  
  samplesLong <- samplesLong %>% mutate(Wavelength1 = Wavelength) %>%
    relocate(Wavelength1, .before = Wavelength)
  samplesLong <- samplesLong %>% 
    mutate(Wavelength = as.numeric(str_extract(Wavelength1,"(\\d)+")))
  samplesLong <- samplesLong %>% mutate_at(c("Wavelength1","Class"), as.factor)
  samplesLong <- samplesLong %>% mutate_at(c("Wavelength1","Class"), as.factor)
  
  
  g2 <- ggplot(samplesLong, aes(x = Wavelength, y = Intensitive) ) +
    geom_line(aes(color = Class)) + 
    theme_minimal()  
  
 # ggplot(samplesLong %>% filter(Class %in% c("B","B-bad") ), aes(x = Wavelength, y = Intensitive) ) +
#    geom_line(aes(color = Class)) + 
#    theme_minimal()
  
  plotMeanSamples <- list(g1,g2,samplesLong)
}

  

plotMean2Samples <- function(sample1, sample2)
{
  xbar1 <- sample1 %>% apply(2,mean)
  xbar2 <- sample2 %>% apply(2,mean)
  Wavelength1 <- names(xbar1)
  
  
  df <-tibble(Wavelength1 = names(xbar1), Sample1 = xbar1, Sample2 = xbar2 )
  df <- df %>% mutate(Wavelength = as.numeric(str_extract(Wavelength1,"(\\d)+"))) 
  df <- df %>% relocate(Wavelength, .before = Wavelength1)
  
  dfLong <- pivot_longer(df, cols = starts_with("Sample"), names_to = "Class", 
                         values_to = "Mean")
  
  
  dfLong <- dfLong %>% relocate(Wavelength, .after = Class)
  dfLong <- dfLong %>% mutate_at(c("Wavelength1","Class"), as.factor)
  dfLong <- dfLong %>% relocate(Class, .before = Wavelength1)
  

  #dfLong <- arrange(dfLong, Class, Wavelength)
  
  plotMean2Samples <- ggplot(dfLong, aes(x = Wavelength, y = Mean) ) +
    geom_line(aes(color = Class)) + 
  theme_minimal()  

}


newClassSampleDiagVarOld <- function(nsamples,mu, sigma)
  # mu : vector containing the mean spectra of the class
  # sigma: vector containing the variance at each wavelength  
{
  p <- length(mu)
  output <- array(0, c(nsamples,p))
  for (j in 1:p)
    output[,j] <- rnorm(nsamples,mu[j],sqrt(sigma[j]) )
  
  output <- as.data.frame(output)
  colnames(output) <- names(mu)
  
  return(output)  
}



newClassSampleDiagVar <- function(nsamples,mu, sigma)
# mu : vector containing the mean spectra of the class
# sigma: vector containing the variance at each wavelength  
{
  p <- length(mu)
  output <- array(0, c(nsamples,p))
  temp <- array(0,c(1000,p))
  for (i in 1:nsamples)
  {
    temp <- newSampleDiagVar(1000,mu,sigma)
    cat ("\n i <-", i, "from ", nsamples )
    output[i,] <- apply(temp,2,mean)
  }
  
  output <- as.data.frame(output)
  colnames(output) <- names(mu)
                          
  return(output)  
}


newSampleDiagVar <- function(nsamples,mu, sigma)
  # mu : vector containing the mean spectra of the class
  # sigma: vector containing the variance at each wavelength  
{
  flag <- numeric() # 1 is a matrix  and 0 is a vector
  if(ncol(sigma) != nrow(sigma)) stop("Sigma is not a square matrix")
  if(is.matrix(sigma)) {flag <- 1 
  } else if(is.vector(sigma)) flag <-0
    
  p <- length(mu)
  output <- array(0, c(nsamples,p))
  for (i in 1:nsamples)
  for (j in 1:p)
    if (flag == 0) {
      output[i,j] <- rnorm(1,mu[j],sqrt(sigma[j]) )
    } else if(flag == 1) {
      output[i,j] <- rnorm(1,mu[j],sqrt(sigma[j,j]))
    }
  
  output <- as.data.frame(output)
  colnames(output) <- names(mu)
  
  return(output)  
}

newSample <- function(nsamples, mu ,sigma, m = 1000)
{
  flag <- numeric() # 1 is a matrix  and 0 is a vector
  if(ncol(sigma) != nrow(sigma)) stop("Sigma is not a square matrix")
  if(!is.matrix(sigma)) stop("Sigma has to be a matrix of dimensions p times p")

  p <- length(mu)
  output <- array(0, c(nsamples,p))
  
  for (i in 1:nsamples)
  {
      cat("\n Generating sample i = ", i , "from ", nsamples)
      output[i,] <-  apply(rMVNorm(m,mu,sigma),2,mean)
  }

  output <- as.data.frame(output)
  colnames(output) <- names(mu)
  
  return(output)  
  
}

newclassSamplefrom <- function(mu, sigma,peaks,ws,c,nsamples,type){
  # mu :      vector containing the mean spectra of reference
  # sigma:    contain variance of each value of mu
  # peaks:    vector that contains the peaks where the mu would be modified around
  # ws:       number of wavelength around the peak that would be modified creating a window 
  # c:        vector of same length than peaks and contains the constant for each peak to make separation
  # nsamples: number of samples to be generated
  # nsamples: type 1 c * exp^|t-s|
  #           type 2  exp^(c*|t-s|)
  #           type 3  c * |t-s|  
  #           type 4 exp^(c*(t-s)^2)
  
  flag1 <- ifelse(length(ws) == 1,1,0)
  flag2 <- ifelse(length(c) == 1,1,0)
  
  
  
  mean2 <- mu
  p <- length(mu)
  npeaks <- length(peaks)
  output <- array(0, c(nsamples,p))
  
  if (flag1 == 1) ws <- array(ws, dim = npeaks) 
  if (flag2 == 1) c <- array(c, dim = npeaks)
    
  
  for(peak in 1:npeaks)
  {
    
      li <- peaks[peak] - ws[peak]
      ls <- peaks[peak] + ws[peak]
      
    
    
    for(j in li:ls)
    {
      if (type == 1)
        mean2[j] <- mean2[j] + c[peak]*exp(-abs(j-peaks[peak]))
      else if(type == 2)
        mean2[j] <- mean2[j] + exp(-c[peak]*abs(j-peaks[peak]))
      else if(type == 3)
        mean2[j] <- mean2[j] + c[peak]*abs(j-peaks[peak])
      else if(type == 4)
        mean2[j] <- mean2[j] + exp(-c[peak]* ((j-peaks[peak])^2) )
      else if(type == 5)
        mean2[j] <- mean2[j] + exp(-( ( (j-peaks[peak])/c[peak] )^2 )/2 )
      
    }
  }
  
  if (is.matrix(sigma)) sigma <- diag(sigma)
      output <- gen(nsamples, mean2, sigma  )
    # output <- rMVNorm(nsamples, mean2, sigma)  
    


  colnames(output) <- names(mu) 
  output<-as.data.frame(output)
  newclassSamplefrom <- output
  
}
      
  
    

newClassSample <- function(mu, sigma, peaks, ws, c, nsamples, type){

  # mu :      vector containing the mean spectra of reference
  # sigma:    contain the variance covariance matrix of the class
  # peaks:    vector that contains the peaks where the mu would be modified around
  # ws:       number of wavelength around the peak that would be modified creating a window 
  # c:        vector of same length than peaks and contains the constant for each peak to make separation
  # nsamples: number of samples to be generated
  # nsamples: type 1 c * exp^|t-s|
  #           type 2  exp^(c*|t-s|)
  #           type 3  c * |t-s|  
  #           type 4 exp^(c*(t-s)^2)
  
  mean2 <- mu
  p <- length(mu)
  npeaks <- length(peaks)
  output <- array(0, c(nsamples,p))
  
  
  for(peak in 1:npeaks)
  {
    li <- peaks[peak] - ws
    ls <- peaks[peak] + ws
    
    for(j in li:ls)
    {
      if (type == 1)
        mean2[j] <- mean2[j] + c[peak]*exp(-abs(j-peaks[peak]))
      else if(type == 2)
        mean2[j] <- mean2[j] + exp(-c[peak]*abs(j-peaks[peak]))
      else if(type == 3)
        mean2[j] <- mean2[j] + c[peak]*abs(j-peaks[peak])
      else if(type == 4)
        mean2[j] <- mean2[j] + exp(-c[peak]* ((j-peaks[peak])^2) )
      
    }
  }
  
  output <- rMVNorm(nsamples, mean2, sigma)  
  
}





# 
ftest <- function(df_train, l)
{
 # Rank variables which give more separation of classes 
  c <- ncol(df_train)
  i <- 2
  ncol(df_train)
  fvalue <- rep(0.0,(c-2) )
  
  labs <- colnames(df_train %>% dplyr::select(where(is.numeric)))
  ini <- c - length(labs) +1
  
  for(i in ini:c)
  {
    df1 <- data.frame(df_train[,c(1,i)])
    Class <- df1[l]
    aov.r = lm(df1[,2] ~ Class, data = df1)
    fvalue[i-2] <-summary(aov.r)$fstatistic[1]
  }
  
  output <- data.frame(Var = labs, Ftest = fvalue)
  output <- output[order(-output$Ftest),]
  return(output)
}




# Correlation structure ---------------------------------------------------

# AR(1) correlation structure  
ar1_cor <- function(n,rho)
{
  exponent <- abs(matrix(1:n-1,nrow=n, ncol = n,byrow =T) - (1:n-1))
  rho^exponent
}


exp_cor <- function(n,rho)
{
  exponent <- abs(matrix(1:n-1,nrow=n, ncol = n,byrow =T) - (1:n-1))
  exp(-1*rho*exponent)
}



# Simulation from 2 mixture normal distributions --------------------------

Sim_2clases <- function(d,mu,X,S)
{
  # d distance between both clases, it is a vector of the same dimension that the response
  # mu it represent a common mean and it has the same dimension that the response
  # S is the matrix covariance matrix that can have different patterns
  # X is a Bernoulli random variable.
  
  n <- length(X)
  p <- length(mu)
  aux <- matrix(0, nrow = n, ncol = p)
  zeros <- rep(0,p)
  for (j in 1:n)
  {
    W <- rMVNorm(1, mean = zeros, sigma = S,"svd")
    aux[j,] <- mu - d + 2*d*X[j] + W[1,]
  }
  head(aux)
  
  output <- data.frame(cbind(X,aux))
  colnames(output) <- c("y",rownames(d))
  return(output)
}




# curves for creating background and spectra signal according to Chen's paper
G <- function(t,Ph,Pc,W)
{
  p <- length(t)
  g <-rep(0,p)
  for(i in 1:p)
    g[i] <- Ph*exp(- ((t[i]-Pc)/W)^2 )
  
  return(g)
}


# Plot first eleven simulations of a dataset
plot_11Sim <-function(X1_sim)
{
  
  plot(range,X1_sim[1,], col = "black", type = "l", xlab = "wavelength (nm)", ylab = "absorbance",
       ylim = c(min(X1_sim),max(X1_sim)))
  lines(range, X1_sim[2,], col = "black")
  lines(range, X1_sim[3,], col = "black")
  lines(range, X1_sim[4,], col = "black")
  lines(range, X1_sim[5,], col = "black")
  lines(range, X1_sim[6,], col = "blue",lty=3)
  lines(range, X1_sim[7,], col = "blue",lty=3)
  lines(range, X1_sim[8,], col = "blue",lty=3)
  lines(range, X1_sim[9,], col = "blue",lty=3)
  lines(range, X1_sim[10,], col = "blue",lty=3)
  lines(range, X1_sim[11,], col = "blue",lty=3)
  legend("bottomleft",legend = c("Original","Simluation"), col = c("black","blue"), lty = c(1,3), cex = 0.8)
}

sim_RevEvW <- function(x1, rango) 
{
  Spec1 <- runif(1,3,9)*dchisq(rango,450)
  Spec2 <- runif(1,0.05,10)*dchisq(rango,runif(1,550,580),runif(1,5,20))
  Spec3 <- runif(1,0.1,10)*dchisq(rango,runif(1,590,630))
  x_bar <- runif(1,1.25,2.3)*mean_spectra(x1,lb,ub)
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3)
  output <-  colSums(Spec)
  return(output)
}

sim_RevEvW1 <- function(x1, rango) 
{
  Spec1 <- runif(1,3,9)*dchisq(rango,450)
  Spec2 <- runif(1,0.05,10)*dchisq(rango,runif(1,550,580))
  Spec3 <- runif(1,0.05,10)*dchisq(rango,runif(1,550,580))
  Spec4 <- runif(1,0.1,10)*dchisq(rango,runif(1,590,630))
  x_bar <- runif(1,1,2.7)*mean_spectra(x1,lb,ub)
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4)
  output <-  colSums(Spec)
  return(output)
}


sim_MpB <- function(x1,rango) 
{
  Spec1 <- runif(1,0.5,2)*dchisq(rango,525)
  Spec2 <- runif(1,3,5)*dnorm(rango,runif(1,575,580),runif(1,10,20))
  Spec3 <- runif(1,10,25)*dnorm(rango,runif(1,580,600),100) #runif(1,2,42)
  Spec4 <- runif(1,5,10)*dnorm(rango,runif(1,620,670),runif(1,30,150))
  x_bar <- runif(1,0.28,0.8)*mean_spectra(x1,lb,ub)
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4)
  output <-  colSums(Spec)
  return(output)
}

sim_MpB1 <- function(x1,rango) 
{
  Spec1 <- runif(1,0.5,2)*dchisq(rango,525)
  Spec2 <- runif(1,3,5)*dnorm(rango,runif(1,575,580),runif(1,5,10))
  Spec3 <- runif(1,3,5)*dnorm(rango,runif(1,575,580),runif(1,10,20))
  Spec4 <- runif(1,10,25)*dnorm(rango,runif(1,580,600),100) #runif(1,2,42)
  Spec5 <- runif(1,5,10)*dnorm(rango,runif(1,620,670),runif(1,30,150))
  x_bar <- runif(1,0.25,1.2)*mean_spectra(x1,lb,ub)
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5)
  output <-  colSums(Spec)
  return(output)
}


sim_NapS <- function(x1,rango)
{
  Spec1 <- runif(1,0.5,12)*dnorm(rango,runif(1,390,405),runif(1,20,50))
  Spec2 <- runif(1,0.8,1.5)*dnorm(rango,runif(1,490,510),100)
  Spec3 <- runif(1,3,3.25)*dnorm(rango,runif(1,515,525),runif(1,50,150))
  Spec4 <- -1.5*dnorm(rango,575,30) #runif(1,2,42)
  Spec5 <- runif(1,10,15)*dnorm(rango,runif(1,517,540),50)
  Spec6 <- runif(1,10,15)*dnorm(rango,runif(1,625,650),100) #runif(1,2,42)
  x_bar <- runif(1,0.05,0.4)*mean_spectra(x1,lb,ub)
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6)
  output <-  colSums(Spec)
  return(output)
}

sim_NapS1 <- function(x1,rango)
{
  Spec1 <- runif(1,0.5,12)*dnorm(rango,runif(1,390,405),runif(1,20,50))
  Spec2 <- runif(1,0.8,1.5)*dnorm(rango,runif(1,490,510),100)
  Spec3 <- runif(1,3,3.25)*dnorm(rango,runif(1,515,525),runif(1,50,150))
  Spec4 <- -1.25*dnorm(rango,575,30) #runif(1,2,42)
  Spec5 <- runif(1,10,15)*dnorm(rango,runif(1,517,540),50)
  Spec6 <- runif(1,10,15)*dnorm(rango,runif(1,625,650),100) #runif(1,2,42)
  x_bar <- runif(1,0.05,0.8)*mean_spectra(x1,lb,ub)
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6)
  output <-  colSums(Spec)
  return(output)
}

sim_class1 <- function(x1,rango)
{
  # range spectra 
  x_bar <- runif(1,1.4,1.5)*mean_spectra1(x1)
  Spec1 <- runif(1,2.5,3) * dchisq(rango,runif(1,560,570))
  Spec2 <- -runif(1,2.5,4) * dnorm(rango,runif(1,420,425),runif(1,8,10) )
  Spec3 <- runif(1,9,11) * dchisq(rango,runif(1,571,580))
  Spec4 <- runif(1,9,11) * dchisq(rango,runif(1,571,580))
  Spec5 <- runif(1,1.5,2) * dchisq(rango,runif(1,580,590))
  Spec6 <- -runif(1,2.5,4) * dnorm(rango,runif(1,670,675), runif(1,10,15) )
  Spec7 <- runif(1,10,15) * dnorm(rango, runif(1,549,551),runif(1,3,5) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7)
  output <-  colSums(Spec)
  return(output)
}

sim_class2 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,1.5,1.8)*mean_spectra1(x1)
  Spec1 <- as.vector(runif(1,0.25,0.5) * dcauchy(scale(rango,center = T,scale = T), scale = 10 ))
  Spec2 <- -runif(1,2.5,4) * dchisq(rango, 700 )
  Spec3 <- runif(1,20,25) * dchisq(rango, 570 )
  Spec4 <- runif(1,20,25) * dchisq(rango, 570 )
  Spec5 <- runif(1,1.5,2) * dchisq(rango,runif(1,580,590))
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5)
  output <-  colSums(Spec)
  return(output)
}

sim_class3 <- function(x1,rango)
{
  # rango desde -3 hasta 3
  x_bar <- runif(1,1.5,1.8)*mean_spectra1(x1)
  Spec1 <- runif(1,0.25,0.8) * dnorm(rango)
  Spec2 <- as.vector(runif(1,0.25,0.8) * dnorm( scale(rango, center = T, scale = T)))
  Spec3 <- as.vector(runif(1,0.5,1.4) * dnorm( scale(rango, center = T, scale = T)))
  Spec4 <- as.vector(runif(1,0.5,1.4) * dnorm( scale(rango, center = T, scale = T),20))
  
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4)
  output <-  colSums(Spec)
  return(output)
}


sim_class4 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,1.5,1.8)*mean_spectra1(x1)
  Spec1 <- runif(1,10,15) * dnorm(rango, 550,10 )
  Spec2 <- runif(1,10,15) * dchisq(rango, 570 )
  Spec3 <- runif(1,10,15) * dnorm(rango, runif(1,460,470),runif(1,20,30 ) )
  Spec4 <- runif(1,10,15) * dnorm(rango, runif(1,620,630), runif(1,20,30) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4)
  output <-  colSums(Spec)
  return(output)
}

sim_class5 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,1.2,1.5)*mean_spectra1(x1)
  Spec1 <- -runif(1,3,3.5) * dnorm(rango, 400,10 )
  Spec2 <- -runif(1,2.5,3) * dnorm(rango, 420,10 )
  Spec3 <- -runif(1,2,2.5) * dnorm(rango, 450,100)
  Spec4 <- runif(1,4.5,5) * dnorm(rango, 490,100)
  Spec5 <- runif(1,3.25,3.5) * dnorm(rango, 550,10)
  Spec6 <- runif(1,3.25,3.5) * dnorm(rango, 570,10)
  Spec7 <- runif(1,3,3.5) * dnorm(rango, 590,5 )
  Spec8 <- runif(1,3.5,4) * dnorm(rango, 600,5 )
  Spec9 <- runif(1,4,4.5) * dnorm(rango, 620,5 )
  Spec10 <- runif(1,4.5,5) * dnorm(rango, 640,5 )
  Spec11 <- runif(1,2,2.5) * dnorm(rango, 650,runif(1,5,7 ) )
  Spec12<- runif(1,5.5,6) * dnorm(rango, 655, runif(1,2,5) )
  Spec13 <- runif(1,6,6.5) * dnorm(rango, 660,runif(1,5,10 ) )
  Spec14<- runif(1,3,3.5) * dnorm(rango, 665, runif(1,2,5) )
  Spec15<- runif(1,3,3.5) * dnorm(rango, 670, runif(1,2,5) )
  Spec16 <- runif(1,3,3.5) * dnorm(rango, 675,runif(1,5,10 ) )
  Spec17<- runif(1,3,3.5) * dnorm(rango, 680, runif(1,2,5) )
  Spec18<- runif(1,3,3.5) * dnorm(rango, 685, runif(1,2,5) )
  Spec19 <- runif(1,3,3.5) * dnorm(rango, 690,runif(1,5,10 ) )
  Spec20<- runif(1,3,3.5) * dnorm(rango, 695, runif(1,2,5) )
  Spec21<- runif(1,3,3.5) * dnorm(rango, 700, runif(1,2,5) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,Spec14,Spec15,
                Spec16,Spec17,Spec18,Spec19,Spec20,Spec21,Spec21)
  rownames(Spec) <- rownames(x_bar)
  output <-  colSums(Spec)
  return(output)
}

sim_class6 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,1,2.5)*mean_spectra1(x1)
  Spec1 <- -runif(1,3,3.5) * dnorm(rango, 400,10 )
  Spec2 <- -runif(1,2.5,3) * dnorm(rango, 420,10 )
  Spec3 <- -runif(1,2,2.5) * dnorm(rango, 450,100)
  Spec4 <- runif(1,4.5,5) * dnorm(rango, 490,100)
  Spec5 <- runif(1,3.25,3.5) * dnorm(rango, 550,10)
  Spec6 <- runif(1,3.25,3.5) * dnorm(rango, 570,10)
  Spec7 <- runif(1,3,3.5) * dnorm(rango, 590,5 )
  Spec8 <- runif(1,3.5,4) * dnorm(rango, 600,5 )
  Spec9 <- runif(1,4,4.5) * dnorm(rango, 620,5 )
  Spec10 <- runif(1,4.5,5) * dnorm(rango, 640,5 )
  Spec11 <- runif(1,2,2.5) * dnorm(rango, 650,runif(1,1,2 ) )
  Spec12<- runif(1,5.5,6) * dnorm(rango, 655, runif(1,2,3) )
  Spec13 <- runif(1,6,6.5) * dnorm(rango, 660,runif(1,5,10 ) )
  Spec14<- runif(1,3,3.5) * dnorm(rango, 665, runif(1,2,5) )
  Spec15<- runif(1,3,3.5) * dnorm(rango, 670, runif(1,2,5) )
  Spec16 <- runif(1,3,3.5) * dnorm(rango, 675,runif(1,5,10 ) )
  Spec17<- runif(1,3,3.5) * dnorm(rango, 680, runif(1,2,5) )
  Spec18<- runif(1,3,3.5) * dnorm(rango, 685, runif(1,2,5) )
  Spec19 <- runif(1,3,3.5) * dnorm(rango, 690,runif(1,5,10 ) )
  Spec20<- runif(1,3,3.5) * dnorm(rango, 695, runif(1,2,5) )
  Spec21<- runif(1,3,3.5) * dnorm(rango, 700, runif(1,2,5) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,Spec14,Spec15,
                Spec16,Spec17,Spec18,Spec19,Spec20,Spec21)
  output <-  colSums(Spec)
  return(output)
}

sim_class7 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,0.5,3.5)*mean_spectra1(x1)
  Spec1 <- -runif(1,3,3.5) * dnorm(rango, 400,10 )
  Spec2 <- -runif(1,2.5,3) * dnorm(rango, 420,10 )
  Spec3 <- -runif(1,2,2.5) * dnorm(rango, 450,100)
  Spec4 <- runif(1,4.5,5) * dnorm(rango, 490,100)
  Spec5 <- runif(1,3.25,3.5) * dnorm(rango, 550,10)
  Spec6 <- runif(1,3.25,3.5) * dnorm(rango, 570,10)
  Spec7 <- runif(1,3,3.5) * dnorm(rango, 590,5 )
  Spec8 <- runif(1,3.5,4) * dnorm(rango, 600,5 )
  Spec9 <- runif(1,4,4.5) * dnorm(rango, 620,5 )
  Spec10 <- runif(1,4.5,5) * dnorm(rango, 640,5 )
  Spec11 <- runif(1,2,2.5) * dnorm(rango, 650,runif(1,5,7 ) )
  Spec12<- runif(1,5.5,6) * dnorm(rango, 655, runif(1,2,5) )
  Spec13 <- runif(1,6,6.5) * dnorm(rango, 660,runif(1,5,7 ) )
  Spec14<- runif(1,3,3.5) * dnorm(rango, 665, runif(1,2,5) )
  Spec15<- runif(1,3,3.5) * dnorm(rango, 670, runif(1,2,5) )
  Spec16 <- runif(1,3,3.5) * dnorm(rango, 675,runif(1,5,7 ) )
  Spec17<- runif(1,3,3.5) * dnorm(rango, 680, runif(1,2,5) )
  Spec18<- runif(1,3,3.5) * dnorm(rango, 685, runif(1,2,5) )
  Spec19 <- runif(1,3,3.5) * dnorm(rango, 690,runif(1,5,7 ) )
  Spec20<- runif(1,3,3.5) * dnorm(rango, 695, runif(1,2,5) )
  Spec21<- runif(1,3,3.5) * dnorm(rango, 700, runif(1,2,5) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,Spec14,Spec15,
                Spec16,Spec17,Spec18,Spec19,Spec20,Spec21)
  output <-  colSums(Spec)
  return(output)
}


sim_class5 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,1.2,1.5)*mean_spectra1(x1)
  Spec1 <- -runif(1,3,3.5) * dnorm(rango, 400,10 )
  Spec2 <- -runif(1,2.5,3) * dnorm(rango, 420,10 )
  Spec3 <- -runif(1,2,2.5) * dnorm(rango, 450,100)
  Spec4 <- runif(1,4.5,5) * dnorm(rango, 490,100)
  Spec5 <- runif(1,3.25,3.5) * dnorm(rango, 550,10)
  Spec6 <- runif(1,3.25,3.5) * dnorm(rango, 570,10)
  Spec7 <- runif(1,3,3.5) * dnorm(rango, 590,5 )
  Spec8 <- runif(1,3.5,4) * dnorm(rango, 600,5 )
  Spec9 <- runif(1,4,4.5) * dnorm(rango, 620,5 )
  Spec10 <- runif(1,4.5,5) * dnorm(rango, 640,5 )
  Spec11 <- runif(1,2,2.5) * dnorm(rango, 650,runif(1,5,7 ) )
  Spec12<- runif(1,5.5,6) * dnorm(rango, 655, runif(1,2,5) )
  Spec13 <- runif(1,6,6.5) * dnorm(rango, 660,runif(1,5,10 ) )
  Spec14<- runif(1,3,3.5) * dnorm(rango, 665, runif(1,2,5) )
  Spec15<- runif(1,3,3.5) * dnorm(rango, 670, runif(1,2,5) )
  Spec16 <- runif(1,3,3.5) * dnorm(rango, 675,runif(1,5,10 ) )
  Spec17<- runif(1,3,3.5) * dnorm(rango, 680, runif(1,2,5) )
  Spec18<- runif(1,3,3.5) * dnorm(rango, 685, runif(1,2,5) )
  Spec19 <- runif(1,3,3.5) * dnorm(rango, 690,runif(1,5,10 ) )
  Spec20<- runif(1,3,3.5) * dnorm(rango, 695, runif(1,2,5) )
  Spec21<- runif(1,3,3.5) * dnorm(rango, 700, runif(1,2,5) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,Spec14,Spec15,
                Spec16,Spec17,Spec18,Spec19,Spec20,Spec21,Spec21)
  output <-  colSums(Spec)
  return(output)
}

sim_class6 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,1,2.5)*mean_spectra1(x1)
  Spec1 <- -runif(1,3,3.5) * dnorm(rango, 400,10 )
  Spec2 <- -runif(1,2.5,3) * dnorm(rango, 420,10 )
  Spec3 <- -runif(1,2,2.5) * dnorm(rango, 450,100)
  Spec4 <- runif(1,4.5,5) * dnorm(rango, 490,100)
  Spec5 <- runif(1,3.25,3.5) * dnorm(rango, 550,10)
  Spec6 <- runif(1,3.25,3.5) * dnorm(rango, 570,10)
  Spec7 <- runif(1,3,3.5) * dnorm(rango, 590,5 )
  Spec8 <- runif(1,3.5,4) * dnorm(rango, 600,5 )
  Spec9 <- runif(1,4,4.5) * dnorm(rango, 620,5 )
  Spec10 <- runif(1,4.5,5) * dnorm(rango, 640,5 )
  Spec11 <- runif(1,2,2.5) * dnorm(rango, 650,runif(1,1,2 ) )
  Spec12<- runif(1,5.5,6) * dnorm(rango, 655, runif(1,2,3) )
  Spec13 <- runif(1,6,6.5) * dnorm(rango, 660,runif(1,5,10 ) )
  Spec14<- runif(1,3,3.5) * dnorm(rango, 665, runif(1,2,5) )
  Spec15<- runif(1,3,3.5) * dnorm(rango, 670, runif(1,2,5) )
  Spec16 <- runif(1,3,3.5) * dnorm(rango, 675,runif(1,5,10 ) )
  Spec17<- runif(1,3,3.5) * dnorm(rango, 680, runif(1,2,5) )
  Spec18<- runif(1,3,3.5) * dnorm(rango, 685, runif(1,2,5) )
  Spec19 <- runif(1,3,3.5) * dnorm(rango, 690,runif(1,5,10 ) )
  Spec20<- runif(1,3,3.5) * dnorm(rango, 695, runif(1,2,5) )
  Spec21<- runif(1,3,3.5) * dnorm(rango, 700, runif(1,2,5) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,Spec14,Spec15,
                Spec16,Spec17,Spec18,Spec19,Spec20,Spec21)
  output <-  colSums(Spec)
  return(output)
}

sim_class8 <- function(x1,rango)
{
  # rango spectra
  x_bar <- runif(1,0.7,4.5)*mean_spectra1(x1)
  Spec1 <- -runif(1,3,3.5) * dnorm(rango, 400,10 )
  Spec2 <- -runif(1,2.5,3) * dnorm(rango, 420,10 )
  Spec3 <- -runif(1,2,2.5) * dnorm(rango, 450,100)
  Spec4 <- runif(1,4.5,5) * dnorm(rango, 490,100)
  Spec5 <- runif(1,3.25,3.5) * dnorm(rango, 550,10)
  Spec6 <- runif(1,3.25,3.5) * dnorm(rango, 570,10)
  Spec7 <- runif(1,3,3.5) * dnorm(rango, 590,5 )
  Spec8 <- runif(1,3.5,4) * dnorm(rango, 600,5 )
  Spec9 <- runif(1,4,4.5) * dnorm(rango, 620,5 )
  Spec10 <- runif(1,4.5,5) * dnorm(rango, 640,5 )
  Spec11 <- runif(1,2,2.5) * dnorm(rango, 650,runif(1,1,2 ) )
  Spec12<- runif(1,5.5,6) * dnorm(rango, 655, runif(1,2,5) )
  Spec13 <- runif(1,6,6.5) * dnorm(rango, 660,runif(1,1,2 ) )
  Spec14<- runif(1,3,3.5) * dnorm(rango, 665, runif(1,2,5) )
  Spec15<- runif(1,3,3.5) * dnorm(rango, 670, runif(1,2,5) )
  Spec16 <- runif(1,3,3.5) * dnorm(rango, 675,runif(1,1,2 ) )
  Spec17<- runif(1,3,3.5) * dnorm(rango, 680, runif(1,2,5) )
  Spec18<- runif(1,3,3.5) * dnorm(rango, 685, runif(1,2,5) )
  Spec19 <- runif(1,3,3.5) * dnorm(rango, 690,runif(1,1,2 ) )
  Spec20<- runif(1,3,3.5) * dnorm(rango, 695, runif(1,2,5) )
  Spec21<- runif(1,3,3.5) * dnorm(rango, 700, runif(1,2,5) )
  Spec <- rbind(as.vector(x_bar),Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,Spec14,Spec15,
                Spec16,Spec17,Spec18,Spec19,Spec20,Spec21)
  output <-  colSums(Spec)
  return(output)
}




sim_ChiNude <- function(x1,rango)
{
  # "Nude - Chique Nude
  x_bar <-  mean_spectra(df,rango)
  c <- runif(1,2.5,5)
  Spec1 <-  as.vector(x_bar)
  Spec2 <- runif(1,3,4) * dchisq(rango,runif(1,400,405))
  Spec3 <- runif(1,2,3) * dchisq(rango,runif(1,405,415))
  Spec4 <- runif(1,1.5,2) * dchisq(rango,runif(1,416,440))
  Spec5 <- runif(1,1.4,2) * dchisq(rango,runif(1,441,450))
  Spec6 <- runif(1,1.1,1.7) * dchisq(rango,runif(1,450,560))
  Spec7 <- runif(1,1.1,1.7) * dchisq(rango,runif(1,461,470))
  Spec8 <- runif(1,1.1,1.7) * dchisq(rango,runif(1,471,480))
  Spec9 <- runif(1,2.5,3) * dchisq(rango,runif(1,480,490))
  Spec10 <- runif(1,2.5,3) * dchisq(rango,runif(1,490,500))
  Spec11 <- runif(1,1.5,2) * dchisq(rango,runif(1,500,510))
  Spec12 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,600,650))
  Spec13 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,651,660))
  Spec14 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,660,670))
  Spec15 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,670,700))
  Spec16 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,670,680))
  Spec17 <- -runif(1,3.5,4) * dchisq(rango,runif(1,680,690))
  Spec18 <- -runif(1,4.5,5) * dchisq(rango,runif(1,690,700))
  Spec <- rbind(Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,
                Spec14,Spec15,Spec16,Spec17,Spec18)
  output <- c* colSums(Spec)
  return(output)
}


sim_SportVert <- function(x1,rango)
{
  # "Sportsgirl - Vertigo"
  x_bar <-  mean_spectra(df,rango)
  c <- runif(1,2,6)
  Spec1 <-  as.vector(x_bar)
  Spec2 <- runif(1,2.25,2.5) * dnorm(rango,400,10)
  Spec3 <- runif(1,1.3,1.4) * dchisq(rango,runif(1,411,420))
  Spec4 <- runif(1,1.05,1.15) * dchisq(rango,runif(1,421,430))
  Spec5 <- runif(1,1.05,1.15) * dchisq(rango,runif(1,431,440))
  Spec6 <- runif(1,1.05,1.15) * dchisq(rango,runif(1,441,450))
  Spec7 <- runif(1,1.05,1.15) * dchisq(rango,runif(1,451,460))
  Spec8 <- runif(1,1.05,1.15) * dchisq(rango,runif(1,461,470))
  Spec9 <- runif(1,1.05,1.15) * dchisq(rango,runif(1,471,480))
  Spec10 <- runif(1,1.05,1.25) * dchisq(rango,runif(1,481,490))
  Spec11 <- runif(1,1.05,1.25) * dchisq(rango,runif(1,491,500))
  Spec12 <- runif(1,1.05,1.25) * dchisq(rango,runif(1,501,510))
  Spec13 <- runif(1,1.05,1.25) * dchisq(rango,runif(1,511,520))
  Spec14 <- runif(1,1.05,1.25) * dchisq(rango,runif(1,521,530))
  Spec15 <- runif(1,1.05,1.25) * dchisq(rango,runif(1,531,540))
  Spec16 <- runif(1,1.05,1.25) * dchisq(rango,runif(1,541,550))
  Spec17 <- runif(1,1.5,2) * dchisq(rango,runif(1,551,560))
  Spec18 <- runif(1,1.5,2) * dchisq(rango,runif(1,561,570))
  Spec19 <- runif(1,0.5,1.5) * dchisq(rango,runif(1,571,580))
  Spec20 <- runif(1,0.5,1.5) * dchisq(rango,runif(1,581,590))
  Spec21 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,521,530))
  Spec22 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,531,540))
  Spec23 <- -runif(1,0.5,1.5) * dchisq(rango,runif(1,541,550))
  Spec24 <- -runif(1,4.5,5) * dchisq(rango,runif(1,551,560))
  Spec25 <- -runif(1,4.5,5) * dchisq(rango,runif(1,690,700))
  
  
  
  Spec <- rbind(Spec1,Spec2,Spec3,Spec4,Spec5,Spec6,Spec7,Spec8,Spec9,Spec10,Spec11,Spec12,Spec13,
                Spec14,Spec15,Spec16,Spec17,Spec18,Spec19,Spec20,Spec21,Spec22,Spec23,Spec24,Spec25)
  output <- c* colSums(Spec)
  return(output)
}




sim_spectra1 <- function(x1,rango, n, clase, output_samples)
{
  # Calculate the mean spectra of the different curves
  t <- matrix(0, nrow = n,ncol = ncol(x1))
  # Generating n simulations
  
  for(j in 1:n)
  {
      if(clase == "RevlonWine")  t[j,] <- sim_RevEvW(x1,rango)
      else if(clase == "RevlonWine1")        t[j,] <- sim_RevEvW1(x1,rango)
      else if(clase == "ModelsPreferB")        t[j,] <- sim_MpB(x1,rango)
      else if(clase == "ModelsPreferB1")        t[j,] <- sim_MpB1(x1,rango)
      else if(clase == "NapoleonScarlet")        t[j,] <- sim_NapS(x1,rango)
      else if(clase == "NapoleonScarlet1")        t[j,] <- sim_NapS1(x1,rango)
      else if(clase == "ChiNude")        t[j,] <- sim_ChiNude(x1,rango)
      else if(clase == "Class1")        t[j,] <- sim_class1(x1,rango)
      else if(clase == "Class2")        t[j,] <- sim_class2(x1,rango)
      else if(clase == "Class3")        t[j,] <- sim_class3(x1,rango)
      else if(clase == "Class4")        t[j,] <- sim_class4(x1,rango)
      else if(clase == "Class5")        t[j,] <- sim_class5(x1,rango)
      else if(clase == "Class6")        t[j,] <- sim_class6(x1,rango)
      else if(clase == "Class7")        t[j,] <- sim_class7(x1,rango)
      else if(clase == "Class8")        t[j,] <- sim_class8(x1,rango)
      
      
  }
  
  if (output_samples == 1)  return(t)  else   return( rbind(x1,t) )
}

mean_spectra1 <- function (x1){
  x1_bar <-as.matrix(colMeans(x1))
  return(x1_bar)
}

mean_spectra <- function (x1,lb,ub){
  x1_bar <-as.matrix(colMeans(x1))
  return(x1_bar)
}


sim_spectra <- function(x1,lb,ub, G, n, output_samples)
{
  # Calculate the mean spectra of the different curves
  x1_bar <-as.matrix(colMeans(x1))
  # construct a matrix of 1 
  ones <- as.matrix(rep(1,nrow(x1)))
  
  # Construct the matrix of mean for each columns
  X1_bar <- (ones %*% t(x1_bar ) )
  
  # Centering the dataset
  Xc <- (as.matrix(x1) - X1_bar)
  # Calculate the covariance matrix
  S<-t(Xc) %*% Xc 
  # Select the diagonal
  Sd<-diag(S)
  
  # initialise t
  t <- x1_bar

  # Generating n simulations
  for(j in 1:n)
  {
    for(i in 1:length(t))
    {
      t[i] <- rnorm(length(t[i]),x1_bar[i], G)
    }
    t <- runif(1,min(Xc)*1.05,max(Xc)*0.95) + (t + dnorm(length(t),450,50))
    if (j == 1)
      X1_sim <-rbind(x1,as.data.frame(t(t)))
    else (j > 1)  
    X1_sim <-rbind(X1_sim,as.data.frame(t(t)))
    
  }
  if (output_samples == 1) return(tail(X1_sim,n)) 
  else return(X1_sim)
  
}
# Replicate

pls_da_mod <- function(train_df,test_df,trainl,testl, ncomponents)
{
  c.optm <- 1
  res <- matrix(0, nrow = ncomponents, ncol  = 2)
  for (i in 1: ncomponents)
  {
    mod <- plsda(train_df,trainl, i)
    pred <- predict(mod, test_df, dist = "max.dist")
    prediction <- pred$class$max.dist[,i]
    res[i,1] <- i
    res[i,2] <- 100 * sum(as.character(prediction) == testl)/nrow(test_df)

  }
  
  c.optm <- which.max(res[,2])
  return(list(model = mod,prediction = as.character(prediction), results = res, components = c.optm))  
}


knn_mod <- function(train_df,test_df,trainl,testl, k)
{
  i <- 1
  k.optm <- 1
  res <- matrix(0,nrow = k, ncol = 2)
  for (i in 1:k) {
    mod <- knn(train = train_df , test = test_df, cl = trainl, k = i)
    res[i,1] <- i 
    res[i,2] <- 100 * sum(testl == as.character(mod))/length(testl)
  }
  
  k.optm <- which.max(res[,2])
  return(list(prediction = as.character(mod), results = res, k.optm = k.optm))  
  
}



rp_glm <- function(train_df,test_df,trainl,testl)
{
  R <-form_matrix(ncol(train_df),3, T, eps = 0.9,"achlioptas")
  Xrp <- as.matrix(train_df) %*% R
  Xrp_test <- as.matrix(test_df) %*% R
  
  mod <- multinom(trainl~. , data = as.data.frame(Xrp))
  pred <- predict(mod, newdata = Xrp_test)
  accuracy <- sum(as.character(testl) == pred)/nrow(test_df)

  
  return(list(R = R , model = mod, prediction = pred, accuracy = accuracy))  
  
}
  

range_normalisation <- function (X)
{
  aux1 <- apply(X,1,min)
  aux2 <- apply(X,1,max)
  aux3 <- 1/(aux2 - aux1)
  
  aux1 <- as.matrix(aux1)
  unos <- t(rep(1, ncol(X)))
  min <- aux1 %*% unos
  Xmin = X - min
  filas <- ncol(X)
  I2 <- diag( 1, filas) 
  I3 <- diag(1, length(aux3))
  diag(I3) <- aux3
  s <- I3 %*% as.matrix(Xmin)
  # Normalization row-wise transformation of a matrix X
  X_length1 <- s/sqrt(apply(s^2,1,sum))
  return(X_length1)
}

one_normalisation <- function (X)
{
  X_centred <- scale(X,center=T,scale=F)
  # Normalization row-wise transformation of a matrix X
  X_length1 <- X_centred/sqrt(apply(X_centred^2,1,sum))

  return(X_length1)
}


#train <-df_red_abs
#lb = 203
#ub = 503
#nc = 301 
#p = 20
#nc = 20
#test <- df_red_abs_val
LDA_pca <- function(train, x, p, nc, test, r)
{
 # nc:           number of principal components to calculate
 # train:        complete number of 
 # x:            just covariates
 # p:            number of principal components to use
  
   l <- list()
   # column-wise manipulation
   X_centred <- scale(X,center=T,scale=F)

   # Normalization row-wise transformation of a matrix X
   X_length1 <- X_centred/sqrt(apply(X_centred^2,1,sum))
   X_nipals <- chemometrics::nipals(X_length1, a = ncol(X_centred))
  # X_nipals <- nipals(X_length1, a = length(r))
   
   scores <- data.frame(X_nipals$T)
   loadings <- X_nipals$P
   
 # LDA Abosrbance Red -----------------------------------------------------------------
 
 # Using only p components
 
 
   X_svd <- svd(X_length1)
 
   eigenvalues <- X_svd$d
   cum_var <- cumsum(100*(sort(eigenvalues[1:nc], decreasing = T))/sum(eigenvalues))
   
   if (p == 0)
       p <-length(cum_var[cum_var < 100])
  
   total <- sum(eigenvalues)
 
   X_covariates = as.matrix(X) %*% loadings[,c(1:p)]
   
   #X_covariates = as.matrix(X[,r]) %*% loadings[1:nc,c(1:p)]
   
   mod.red.train <- lda(X_covariates, train$Brand)
 
   XN <- test[,r]
 
   XN_covariates = as.matrix(XN) %*% loadings[,c(1:p)]

   mod.red.test <- predict(mod.red.train, newdata = XN_covariates)
 
   t <- table(test$Brand, mod.red.test$class)
   accuracy <- sum(as.vector(test$Brand) == mod.red.test$class) / sum(t)
 
 # 3  PC  Accuracy 51%
 # 5  PC  Accuracy 69%
 # 10 PC  Accuracy 83%

   l$loadings <- loadings
   l$scores <- scores
   l$eigenvalues <- eigenvalues
   l$model <- mod.red.train
   l$confusion_matrix <- t
   l$accuracy <- accuracy
   l$npcs <- p
   l$cum_var <- cum_var
   return(l)

}


LDA_pca_ftir <- function(train, x, p, nc, test, r)
{
  # nc:           number of principal components to calculate
  # train:        complete number of 
  # x:            just covariates
  # p:            number of principal components to use
  l <- list()
  # column-wise manipulation
  X_centred <- scale(X,center=T,scale=F)
  
  # Normalization row-wise transformation of a matrix X
  X_length1 <- X_centred/sqrt(apply(X_centred^2,1,sum))
  X_nipals <- chemometrics::nipals(X_length1, a = nc)
  # X_nipals <- nipals(X_length1, a = length(r))
  
  scores <- data.frame(X_nipals$T)
  loadings <- X_nipals$P
  
  # LDA Abosrbance Red -----------------------------------------------------------------
  
  # Using only p components
  
  
  X_svd <- svd(X_length1)
  
  eigenvalues <- X_svd$d
  cum_var <- cumsum(100*(sort(eigenvalues[1:nc], decreasing = T))/sum(eigenvalues))
  
  if (p == 0)
    p <-length(cum_var[cum_var < 100])
  
  total <- sum(eigenvalues)
  
  X_covariates = as.matrix(X) %*% loadings[,c(1:p)]
  
  #X_covariates = as.matrix(X[,r]) %*% loadings[1:nc,c(1:p)]
  
  mod.red.train <- lda(X_covariates, train$Brand)
  
  XN <- test[,r]
  
  XN_covariates = as.matrix(XN) %*% loadings[,c(1:p)]
  
  mod.red.test <- predict(mod.red.train, newdata = XN_covariates)
  
  t <- table(test$Brand, mod.red.test$class)
  accuracy <- sum(as.vector(test$Brand) == mod.red.test$class) / sum(t)
  
  # 3  PC  Accuracy 51%
  # 5  PC  Accuracy 69%
  # 10 PC  Accuracy 83%
  
  l$loadings <- loadings
  l$scores <- scores
  l$eigenvalues <- eigenvalues
  l$model <- mod.red.train
  l$confusion_matrix <- t
  l$accuracy <- accuracy
  l$npcs <- p
  l$cum_var <- cum_var
  return(l)
  
}

