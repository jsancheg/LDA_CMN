# File: CompareModels.R
# Author: Jorge Sanchez
# Date: 05/02/2022
# Purpose: Fit LDA, QDA, and a contaminated mixture of normal model to a
#          to a simulated spectra dataset of two classes to discriminate 
#          samples of these 2 classes



# 1) Initialize paths and functions to use --------------------------------

work_path <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/"
setwd(work_path)
source("utilities.R")
#source("E:/University of Glasgow/Literature review/R Code/Punzo/ContaminatedMixt-master/R/main.R")



# 2) Extract, transform, and load meat dataset ----------------------------

source("ETMeat.R")



# 3) Plot Pork meat spectra because it would be use to created Class A -------
x <- unique(dfMeat1$Wavelength)
ymin <- min(dfMeatWide[,-c(1:3)])
ymax <- max(dfMeatWide[,-c(1:3)])

dfMeatWide[1:2,1:4]

# plot Pork
plot(x, y = dfPorkWide[1,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "blue",
     main = "Pork spectra")
for(i in 2:nrow(dfPorkWide))
  lines(x, y = dfPorkWide[i,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "blue")



# 4) Simulate Class A and B, simulating each wavelength separately --------

# setting up parameters

# Use size of class A as 40 is the sample size of each class 
# in the original dataset
nSim <- 45  

peaks = c(232,330,382)
ws = c(50,50,50)
shape = c(4,4,4)
c = c(0.002,0.01,0.005)
models <- c("EII","VII")
classes <- c("A","B")
s1 <- sPork

# Simulating Class A 
sample4.1 <- gen(nSim, XbarPork, s1 )
sample4.1df <- SampleToDf(sample4.1,"A")

# Simulate class B by modifying mean of class A using
#  window size (ws) = 30, c = 0.009, shape = 4

mu4.2 <- modMean(XbarPork,330,ws = 30,c = 0.009,shape = 4)
sample4.2 <- gen(nSim, mu4.2[[1]], s1)
sample4.2df <- SampleToDf(sample4.2,"B")
samples4.2 <- rbind(sample4.1df,sample4.2df)


# Plot class  A and B
ymin <- min(sample4.1,sample4.2)
ymax <- max(sample4.1,sample4.2)

plot(x, y = sample4.1df[1,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "orange",
     main = "Class A and Class B", ylab = "Itensity")
for (i in 2:40)
  lines(x, y = sample4.1df[i,-c(1:2)],  type = "l", col = "orange")
for (i in 1:40)
  lines(x, y = sample4.2df[i,-c(1:2)],  type = "l", col = "green")
legend("topleft", title = "Classes",legend = c("A","B"), 
       col = c("orange","green"), lty = c(1,1), cex = 1 )



# 5) Fit LDA model using the entire range of wavelengths ------------------

n <- nrow(samples4.2)
p <- ncol(samples4.2)

ind.test <- sample(1:n,round(n/3))
test.data <- samples4.2[ind.test,-2]
test.label <- samples4.2[ind.test,]$Class

train.data <- samples4.2[-ind.test,-2]
train.label <- samples4.2[-ind.test,]$Class

corr.class.rate <- matrix(0,nrow = 1, ncol = 2)
error.rate <- matrix(0, nrow = 1, ncol = 2)
colnames(corr.class.rate) <- c("train", "test")
colnames(error.rate) <- c("train", "test")


lda.fit <- lda(Class ~., data = train.data)
pred.train <- predict(lda.fit,train.data)
pred.class <- predict(lda.fit,test.data)
corr.class.rate[1,1] <- sum(diag(table(train.label, levels(train.label)[max.col(pred.train$posterior)])))/ (length(train.label))
corr.class.rate[1,2] <- sum(diag(table(test.label, levels(test.label)[max.col(pred.class$posterior)])))/ (length(test.label))
error.rate[1,1] <- 1 - corr.class.rate[1,1]
error.rate[1,2] <- 1- corr.class.rate[1,2]

# Correct classification rate
corr.class.rate
error.rate

ldahist(data = pred.class$x[,1], g=train.data$Class)
pred.train$x
aux <-rep(0,length(pred.train$x))

# Check the class of the train data and the vector that contains the 
# 1st  linear discriminant function

class(train.data)
class(lda.fit$scaling)

# Check the dimension of the train data and predicted values
dim(as.matrix(train.data[,-1]))
dim(lda.fit$scaling)

# Project the train data onto the first discriminant analysist
projectLD1 <- as.matrix(train.data[,-1]) %*% lda.fit$scaling


dataset <- data.frame(Class=train.data$Class, lda=pred.train$x )
ggplot(dataset, aes(x=LD1)) + 
  geom_density(aes(group=Class, colour=Class, fill=Class), alpha=0.3)

#visualise how LD1 and LD2 together separate the three classes
plot(aux,pred.train$x, col=train.data$Class,
     pch=as.numeric(train.data$Class), xlab="aux", ylab="LD1")
legend("topleft", title = "class", legend = c("A","B"), 
       col = c("black","red"), lty = c(1,1),cex = 1.2)

#create a classification rule based on LD1
pred.LD1 <-rep("B",nrow(train.data))
pred.LD1[pred.train$x < -5] <- "A"
sum(pred.LD1!=train.data$Class)

plot(x = Wavelength, y = lda.fit$scaling, type = "l", 
     ylab = "Loadings")

temp <- data.frame(Wavelengths = rownames(lda.fit$scaling), 
                   Weights = abs(lda.fit$scaling) ) 
head(temp)
class(temp)

# Rank wavelengths by their weights in the 1st discriminant function
Wavelengths.rank <-  temp[order(-temp$
                                  LD1),]

head(Wavelengths.rank,50)





# 8) defining the number of variables for LDA and CMN and plot them -------

lim <- 100
lda.mod1 <- fit_LDA(train.data,"Class")
lda.mod1$Wavelengths.ranked[1:lim,]
vars <- lda.mod1$Wavelengths.ranked
vars.top<-lda.mod1$Wavelengths.ranked[1:lim,]$Wavelengths
plotRestWavelengths(train.data, vars, lim , "Class")

vars.top
vars.mod <- c(sapply(c("Class",vars.top),match, table = colnames(train.data) ) )
vars.mod
Strain.data <- train.data[,vars.mod] #add vars
Strain.label <- Strain.data$Class
Stest.data <- test.data[,vars.mod]
Stest.label <- Stest.data$Class


# 9) LDA with subset of variables -----------------------------------------

lda.mod<- fit_LDA(Strain.data, "Class", Stest.data,Stest.label)
lda.mod$Wavelengths.ranked
lda.mod$error.rate

lda.mod$Cm_train
lda.mod$Cm_test

sum(diag(lda.mod$Cm_train))/sum(lda.mod$Cm_train)
1- sum(diag(lda.mod$Cm_train))/sum(lda.mod$Cm_train)

sum(diag(lda.mod$Cm_test))/sum(lda.mod$Cm_test)
1- sum(diag(lda.mod$Cm_test))/sum(lda.mod$Cm_test)

# 10) MClust EII and VII------------------------------------------------------
# Mclust

component <- 2
models <- c("EII","VII")
n.models <- length(models)

tabMClust <- matrix(NA, nrow = length(models), ncol = 2)
rownames(tabMClust) <- models
colnames(tabMClust) <- c("Train error","Test error")
ccmatrix <- list()

for (j in 1:n.models )
{
  Mtrain <- mstep(Strain.data[,-1],modelName =  models[j],
                  z= unmap(Strain.label))
  Etrain <- estep(Mtrain$modelName,
                  data = Strain.data[,-1], parameters = Mtrain$parameters)
  
  tabTrain <- table(max.col(Etrain$z,"first"),Strain.label)
  
  Etest <- estep(Mtrain$modelName,
                 data = Stest.data[,-1], parameters = Mtrain$parameters)
  
  tabTest <- table(max.col(Etest$z, "first"), Stest.label)
  
  
  tabMClust[j,1] <- sum( max.col(Etrain$z) != as.numeric(Strain.label) ) / length(Strain.label)
  tabMClust[j,2] <- sum( max.col(Etest$z) != as.numeric(Stest.label)   ) / length(Stest.label)
  
  ccmatrix[[j]] <- list(tabTrain,tabTest)
  
}

tabMClust


# 10) CMN ---------------------------------------------------------------------

# error rate table 
tabCMN <- matrix(NA, nrow = n.models , ncol = 2)
bad.points <- list()
estimates <- list()
rownames(tabCMN) <- models
colnames(tabCMN) <- c("Train","Test")
for (m in 1:n.models)
{
  mod <- CNmixt(X = as.matrix(Strain.data[,-1]), G = component,
                contamination = T,
                model = models[m],
                label = as.numeric(Strain.label),
                initialization = "random.post",
                seed = 12, parallel = F  )      
  
  bad.points[[m]] <- list(model = models[m], badPoins = mod$models[[1]]$detection)
  estimates [[m]]<- list(model = models[m], alpha = mod$models[[1]]$alpha,eta = mod$models[[1]]$eta)
  predTrain<- CNpredict(as.matrix(Strain.data[,-1]), 
                        prior= mod$models[[1]]$prior,
                        mu = mod$models[[1]]$mu,
                        invSigma = mod$models[[1]]$invSigma)
  
  tabTrain <- table(predTrain,as.numeric(Strain.label))
  tabTrain
  
  tabCMN[m,1] <- sum(predTrain != as.numeric(Strain.label))/length(predTrain)
  
  predtest <- CNpredict(as.matrix(Stest.data[,-1]), 
                        prior = mod$models[[1]]$prior, 
                        mu = mod$models[[1]]$mu, 
                        invSigma = mod$models[[1]]$invSigma)
  
  tabTest<- table(predtest, as.numeric(Stest.label))
  tabCMN [m,2] <- sum(predtest != as.numeric(Stest.label))/length(predtest)
  
  
}

tabCMN
bad.points
estimates

# CMN function -----------------------------------------------------------




# 11) Simulating bigger samples -----------------------------------------------
nSim <- 700
set.seed(123)
# Simulating Class A 
sample4.3 <- gen(nSim, XbarPork, s1 )
sample4.3df <- SampleToDf(sample4.3,"A")

# Simulate class B by modifying mean of class A using
#  window size (ws) = 30, c = 0.009, shape = 4

mu4.3 <- modMean(XbarPork,330,ws = 30,c = 0.009,shape = 4)
sample4.4 <- gen(nSim, mu4.3[[1]], s1)
sample4.4df <- SampleToDf(sample4.4,"B")
samples4.3 <- rbind(sample4.3df,sample4.4df)

# Plot data

ymin <- min(sample4.3,sample4.4)
ymax <- max(sample4.3,sample4.4)

plot(Wavelength, y = sample4.3df[1,-c(1:2)], ylim = c(ymin,ymax), type = "l", col = "orange",
     main = "Class A and Class B", ylab = "Itensity")
for (i in 2:nSim)
  lines(Wavelength, y = sample4.3df[i,-c(1:2)],  type = "l", col = "orange")
for (i in 1:nSim)
  lines(Wavelength, y = sample4.4df[i,-c(1:2)],  type = "l", col = "green")
legend("topleft", title = "Classes",legend = c("A","B"), 
       col = c("orange","green"), lty = c(1,1), cex = 1 )




# 14) Split dataset in training and test ----------------------------------

n <- nrow(samples4.3)
p <- ncol(samples4.3)
set.seed(123)
ind.test1 <- sample(1:n,round(n/3))
test.data1 <- samples4.3[ind.test1,-2]
test.label1 <- samples4.3[ind.test1,]$Class

train.data1 <- samples4.3[-ind.test1,-2]
train.label1 <- samples4.3[-ind.test1,]$Class

corr.class.rate1 <- matrix(0,nrow = 1, ncol = 2)
error.rate1 <- matrix(0, nrow = 1, ncol = 2)
colnames(corr.class.rate1) <- c("train", "test")
colnames(error.rate1) <- c("train", "test")


# 15) Defining number of wavelengths --------------------------------------
contamination = T
initialization = "random.post"
 parallel = F

qda.mod1 <- fit_QDA(train.data1, "Class")
cbind(qda.mod1$Wavelengths.ranked$Wavelengths,lda.mod1$Wavelengths.ranked$Wavelengths)
vars.lda <- lda.mod1$Wavelengths.ranked
vars1 <- qda.mod1$Wavelengths.ranked
lim1 <- c(seq(25,155,10),156,seq(160,440,10))
lim1 <- seq(25,425,25)
lim1 <- seq(115,120,1)
resQDA <- array(NA,dim =c(length(lim1),2,2))
resLDA <- array(NA, dim = c(length(lim1),2,2))
resMClust <- array(NA, dim = c(length(lim1),2,2,n.models))
resCMN <- array(NA, dim = c(length(lim1),2,2,n.models))


dimnames(resQDA) <- list(lim1,c("train","test"),
                         c("error rate","Correct classification rate"))
dimnames(resLDA) <- list(lim1,c("train","test"),
                         c("error rate","Correct classification rate"))
if(length(models)>1)
{
  dimnames(resMClust) <- list(lim1,c("train","test"),
                              c("error rate","Correct classification rate"),
                              models)
  
}else 
{
  dimnames(resMClust) <- list(lim1,c("train","test"),
                              c("error rate","Correct classification rate"))
}
  
  
dimnames(resCMN) <- list(lim1,c("train","test"),
                            c("error rate","Correct classification rate"),
                            models)


for (l in 1:length(lim1))
{
  vars.top.qda <-qda.mod1$Wavelengths.ranked[1:lim1[l],]$Wavelengths
  vars.top<-lda.mod1$Wavelengths.ranked[1:lim1[l],]$Wavelengths
  # plotRestWavelengths(train.data1, vars1, lim1[l] , "Class")
  
  vars.top
  vars.top.qda
  vars.mod <- c(sapply(c("Class",vars.top),match, table = colnames(train.data1) ) )
  vars.mod.qda <- c(sapply(c("Class",vars.top.qda),match, table = colnames(train.data1) ) )
  Strain.data <- train.data1[,vars.mod] #add vars
  Strain.label <- Strain.data$Class
  Stest.data <- test.data1[,vars.mod]
  Stest.label <- Stest.data$Class
  
  Strain.data.qda <- train.data1[,vars.mod.qda] #add vars
  Strain.label.qda <- Strain.data$Class
  Stest.data.qda <- test.data1[,vars.mod.qda]
  Stest.label.qda <- Stest.data$Class
  
  qda.mod2 <- fit_QDA(Strain.data.qda, "Class", Stest.data.qda,Stest.label.qda)
  lda.mod2 <- fit_LDA(Strain.data, "Class", Stest.data,Stest.label)
  EDDA.mod2 <- fit_EDDA(Strain.data[,-1],Strain.data$Class,
                        Stest.data[,-1],Stest.data$Class,
                        models,components = 2)
  cat("\n", "variables = ",lim1[l])
  v1 <- tryCatch({
    CMN.mod2 <- fit_CMN(Strain.data[,-1],Strain.data$Class,
                      Stest.data[,-1],Stest.data$Class,
                      component = 2, models = models,
                      contamination = contamination, 
                      initialization = initialization,
                      parallel = parallel)
  }, error = function(err)
  {
    print(paste("Error: ", err))
  })
  
  qda.mod2$Cm_train
  qda.mod2$Cm_test
  
  resQDA[l,,1] <- qda.mod2$error.rate
  resQDA[l,,2] <- qda.mod2$corr.class.rate
  resLDA[l,,1] <- lda.mod2$error.rate
  resLDA[l,,2] <- lda.mod2$corr.class.rate

  for(m in 1:n.models)
  {
    # error rate
    resMClust[l,,1,m] <- EDDA.mod2$error.rate[m,]
    # correct classification rate
    resMClust[l,,2,m] <- EDDA.mod2$corr.class.rate[m,]
    resCMN[l,,1,] <- t(CMN.mod2[[m]]$error.rate)
    resCMN[l,,2,] <- t(CMN.mod2[[m]]$corr.class.rate)
    
  }
}

round(resLDA,2)
round(resQDA,2)
round(resMClust,2)
round(resCMN,2)

length(XbarPork)
length(mu4.2$Mean2)

XbarPork== mu4.2$Mean
temp.vars <- names(mu4.2$Mean2[XbarPork!= mu4.2$Mean2])[1:22]

vars.sel <- match(c("Class",temp.vars), 
                  colnames(samples4.2) )
vars.sel1 <- match(temp.vars, 
                  colnames(samples4.2) )

resumen <- Models_Spectra(train.data1[,vars.sel],"Class",
                          test.data1[,vars.sel1],
                          test.data1$Class,
                          models = c("EII","VII","EEI"),
                          components = 2)

resumen
# 60) Function that takes a set of variables and run all models ------------

# This function is called "Models_Spectra"
# It takes 9 parameters
# X:         A dataframe or matrix that contains the response variable and covariates
# ColClass:  A character variable that contains the name of the response variable
# test:      A dataframe or matrix that contains the covariates
# testl:     A vector that contains the test label   
# component: Number of components
# models:    Models that are going to be considered
# contamination: boolean variable that contains if there is contamination or not
# initialization: methods of initialization of the first estimates
# parallel:       boolean variable that indicate if parallel computation should be used
# Output: 
# Correct classification error: Table containing the correct classification error
# Error classification rate:    Table containing the error classification error    
# Wavelengths:  Contains the wavelengths ordered by their absolute weights in the 1st
#             linear discriminant function.


Models_Spectra <- function(X, ColClass, test, testl, models, 
                           components, contamination = T,
                           initialization = "random.post",
                           parallel = "F")
{
  # train include all the covariates
  n.models <- length(models)
  m <- 2*n.models + 2
  res <- array(NA,dim = c(m,2,2))
  rowlegends <- c("LDA","QDA",
                  paste("MCLUST-",models, colapse = NULL, sep = ""),
                  paste("CMN-",models, colapse = NULL, sep = "")  )
  
  dimnames(res) <- list(rowlegends,c("train","test"),
                           c("error rate","Correct classification rate"))
  
  


  Xtrain <- X %>% select_if(is.numeric) 
  index_class <-  match(ColClass,colnames(X))
  trainl <- X[,index_class]  
  qda.mod2 <- fit_QDA(X, ColClass, test,testl)
  lda.mod2 <- fit_LDA(X, ColClass, test,testl)
  res[1,,1] <- lda.mod2$error.rate
  res[1,,2] <- lda.mod2$corr.class.rate
  res[2,,1] <- qda.mod2$error.rate
  res[2,,2] <- qda.mod2$corr.class.rate
  
  for(model in 1:n.models)
  {
    
    EDDA.mod2 <- fit_EDDA(Xtrain,trainl,
                          test,testl,
                          models[model],components = 2)
    res[2+model,,1] <- lda.mod2$error.rate
    res[2+model,,2] <- lda.mod2$corr.class.rate
  }  
  aux <- 1
   for(model1 in 1:n.models) 
  {
    v1 <- tryCatch({
      cat("\n", "model = ",models[model1], model+model1,res[model+model1,,])
      
    CMN.mod2 <- fit_CMN(Xtrain,trainl,
                          test,testl,
                          component = 2, models = models[model1],
                          contamination = contamination, 
                          initialization = initialization,
                          parallel = parallel,
                          levelgood = 0.9)
    res[2+model + model1,,1] <- t(CMN.mod2[[1]]$error.rate)
    res[2+model + model1,,2] <- t(CMN.mod2[[1]]$corr.class.rate)
    }, error = function(err)
    {
      cat("\n",model + model1)
      print(paste("Error: ", err))
    })
    
  }
  return(res)  
}

