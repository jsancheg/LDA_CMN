library(ContaminatedMixt)
pathWd <-"/home/jsancheg/git_environment/LDA_CMN/"
setwd(pathWd)

dir(pathWd)
source("CMNFunctionsV2.R")

pathOutput <- "/home/jsancheg/git_environment/LDA_CMN/Scenarios/"

dir(pathOutput)

fileRDS<- readRDS(paste0(pathOutput,"S_2_2_5_3000_75_BAL_SCBSV_VD_A8080_E2020_10.RDS"))
GenData <-fileRDS$GenData[[1]]
par <- fileRDS$par


G = length(unique(GenData$l))
Xtrain <- GenData$Xtrain
Xtest <- GenData$Xtest
ltrain <- GenData$ltrain
ltest <- GenData$ltest
vtest <- GenData$vtest


# ContaminatedMixt::m step

estimates <- ContaminatedMixt::m.step(Xtrain,modelname = "EEI",z = unmap(ltrain), mmax = 10)
estimates$mu
estimates$Sigma


# predict using the function fitCMN from ContaminatedMixt packages
# supervised version
fitCMN <- CNmixt(Xtrain,G,contamination = TRUE,model = "VVV", 
                 initialization = "mixt",label = ltrain, 
                 iter.max = 10)

fitCMN$models[[1]]$mu
fitCMN$models[[1]]$Sigma
fitCMN$models[[1]]$alpha
fitCMN$models[[1]]$eta

fitCMN$models[[1]]$label
fitCMN$models[[1]]$v


# supervised version self coded
fitCMN_SelfCoded <- ModelAccuracy2(Xtrain,Xtest,ltrain,ltest,"VVV",niterations = 10)

fitCMN_SelfCoded$mu[[10]]
fitCMN_SelfCoded$sigma[[10]]
fitCMN_SelfCoded$alpha[[10]]
fitCMN_SelfCoded$eta[[10]]

fitCMN_SelfCoded$
fitCMN_SelfCoded$v[[10]]

predCMN <- CNpredict(newdata = Xtest, 
                     prior = fitCMN$models[[1]]$prior,
                     mu = fitCMN$models[[1]]$mu,
                     invSigma = fitCMN$models[[1]]$invSigma,
                     alpha = NULL,
                     eta = NULL)


n <- nrow(Xtest)
p <- ncol(Xtest)
prior <-fitCMN$models[[1]]$prior
mu = fitCMN$models[[1]]$mu
invSigma = fitCMN$models[[1]]$invSigma
alpha = array(c(rep(fitCMN$models[[1]]$alpha,each = p*p)), dim = c(p,p,G))
eta = matrix(c(rep(fitCMN$models[[1]]$eta,p)), nrow = p,ncol = G,byrow = TRUE)


predCMN <- CNpredict(newdata = Xtest, 
                     prior = prior,
                     mu = mu,
                     invSigma = invSigma,
                     alpha = alpha,
                     eta = eta)

length(eta) == c(p,G)


dim(fitCMN$models[[1]]$mu) == c(p,G)
dim(fitCMN$models[[1]]$invSigma)==c(p,p,G)
length(fitCMN$models[[1]]$prior)
  
if (!all(dim(mu)==c(p,G), dim(invSigma)==c(p,p,G),length(prior)==c(G))) stop("Error in the dimensions of arguments.")

length(fitCMN$models[[1]]$model)
                                          