library(readxl)
library(skimr)
library(rgl)
library(stringi)
library(stringr)
library(fda)
library(chemometrics)
library(scatterplot3d)
library(RandPro)
library(VGAM)
library(scales)
library(pls) # partial least square
#library(mixOmics) # partial least square - discriminant analysis
library(ROCR)
library(plyr)
library(tictoc)
library(pROC)
library(caret)
library(e1071) # SVM
library(class) # k nearest neighborhood
library(gmodels) # CrossTable
library(ggplot2)
library(gridExtra) # combine plots
library(ggthemes)
library(ggfortify)
library(tidyverse) # rbernoulli function
library(fastDummies)
library(factoextra) # PCA , Factor analysis
library(mdatools) # SIMCA
library(nnet) # Multinomial regression
library(glmnet) # Multinomial regression with regularization
library(DIRECT)# Multinormal distribution
#work_path <- "E:/University of Glasgow/Second Year/Literature review/R Code/"
library(nlme) # Correlation structure
library(MixSim) # Simulating multinomial distribution
library(MASS)   # Linear Mixed Models
library(lme4)   # Linear Mixed Models
library(gee)    # Linear Mixed Models Generalised estimating equations  rMVNorm
library(forecast) # time series function autoarima
library(tseries) # time series
library(dplyr)
library(MBESS) # cor2cov
library(ContaminatedMixt)# Mixture of contaminated normal distributions
library(mnormt)
library(clustvarsel) # Mixture of normal distributions
library(parallel)
library(parallelly)
library(doParallel)
library(BMA) # function bicreg
library(mclust)
library(MLmetrics) # calculate metrrics accuracy, precision, recall, F1 score
rm(list = ls())

WorkPath <- getwd()
source("functions.R")
source("load_datasets.r")

