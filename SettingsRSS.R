getwd()
setwd("E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN")
source("SimClassEM20Steps.R")
nruns = 10
selectedvariables <- list()


# Dataset A.5 (contaminated) ----------------------------------------------
mu1 <- rep(0,100)
mu2 <- c(0,6,0,6,rep(0,96))
mu <- cbind(mu1,mu2)
sg <- diag(1,100)
pig<- c(0.5,0.5)
nobservations = 320
ptraining = 0.75
alphag <-c(0.9,0.8)
etag <- c(20,30)
alpharef <- 0.99
tol <- 0.01
set.seed(123)

GenDataA.5 <- SimGClasses(mu,sg,pig,nobservations,ptraining,alphag,etag)

mod <- ModelAccuracy2(GenDataA.5$Xtrain,GenDataA.5$Xtest,
                      GenDataA.5$ltrain,GenDataA.5$ltest,"E",
                      alpharef, tol)

dfRW <- getOW(GenDataA.5$Xtrain,GenDataA.5$ltrain)
RW <- dfRW$Var


mod_search <- fHLvarSearch2(GenDataA.5$Xtrain, GenDataA.5$Xtest, RW,
                            GenDataA.5$ltrain, GenDataA.5$ltest, "E", 
                            alpharef =0.99,tol=0.01,epsilon = 0)

variables_True_model <- c("X2","X4")


pos_True_model <- findPosModel(mod_search$models,variables_True_model)  
TrueModel <- mod_search$models[[pos_True_model]]

mod_search$first20AccCM
mod_search$first20EMclassprediction

TrueModel$lpredlabel

# Filter contaminated and non-contaminated samples
G <- length(unique(GenDataA.5$ltrain))

lind_nocont_class <- list()
lind_cont_class <- list()
for(g in 1:G)
  {
  # non contaminated samples in classes
    lind_nocont_class[[g]] <- which(GenDataA.5$vtest[,g]!=0 & GenDataA.5$vtest[,g]!=-1)
  # contaminated samples in classes
    lind_cont_class[[g]] <- which(GenDataA.5$vtest[,g]==0 & GenDataA.5$vtest[,g]!=-1)
  }

ind_nocont_class <- unlist(lind_nocont_class)
ind_cont_class <- unlist(lind_cont_class)

Nsteps <- length(mod_search$first20AccCM)
Accuracy_non_cont_samples <- rep(0,Nsteps)
Accuracy_cont_samples <- rep(0,Nsteps)

i_step<-1
for(i_step in 1:Nsteps)
{
  #selected model
 predselected_Model_noncont_samples <- mod$lpredlabel[[i_step]][ind_nocont_class]
 predselected_Mode_cont_samples <- mod$lpredlabel[[i_step]][ind_cont_class]     
 
 predTrueModel_nocont_samples <- TrueModel$lpredlabel[[i_step]][ind_nocont_class]
 predTrueModel_cont_samples <- TrueModel$lpredlabel[[i_step]][ind_cont_class]

}

# predicted class for non contaminated samples for selected model
pred_nocont_samples <- c(mod$models[[pos]]$predlabel[ind_nocont_class1],
                         mod$models[[pos]]$predlabel[ind_nocont_class2])

pred_cont_samples <- c(mod$models[[pos]]$predlabel[ind_cont_class1],
                       mod$models[[pos]]$predlabel[ind_cont_class2])

# predicted class for non contaminated samples for the true model
pred_TrueModel_nocont_samples <- c(mod$models[[pos_True_model]]$predlabel[ind_nocont_class1],
                                   mod$models[[pos_True_model]]$predlabel[ind_nocont_class2])

pred_TrueModel_cont_samples <- c(mod$models[[pos_True_model]]$predlabel[ind_cont_class1],
                                 mod$models[[pos_True_model]]$predlabel[ind_cont_class2])



TrueModel$laccTest_c
TrueModel$lpredlabel





mod_search$models[[5]]$PM
mod_search$models[[5]]$laccTest_c[[]]
mod_search$models[[5]]$accTestC


mod_search$models[[10]]$accTestNc
mod_search$models[[10]]$accTestC


mod_search$models[[15]]$accTestNc
mod_search$models[[15]]$accTestC


mod_search$models[[20]]$accTestNc
mod_search$models[[20]]$accTestC


mod$laccTest_c

GenDataA.5$vtrain

GenDataA.5$vtrain

plot(GenDataD.1$Xtrain, col = GenDataD.1$vtrain+2, 
     pch = 15+GenDataD.1$ltrain,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Class A","Contaminated class A"), 
       col = c("green","red"),
       pch = c(16,16))

