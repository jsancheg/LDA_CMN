source("Semisupervised.R")
source("ListScenarios.R")

source("GSFile.R")

dir(pathSFiles)

#fileRDS <- readRDS(paste0(pathSFiles,"SV_2_2_5_3000_75_INB_SCBSV_VD_A8090_E530_10.RDS"))

#fileRDS <- readRDS(paste0(pathSFiles,"SV_2_2_5_3000_75_BAL_SCBSV_VD_A8090_E530_10.RDS"))

fileRDS <- readRDS(paste0(pathSFiles,"SV_2_2_5_3000_75_BAL_SCBNSV_VD_A8090_E530_10.RDS"))


Model <- c("EII","VII","EEI","VEI","EEE","VVV")


GenerateSFile(file_name = "S_2_2_5_3000_75_BAL_SCBNSV_VD_A8090_E530_10.RDS", pathScenarios = pathScenarios,
              pathOutput = pathSFiles, Model = Model)


fileRDS <- readRDS(paste0(pathSFiles,"SV_2_2_5_3000_75_BAL_SCBNSV_VD_A8090_E530_10.RDS"))

View(fileRDS$Metrics)


models <- fileRDS$Estimates[[9]]$models

fileRDS$Estimates[[9]]$models[[1]]$CCRTestC

models[[1]]$CCRTestC

PMs <- lapply(models, function(m) m$PM)
PMs

CCRs <- lapply(models,function(m) m$CCRTestC)
CCRs

best_index <- which.max(unlist(CCRs))

best_index

PMs[best_index]

nmodels

i <- 1

for(i in 1:length(models) )
{
  cat("\n Model: ", models[[i]]$PM, "- Covariance Structure: ", models[[i]]$fitted_C_model, 
      "CCR Class: ",models[[i]]$CCRTestC )
  
}

mean(fileRDS$Metrics$CCR_SM[i] )

i <- 9

model_metric <- fileRDS$Metrics
i <- 9

# Metrics for label prediction
label_prediction <- cbind(model_metric$Nsim[i], model_metric$CCR_TM[i], model_metric$Recall_TM[i], model_metric$Specificity_TM[i],
                            model_metric$CCR_SM[i], model_metric$Recall_SM[i], model_metric$Specificity_SM[i],
                          model_metric$CCR_SaturatedM[i], model_metric$Recall_SaturatedM[i], model_metric$Specificity_SaturatedM[i])


titles <- c( "Simulation","CCR_True","Semsitivity_True","Specificity_True","CCR_Selected","Sensitivity_Selected","Specificity_Selected",
             "CCR_All","Semsitivity_All","Specificity_All")
colnames(label_prediction) <- titles
label_prediction

# Metrics for contamination prediction
label_contamination <- cbind(model_metric$Nsim[i], model_metric$CCRCont_TM[i], model_metric$RecallV_TM[i], model_metric$SpecificityV_TM[i],
                  model_metric$CCRCont_SM[i], model_metric$RecallV_SM[i], model_metric$Specificity_SM[i],#
                  model_metric$CCRCont_SaturatedM[i], model_metric$RecallV_SaturatedM[i], model_metric$SpecificityV_SaturatedM[i])

colnames(label_contamination) <- titles

performance_metrics <- round(rbind(label_prediction,label_contamination),2)

rownames(performance_metrics) <- c("Class","Contamination")

performance_metrics


xtest_scenario1 <- fileRDS$GenData[[9]]$Xtest
ltest_scenario1 <- fileRDS$GenData[[9]]$ltest
vtest_scenario1 <- fileRDS$GenData[[9]]$vtest

vpred_scenario1 <- fileRDS$Estimates[[9]]$vTestHat_SM
lpred_scenario1 <- fileRDS$Estimates[[9]]$lTestHat_SM

table(vtest_scenario1,vpred_scenario1)

cond1_test <- paste0(ltest_scenario1,lpred_scenario1)
table(cond1_test)

# 19  fillet black dot denotes a TN ( correct prediction of uncontaminated observation for either 1st or 2nd class)
# 17  filled black triangle  denotes a TP (correct prediction of contaminated observation for either 1st or 2nd class)
#  3  + denotes a FP (wrongly predicted as contaminated observation when it was uncontaminated observation belonging to the 1st class)  
#  4  x  denotes a FN (wrongly predicted as uncontaminated observation when it was contaminated observation belonging to the 1st class)
#  1  circle  denotes a FP (wrongly predicted as contaminated observation when it was uncontaminated observation belonging to the 2nd class)
#  8  * star  denotes a FN (wrongly predicted as uncontaminated observation when it was contaminated observation belonging to the 2nd class) 
pairs(Xtest[,c(2,4,5)], panel = function(x,y, ...) {
  points(x,y, 
         col = ifelse(cond1_test == "11" ,"lightblue",
                      ifelse(cond1_test == "22" ,"lightgreen",
                             ifelse(cond1_test=="12"  ,"orange","orange"))),
         pch = ifelse(cond1_test == "11" ,19,
                      ifelse(cond1_test == "22",17,
                             ifelse(cond1_test=="12" ,3,4))),
         cex = 1,
  )
  #           text(x[indBreal_T0_P1_Testv],y[indBreal_T0_P1_Testv],
  #           labels=c(indBreal_T0_P1_Testv),pos = 4)
})






cond2_test <- paste0(ltest,vtest_scenario1,vpred_scenario1)
# 1st class colour in lightblue
# 2nd class colour in lightgreen


# 19  fillet black dot denotes a TN ( correct prediction of uncontaminated observation for either 1st or 2nd class)
# 17  filled black triangle  denotes a TP (correct prediction of contaminated observation for either 1st or 2nd class)
#  3  + denotes a FP (wrongly predicted as contaminated observation when it was uncontaminated observation belonging to the 1st class)  
#  4  x  denotes a FN (wrongly predicted as uncontaminated observation when it was contaminated observation belonging to the 1st class)
#  1  circle  denotes a FP (wrongly predicted as contaminated observation when it was uncontaminated observation belonging to the 2nd class)
#  8  * star  denotes a FN (wrongly predicted as uncontaminated observation when it was contaminated observation belonging to the 2nd class) 
pairs(Xtest, panel = function(x,y, ...) {
  points(x,y, 
         col = ifelse(cond2_test == "111" ,"lightblue",
                      ifelse(cond2_test == "100" ,"lightblue",
                             ifelse(cond2_test=="110"  ,"brown",
                                    ifelse(cond2_test == "101" ,"brown",
                                           ifelse(cond2_test =="211", "lightgreen",
                                                  ifelse(cond2_test == "200", "lightgreen",
                                                         ifelse(cond2_test == "210","purple", "purple") )       )       )    )) ),
         pch = ifelse(cond2_test == "111" | cond2_test == "211",19,
                      ifelse(cond2_test == "100" | cond2_test =="200",17,
                             ifelse(cond2_test=="110" ,3,
                                    ifelse(cond2_test == "101", 4,
                                           ifelse(cond2_test == "210",1,8)))) ),
         cex = 1,
  )
  #           text(x[indBreal_T0_P1_Testv],y[indBreal_T0_P1_Testv],
  #           labels=c(indBreal_T0_P1_Testv),pos = 4)
})

Xtrain <- fileRDS$GenData[[9]]$Xtrain
Xtest <- fileRDS$GenData[[9]]$Xtest
ltrain <- fileRDS$GenData[[9]]$ltrain
ltest <- fileRDS$GenData[[9]]$ltest
vtrain <- fileRDS$GenData[[9]]$vtrain
vtest <- fileRDS$GenData[[9]]$vtest


mod1_class <- GreedySearch(Xtrain[ltrain == 2,],Xtest[ltest == 2,],ltrain[ltrain == 2], ltest[ ltest == 2],
                           vtrain[ltrain == 2], vtest[ltrain == 2], CE = Model, pnolabeled = 0, iterations - 10)
