getwd()
source("ListScenarios.R")
list_files_to_process <- dir(pathSFiles_Old)

n <- length(list_files_to_process)
n

x <- list_files_to_process[1]



fileRDS <- readRDS(paste0(pathSFiles_Old,x))
i <- 1

true_test_l_class <- fileRDS$GenData[[i]]$ltest
true_test_v_cont <- fileRDS$GenData[[i]]$vtest
pred_test_class_tm <- fileRDS$Estimates[[i]]$lTestHat_TM
pred_test_v_tm <- fileRDS$Estimates[[i]]$vTestHat_TM
pred_test_class_sm <-fileRDS$Estimates[[i]]$lTestHat_SM
pred_test_v_sm <- fileRDS$Estimates[[i]]$vTestHat_SM
pred_test_class_all <-fileRDS$Estimates[[i]]$lTestHat_SaturatedM
pred_test_v_all <- fileRDS$Estimates[[i]]$vTestHat_SaturatedM

cond_v_tm <- paste0(true_test_v_cont, pred_test_v_tm)
cond_v_sm <- paste0(true_test_v_cont, pred_test_v_sm)
cond_v_all <- paste0(true_test_v_cont,pred_test_v_all)


cond_class_tm <- paste0(true_test_l_class,pred_test_class_tm)   
cond_class_sm <- paste0(true_test_l_class,pred_test_class_sm)
cond_class_all <- paste0(true_test_l_class,pred_test_class_all)

