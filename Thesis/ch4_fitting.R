library(stringr)
source("Semisupervised.R")
source("Semisupervised_Vectorize.R")
source("SimulateScenario.R")
source("GSSFile.R")
source("ListScenarios.R")

ssmetrics_21_03_2024 <- readRDS("SSMetrics_2024_03_21.RDS")

ssmetrics <- ssmetrics_21_03_2024
colnames(ssmetrics)

head(ssmetrics$File)
options(scipen = 999)

missing_values <- colSums(is.na(ssmetrics))
missing_values
total_rows <- nrow(ssmetrics)/3
total_rows

ssdf <-ssmetrics

# select files of interest

ssdf <- ssdf %>% mutate(Precision_Cont = Precicison_Cont)


Files_for_SS_Scenario <- ssdf %>%filter(Training_Proportion=="75",Class_Proportion=="BAL",Number_Separating_Variables==3,Number_Classes == 2,
               Number_Variables == 5) 

Files_for_SS_Scenario
length(unique(Files_for_SS_Scenario$File))

x <- unlist(Files_for_SS_Scenario$File)[1]

x

parameters_for_simulation <- get_factors_from_file_name(x)

class(parameters_for_simulation)
as.matrix(parameters_for_simulation)

SimScenario(as.matrix(parameters_for_simulation),10,pathScenarios)

nFiles <- length(unique(Files_for_SS_Scenario$File))

sapply(2:nFiles, function(i) {
        length_name <-str_length(unlist(Files_for_SS_Scenario$File)[i]) 
        
        aux_x <- substr(Files_for_SS_Scenario$File[i],4,length_name)

        x <- paste0("S",aux_x)
        parameters_for_simulation <- get_factors_from_file_name(x)
        
        SimScenario(as.matrix(parameters_for_simulation),10,pathScenarios)
})

unlabelled <- rep(1:5)/10
unlabelled 

# semi-supervised with 10%, 20%, 30%, 40% of labelled data

for(i in 0.6:0.9)
{
  sapply(str_replace(unique(Files_for_SS_Scenario$File),"SSV_","S_"),function(x) {
    
    Model <- c("EII","VII","EEI","VEI","EEE","VVV")
    
    if(! length(intersect(x,dir(pathSSFiles) ) ) >0 )
      GenerateSSFile_vectorize(file_name = x,pathScenarios = pathScenarios,pathOutput = pathSSFiles,Model = Model,
                               Search = "GS",pnolabeled = i)
    
  })
  
  
}



# supervised
sapply(str_replace(unique(Files_for_SS_Scenario$File),"SSV_","S_"),function(x) {
  
  Model <- c("EII","VII","EEI","VEI","EEE","VVV")
  
  if(! length(intersect(x,dir(pathSSFiles) ) ) >0 )
    GenerateSSFile_vectorize(file_name = x,pathScenarios = pathScenarios,pathOutput = pathSSFiles,Model = Model,
                             Search = "GS",pnolabeled = 0)
  
})


x <- unlist(Files_for_SS_Scenario$File)[2]


Model <- c("EII","VII","EEI","VEI","EEE","VVV")


dir(pathSSFiles)


pseudo_label_df <- data.frame( File = character(), no_labelled_data = numeric(),
                               CCR_class_tm = numeric(), sensitivity_class_tm = numeric(),
                               specificity_class_tm = numeric(), precision_class_tm = numeric(),
                               f1_class_tm = numeric(), CCR_cont_tm = numeric(),
                               sensitivity_cont_tm = numeric(), specificity_cont_tm = numeric(),
                               precision_cont_tm = numeric(), f1_cont_tm = numeric(),
                               CCR_class_sm = numeric(), sensitivity_class_sm = numeric(),
                               specificity_class_sm = numeric(), precision_class_sm = numeric(),
                               f1_class_sm = numeric(), CCR_cont_sm = numeric(),
                               sensitivity_cont_sm = numeric(), specificity_cont_sm = numeric(),
                               precision_cont_sm = numeric(), f1_cont_sm = numeric(),
                               CCR_class_all = numeric(), sensitivity_class_all = numeric(),
                               specificity_class_all = numeric(), precision_class_all = numeric(),
                               f1_class_all = numeric(), CCR_cont_all = numeric(),
                               sensitivity_cont_all = numeric(), specificity_cont_all = numeric(),
                               precision_cont_all = numeric(), f1_cont_all = numeric()
                               
                               )


file_name <- "SSV_2_3_5_3000_75_BAL_SCBSNSV_VD_A8090_E530_10_60.RDS"
list_files <- dir(pathSSFiles)

i <- 1
cont_sim <- 1
ifile<-1

 for(file_name in list_files )
 {
   
   ss_file <- readRDS(paste0(pathSSFiles,file_name))
   parameters <- str_split(file_name, "_", simplify = TRUE)  
   parameters
   
   number_of_classes <- as.numeric(parameters[2])
   number_of_classes
   
   nsim <- length(ss_file$Estimates)
   
   
   for (i in 1:nsim)
   {
     # True Model
     ind_pseudo_label_tm_train <- ss_file$Estimates[[i]]$PseudoInformation_TM$Unlabelled_index
     pred_class_labels_tm_train <- ss_file$Estimates[[i]]$PseudoInformation_TM$Pseudo_labels_Class_Train
     pred_cont_labels_tm_train <- ss_file$Estimates[[i]]$PseudoInformation_TM$Pseudo_labels_V_Train
     no_labelled_percentage_tm_train <- ss_file$Estimates[[i]]$PseudoInformation_TM$Percentage_of_no_labeled
     true_class_labels_tm_train <- ss_file$Estimates[[i]]$PseudoInformation_TM$True_labels_Class_Train    
     true_cont_labels_tm_train <- ss_file$GenData[[i]]$vtrain[ind_pseudo_label_train]
     
     
     # Selected Model
     ind_pseudo_label_train <- ss_file$Estimates[[i]]$PseudoInformation_SM$Unlabelled_index
     pred_class_labels_train <- ss_file$Estimates[[i]]$PseudoInformation_SM$Pseudo_labels_Class_Train
     pred_cont_labels_train <- ss_file$Estimates[[i]]$PseudoInformation_SM$Pseudo_labels_V_Train
     no_labelled_percentage_train <- ss_file$Estimates[[i]]$PseudoInformation_SM$Percentage_of_no_labeled
     true_class_labels_train <- ss_file$Estimates[[i]]$PseudoInformation_SM$True_labels_Class_Train    
     true_cont_labels_train <- ss_file$GenData[[i]]$vtrain[ind_pseudo_label_train]
     
     # Saturated model including all variables
     ind_pseudo_label_all_train <- ss_file$Estimates[[i]]$PseudoInformation_SaturatedM$Unlabelled_index
     pred_class_labels_all_train <- ss_file$Estimates[[i]]$PseudoInformation_SaturatedM$True_labels_Class_Train
     pred_cont_labels_all_train <- ss_file$Estimates[[i]]$PseudoInformation_SaturatedM$Pseudo_labels_V_Train
     no_labelled_percentage_all_train <- ss_file$Estimates[[i]]$PseudoInformation_SaturatedM$Percentage_of_no_labeled
     true_class_labels_all_train <- ss_file$Estimates[[i]]$PseudoInformation_SaturatedM$True_labels_Class_Train    
     true_cont_labels_all_train <- ss_file$GenData[[i]]$vtrain[ind_pseudo_label_train]
     
     length(ind_pseudo_label_all_train)
     length(pred_class_labels_all_train)
     length(pred_cont_labels_all_train)
     length(true_class_labels_all_train)
     length(true_cont_labels_all_train)  
     
     table(true_cont_labels_train,pred_cont_labels_train)
     
     
     
     
     
     # True Model metrics ------------------------------------------------------
     
     # Class metrics
     
     ccr_class_tm <- MLmetrics::Accuracy(pred_class_labels_tm_train,true_class_labels_tm_train)
     sensitivity_class_1_tm <- MLmetrics::Sensitivity(true_class_labels_tm_train,pred_class_labels_tm_train,1)
     sensitivity_class_2_tm <- MLmetrics::Sensitivity(true_class_labels_tm_train,pred_class_labels_tm_train,2)
     sensitivity_class_tm <- mean(sensitivity_class_1_tm,sensitivity_class_2_tm)
     if(number_of_classes == 3)  sensitivity_class_3_tm <- MLmetrics::Sensitivity(true_class_labels_tm_train,pred_class_labels_tm_train,3)
     if(number_of_classes == 3) sensitivity_class_tm <- mean(sensitivity_class_1_tm,sensitivity_class_2_tm,sensitivity_class_3_tm)
     
     specificity_class_1_tm <- MLmetrics::Specificity(true_class_labels_tm_train,pred_class_labels_tm_train,1)
     specificity_class_2_tm <- MLmetrics::Specificity(true_class_labels_tm_train,pred_class_labels_tm_train,2)
     specificity_class_tm <- mean(specificity_class_1_tm, specificity_class_2_tm)
     if(number_of_classes == 3) specificity_class_3_tm <- MLmetrics::Specificity(true_class_labels_tm_train,pred_class_labels_tm_train,3)
     if(number_of_classes == 3) specificity_class_tm <- mean(specificity_class_1_tm,specificity_class_2_tm,sensitivity_class_3_tm)
     
     precision_class_1_tm <- MLmetrics::Precision(true_class_labels_tm_train, pred_class_labels_tm_train,1)
     precision_class_2_tm <- MLmetrics::Precision(true_class_labels_tm_train, pred_class_labels_tm_train,2)
     precision_class_tm <- mean(precision_class_1_tm, precision_class_2_tm)
     if(number_of_classes == 3) precision_class_3_tm <- MLmetrics::Precision(true_class_labels_tm_ltrain,pred_class_labels_tm_train,3)
     if(number_of_classes == 3) precision_class_tm <- mean(precision_class_1_tm, precision_class_2_tm,precision_class_3_tm)
     
     f1_class_1_tm <- MLmetrics::F1_Score(true_class_labels_tm_train, pred_class_labels_tm_train,1)
     f1_class_2_tm <- MLmetrics::F1_Score(true_class_labels_tm_train, pred_class_labels_tm_train,2)
     f1_class_tm <- mean(f1_class_1_tm,f1_class_2_tm)
     if(number_of_classes == 3) f1_class_3_tm <- MLmetrics::F1_Score(true_class_labels_tm_train, pred_class_labels_tm_train,3)
     if(number_of_classes == 3) f1_class_tm <- mean(f1_class_1_tm,f1_class_2_tm,f1_class_3_tm)
     
     # Contaminated metrics
     
     ccr_cont_tm <- MLmetrics::Accuracy(pred_cont_labels_tm_train,true_cont_labels_tm_train)
     sensitivity_cont_tm <- MLmetrics::Sensitivity(true_cont_labels_tm_train,pred_cont_labels_tm_train,0)
     specificity_cont_tm <- MLmetrics::Specificity(true_cont_labels_tm_train,pred_cont_labels_tm_train,0)
     precision_cont_tm <- MLmetrics::Precision(true_cont_labels_tm_train,pred_cont_labels_tm_train,0)
     f1_score_cont_tm <- MLmetrics::F1_Score(true_cont_labels_tm_train,pred_cont_labels_tm_train,0)
     
     
     # Selected Model ----------------------------------------------------------
     # Class metrics
     
     ccr_class_sm <- MLmetrics::Accuracy(pred_class_labels_train,true_class_labels_train)
     sensitivity_class_1_sm <- MLmetrics::Sensitivity(true_class_labels_train,pred_class_labels_train,1)
     sensitivity_class_2_sm <- MLmetrics::Sensitivity(true_class_labels_train,pred_class_labels_train,2)
     sensitivity_class_sm <- mean(sensitivity_class_1_sm, sensitivity_class_2_sm)
     if(number_of_classes == 3)  sensitivity_class_3_sm <- MLmetrics::Sensitivity(true_class_labels_train,pred_class_labels_train,3)
     if(number_of_classes == 3)   sensitivity_class <- mean(sensitivity_class_1_sm, sensitivity_class_2_sm, sensitivity_class_3_sm)
     
     
     specificity_class_1_sm <- MLmetrics::Specificity(true_class_labels,pred_class_labels_train,1)
     specificity_class_2_sm <- MLmetrics::Specificity(true_class_labels,pred_class_labels_train,2)
     specificity_class_sm <- mean(specificity_class_1_sm, specificity_class_2_sm)
     if(number_of_classes == 3) specificity_class_3_sm <- MLmetrics::Specificity(true_class_labels_train,pred_class_labels_train,3)
     if(number_of_classes == 3)   specificity_class <- mean(specificity_class_1_sm, specificity_class_2_sm, specificity_class_3_sm)
     
     
     precision_class_1_sm <- MLmetrics::Precision(true_class_labels_train, pred_class_labels_train,1)
     precision_class_2_sm <- MLmetrics::Precision(true_class_labels_train, pred_class_labels_train,2)
     precision_class_sm <- mean(precision_class_1_sm, precision_class_2_sm)
     if(number_of_classes == 3) precision_class_3_sm <- MLmetrics::Precision(true_class_labels_ltrain,pred_class_labels_train,3)
     if(number_of_classes == 3)   precision_class_sm <- mean(precision_class_1_sm, precision_class_2_sm, precision_class_3_sm)
     
     
     f1_class_1_sm <- MLmetrics::F1_Score(true_class_labels_train, pred_class_labels_train,1)
     f1_class_2_sm <- MLmetrics::F1_Score(true_class_labels_train, pred_class_labels_train,2)
     f1_class_sm <- mean(f1_class_1_sm, f1_class_2_sm)
     if(number_of_classes == 3) f1_class_3_sm <- MLmetrics::F1_Score(true_class_labels_train, pred_class_labels_train,3)
     if(number_of_classes == 3)   f1_class_sm <- mean(f1_class_1_sm, f1_class_2_sm, f1_class_3_sm)
     
     
     
     # Contaminated metrics
     
     ccr_cont_sm <- MLmetrics::Accuracy(pred_cont_labels_train,true_cont_labels_train)
     sensitivity_cont_sm <- MLmetrics::Sensitivity(true_cont_labels_train,pred_cont_labels_train,0)
     specificity_cont_sm <- MLmetrics::Specificity(true_cont_labels_train,pred_cont_labels_train,0)
     precision_cont_sm <- MLmetrics::Precision(true_cont_labels_train,pred_cont_labels_train,0)
     f1_score_cont_sm <- MLmetrics::F1_Score(true_cont_labels_train,pred_cont_labels_train,0)
     
     
     
     # Saturated model metrics -------------------------------------------------
     
     
     # Class metrics
     
#     ccr_class_all <- MLmetrics::Accuracy(pred_class_labels_all_train,true_class_labels_all_train)
#     sensitivity_class_1_all <- MLmetrics::Sensitivity(true_class_labels_all_train,pred_class_labels_all_train,1)
#     sensitivity_class_2_all <- MLmetrics::Sensitivity(true_class_labels_all_train,pred_class_labels_all_train,2)
#     sensitivity_class_all <- mean(sensitivity_class_1_all, sensitivity_class_2_all)
#     if(number_of_classes == 3)  sensitivity_class_3_all <- MLmetrics::Sensitivity(true_class_labels_all_train,pred_class_labels_all_train,3)
#     if(number_of_classes == 3)  sensitivity_class_all <- mean(sensitivity_class_1_all, sensitivity_class_2_all, sensitivity_class_3_all)
     
     
 #    specificity_class_1_all <- MLmetrics::Specificity(true_class_labels_all_train,pred_class_labels_all_train,1)
#     specificity_class_2_all <- MLmetrics::Specificity(true_class_labels_all_train,pred_class_labels_all_train,2)
#     specificity_class <- mean(specificity_class_1_all, specificity_class_2_all)
#     if(number_of_classes == 3) specificity_class_3_all <- MLmetrics::Specificity(true_class_labels_all_train,pred_class_labels_all_train,3)
#     if(number_of_classes == 3)  specificity_class <- mean(specificity_class_1_all, specificity_class_2_all, specificity_3_all)
     
#     precision_class_1_all <- MLmetrics::Precision(true_class_labels_all_train, pred_class_labels_all_train,1)
#     precision_class_2_all <- MLmetrics::Precision(true_class_labels_all_train, pred_class_labels_all_train,2)
#     precision_class <- mean(precision_class_1_all, precision_class_2_all)
#     if(number_of_classes == 3) precision_class_3_all <- MLmetrics::Precision(true_class_labels_all_ltrain,pred_class_labels_all_train,3)
#     if(number_of_classes == 3)  precision_class <- mean(precision_class_1_all, precision_class_2_all, precision_class_3_all)
     
#     f1_class_1_all <- MLmetrics::F1_Score(true_class_labels_all_train, pred_class_labels_all_train,1)
#     f1_class_2_all <- MLmetrics::F1_Score(true_class_labels_all_train, pred_class_labels_all_train,2)
#     f1_class_all <- mean(f1_class_1_all, f1_class_2_all)
#     if(number_of_classes == 3) f1_class_3_all <- MLmetrics::F1_Score(true_class_labels_all_train, pred_class_labels_all_train,3)
#     if(number_of_classes == 3)   f1_class_all <- mean(f1_class_1_all, f1_class_2_all, f1_class_3_all)
     
     
     # Contaminated metrics
     
     #ccr_cont_all <- MLmetrics::Accuracy(pred_cont_labels_all_train,true_cont_labels_all_train)
     #sensitivity_cont_all <- MLmetrics::Sensitivity(true_cont_labels_all_train,pred_cont_labels_all_train,0)
     #specificity_cont_all <- MLmetrics::Specificity(true_cont_labels_all_train,pred_cont_labels_all_train,0)
     #precision_cont_all <- MLmetrics::Precision(true_cont_labels_all_train,pred_cont_labels_all_train,0)
     #f1_score_cont_all <- MLmetrics::F1_Score(true_cont_labels_all_train,pred_cont_labels_all_train,0)
     
     
     #length(ss_file$GenData[[1]]$ltrain)
     
     #length(pred_cont_labels_all_train)
     
     #length(true_cont_labels_all_train)
     #length(true_class_labels)
     
     
     
     # save performance metrics ------------------------------------------------
     colnames(pseudo_label_df)
     pseudo_label_df[cont_sim,"File"] <- file_name
     pseudo_label_df[cont_sim,"no_labelled_data"] <- ss_file$Estimates[[i]]$PseudoInformation_SM$Percentage_of_no_labeled
     
     # true model
     pseudo_label_df[cont_sim,"CCR_class_tm"] <- ccr_class_tm
     pseudo_label_df[cont_sim,"sensitivity_class_tm"] <- sensitivity_class_tm
     pseudo_label_df[cont_sim,"specificity_class_tm"] <- specificity_class_tm
     pseudo_label_df[cont_sim,"precision_class_tm"] <- precision_class_tm
     pseudo_label_df[cont_sim,"f1_class_tm"] <- f1_class_tm
     pseudo_label_df[cont_sim,"CCR_cont_tm"] <- ccr_cont_tm
     pseudo_label_df[cont_sim,"sensitivity_cont_tm"] <- sensitivity_cont_tm
     pseudo_label_df[cont_sim,"specificity_cont_tm"] <- specificity_cont_tm
     pseudo_label_df[cont_sim,"precision_cont_tm"] <- precision_cont_tm
     pseudo_label_df[cont_sim,"f1_cont_tm"] <- f1_score_cont_tm
     
     # selected model
     pseudo_label_df[cont_sim,"CCR_class_sm"] <- ccr_class_sm
     pseudo_label_df[cont_sim,"sensitivity_class_sm"] <- sensitivity_class_sm
     pseudo_label_df[cont_sim,"specificity_class_sm"] <- specificity_class_sm
     pseudo_label_df[cont_sim,"precision_class_sm"] <- precision_class_sm
     pseudo_label_df[cont_sim,"f1_class_sm"] <- f1_class_sm
     pseudo_label_df[cont_sim,"CCR_cont_sm"] <- ccr_cont_sm
     pseudo_label_df[cont_sim,"sensitivity_cont_sm"] <- sensitivity_cont_sm
     pseudo_label_df[cont_sim,"specificity_cont_sm"] <- specificity_cont_sm
     pseudo_label_df[cont_sim,"precision_cont_sm"] <- precision_cont_sm
     pseudo_label_df[cont_sim,"f1_cont_sm"] <- f1_score_cont_sm
     
     # saturated model
     #pseudo_label_df[cont_sim,"CCR_class_all"] <- ccr_class_all
     #pseudo_label_df[cont_sim,"sensitivity_class_all"] <- sensitivity_class_all
     #pseudo_label_df[cont_sim,"specificity_class_all"] <- specificity_class_all
     #pseudo_label_df[cont_sim,"precision_class_all"] <- precision_class_all
     #pseudo_label_df[cont_sim,"f1_class_all"] <- f1_class_all
     #    pseudo_label_df[cont_sim,"CCR_cont_all"] <- ccr_cont_all
     #    pseudo_label_df[cont_sim,"sensitivity_cont_all"] <- sensitivity_cont_all
     #    pseudo_label_df[cont_sim,"specificity_cont_all"] <- specificity_cont_all
     #    pseudo_label_df[cont_sim,"precision_cont_all"] <- precision_cont_all
     #    pseudo_label_df[cont_sim,"f1_cont_all"] <- f1_score_cont_all
     
     cont_sim <- cont_sim + 1
     
     cat("\n simulations :" ,cont_sim , "number: ", ifile,"- File: ", file_name)
   }
    ifile <- ifile + 1
 }

saveRDS(pseudo_label_df,"pseudo_validation.RDS")  


  