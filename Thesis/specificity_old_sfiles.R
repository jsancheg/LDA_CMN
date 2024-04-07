library(dbplyr)
library(MLmetrics)
getwd()
source("Semisupervised.R")
source("ListScenarios.R")

path_input <- pathSFiles_Old
path_output <- getwd()
name_output_file <- "sMetrics_OLd_2024_03_18_wider.RDS"

if( all(str_detect(name_output_file, dir(path_output))==FALSE )   ) 
  {
    wider_metrics <- create_metrics_wider_format(path_input,path_output,name_output_file)
  } else wider_metrics <- readRDS(name_output_file)


metrics_na_0 <-wider_metrics

metrics_na_0 <- add_diff_to_wider <- function(wider_metrics)
  

  Convert_wider_to_Longer <- function(df)
  {
      combine_df <- df
    
    
    #    return(combine_df)
    combine_df <- combine_df %>% mutate(Replicate = Nsim)
    combine_df <- combine_df %>% mutate(Nsim = row_number())
    combine_df <- combine_df %>% relocate(File, .before = Nsim)
    combine_df <- mutate(combine_df, Model_Selected = Model_SM)
    
    aux_df <- combine_df %>% dplyr::select(File,Nsim,Replicate,
                                           Model_TM,Model_SM,Model_SaturatedM,
                                           Nvars_TM,Nvars_SM,Nvars_SaturatedM,
                                           CCR_TM,CCR_SM,CCR_SaturatedM,
                                           CCRCont_TM,CCRCont_SM,CCRCont_SaturatedM,
                                           CCRNoCont_TM,CCRNoCont_SM,CCRNoCont_SaturatedM,
                                           Precision_TM,Precision_SM,Precision_SaturatedM,
                                           Recall_TM,Recall_SM,Recall_SaturatedM,
                                           F1_TM,F1_SM,F1_SaturatedM,
                                           PrecisionV_TM,PrecisionV_SM,PrecisionV_SaturatedM,
                                           RecallV_TM,RecallV_SM,RecallV_SaturatedM,
                                           F1V_TM,F1V_SM,F1V_SaturatedM)
    colnames(aux_df)
    #1: True
    #2: Selected
    #3: Complete
    
    aux_df1 <- aux_df %>% dplyr::rename("A1" = "Model_TM",
                                        "A2" = "Model_SM",
                                        "A3" = "Model_SaturatedM",
                                        "B1" = "Nvars_TM",
                                        "B2" = "Nvars_SM",
                                        "B3" = "Nvars_SaturatedM",
                                        "C1" = "CCR_TM" ,
                                        "C2"= "CCR_SM" ,
                                        "C3" = "CCR_SaturatedM",
                                        "D1" = "CCRCont_TM"  ,
                                        "D2" = "CCRCont_SM"  ,
                                        "D3" = "CCRCont_SaturatedM",
                                        "N1" = "CCRNoCont_TM",
                                        "N2" = "CCRNoCont_SM",
                                        "N3" = "CCRNoCont_SaturatedM" ,
                                        "P1" = "Precision_TM"  ,
                                        "P2" = "Precision_SM" ,
                                        "P3" = "Precision_SaturatedM" ,
                                        "R1" = "Recall_TM" ,
                                        "R2" = "Recall_SM" ,
                                        "R3" = "Recall_SaturatedM",
                                        "S1" = "F1_TM",
                                        "S2" = "F1_SM",
                                        "S3" = "F1_SaturatedM",
                                        "V1" = "PrecisionV_TM" ,
                                        "V2" = "PrecisionV_SM" ,
                                        "V3" = "PrecisionV_SaturatedM",
                                        "W1" = "RecallV_TM",
                                        "W2" = "RecallV_SM",
                                        "W3" = "RecallV_SaturatedM",
                                        "Z1" = "F1V_TM",
                                        "Z2" = "F1V_SM",
                                        "Z3" = "F1V_SaturatedM"
    )
    
    aux_df_Long <- aux_df1 %>% 
      tidyr::pivot_longer(
        cols = A1:Z3,
        names_to = c(".value","Variables"),
        names_pattern = "(.)(.)"
      )
    
    aux_df2 <- aux_df_Long %>% dplyr::rename("Model" = "A",
                                             "Number_Variables" = "B",
                                             "CCR" = "C" ,
                                             "CCR_Cont" = "D"  ,
                                             "CCR_No_Cont" = "N",
                                             "Precision_Class" = "P"  ,
                                             "Recall_Class" = "R" ,
                                             "F1_Class" = "S",
                                             "Precision_Cont" = "V" ,
                                             "Recall_Cont" = "W",
                                             "F1_Cont" = "Z")
    
    
    aux_df2 <- aux_df2 %>% mutate(Model1 = sapply(Model, sort_labels)) %>%
      relocate(Model1, .after = Model)
    
    aux_df2 <- aux_df2 %>% mutate(Model_Size = str_count(Model,"-")+1) %>%
      relocate(Model_Size, .after = Model)
    
    aux_df2 <- aux_df2 %>% mutate(IncludeX2 = as.numeric(str_detect(Model1,"X2")),
                                  IncludeX4 = as.numeric(str_detect(Model1,"X4")),
                                  IncludeX5 = as.numeric(str_detect(Model1,"X5"))) %>%
      relocate(IncludeX2,IncludeX4,IncludeX5, .after = Model1)
    
    aux_df2 <- aux_df2 %>% mutate(Number_Classes = 
                                    str_split(File,"_",simplify = TRUE)[,2])
    
    aux_df2 <- aux_df2 %>% mutate(Number_Separating_Variables = 
                                    str_split(File,"_",simplify = TRUE)[,3])
    
    aux_df2 <- aux_df2 %>% mutate(Number_Variables = 
                                    str_split(File,"_",simplify = TRUE)[,4])
    
    aux_df2 <- aux_df2 %>% mutate(Number_Observations = 
                                    str_split(File,"_",simplify = TRUE)[,5])
    
    aux_df2 <- aux_df2 %>% mutate(Training_Proportion = 
                                    str_split(File,"_",simplify = TRUE)[,6])
    
    aux_df2 <- aux_df2 %>% mutate(Class_Proportion = 
                                    str_split(File,"_",simplify = TRUE)[,7])
    
    aux_df2 <- aux_df2 %>% mutate(Covariance_Structure = 
                                    str_split(File,"_",simplify = TRUE)[,8])
    
    aux_df2 <- aux_df2 %>% mutate(Group_Mean_Distance = 
                                    str_split(File,"_",simplify = TRUE)[,9])
    
    aux_df2 <- aux_df2 %>% mutate(AlphaC = 
                                    str_split(File,"_",simplify = TRUE)[,10])
    
    aux_df2 <- aux_df2 %>% mutate(EtaC = 
                                    str_split(File,"_",simplify = TRUE)[,11])
    
    aux_df2_no_na <- na.omit(aux_df2)
    
    aux_df2 <- aux_df2 %>% mutate(Variables = recode(Variables,
                                                     '1' = "True",
                                                     '2' = "Selected",
                                                     '3' = "All"))
    
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Variables = recode(Variables,
                                                                 '1' = "True",
                                                                 '2' = "Selected",
                                                                 '3' = "All"))
    
    aux_df2$Covariance_Structure2 <- aux_df2$Covariance_Structure
    
    aux_df2_no_na$Covariance_Structure2 <- aux_df2_no_na$Covariance_Structure
    
    
    aux_df2 <- aux_df2 %>% mutate(Covariance_Structure2 = recode(Covariance_Structure,
                                                                 "SCBSV" = "SV",
                                                                 "SCBSNSV" = "SNSV",
                                                                 "SCBNSV" = "NSV",
                                                                 "IND" = "IND"))
    
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Covariance_Structure2 = recode(Covariance_Structure,
                                                                             "SCBSV" = "SV",
                                                                             "SCBSNSV" = "SNSV",
                                                                             "SCBNSV" = "NSV",
                                                                             "IND" = "IND"))
    
    n <- nrow(aux_df2)
    n_no_na <- nrow(aux_df2_no_na)
    
    aux_alpha <- sapply(aux_df2$AlphaC, function(alpha_str)
    {
      if(alpha_str == "A808090"){
        return(c(80,80,90))
      } else if(alpha_str == "A8090") return(c(80,90,0))
    })
    
    aux_alpha_no_na <- sapply(aux_df2_no_na$AlphaC, function(alpha_str)
    {
      if(alpha_str == "A808090"){
        return(c(80,80,90))
      } else if(alpha_str == "A8090") return(c(80,90,0))
    })
    
    Alpha_int <- matrix(aux_alpha,ncol = 3, nrow = n, byrow = TRUE)
    Alpha_int_no_na <- matrix(aux_alpha_no_na,ncol = 3, nrow = n_no_na, byrow = TRUE)
    
    aux_eta <- sapply(aux_df2$EtaC, function(Eta_str)
    {
      if(Eta_str == "E5530"){
        return(c(5,5,30))
      } else if(Eta_str == "E530") return(c(5,30,0))
    })
    
    aux_eta_no_na <- sapply(aux_df2_no_na$EtaC, function(Eta_str)
    {
      if(Eta_str == "E5530"){
        return(c(5,5,30))
      } else if(Eta_str == "E530") return(c(5,30,0))
    })
    
    
    Eta_int <- matrix(aux_eta, ncol = 3, nrow = n, byrow = TRUE)
    Eta_int_no_na <- matrix(aux_eta_no_na, ncol = 3, nrow = n_no_na, byrow = TRUE)
    
    aux_df2 <- aux_df2 %>% mutate(Alpha1 = Alpha_int[,1])
    aux_df2 <- aux_df2 %>% mutate(Alpha2 = Alpha_int[,2])
    aux_df2 <- aux_df2 %>% mutate(Alpha3 = Alpha_int[,3])
    
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Alpha1 = Alpha_int_no_na[,1])
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Alpha2 = Alpha_int_no_na[,2])
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Alpha3 = Alpha_int_no_na[,3])
    
    
    aux_df2 <- aux_df2 %>% mutate(Eta1 = Eta_int[,1])
    aux_df2 <- aux_df2 %>% mutate(Eta2 = Eta_int[,2])
    aux_df2 <- aux_df2 %>% mutate(Eta3 = Eta_int[,3])
    
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Eta1 = Eta_int_no_na[,1])
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Eta2 = Eta_int_no_na[,2])
    aux_df2_no_na <- aux_df2_no_na %>% mutate(Eta3 = Eta_int_no_na[,3])
    
    
    aux_df2$Model[aux_df2$Number_Separating_Variables == 2 & aux_df2$Model == ""] <- "X2-X4"
    aux_df2$Model[aux_df2$Number_Separating_Variables == 3 & aux_df2$Model ==""] <- "X2-X4-X5"
    aux_df2$Model1[aux_df2$Number_Separating_Variables == 2 & aux_df2$Model1 == ""] <- "X2-X4"
    aux_df2$Model1[aux_df2$Number_Separating_Variables == 3 & aux_df2$Model1 ==""] <- "X2-X4-X5"
    
    
    aux_df2_no_na$Model[aux_df2_no_na$Number_Separating_Variables == 2 & aux_df2_no_na$Model == ""] <- "X2-X4"
    aux_df2_no_na$Model[aux_df2_no_na$Number_Separating_Variables == 3 & aux_df2_no_na$Model ==""] <- "X2-X4-X5"
    aux_df2_no_na$Model1[aux_df2_no_na$Number_Separating_Variables == 2 & aux_df2_no_na$Model1 == ""] <- "X2-X4"
    aux_df2_no_na$Model1[aux_df2_no_na$Number_Separating_Variables == 3 & aux_df2_no_na$Model1 ==""] <- "X2-X4-X5"
    
    
    aux_df2$Model_Size[aux_df2$Number_Separating_Variables == 2 & aux_df2$Model ==""] <- 2
    aux_df2$Model_Size[aux_df2$Number_Separating_Variables == 3 & aux_df2$Model ==""] <- 3
    
    aux_df2_no_na$Model_Size[aux_df2_no_na$Number_Separating_Variables == 2 & aux_df2_no_na$Model ==""] <- 2
    aux_df2_no_na$Model_Size[aux_df2_no_na$Number_Separating_Variables == 3 & aux_df2_no_na$Model ==""] <- 3
    
    
    Output <- aux_df2 %>% relocate(c(Number_Classes,Number_Separating_Variables,
                                     Number_Variables,Number_Observations,
                                     Training_Proportion,Class_Proportion,
                                     Covariance_Structure,Covariance_Structure2,
                                     Group_Mean_Distance,AlphaC,EtaC,Alpha1,Alpha2,Alpha3,
                                     Eta1,Eta2,Eta3),
                                   .after = File )
    

    return(Output)
  }

  
  
  
  
  
metrics_na_0[is.na(metrics_na_0)] <- 0

colnames(metrics_na_0)

na_counts <- colSums(is.na(metrics_na_0))

                
                        
colnames(df0)

aux_df <- aux_df0 %>% dplyr::select(File,Nsim,Replicate,
                                       Model_TM,Model_SM,Model_SaturatedM,
                                       Nvars_TM,Nvars_SM,Nvars_SaturatedM,
                                       CCR_TM,CCR_SM,CCR_SaturatedM,
                                       CCRCont_TM,CCRCont_SM,CCRCont_SaturatedM,
                                       CCRNoCont_TM,CCRNoCont_SM,CCRNoCont_SaturatedM,
                                       Precision_TM,Precision_SM,Precision_SaturatedM,
                                       Recall_TM,Recall_SM,Recall_SaturatedM,
                                       F1_TM,F1_SM,F1_SaturatedM,
                                       PrecisionV_TM,PrecisionV_SM,PrecisionV_SaturatedM,
                                       RecallV_TM,RecallV_SM,RecallV_SaturatedM,
                                       F1V_TM,F1V_SM,F1V_SaturatedM,
                                       TP_TM, TN_TM, FP_TM, FN_TM,
                                       TP_SM, TN_SM, FP_TM, FN_TM,
                                       TP_ALL, TN_ALL, FP_ALL, FN_ALL,
                                       diffccr_claass_sm_tm,diffccr_class_all_tm,
                                      diffccr_class_sm_all, )
colnames(aux_df)


