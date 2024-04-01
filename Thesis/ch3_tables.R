

library(emmeans)  # Estimated marginal means and p values
# The package can compute contrasts or linear combinations of these marginal means with various multiplicity adjustments. 
#  One can also estimate and contrast slopes of trend lines. Some graphical displays of these results are provided.
# ------------------------------------------------------------------------------

library(sjstats)  # Partial eta squared and cohens f effect size
# This package aims at providing, first, shortcuts for statistical measures, which otherwise could only be 
# calculated with additional effort (like Cramer's V, Phi, or effect size statistics like Eta or Omega squared), 
# or for which currently no functions available.

# ------------------------------------------------------------------------------

library(lme4)     # estimated the multi level model (random intercept for simulations)

library(lmerTest) # Gives more comprehensive anova output with p values
# The lmerTest package provides p-values in type I, II or III anova and summary tables for lin- ear mixed models 
# (lmer model fits cf. lme4) via Satterthwaite's degrees of freedom method; a Kenward-Roger method is also available via the pbkrtest package
# ------------------------------------------------------------------------------


library(MuMIn)    # R2 for the model
# contains functions to streamline information-theoretic model selection and carry out model averaging based on information criteria.

# ------------------------------------------------------------------------------

library(effectsize)


library(nlme)

library(tidyr)

library(dplyr)



fileRDS <- "/home/jsancheg/Documents/SFiles/SV_2_2_100_3000_75_BAL_SCBSNSV_VO_A8090_E530_10.RDS" 



fit_sfile <- readRDS(fileRDS)

round(fit_sfile$Metrics$CCR_TM,2)

round(fit_sfile$Metrics$CCR_SM,2)

round(fit_sfile$Metrics$CCR_SaturatedM,2)


fit_sfile$Metrics$RecallV_SM




fileRDS_10 <- "/home/jsancheg/Documents/SFiles/SV_2_2_5_3000_75_BAL_SCBNSV_VD_A8090_E530_10.RDS" 

fit_sfile <- readRDS(fileRDS_10)

cat(round(fit_sfile$Metrics$CCR_TM,2)[9] , "-", round(fit_sfile$Metrics$RecallV_TM,2)[9], "-", round(fit_sfile$Metrics$PrecisionV_TM,2)[9]) 

cat(round(fit_sfile$Metrics$CCR_SM,2)[9] , "-", round(fit_sfile$Metrics$RecallV_SM,2)[9], "-", round(fit_sfile$Metrics$PrecisionV_SM,2)[9]) 

cat(round(fit_sfile$Metrics$CCR_SaturatedM,2)[9] , "-", round(fit_sfile$Metrics$RecallV_SaturatedM,2)[9], "-", round(fit_sfile$Metrics$PrecisionV_SaturatedM,2)[9]) 



fileRDS_10 <- "/home/jsancheg/Documents/SFiles/SV_2_2_5_3000_75_BAL_SCBNSV_VD_A8090_E530_10.RDS" 



# Read rds files with metrics ---------------------------------------------



Smetrics_with_na <- readRDS("SMetrics_OLD_2024_03_18.RDS")

Smetrics_no_na <- readRDS("SMetrics_OLD_2024_03_18_no_na.RDS")

missing_values <- colSums(is.na(Smetrics_no_na))

Smetrics <- readRDS("SMetrics_OLD_2024_03_18.RDS")

smetrics_old <- readRDS("Metrics.RDS")
smetrics_23_02_2024 <- readRDS("Metrics_SFiles.RDS")

colnames(smetrics_old)

median(smetrics_old$AccuracyTM, na.rm = TRUE)
median(smetrics_old$AccuracySM, na.rm = TRUE)
median(smetrics_old$AccuracySaturatedM, na.rm = TRUE)

median(smetrics_old$recall_TM_V,na.rm = TRUE)
median(smetrics_old$recall_SM_V,na.rm = TRUE)
median(smetrics_old$recall_saturated_V,na.rm = TRUE)


median(smetrics_old$AccuracyTM, na.rm = TRUE)
median(smetrics_old$AccuracySM, na.rm = TRUE)
median(smetrics_old$AccuracySaturatedM, na.rm = TRUE)




options(scipen = 999)

sdf <- Smetrics 
sdf_no_na <- Smetrics_no_na

svo_df <- sdf %>% dplyr::filter(Group_Mean_Distance == "VO")
svo_df_no_na <- sdf_no_na %>% dplyr::filter(Group_Mean_Distance == "VO")
missing_values <- colSums(is.na(svo_df_no_na))

sdf$File <- factor(sdf$File)

svo_df <- svo_df %>% mutate(Precision_Cont = Precicison_Cont)


format_columns <- function(df)
{
  df$Number_Classes <- as.numeric(df$Number_Classes)
  df$Number_Separating_Variables <- as.numeric(df$Number_Separating_Variables)
  df$Number_Variables <- as.numeric(df$Number_Variables)
  df$Number_Observations <- as.numeric(df$Number_Observations)
  df$Training_Proportion <- as.numeric(df$Training_Proportion)
  df$Class_Proportion <- factor(df$Class_Proportion)
  df$Covariance_Structure <- factor(df$Covariance_Structure)
  df$Covariance_Structure2 <- factor(df$Covariance_Structure2)
  df$Group_Mean_Distance <- factor(df$Group_Mean_Distance)
  df$Alpha1 <- as.numeric(df$Alpha1)
  df$Alpha2 <- as.numeric(df$Alpha2)
  df$Alpha3 <- as.numeric(df$Alpha3)
  
  df$Eta1 <- as.numeric(df$Eta1)
  df$Eta2 <- as.numeric(df$Eta2)
  df$Eta3 <- as.numeric(df$Eta3)
  
  df$Variables <- factor(df$Variables)
  df$Model_Size <- as.numeric(df$Model_Size)
  df$Model1 <- factor(df$Model1)
  
  
  df$Variables <- relevel(df$Variables, ref = "True")
  
  df$Number_Separating_Variables_Included <- ifelse(df$Number_Separating_Variables == 2, df$IncludeX2 + df$IncludeX4, df$IncludeX2 + df$IncludeX4 + df$IncludeX5 )
  df$Number_Non_Informative_Variables_Included <- df$Model_Size - df$Number_Separating_Variables_Included
  
  df$Inclusion_Correctness <- df$Number_Separating_Variables_Included/df$Number_Separating_Variables
  df$Exclusion_Correctness <- 1-df$Number_Non_Informative_Variables_Included/(df$Number_Variables - df$Number_Separating_Variables)
  
  return(df)    
}

sdf$Number_Classes <- as.numeric(sdf$Number_Classes)
sdf$Number_Separating_Variables <- as.numeric(sdf$Number_Separating_Variables)
sdf$Number_Variables <- as.numeric(sdf$Number_Variables)
sdf$Number_Observations <- as.numeric(sdf$Number_Observations)
sdf$Training_Proportion <- as.numeric(sdf$Training_Proportion)
sdf$Class_Proportion <- factor(sdf$Class_Proportion)
sdf$Covariance_Structure <- factor(sdf$Covariance_Structure)
sdf$Covariance_Structure2 <- factor(sdf$Covariance_Structure2)
sdf$Group_Mean_Distance <- factor(sdf$Group_Mean_Distance)
sdf$Alpha1 <- as.numeric(sdf$Alpha1)
sdf$Alpha2 <- as.numeric(sdf$Alpha2)
sdf$Alpha3 <- as.numeric(sdf$Alpha3)

sdf$Eta1 <- as.numeric(sdf$Eta1)
sdf$Eta2 <- as.numeric(sdf$Eta2)
sdf$Eta3 <- as.numeric(sdf$Eta3)

sdf$Variables <- factor(sdf$Variables)
sdf$Model_Size <- as.numeric(sdf$Model_Size)
sdf$Model1 <- factor(sdf$Model1)


sdf$Variables <- relevel(sdf$Variables, ref = "True")

sdf$Number_Separating_Variables_Included <- ifelse(sdf$Number_Separating_Variables == 2, sdf$IncludeX2 + sdf$IncludeX4, sdf$IncludeX2 + sdf$IncludeX4 + sdf$IncludeX5 )
sdf$Number_Non_Informative_Variables_Included <- sdf$Model_Size - sdf$Number_Separating_Variables_Included

sdf$Inclusion_Correctness <- sdf$Number_Separating_Variables_Included/sdf$Number_Separating_Variables
sdf$Exclusion_Correctness <- 1-sdf$Number_Non_Informative_Variables_Included/(sdf$Number_Variables - sdf$Number_Separating_Variables)

sdf_without_na <- format_columns(sdf_no_na)
svo_df <- format_columns(svo_df)
svo_df_no_na <- format_columns(svo_df_no_na)

sdf100 <- sdf %>% dplyr::filter(Number_Variables == 100)
sdf5 <- sdf %>% dplyr::filter(Number_Variables == 5)

colnames(sdf)

sdf <- sdf %>% mutate(Precision_Cont = Precicison_Cont) 
sdf <- sdf

levels(sdf$Variables)

new_order <- c("True","Selected","All")
Variables1 <- sdf$Variables


sdf <- sdf %>% mutate(Variables1 = Variables) %>% select (-Variables)

sdf$Variables <- factor(Variables1, levels = new_order)



library(dplyr)

Inclusion_Exclusion1 <- sdf_without_na %>% filter(Variables == "Selected")%>% dplyr::select(Number_Variables, Number_Separating_Variables, 
                                                                                            IncludeX2, IncludeX4, IncludeX5, Model_Size,
                                                                                            Inclusion_Correctness, Exclusion_Correctness)
Inclusion_Exclusion1




# for 5 variables and 3 groups

Inclusion_Exclusion1 %>% filter(Number_Variables == 5, Number_Separating_Variables == 3) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
  summarise_at(c("Model_Size","IncludeX2", "IncludeX4", "IncludeX5",  "Inclusion_Correctness", "Exclusion_Correctness"), mean) 

# for 100 variables and 3 groups

Inclusion_Exclusion1 %>% filter(Number_Variables == 100, Number_Separating_Variables == 3) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
  summarise_at(c("Model_Size","IncludeX2","IncludeX4","IncludeX5","Inclusion_Correctness","Exclusion_Correctness"), mean) 


# for 5 variables and 2 groups

Inclusion_Exclusion1 %>% filter(Number_Variables == 5, Number_Separating_Variables == 2) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
  summarise_at(vars("Model_Size","IncludeX2", "IncludeX4",  "Inclusion_Correctness", "Exclusion_Correctness"), mean) 


# for 100 variables and 2 groups

Inclusion_Exclusion1 %>% filter(Number_Variables == 100, Number_Separating_Variables == 2) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
  summarise_at(c("Model_Size","IncludeX2","IncludeX4","Inclusion_Correctness","Exclusion_Correctness"), mean) 






total_scenarios_with_2_separating_variables <- sum(sdf_without_na$Number_Separating_Variables == 2 & sdf_without_na$Variables == "Selected")
total_scenarios_with_3_separating_variables <- sum(sdf_without_na$Number_Separating_Variables == 3 & sdf_without_na$Variables == "Selected")

subtotal <- Inclusion_Exclusion1  %>%group_by(Number_Separating_Variables) %>%
  summarise(across(where(is.numeric),sum,na.rm=TRUE))
subtotal

# Proportion X2, X4 are included when there are 2 separating variables
round(cbind(subtotal[1,c(2:3)])/total_scenarios_with_2_separating_variables,2)

# Proportion X2, X4, X5 are included when there are 2 separating variables
round(cbind(subtotal[2,c(2:4)])/total_scenarios_with_3_separating_variables,2)

Inclusion_Exclusion2 <- sdf_without_na %>% filter(Variables == "Selected") %>% dplyr::select(Model,Number_Separating_Variables,Model_Size,Inclusion_Correctness,Exclusion_Correctness)
head(Inclusion_Exclusion2)

Inclusion_Exclusion2 %>% dplyr::select(Number_Separating_Variables,Inclusion_Correctness,Exclusion_Correctness ) %>% 
  group_by(Number_Separating_Variables)%>%summarise(across(where(is.numeric),mean,na.rm = TRUE))


Inclusion_Exclusion3 <- Inclusion_Exclusion2 %>% dplyr::select(Number_Separating_Variables,Exclusion_Correctness ) %>%
  group_by(Number_Separating_Variables)%>%summarise(across(where(is.numeric),sum,na.rm = TRUE))

round(cbind(Inclusion_Exclusion3[1,2]))/total_scenarios_with_2_separating_variables



cross_variables <- sdf %>% dplyr::select(Variables,CCR,Recall_Class,F1_Class,Recall_Cont,Precision_Cont,F1_Cont) %>%
  group_by(Variables) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE ))

cross_variables

cross_variables <- sdf %>% dplyr::select(Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Variables) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE ))

cross_variables


bp <- boxplot(CCR ~ Variables, data = sdf)
summary_stats <- bp$stats
rownames(summary_stats) <- c("min","q1","median","q3","max")
colnames(summary_stats) <- c("True","Selected","All")
summary_stats[3,]

bp <- boxplot(Recall_Cont ~ Variables, data = sdf)
summary_stats <- bp$stats
rownames(summary_stats) <- c("min","q1","median","q3","max")
colnames(summary_stats) <- c("True","Selected","All")
summary_stats[3,]

bp <- boxplot(Recall_Cont ~ Number_Classes + Variables, data = sdf)
summary_stats <- bp$stats
rownames(summary_stats) <- c("min","q1","median","q3","max")
colnames(summary_stats) <- c("True","Selected","All")
summary_stats[3,]


bp <- boxplot(Recall_Cont ~ Number_Separating_Variables + Variables, data = sdf)
summary_stats <- bp$stats
rownames(summary_stats) <- c("min","q1","median","q3","max")
colnames(summary_stats) <- c("True","True","Selected","Selected","All","All")

summary_stats[3,]

bp <- boxplot(Recall_Cont ~ Number_Variables + Variables, data = sdf)
summary_stats <- bp$stats
rownames(summary_stats) <- c("min","q1","median","q3","max")
colnames(summary_stats) <- c("True","True","Selected","Selected","All","All")

summary_stats[3,]



# Subsets evaluated by factors with the median



sdf <- sdf %>% mutate(Variables1 = Variables) %>% select (-Variables)
sdf$Variables <- factor(Variables1, levels = new_order)
sdf %>% select(Variables,Variables1)



new_order <- c("True","Selected","All")

levels(svo_df$Variables)
svo_df <- sdf %>% filter(Group_Mean_Distance == "VO")



# Cross tables all factors ------------------------------------------------


cross_variables <- sdf %>% dplyr::select(Variables,CCR,Recall ,Recall_Cont,Precision_Cont) %>%
  group_by(Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE ))

cross_variables


cross_number_classes <- sdf %>% dplyr::select(Number_Classes,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Classes,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))
cross_number_classes

cross_number_separating_variables <- sdf %>% dplyr::select(Number_Separating_Variables,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Separating_Variables,Variables)%>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_number_separating_variables

cross_number_variables <- sdf %>% dplyr::select(Number_Variables,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Variables,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_number_variables

cross_training_proportion <- sdf %>% dplyr::select(Training_Proportion,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Training_Proportion,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_training_proportion

cross_class_proportion <- sdf %>% dplyr::select(Class_Proportion,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Class_Proportion,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_class_proportion

cross_covariance_structure <- sdf %>% dplyr::select(Covariance_Structure,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Covariance_Structure,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_covariance_structure 

cross_group_mean_distance <- sdf %>% dplyr::select(Group_Mean_Distance,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Group_Mean_Distance,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_group_mean_distance

colnames(ssdf)



# Cross tables filter by Very overlapping ---------------------------------






svo_df <- svo_df %>% mutate(Precision_Cont = Precicison_Cont)

#svo_df$Variables <- factor(Variables1, levels = new_order)
levels(svo_df$Variables)

cross_variables <- svo_df %>% dplyr::select(Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE ))

cross_variables


cross_number_classes <- svo_df %>% dplyr::select(Number_Classes,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Classes,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))
cross_number_classes



cross_number_separating_variables <- svo_df %>% dplyr::select(Number_Separating_Variables,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Separating_Variables,Variables)%>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_number_separating_variables

cross_number_variables <- svo_df %>% dplyr::select(Number_Variables,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Variables,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_number_variables

cross_training_proportion <- svo_df %>% dplyr::select(Training_Proportion,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Training_Proportion,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_training_proportion

cross_class_proportion <- svo_df %>% dplyr::select(Class_Proportion,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Class_Proportion,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_class_proportion

cross_covariance_structure <- svo_df %>% dplyr::select(Covariance_Structure,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Covariance_Structure,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_covariance_structure 

cross_group_mean_distance <- svo_df %>% dplyr::select(Group_Mean_Distance,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Group_Mean_Distance,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_group_mean_distance

colnames(ssdf)



cross_class_proportion <- ssdf_without_na %>% dplyr::select(Variables,Class_Proportion,CCR,Recall_Cont,Precicison_Cont)

cross_class_proportion %>% group_by(Class_Proportion,Variables) %>%
  summarise(Mean_ccr = mean(CCR, na.rm = TRUE),
            Mean_recall = mean(Recall_Cont, na.rm = TRUE),
            Mean_precision = mean(Precicison_Cont, na.rm = TRUE))

colnames(ssdf_without_na)

