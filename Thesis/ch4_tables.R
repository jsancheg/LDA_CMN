

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


# Read rds files with metrics ---------------------------------------------



ssmetrics_23_03_2024 <- readRDS("Metrics_SSFiles.RDS")

ssmetrics_10_03_2024 <- readRDS("SSMetrics_Old_2024_03_10.RDS")

ssmetrics_21_03_2024 <- readRDS("SSMetrics_2024_03_21.RDS")

ssmetrics <- ssmetrics_21_03_2024
colnames(ssmetrics)

head(ssmetrics$File)
options(scipen = 999)

missing_values <- colSums(is.na(ssmetrics_old))
missing_values
total_rows <- nrow(ssmetrics_old)/3
total_rows

ssdf <-ssmetrics
ssdf_no_na <- na.omit(ssdf)

colnames(ssdf)
ssvo_df <- ssdf %>% dplyr::filter(Group_Mean_Distance == "VO")
ssvo_df_no_na <- ssdf_no_na %>% dplyr::filter(Group_Mean_Distance == "VO")
missing_values <- colSums(is.na(ssvo_df_no_na))

ssdf$File <- factor(ssdf$File)



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

ssdf$Number_Classes <- as.numeric(ssdf$Number_Classes)
ssdf$Number_Separating_Variables <- as.numeric(ssdf$Number_Separating_Variables)
ssdf$Number_Variables <- as.numeric(ssdf$Number_Variables)
ssdf$Number_Observations <- as.numeric(ssdf$Number_Observations)
ssdf$Training_Proportion <- as.numeric(ssdf$Training_Proportion)
ssdf$Class_Proportion <- factor(ssdf$Class_Proportion)
ssdf$Covariance_Structure <- factor(ssdf$Covariance_Structure)
ssdf$Covariance_Structure2 <- factor(ssdf$Covariance_Structure2)
ssdf$Group_Mean_Distance <- factor(ssdf$Group_Mean_Distance)
ssdf$Alpha1 <- as.numeric(ssdf$Alpha1)
ssdf$Alpha2 <- as.numeric(ssdf$Alpha2)
ssdf$Alpha3 <- as.numeric(ssdf$Alpha3)

ssdf$Eta1 <- as.numeric(ssdf$Eta1)
ssdf$Eta2 <- as.numeric(ssdf$Eta2)
ssdf$Eta3 <- as.numeric(ssdf$Eta3)

ssdf$Variables <- factor(ssdf$Variables)
ssdf$Model_Size <- as.numeric(ssdf$Model_Size)
ssdf$Model1 <- factor(ssdf$Model1)


ssdf$Variables <- relevel(ssdf$Variables, ref = "True")

ssdf$Number_Separating_Variables_Included <- ifelse(ssdf$Number_Separating_Variables == 2, ssdf$IncludeX2 + ssdf$IncludeX4, ssdf$IncludeX2 + ssdf$IncludeX4 + ssdf$IncludeX5 )
ssdf$Number_Non_Informative_Variables_Included <- ssdf$Model_Size - ssdf$Number_Separating_Variables_Included

ssdf$Inclusion_Correctness <- ssdf$Number_Separating_Variables_Included/ssdf$Number_Separating_Variables
ssdf$Exclusion_Correctness <- 1-ssdf$Number_Non_Informative_Variables_Included/(ssdf$Number_Variables - ssdf$Number_Separating_Variables)

ssdf_without_na <- format_columns(ssdf_no_na)
ssvo_df <- format_columns(ssvo_df)
ssvo_df_no_na <- format_columns(ssvo_df_no_na)

ssdf100 <- ssdf %>% dplyr::filter(Number_Variables == 100)
ssdf5 <- ssdf %>% dplyr::filter(Number_Variables == 5)

colnames(ssdf)

ssdf <- ssdf %>% mutate(Precision_Cont = Precicison_Cont) 


levels(ssdf$Variables)

new_order <- c("True","Selected","All")
Variables1 <- ssdf$Variables


ssdf <- ssdf %>% mutate(Variables1 = Variables) %>% select (-Variables)

ssdf$Variables <- factor(Variables1, levels = new_order)

levels(ssdf$Variables)


library(dplyr)

Inclusion_Exclusion1 <- ssdf %>% filter(Variables == "Selected")%>% dplyr::select(Number_Separating_Variables, IncludeX2,IncludeX4,IncludeX5)
Inclusion_Exclusion1

total_scenarios_with_2_separating_variables <- sum(ssdf$Number_Separating_Variables == 2 & ssdf$Variables == "Selected")
total_scenarios_with_3_separating_variables <- sum(ssdf$Number_Separating_Variables == 3 & ssdf$Variables == "Selected")

subtotal <- Inclusion_Exclusion1  %>%group_by(Number_Separating_Variables) %>%
  summarise(across(where(is.numeric),sum,na.rm=TRUE))
subtotal

# Proportion X2, X4 are included when there are 2 separating variables
round(cbind(subtotal[1,c(2:3)])/total_scenarios_with_2_separating_variables,2)

# Proportion X2, X4, X5 are included when there are 2 separating variables
round(cbind(subtotal[2,c(2:4)])/total_scenarios_with_3_separating_variables,2)

Inclusion_Exclusion2 <- ssdf %>% filter(Variables == "Selected") %>% 
  dplyr::select(Model,Number_Separating_Variables,Model_Size,Inclusion_Correctness,Exclusion_Correctness)
head(Inclusion_Exclusion2)

Inclusion_Exclusion2 %>% dplyr::select(Number_Separating_Variables,Inclusion_Correctness,Exclusion_Correctness ) %>% 
  group_by(Number_Separating_Variables)%>%summarise(across(where(is.numeric),mean,na.rm = TRUE))


Inclusion_Exclusion3 <- Inclusion_Exclusion2 %>% dplyr::select(Number_Separating_Variables,Exclusion_Correctness ) %>%
  group_by(Number_Separating_Variables)%>%summarise(across(where(is.numeric),sum,na.rm = TRUE))

round(cbind(Inclusion_Exclusion3[1,2]))/total_scenarios_with_2_separating_variables



# Cross for all factors ---------------------------------------------------


cross_variables <- ssdf %>% dplyr::select(Variables,CCR,Recall_Cont) %>%
  group_by(Variables) %>% summarise(across(where(is.numeric), mean, na.rm = TRUE ))

cross_variables

cross_group_mean_distance <- ssdf %>% dplyr::select(Group_Mean_Distance,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Group_Mean_Distance,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_group_mean_distance

cross_number_classes <- ssdf %>% dplyr::select(Number_Classes,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Classes,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))
cross_number_classes

cross_class_proportion <- ssdf %>% dplyr::select(Class_Proportion,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Class_Proportion,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_class_proportion


cross_number_variables <- ssdf %>% dplyr::select(Number_Variables,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Variables,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_number_variables

cross_training_proportion <- ssdf %>% dplyr::select(Training_Proportion,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Training_Proportion,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_training_proportion

cross_covariance_structure <- ssdf %>% dplyr::select(Covariance_Structure,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Covariance_Structure,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_covariance_structure 


cross_number_separating_variables <- ssdf %>% dplyr::select(Number_Separating_Variables,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Number_Separating_Variables,Variables)%>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_number_separating_variables







colnames(ssdf)




# Subsets evaluated by factors with the median

new_order <- c("True","Selected","All")

levels(vo_df$Variables)
sdf <- ssdf

Variables1 <- sdf$Variables



sdf <- sdf %>% mutate(Variables1 = Variables) %>% select (-Variables)
sdf$Variables <- factor(Variables1, levels = new_order)
sdf %>% select(Variables,Variables1)

levels(sdf$Variables)

# Metrics for very overlapping cases --------------------------------------

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





# Cross for very overlapping cases ----------------------------------------




new_order <- c("True","Selected","All")
Variables1 <- vo_df$Variables

levels(vo_df$Variables)
svo_df <- vo_df


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




# Simulation Crosstables --------------------------------------------------
colnames(ssdf)
  ssdf %>% filter(Training_Proportion == "75", Class_Proportion == "BAL", Number_Separating_Variables == 3,Number_Classes == 2)
cross_5var_2bal_classes <- ssdf %>%filter(Training_Proportion == "75", Class_Proportion == "BAL",
                                           Number_Separating_Variables == 3, Number_Classes == 2) %>%
  dplyr::select(File,Group_Mean_Distance,Covariance_Structure,Number_Variables,Number_Classes,Variables,CCR,Recall_Cont,Precision_Cont) %>%
  group_by(Group_Mean_Distance,Covariance_Structure,Number_Variables,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

cross_5var_2bal_classes
View(cross_5var_2bal_classes)

cross_5var_2bal_classes$File

