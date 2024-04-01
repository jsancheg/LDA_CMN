

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
library(stringr)
library(dplyr)
library(ggplot2)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(cowplot)
library(patchwork)
library(ggpubr)

# very bad example for identifying contaminated samples
# fileRDS <- "/home/jsancheg/Documents/SFiles/SV_2_2_5_3000_75_BAL_SCBSV_VD_A8090_E530_10.RDS" 



# Plot of the training and test set of one replicate ----------------------------------------------------


fileRDS <- "/home/jsancheg/Documents/SFiles/SV_2_2_5_3000_75_BAL_SCBNSV_VD_A8090_E530_10.RDS" 

 

fit_sfile <- readRDS(fileRDS)

fit_sfile$Metrics_SM
fit_sfile$Metrics$RecallV_SM

sim <- 9

fit_sfile$Metrics_SMfit_sfile$Metrics$Model_SM[sim]
fit_sfile$Metrics$RecallV_SM[sim]
fit_sfile$Metrics$RecallV_TM[sim]
fit_sfile$Metrics$Recall_SaturatedM[sim]

posSM <- fit_sfile$Estimates[[sim]]$posSM

Xtrain <- fit_sfile$GenData[[sim]]$Xtrain
Xtest <- fit_sfile$GenData[[sim]]$Xtest
vtrain <- fit_sfile$GenData[[sim]]$vtrain
vtest <- fit_sfile$GenData[[sim]]$vtest
ltrain <- fit_sfile$GenData[[sim]]$ltrain
ltest <- fit_sfile$GenData[[sim]]$ltest

pred_vtest <- fit_sfile$Estimates[[sim]]$vTestHat_SM
pred_ltest <- fit_sfile$Estimates[[sim]]$lTestHat_SM
#pred_vtest <- fit_sfile$Estimates[[sim]]$models[[posSM]]$vtest_hat

# test set by class
pairs(Xtest, panel = function(x,y, ...) {
  points(x,y, 
         col = ifelse(cond1_test == "11" ,"blue",
                      ifelse(cond1_test == "22" ,"green",
                             ifelse(cond1_test=="12"  ,"blue","green"  )    )) ,
         pch = ifelse(cond1_test == "11" ,19,
                      ifelse(cond1_test == "22" ,17,
                             ifelse(cond1_test=="12" ,0, 5)) ),
         cex = 1,
  )
  #           text(x[indBreal_T0_P1_Testv],y[indBreal_T0_P1_Testv],
  #           labels=c(indBreal_T0_P1_Testv),pos = 4)
})


# class predictions
  cond1_test <- paste0(ltest,pred_ltest)
  unique(cond1_test)
  
  table(ltest,pred_ltest)
  
  pairs(Xtest, panel = function(x,y, ...) {
    points(x,y, 
           col = ifelse(cond1_test == "11" ,"blue",
                        ifelse(cond1_test == "22" ,"green",
                               ifelse(cond1_test=="12"  ,"blue","green"  )    )) ,
           pch = ifelse(cond1_test == "11" ,19,
                        ifelse(cond1_test == "22" ,17,
                               ifelse(cond1_test=="12" ,0, 5)) ),
           cex = 1,
    )
    #           text(x[indBreal_T0_P1_Testv],y[indBreal_T0_P1_Testv],
    #           labels=c(indBreal_T0_P1_Testv),pos = 4)
  })
  
  
  
# Contaminated observations  
  cond2_test <- paste0(ltest,vtest,pred_vtest)
  length(ltest)
  length(vtest)
  length(pred_vtest)
  
  table(vtest,pred_vtest)
  MLmetrics::Recall(vtest,pred_vtest,positive = 0)
  
  
  pairs(Xtest, panel = function(x,y, ...) {
    points(x,y, 
           col = ifelse(cond2_test == "111" ,"blue",
                        ifelse(cond2_test == "100" ,"red",
                               ifelse(cond2_test=="110"  ,"blue",
                                      ifelse(cond2_test == "101" ,"red",
                                             ifelse(cond2_test =="211", "green",
                                                    ifelse(cond2_test == "200", "coral",
                                                           ifelse(cond2_test == "210","green", "coral") )       )       )    )) ),
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
  
  
  
  pos <- fit_sfile$Estimates[[sim]]$posSM
  fit_sfile$Metrics[9,]
  
  fit_sfile$Estimates[[sim]]$models[[pos]]$PM
  
  for (m1 in 1:7)
  {
    cat("\n ",fit_sfile$Estimates[[sim]]$models[[m1]]$PM,"-" ,fit_sfile$Estimates[[sim]]$models[[m1]]$fitted_C_model,
        "CCR",round(fit_sfile$Estimates[[sim]]$models[[m1]]$CCRTestC,2) ,"\n")

  }
  
  
  fit_sfile$Metrics$CCR_TM
  
  fit_sfile$Metrics$CCR_SM
  
  fit_sfile$Metrics$CCR_SaturatedM
  
  fit_sfile$Metrics$Recall_TM
  
  fit_sfile$Metrics$Recall_SM
  
  fit_sfile$Metrics$Recall_SaturatedM
  
  
  

# Reading file metrics and format it --------------------------------------

  Smetrics_with_na <- readRDS("SMetrics_OLD_2024_03_18.RDS")
  
  Smetrics_no_na <- readRDS("SMetrics_OLD_2024_03_18_no_na.RDS")
  
  missing_values <- colSums(is.na(Smetrics_no_na))
  
  Smetrics <- readRDS("SMetrics_OLD_2024_03_18.RDS")
  
  
  missing_values
  options(scipen = 999)
  
  ssdf <- Smetrics 
  ssdf_no_na <- Smetrics_no_na

  vo_df <- ssdf %>% dplyr::filter(Group_Mean_Distance == "VO")
  vo_df_no_na <- ssdf_no_na %>% dplyr::filter(Group_Mean_Distance == "VO")
  svo_df <- vo_df
  
  new_order <- c("True","Selected","All")
  Variables1 <- vo_df$Variables
  
  svo_df <- svo_df %>% mutate(Variables1 = Variables) %>% select (-Variables)
  
  svo_df$Variables <- factor(Variables1, levels = new_order)
  
  
  missing_values <- colSums(is.na(vo_df_no_na))
  
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
  
    
  ssdf100 <- ssdf %>% dplyr::filter(Number_Variables == 100)
  ssdf5 <- ssdf %>% dplyr::filter(Number_Variables == 5)
  
  # all set of variables
  ccr_df <- ssdf %>% dplyr::select(Nsim,CCR)
  
  ccr_df <- dplyr::mutate(group_by(ccr_df,Nsim), meanCCR = mean(CCR))
  
  ccr_ordered <- dplyr::arrange(ccr_df, meanCCR)
  
  head(ccr_df)
  tail(ccr_df)
  
  groups <- max(ccr_df$Nsim)
  reps <- 3


# Box plot of difference amon set of Variables ----------------------------


new_order <- c("True","Selected","All")
Variables1 <- ssdf$Variables

sdf <- ssdf

sdf$Variables <- factor(Variables1, levels = new_order)



  mean.acc1 <- ssdf %>% group_by(Variables) %>%
  summarise(mean.CCR  = mean(CCR))

  mean.acc1
  
  mean.acc1 <- sdf %>% group_by(Variables) %>%
    summarise(mean.CCR  = mean(CCR))
  
  mean.acc1

  level1 <- "True"
  level2 <- "Selected"
  level3 <- "All"
  
  
    
  # Box plots for different factors ------------------------
colnames(ssdf)
  var.acc1 <- ssdf %>% group_by(Group_Mean_Distance) %>%
    summarise_at(c("CCR","Recall_Class"),  var, na.rm  = TRUE)


  var.acc2 <- ssdf %>% group_by(Number_Classes) %>%
    summarise(var.CCR  = var(CCR))
  
  var.acc3 <- ssdf %>% group_by(Covariance_Structure) %>%
    summarise(var.CCR = var(CCR))
  
  
  mean.acc1 <- ssdf %>% dplyr::group_by(Group_Mean_Distance) %>%
    summarise(mean.CCR  = mean(CCR))

  ggplot(mean.acc1, aes(x = reorder(Nsim,mean.CCR ), y = mean.CCR, 
                            color = Group_Mean_Distance)) + geom_point()
  
  
    
  median.recall <- ssdf %>%  dplyr::group_by(Group_Mean_Distance,Nsim) %>%
    summarise(median.Recall = median(Recall_Cont))

  
  ggplot(median.recall, aes(x = reorder(Nsim,median.Recall ), y = median.Recall, 
                        color = Group_Mean_Distance)) + geom_point()
  
  

  # with NA 
  g1 <- ggplot(sdf, aes(x = Class_Proportion, y = CCR,color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Proportion") + ylim(0.65,1) 
  
  
  
  g2 <- ggplot(sdf, aes(x = as.factor(Number_Classes), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Number of classes") + ylim(0.65,1)
  
  g3 <- ggplot(sdf, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.65,1)
  
  
  g4 <- ggplot(sdf, aes(x = Group_Mean_Distance, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("MD","VD","VO"))+
    ylab("Test CCR") + xlab("Group mean distance") + ylim(0.65,1)
  
  g5 <- ggplot(sdf, aes(x = Number_Variables, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("5","100"))+
    ylab("Test CCR") + xlab("Number of variables") + ylim(0.65,1)
  
  g6 <- ggplot(sdf, aes(x = as.factor(Number_Separating_Variables) , y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("75","85"))+
    ylab("Test CCR") + xlab("Number of separating variables") + ylim(0.65,1)
  
  
  # to combine plots we required "patchwork" library
  combine <- g1 + g2 + g3 + g4 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  

# with very overlapping scenarios -----------------------------------------

  
  # with NA very overlapping scenarios
  g1 <- ggplot(sdf, aes(x = Class_Proportion, y = CCR,color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Proportion") + ylim(0.65,1) 
  
  
  
  g2 <- ggplot(sdf, aes(x = as.factor(Number_Classes), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Number of classes") + ylim(0.65,1)
  
  g3 <- ggplot(sdf, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.65,1)
  
  
  g4 <- ggplot(sdf, aes(x = Group_Mean_Distance, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("Medium distance","Very distance","Very Overlapping"))+
    ylab("Test CCR") + xlab("Group mean distance") + ylim(0.65,1)
  
  g5 <- ggplot(sdf, aes(x = Number_Variables, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("5","100"))+
    ylab("Test CCR") + xlab("Number of variables") + ylim(0.65,1)
  
  g6 <- ggplot(sdf, aes(x = Number_Separating_Variables , y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("75","85"))+
    ylab("Test CCR") + xlab("Training proportion") + ylim(0.65,1)
  
  
  # to combine plots we required "patchwork" library
  combine <- g1 + g2 + g3 + g4 + g5 + g6& theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  

  # without NA
  g100 <- ggplot(ssdf_no_na, aes(x = Class_Proportion, y = CCR,color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Proportion") + ylim(0.65,1) 
  
  g200 <- ggplot(ssdf_no_na, aes(x = as.factor(Number_Classes), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Number of classes") + ylim(0.65,1)
  
  g300 <- ggplot(ssdf_no_na, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.65,1)
  
  
  g400 <- ggplot(ssdf_no_na, aes(x = Group_Mean_Distance, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("Medium distance","Very distance","Very Overlapping"))+
    ylab("Test CCR") + xlab("Group mean distance") + ylim(0.65,1)

  g500 <- ggplot(ssdf_no_na, aes(x = Number_Variables, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("5","100"))+
    ylab("Test CCR") + xlab("Number of variables") + ylim(0.65,1)
  
  g600 <- ggplot(ssdf_no_na, aes(x = Number_Separating_Variables , y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("75","85"))+
    ylab("Test CCR") + xlab("Training proportion") + ylim(0.65,1)
  
  
  # to combine plots we required "patchwork" library
  combine <- g100 + g200 + g300 + g400  + g500 + g600 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  
  #in 100 dimensions
  g100 <- ggplot(ssdf100, aes(x = Class_Proportion, y = CCR,color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Proportion") + ylim(0.65,1) 
  
  
  
  g200 <- ggplot(ssdf100, aes(x = as.factor(Number_Classes), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Number of classes") + ylim(0.65,1)
  
  g300 <- ggplot(ssdf100, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.65,1)
  
  
  g400 <- ggplot(ssdf100, aes(x = Group_Mean_Distance, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("Medium distance","Very distance","Very Overlapping"))+
    ylab("Test CCR") + xlab("Group mean distance") + ylim(0.65,1)
  
  # to combine plots we required "patchwork" library
  combine <- g100 + g200 + g300 + g400 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")

  # For very overlapping cases  cross
        
  g110 <- ggplot(svo_df, aes(x = Class_Proportion, y = CCR,color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Proportion") + ylim(0.65,1) 
  
  
  
  g210 <- ggplot(svo_df, aes(x = as.factor(Number_Classes), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Number of classes") + ylim(0.65,1)
  
  g310 <- ggplot(svo_df, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.65,1)
  
  
  g410 <- ggplot(svo_df, aes(x = as.factor(Number_Variables), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("5","100"))+
    ylab("Test CCR") + xlab("Number of variables") + ylim(0.65,1)
  
  # to combine plots we required "patchwork" library
  combine <- g110 + g210 + g310 + g410 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  
  # Very overlapping excluding NA
  
  g120 <- ggplot(vo_df_no_na, aes(x = Class_Proportion, y = CCR,color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Proportion") + ylim(0.65,1) 
  
  
  g220 <- ggplot(vo_df_no_na, aes(x = as.factor(Number_Classes), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Number of classes") + ylim(0.65,1)
  
  g320 <- ggplot(vo_df_no_na, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.65,1)
  
  
  g420 <- ggplot(vo_df_no_na, aes(x = as.factor(Number_Variables), y = CCR, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("5","100"))+
    ylab("Test CCR") + xlab("Number of variables") + ylim(0.65,1)
  
  # to combine plots we required "patchwork" library
  combine <- g120 + g220 + g320 + g420 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  # Sensitivity for very overlapping with NA
  
  g130 <- ggplot(svo_df, aes(x = Class_Proportion, y = Recall_Cont,color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test Sensitivity") + xlab("Proportion") + ylim(0.15,1) 
  
  
  g230 <- ggplot(svo_df, aes(x = as.factor(Number_Classes), y = Recall_Cont, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test Sensitivity") + xlab("Number of classes") + ylim(0.15,1)
  
  g330 <- ggplot(svo_df, aes(x = Covariance_Structure, y = Recall_Cont, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    ylab("Test Sensitivity") + xlab("Covariance strucutre") + ylim(0.15,1)
  
  
  g430 <- ggplot(svo_df, aes(x = as.factor(Number_Variables), y = Recall_Cont, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("5","100"))+
    ylab("Test Sensitivity") + xlab("Number of variables") + ylim(0.15,1)
  
  g530 <- ggplot(svo_df, aes(x = Number_Variables, y = Recall_Cont, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("5","100"))+
    ylab("Test Sensitivity") + xlab("Number of variables") + ylim(0.65,1)
  
  g630 <- ggplot(svo_df, aes(x = Number_Separating_Variables , y = Recall_Cont, color = Variables)) +
    geom_boxplot_pattern(pattern_color = "white",
                         pattern_fill = "black",
                         aes(pattern= Variables))+
    scale_x_discrete(labels = c("75","85"))+
    ylab("Test Sensitivity") + xlab("Training proportion") + ylim(0.65,1)
  
  
  
  # to combine plots we required "patchwork" library
  combine <- g130 + g230 + g330 + g430  & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  summary_table <- ssdf_no_na %>% dplyr::select(Variables,Class_Proportion,CCR,Recall_Cont) 
  summary_table %>% dplyr::group_by(Variables,Class_Proportion) %>% summarise
    
# Plot of ordered simulations by CCR --------------------------------------


    
  ccr_ordered$Simulaciones <- rep( 1:groups,rep(reps,groups) )
  
  g100_all_set_variables <- ggplot(ccr_ordered, aes(Simulaciones,CCR)) + xlab("(a) - Ordered simulations by CCR for all set of variables (true, selected, and all)") +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  
  ccr_all_set_variables <- ccr_ordered
  # for CCR for only true variables
  ccr_df <- ssdf %>% filter(Variables == "True") %>% dplyr::select(File,CCR)
  
  ccr_df <- dplyr::mutate(group_by(ccr_df,File),meanCCR = mean(CCR))
  
  ccr_ordered <- dplyr::arrange(ccr_df,meanCCR)
  
  tail  (ccr_df)
  
  groups <- length(unique(ccr_df$File))
  reps <- 10
  
  ccr_ordered$Scenarios <- rep( 1:groups,rep(reps,groups) )
  
  g100_true_variables <- ggplot(ccr_ordered, aes(Scenarios,CCR)) + xlab( "(b) - Ordered simulations by CCR for true variables" ) +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  ccr_true_variables <- ccr_ordered
  # for CCR for only selected variables
  ccr_df <- ssdf %>% filter(Variables == "Selected") %>% dplyr::select(File,CCR)
  
  ccr_df <- mutate(group_by(ccr_df,File),meanCCR = mean(CCR))
  
  ccr_df
  
  ccr_ordered <- arrange(ccr_df,meanCCR)
  
  tail(ccr_df)
  
  groups <- length(unique(ccr_df$File))
  reps <- 10
  
  ccr_ordered$Scenarios <- rep( 1:groups,rep(reps,groups) )
  
  g100_selected_variables <- ggplot(ccr_ordered, aes(Scenarios,CCR)) + xlab( "(c) - Ordered simulations by CCR for selected variables") +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  ccr_selected_variables <- ccr_ordered
  
  # for CCR for all variables
  
  ccr_df <- ssdf %>% filter(Variables == "All") %>% dplyr::select(File,CCR)
  
  ccr_df <- mutate(group_by(ccr_df,File),meanCCR = mean(CCR))
  
  ccr_df
  
  ccr_ordered <- arrange(ccr_df,meanCCR)
  
  tail(ccr_df)
  
  groups <- length(unique(ccr_df$File))
  reps <- 10
  
  ccr_ordered$Scenarios <- rep( 1:groups,rep(reps,groups) )
  
  g100_all_variables <- ggplot(ccr_ordered, aes(Scenarios,CCR)) + xlab (" (d) - Ordered simulations by CCR for all variables") +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  ccr_all_variables <- ccr_ordered
  
  combine <- g100_all_set_variables + g100_true_variables + 
    g100_selected_variables + g100_all_variables & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  ggplot(ssdf, aes(x = Variables, y = CCR))  + 
    geom_point(size = 1,shape = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 colour = "black", width = 0.1,
                 position = position_nudge(x = 0.15)) +
    stat_summary(fun = mean, geom = "point",
                 size = 3,
                 position = position_nudge(x = 0.15)) +
    labs(x = "Set of variables", y = "CCR")
  
  par (cex = 0.6)
  colnames(ssdf)
  
  


  
  # Create plot CCR variability among and within set of variables ---------------
  
  
  # all set of variables
  ccr_df <- ssdf %>% dplyr::select(Nsim,CCR)
  
  ccr_df <- dplyr::mutate(group_by(ccr_df,Nsim), meanCCR = mean(CCR))
  
  ccr_ordered <- dplyr::arrange(ccr_df, meanCCR)
  
  head(ccr_df)
  tail(ccr_df)
  
  groups <- max(ccr_df$Nsim)
  reps <- 3
  
  ccr_ordered$Simulaciones <- rep( 1:groups,rep(reps,groups) )
  
  g100_all_set_variables <- ggplot(ccr_ordered, aes(Simulaciones,CCR)) + xlab("(a) - Ordered simulations by CCR for all set of variables (true, selected, and all)") +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  g100_all_set_variables <- ggplot(ccr_ordered, aes(Simulaciones,CCR)) + 
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  
  g100_all_set_variables
  
  ccr_all_set_variables <- ccr_ordered
  
  
  # for CCR for only true variables
  ccr_df <- ssdf %>% filter(Variables == "True") %>% dplyr::select(File,CCR)
  
  ccr_df <- dplyr::mutate(group_by(ccr_df,File),meanCCR = mean(CCR))
  
  ccr_ordered <- dplyr::arrange(ccr_df,meanCCR)
  
  tail  (ccr_df)
  
  groups <- length(unique(ccr_df$File))
  reps <- 10
  
  ccr_ordered$Scenarios <- rep( 1:groups,rep(reps,groups) )
  
  g100_true_variables <- ggplot(ccr_ordered, aes(Scenarios,CCR)) + xlab( "(b) - Ordered simulations by CCR for true variables" ) +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  ccr_true_variables <- ccr_ordered
  
  
  
  # for CCR for only selected variables
  ccr_df <- ssdf %>% filter(Variables == "Selected") %>% dplyr::select(File,CCR)
  
  ccr_df <- dplyr::mutate(group_by(ccr_df,File),meanCCR = mean(CCR))
  
  ccr_df
  
  ccr_ordered <- dplyr::arrange(ccr_df,meanCCR)
  
  tail(ccr_df)
  
  groups <- length(unique(ccr_df$File))
  reps <- 10
  
  ccr_ordered$Scenarios <- rep( 1:groups,rep(reps,groups) )
  
  g100_selected_variables <- ggplot(ccr_ordered, aes(Scenarios,CCR)) + xlab( "(c) - Ordered simulations by CCR for selected variables") +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  ccr_selected_variables <- ccr_ordered
  
  # for CCR for all variables
  
  ccr_df <- ssdf %>% dplyr::filter(Variables == "All") %>% dplyr::select(File,CCR)
  
  ccr_df <- dplyr::mutate(group_by(ccr_df,File),meanCCR = mean(CCR))
  
  ccr_df
  
  ccr_ordered <- dplyr::arrange(ccr_df,meanCCR)
  
  tail(ccr_df)
  
  groups <- length(unique(ccr_df$File))
  reps <- 10
  
  ccr_ordered$Scenarios <- rep( 1:groups,rep(reps,groups) )
  
  g100_all_variables <- ggplot(ccr_ordered, aes(Scenarios,CCR)) + xlab (" (d) - Ordered simulations by CCR for all variables") +
    stat_summary(fun.data = mean_se, geom = "linerange",
                 colour = "black")
  
  ccr_all_variables <- ccr_ordered
  
  combine <- g100_all_set_variables + g100_true_variables + 
    g100_selected_variables + g100_all_variables & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  
  # Create plot Sensitivity variability among and within groups -------------
  