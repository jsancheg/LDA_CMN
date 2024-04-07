

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

# test set by classes

# 19  fillet black dot denotes a TN ( correct prediction of uncontaminated observation for either 1st or 2nd class)
# 17  filled black triangle  denotes a TP (correct prediction of contaminated observation for either 1st or 2nd class)
#  3  + denotes a FP (wrongly predicted as contaminated observation when it was uncontaminated observation belonging to the 1st class)  
#  4  x  denotes a FN (wrongly predicted as uncontaminated observation when it was contaminated observation belonging to the 1st class)
#  1  circle  denotes a FP (wrongly predicted as contaminated observation when it was uncontaminated observation belonging to the 2nd class)
#  8  * star  denotes a FN (wrongly predicted as uncontaminated observation when it was contaminated observation belonging to the 2nd class) 

pairs(Xtest, panel = function(x,y, ...) {
  points(x,y, 
         col = ifelse(ltest == 1 ,"blue", "green") ,
         pch = ifelse(ltest == 1 ,9,10),
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
  
  sm_dif <-readRDS("sMetrics_wider_with_dif_2024_03_18.RDS")
  

  missing_values
  options(scipen = 999)
  

# plots Chapter 3 differences ---------------------------------------------

sdf <- sm_dif  


# Group mean distance fixed vs other factor varied ----------------------

  
  colnames(sdf)
  
  
  sdf <- sdf %>% mutate(diffSensitivity_class_sm_tm = diffSensitiviy_class_sm_tm) %>% select(c(-diffSensitiviy_class_sm_tm) )
  
  
  df_long <- sdf %>% pivot_longer(c(diffccr_class_sm_tm, diffccr_class_all_tm, diffccr_class_sm_all, diffSensitivity_class_sm_tm,
                                    diffSensitivity_class_all_tm, diffSensitivity_class_sm_all, diffSpecificity_class_sm_tm,  
                                    diffSpecificity_class_all_tm, diffSpecificity_class_sm_all, diffPrecision_class_sm_tm,
                                    diffPrecision_class_all_tm,   diffPrecision_class_sm_all,   diffF1_class_sm_tm,          
                                    diffF1_class_all_tm,          diffF1_class_sm_all,          diffCCRV_sm_tm, 
                                    diffCCRV_all_tm,              diffCCRV_sm_all,  diffSensitivityV_sm_tm,   diffSensitivityV_all_tm,
                                    diffSensitivityV_sm_all,      diffSpecificityV_sm_tm,       diffSpecificityV_all_tm,     
                                    diffSpecificityV_sm_all,      diffPrecisionV_sm_tm,         diffPrecisionV_all_tm,  
                                    diffPrecisionV_sm_all,        diffF1V_sm_tm, diffF1V_all_tm, diffF1V_sm_all  ),
                                  names_to = "Comparison",
                                  values_to = "Dif")
    
    unique(df_long$Comparison)
  
  g3.10 <- ggplot(df_long %>% filter(Comparison %in% c("diffccr_class_sm_tm","diffccr_class_all_tm","diffccr_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Group_Mean_Distance))  +
    ylab("Dif. in test class CCR") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.10  
  
          
  g3.20 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivity_class_sm_tm","diffSensitivity_class_all_tm","diffSensitivity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Group_Mean_Distance))  +
    ylab("Dif. in test class Sensitivity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.20
  
  
  df_long %>% select(Group_Mean_Distance,Comparison,Dif) %>% filter(Comparison == "diffSensitivity_class_all_tm") %>% 
    group_by(Group_Mean_Distance,Comparison) %>% 
    summarise_at(vars("Dif"), mean) 
  
      
  df_long %>% select(Group_Mean_Distance,Comparison,Dif) %>% filter(Comparison == "diffSensitivity_class_sm_all") %>% 
    group_by(Group_Mean_Distance,Comparison) %>% 
  summarise_at(vars("Dif"), mean) 
  
  g3.30 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Group_Mean_Distance))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.30  
  
  
  g3.40 <- ggplot(df_long %>% filter(Comparison %in% c("diffCCRV_sm_tm","diffCCRV_all_tm","diffCCRV_sm_all")),
                  aes(x = Comparison, y = Dif, color = Group_Mean_Distance))  +
    ylab("Dif. in test contamination CCR") + xlab("Model comparisons") + ylim (-0.5,0.5) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.40  
  
  
  g3.50 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivityV_sm_tm","diffSensitivityV_all_tm","diffSensitivityV_sm_all")),
                  aes(x = Comparison, y = Dif, color = Group_Mean_Distance))  +
    ylab("Dif. in test contamination Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.50  
  
  df_long %>% select(Group_Mean_Distance,Comparison,Dif) %>% filter(Comparison == "diffSensitivityV_all_tm") %>% 
    group_by(Group_Mean_Distance,Comparison) %>% 
    summarise_at(vars("Dif"), mean) 
  
  df_long %>% select(Group_Mean_Distance,Comparison,Dif) %>% filter(Comparison == "diffSensitivityV_sm_all") %>% 
    group_by(Group_Mean_Distance,Comparison) %>% 
    summarise_at(vars("Dif"), mean) 
  
  
  df_long %>% select(Group_Mean_Distance,Comparison,Dif) %>% filter(Comparison == "diffSensitivityV_sm_tm") %>% 
    group_by(Group_Mean_Distance,Comparison) %>% 
    summarise_at(vars("Dif"), mean) 
  
  g3.60 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Group_Mean_Distance))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.60  
  
  
  
  # to combine plots we required "patchwork" library
  combine <- g3.10 + g3.20 + g3.30 + g3.40  + g3.50 + g3.60 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")

  
  
  

# Number of classes fixed vs other factor varied --------------------------


  df_long$Number_Classes <- factor(df_long$Number_Classes)
  
  g3.70 <- ggplot(df_long %>% filter(Comparison %in% c("diffccr_class_sm_tm","diffccr_class_all_tm","diffccr_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Classes ))  +
    ylab("Dif. in test class CCR") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  
  g3.70  
  
  
  g3.80 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivity_class_sm_tm","diffSensitivity_class_all_tm","diffSensitivity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Classes))  +
    ylab("Dif. in test class Sensitivity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.80  
  
  
  
  g3.90 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Classes))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.90  
  
  
  g3.100 <- ggplot(df_long %>% filter(Comparison %in% c("diffCCRV_sm_tm","diffCCRV_all_tm","diffCCRV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Classes))  +
    ylab("Dif. in test contamination CCR") + xlab("Model comparisons") + ylim (-0.5,0.5) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  g3.100  
  
  
  g3.110 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivityV_sm_tm","diffSensitivityV_all_tm","diffSensitivityV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Classes))  +
    ylab("Dif. in test contamination Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.110  
  
  
  
  g3.120 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Classes))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.120  
  
  # to combine plots we required "patchwork" library
  combine <- g3.70 + g3.80 + g3.90 + g3.100  + g3.110 + g3.120 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  

# Class Proportion fixed while other factor varied ------------------------


  df_long$Class_Proportion <- factor(df_long$Class_Proportion)
  
  g3.130 <- ggplot(df_long %>% filter(Comparison %in% c("diffccr_class_sm_tm","diffccr_class_all_tm","diffccr_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Class_Proportion ))  +
    ylab("Dif. in test class CCR") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  
  g3.130  
  
  
  g3.140 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivity_class_sm_tm","diffSensitivity_class_all_tm","diffSensitivity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Class_Proportion))  +
    ylab("Dif. in test class Sensitivity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.140  
  
  
  
  g3.150 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Class_Proportion))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.150  
  
  
  g3.160 <- ggplot(df_long %>% filter(Comparison %in% c("diffCCRV_sm_tm","diffCCRV_all_tm","diffCCRV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Class_Proportion))  +
    ylab("Dif. in test contamination CCR") + xlab("Model comparisons") + ylim (-0.5,0.5) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  g3.160  
  
  
  g3.170 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivityV_sm_tm","diffSensitivityV_all_tm","diffSensitivityV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Class_Proportion))  +
    ylab("Dif. in test contamination Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.170  
  
  
  
  g3.180 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Class_Proportion))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.180  
  
  # to combine plots we required "patchwork" library
  combine <- g3.130 + g3.140 + g3.150 + g3.160  + g3.170 + g3.180 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  

# Number of variables fixed vs other factors varied -----------------------

  
  df_long$Number_Variables <- factor(df_long$Number_Variables)
  
  g3.190 <- ggplot(df_long %>% filter(Comparison %in% c("diffccr_class_sm_tm","diffccr_class_all_tm","diffccr_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Variables ))  +
    ylab("Dif. in test class CCR") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  
  g3.190  
  
  
  g3.200 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivity_class_sm_tm","diffSensitivity_class_all_tm","diffSensitivity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Variables))  +
    ylab("Dif. in test class Sensitivity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.200  
  
  
  
  g3.210 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Variables))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.210  
  
  
  g3.220 <- ggplot(df_long %>% filter(Comparison %in% c("diffCCRV_sm_tm","diffCCRV_all_tm","diffCCRV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Variables))  +
    ylab("Dif. in test contamination CCR") + xlab("Model comparisons") + ylim (-0.5,0.5) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  g3.220  
  
  
  g3.230 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivityV_sm_tm","diffSensitivityV_all_tm","diffSensitivityV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Variables))  +
    ylab("Dif. in test contamination Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.230  
  
  
  
  g3.240 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Variables))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.240  
  
  # to combine plots we required "patchwork" library
  combine <- g3.190 + g3.200 + g3.210 + g3.220  + g3.230 + g3.240 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  

# Percentage of samples in training fixed and ohter factor varied ---------

  
  df_long$Training_Proportion <- factor(df_long$Training_Proportion)
  
  g3.250 <- ggplot(df_long %>% filter(Comparison %in% c("diffccr_class_sm_tm","diffccr_class_all_tm","diffccr_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Training_Proportion ))  +
    ylab("Dif. in test class CCR") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  
  g3.250  
  
  
  g3.260 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivity_class_sm_tm","diffSensitivity_class_all_tm","diffSensitivity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Training_Proportion))  +
    ylab("Dif. in test class Sensitivity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.260  
  
  
  
  g3.270 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Training_Proportion))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-0.3,0.3) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.270  
  
  
  g3.280 <- ggplot(df_long %>% filter(Comparison %in% c("diffCCRV_sm_tm","diffCCRV_all_tm","diffCCRV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Training_Proportion))  +
    ylab("Dif. in test contamination CCR") + xlab("Model comparisons") + ylim (-0.5,0.5) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  g3.280  
  
  g3.290 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivityV_sm_tm","diffSensitivityV_all_tm","diffSensitivityV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Training_Proportion))  +
    ylab("Dif. in test contamination Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.290  
  
  
  
  g3.300 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Training_Proportion))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.300  
  
  # to combine plots we required "patchwork" library
  combine <- g3.250 + g3.260 + g3.270 + g3.280  + g3.290 + g3.300 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  

# Covariance Structure fixed vs other factor varied -----------------------

  
  g3.310 <- ggplot(df_long %>% filter(Comparison %in% c("diffccr_class_sm_tm","diffccr_class_all_tm","diffccr_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Covariance_Structure))  +
    ylab("Dif. in test class CCR") + xlab("Model comparisons") + ylim (-0.2,0.2) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.310  
  
  
  g3.320 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivity_class_sm_tm","diffSensitivity_class_all_tm","diffSensitivity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Covariance_Structure))  +
    ylab("Dif. in test class Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.320  
  
  
  
  g3.330 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Covariance_Structure))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.330  
  
  
  g3.340 <- ggplot(df_long %>% filter(Comparison %in% c("diffCCRV_sm_tm","diffCCRV_all_tm","diffCCRV_sm_all")),
                  aes(x = Comparison, y = Dif, color = Covariance_Structure))  +
    ylab("Dif. in test contamination CCR") + xlab("Model comparisons") + ylim (-0.2,0.2) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.340  
  
  
  g3.350 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivityV_sm_tm","diffSensitivityV_all_tm","diffSensitivityV_sm_all")),
                  aes(x = Comparison, y = Dif, color = Covariance_Structure))  +
    ylab("Dif. in test contamination Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.350  
  
  
  
  g3.360 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                  aes(x = Comparison, y = Dif, color = Covariance_Structure))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.360  

  
  # to combine plots we required "patchwork" library
  combine <- g3.310 + g3.320 + g3.330 + g3.340  + g3.350 + g3.360 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  

# Number of separating variables fixed other factors varied ---------------
df_long$Number_Separating_Variables <- factor(df_long$Number_Separating_Variables)
  
  g3.370 <- ggplot(df_long %>% filter(Comparison %in% c("diffccr_class_sm_tm","diffccr_class_all_tm","diffccr_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Separating_Variables))  +
    ylab("Dif. in test class CCR") + xlab("Model comparisons") + ylim (-0.2,0.2) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.370  
  
  
  g3.380 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivity_class_sm_tm","diffSensitivity_class_all_tm","diffSensitivity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Separating_Variables))  +
    ylab("Dif. in test class Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.380  
  
  
  
  g3.390 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Separating_Variables))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.390  
  
  
  g3.400 <- ggplot(df_long %>% filter(Comparison %in% c("diffCCRV_sm_tm","diffCCRV_all_tm","diffCCRV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Separating_Variables))  +
    ylab("Dif. in test contamination CCR") + xlab("Model comparisons") + ylim (-0.5,0.5) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.400  
  
  
  g3.410 <- ggplot(df_long %>% filter(Comparison %in% c("diffSensitivityV_sm_tm","diffSensitivityV_all_tm","diffSensitivityV_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Separating_Variables))  +
    ylab("Dif. in test contamination Sensitivity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.410  
  
  
  
  g3.420 <- ggplot(df_long %>% filter(Comparison %in% c("diffSpecificity_class_sm_tm","diffSpecificity_class_all_tm","diffSpecificity_class_sm_all")),
                   aes(x = Comparison, y = Dif, color = Number_Separating_Variables))  +
    ylab("Dif. in test class Specificity") + xlab("Model comparisons") + ylim (-1,1) + geom_boxplot() +
    geom_hline(yintercept = 0) +
    scale_x_discrete(labels = c("ALL - TM","SM - ALL","SM - TM"))
  
  g3.420  
  
  
  # to combine plots we required "patchwork" library
  combine <- g3.370 + g3.380 + g3.390 + g3.400  + g3.410 + g3.420 & theme(legend.position = "bottom")
  combine + plot_layout(guides = "collect")
  
  

# Variables selected  by the greedy search --------------------------------

  # case for two classes inclusion and exclusion
  colnames(sdf)
  sdf$Number_Separating_Variables_Included <- ifelse(sdf$Number_Separating_Variables == 2, sdf$IncludeX2 + sdf$IncludeX4, sdf$IncludeX2 + sdf$IncludeX4 + sdf$IncludeX5 )
  sdf$Number_Non_Informative_Variables_Included <- sdf$Nvars_SM - sdf$Number_Separating_Variables_Included
  
  sdf$Inclusion_Correctness <- sdf$Number_Separating_Variables_Included/sdf$Number_Separating_Variables
  sdf$Exclusion_Correctness <- 1-sdf$Number_Non_Informative_Variables_Included/(sdf$Number_Variables - sdf$Number_Separating_Variables)
  
  
  
  # for 5 variables and 3 groups
  
  library(gridExtra)
  
  sdf %>% filter(Number_Variables == 5, Number_Separating_Variables == 3) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
    summarise_at(c("Nvars_SM","IncludeX2", "IncludeX4", "IncludeX5",  "Inclusion_Correctness", "Exclusion_Correctness"), mean) 

  in_cases_5_3 <- sdf  %>% filter(Number_Variables == 5, Number_Separating_Variables == 3,Variables == "Selected", 
                                    Number_Separating_Variables_Included == 0) %>% dplyr::select(File) %>%
    dplyr::group_by(File) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))
  
  in_cases_5_3
  View(in_cases_5_3)
  
   sdf  %>% filter(Number_Variables == 5, Number_Separating_Variables == 3,Variables == "Selected", 
                                  Number_Separating_Variables_Included == 0) %>% dplyr::select(File)
  
  # for 100 variables and 3 groups
  
  sdf %>% filter(Number_Variables == 100, Number_Separating_Variables == 3) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
    summarise_at(c("Nvars_SM","IncludeX2","IncludeX4","IncludeX5","Inclusion_Correctness","Exclusion_Correctness"), mean) 

  in_cases_100_3 <- sdf  %>% filter(Number_Variables == 100, Number_Separating_Variables == 3,Variables == "Selected", 
                                  Number_Separating_Variables_Included == 0) %>% dplyr::select(File) %>%
    dplyr::group_by(File) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))
  
  in_cases_100_3
  View(in_cases_100_3)
  
    in_heq_15nsv_100_3 <- sdf  %>% filter(Number_Variables == 100, Number_Separating_Variables == 3,Variables == "Selected", 
                                       Model_Size >= 15) %>% dplyr::select(File, Model)  %>%
      dplyr::group_by(File,Model) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))
    
    in_heq_15nsv_100_3
  # for 5 variables and 2 groups
  
  sdf %>% filter(Number_Variables == 5, Number_Separating_Variables == 2) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
    summarise_at(vars("Nvars_SM","IncludeX2", "IncludeX4",  "Inclusion_Correctness", "Exclusion_Correctness"), mean) 
  
  in_cases_5_2 <- sdf  %>% filter(Number_Variables == 5, Number_Separating_Variables == 2,Variables == "Selected", 
                                    Number_Separating_Variables_Included == 0) %>% dplyr::select(File) %>%
    dplyr::group_by(File) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))
  
  in_cases_5_2
  View(in_cases_5_2)
  
  ex_cases_5_2 <- sdf %>% filter(Number_Variables == 5, Number_Separating_Variables ==2, Variables == "Selected",
                                 Exclusion_Correctness == 0) %>% dplyr::select(File) %>% 
    dplyr::group_by(File) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))

  ex_cases_5_2
  View(ex_cases_5_2)
  # for 100 variables and 2 groups
  
  sdf  %>% filter(Number_Variables == 100, Number_Separating_Variables == 2) %>% dplyr::group_by(Number_Separating_Variables,Number_Variables) %>%
    summarise_at(c("Nvars_SM","IncludeX2","IncludeX4","Inclusion_Correctness","Exclusion_Correctness"), mean) 
  
  in_cases_100_2 <- sdf  %>% filter(Number_Variables == 100, Number_Separating_Variables == 2,Variables == "Selected", 
                                 Number_Separating_Variables_Included == 0) %>% dplyr::select(File) %>%
    dplyr::group_by(File) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))
      
  in_cases_100_2
  View(in_cases_100_2)
  
  ex_cases_100_2 <- sdf %>% filter(Number_Variables == 100, Number_Separating_Variables ==2, Variables == "Selected",
                                 Exclusion_Correctness == 0) %>% dplyr::select(File,Model_Size)

  ex_cases_100_2
  
# Inclusion boxplots ------------------------------------------------------
  sd2 <- sdf %>% filter(Number_Separating_Variables == 2 )
  sd2_5 <- sdf %>% filter(Number_Separating_Variables == 2 & Number_Variables == 5)
  sd2_100 <- sdf %>% filter(Number_Separating_Variables == 2 & Number_Variables == 100)
  
  quantile(sd2_5$Nvars_SM,c(0.25,0.5,0.75))
  quantile(sd2_100$Nvars_SM,c(0.25,0.5,0.75))
  
  sd3 <- sdf %>% filter(Number_Separating_Variables == 3 )
  sd3_5 <- sdf %>% filter(Number_Separating_Variables == 3 & Number_Variables == 5 )
  sd3_100 <- sdf %>% filter(Number_Separating_Variables == 3 & Number_Variables == 100 )
  
  quantile(sd3_5$Nvars_SM,c(0.25,0.5,0.75))
  quantile(sd3_100$Nvars_SM,c(0.25,0.5,0.75))
  
  
  # for 2 separating variables
  boxplot(Nvars_SM ~ Number_Variables, data = sd2, col = "lightblue",
          xlab = "Number of variables", 
          ylab = "Number of variables included in the model")
  
colnames(sd2)

  boxplot(Inclusion_Correctness ~ Number_Variables, data = sd2, col = "lightblue",
           xlab = "Number of variables", 
           ylab = "Inclusion Correctness")

  quantile(sd2_5$Inclusion_Correctness,c(0.25,0.5,0.75))
  
  quantile(sd2_100$Inclusion_Correctness,c(0.25,0.5,0.75))
  

 boxplot(Exclusion_Correctness ~ Number_Variables, data = sd2, col = "lightblue",
           xlab = "Number of variables", 
           ylab = "Exclusion Correctness")
   
 quantile(sd2_5$Exclusion_Correctness,c(0.25,0.5,0.75))
 
 quantile(sd2_100$Exclusion_Correctness,c(0.25,0.5,0.75))
 
   
  # for 3 separating variables
  colnames(sd3)

  boxplot(Nvars_SM ~ Number_Variables, data = sd3, col = "lightblue",
          xlab = "Number of variables", 
          ylab = "Number of variables included in the model")
  
  plot3 <- boxplot(Inclusion_Correctness ~ Number_Variables, data = sd2, col = "lightblue",
          xlab = "Number of variables", 
          ylab = "Inclusion Correctness")
  
  plot3
  
  plot4 <- boxplot(Exclusion_Correctness ~ Number_Variables, data = sd2, col = "lightblue",
          xlab = "Number of variables", 
          ylab = "Exclusion Correctness")
  
  
  
  grid.arrange(plot3,plot4 , ncol = 2)
  
  boxplot(sd3$Inclusion_Correctness, sd2$Exclusion_Correctness, col = c("lightblue","lightblue"),
          names = c("Inclusion Correctness", "Exclusion Correctness"))
  

    boxplot()
  