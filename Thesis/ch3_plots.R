
# Load libraries ----------------------------------------------------------



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
library(ggplot2)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(cowplot)
library(patchwork)
library(ggpubr)



# Extract and arrange Dataset ---------------------------------------------

Smetrics_with_na <- readRDS("SMetrics_OLD_2024_03_18.RDS")
Smetrics_no_na <- readRDS("SMetrics_OLD_2024_03_18_no_na.RDS")

SSmetrics <- readRDS("SSMetrics_Old_2024_03_10.RDS")

options(scipen = 999)




ssdf <- SSmetrics 

ssdf$File <- factor(ssdf$File)
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



ssdf$Number_Separating_Variables_Included <- ifelse(ssdf$Number_Separating_Variables == 2, ssdf$IncludeX2 + ssdf$IncludeX4, ssdf$IncludeX2 + ssdf$IncludeX4 + ssdf$IncludeX5 )
ssdf$Number_Non_Informative_Variables_Included <- ssdf$Model_Size - ssdf$Number_Separating_Variables_Included

ssdf$Inclusion_Correctness <- ssdf$Number_Separating_Variables_Included/ssdf$Number_Separating_Variables
ssdf$Exclusion_Correctness <- 1-ssdf$Number_Non_Informative_Variables_Included/(ssdf$Number_Variables - ssdf$Number_Separating_Variables)



ssdf$Variables <- relevel(ssdf$Variables, ref = "True")

ssdf100 <- ssdf %>% dplyr::filter(Number_Variables == 100)
ssdf5 <- ssdf %>% dplyr::filter(Number_Variables == 5)






