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

library(flexplot)
library(MuMIn)    # R2 for the model
                  # contains functions to streamline information-theoretic model selection and carry out model averaging based on information criteria.

# ------------------------------------------------------------------------------

library(effectsize)

 library(nlme)

library(lmeInfo)
library(tidyr)

library(dplyr)
library(ggplot2)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(cowplot)
library(patchwork)
library(ggpubr)
library(ez)
library(multcomp)
library(car)
library(rstatix)


Smetrics <- readRDS("SMetrics_OLD_2024_03_18.RDS")
Smetrics_no_na <- readRDS("SMetrics_OLD_2024_03_18_no_na.RDS")

options(scipen = 999)
 
ssdf <- Smetrics 
ssdf_no_na <- Smetrics_no_na
sum(ssdf_no_na[is.na(ssdf_no_na)])


format_df <-function(df)
{
  df$File <- factor(df$File)
  
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


ssdf$Variables <- relevel(ssdf$Variables, ref = "True")

ssdf$Number_Separating_Variables_Included <- ifelse(ssdf$Number_Separating_Variables == 2, ssdf$IncludeX2 + ssdf$IncludeX4, ssdf$IncludeX2 + ssdf$IncludeX4 + ssdf$IncludeX5 )
ssdf$Number_Non_Informative_Variables_Included <- ssdf$Model_Size - ssdf$Number_Separating_Variables_Included

ssdf$Inclusion_Correctness <- ssdf$Number_Separating_Variables_Included/ssdf$Number_Separating_Variables
ssdf$Exclusion_Correctness <- 1-ssdf$Number_Non_Informative_Variables_Included/(ssdf$Number_Variables - ssdf$Number_Separating_Variables)


ssdf_no_na <- format_df(Smetrics_no_na)

format_df(ssdf_no_na)


levels(ssdf_no_na$Variables)

ssdf100 <- ssdf %>% dplyr::filter(Number_Variables == 100)
ssdf5 <- ssdf %>% dplyr::filter(Number_Variables == 5)

ssdf_no_na <- ssdf_no_na[complete.cases(ssdf_no_na),]
ssdf5_no_na <- ssdf5[complete.cases(ssdf5),]
ssdf100_no_na <- ssdf100[complete.cases(ssdf100),]

colnames(ssdf_no_na)


# Modelling CCR -----------------------------------------------------------


# Fit baseline
baseline <- lmer(CCR ~ 1 + (1|Nsim), data = ssdf_no_na)

icc(baseline)

visualize(baseline, plot = "model")

# Fit a reduced model
# research question do we need to have Variables have random slopes
fixed_slopes <- lmer(CCR ~ Variables + (1|Nsim), data = ssdf_no_na)
 
fixed_slopes <- lmer(CCR~File + (File|Nsim), data = ssdf_no_na)

visualize(fixed_slopes, plot = "model")


 model1000 <- lmer(CCR ~ Variables + Group_Mean_Distance + Class_Proportion + 
                    Number_Separating_Variables + Number_Variables +
                    Number_Observations + Covariance_Structure + (1|Nsim) +
                    (1|File), data = ssdf_no_na)

model1000


model1001.null <- lmer(CCR ~ Variables + Group_Mean_Distance + Class_Proportion + 
                    Number_Separating_Variables + Number_Variables +
                    Number_Observations + Covariance_Structure + (1|Nsim) 
                    , data = ssdf_no_na)

anova(model1001.null,model1000)

model1002 <- lmer(CCR ~ Variables + Group_Mean_Distance + Class_Proportion + 
                          Number_Separating_Variables + Number_Variables +
                          Number_Observations + Covariance_Structure + 
                          (1 + Variables |File), 
                          data = ssdf_no_na, REML = FALSE)

anova(model1000,model1002)

coef(model1002)


model1003 <- lmer(CCR ~ Variables + Group_Mean_Distance + Class_Proportion + 
                    Number_Separating_Variables + Number_Variables +
                    Number_Observations + Covariance_Structure + 
                    (1|Nsim), 
                  data = ssdf_no_na, REML = FALSE)

anova


coef(model1000)

 
 with(ssdf, interaction.plot(Number_Variables,Variables,CCR,
                            ylim = c(0.75,1), lty = c(1,2,3), lwd = 3,
                            ylab = "mean of CCR",xlab = "Number_Variables",trace.label = "Variables"))


with(ssdf, interaction.plot(Group_Mean_Distance,Variables,CCR,
                            ylim = c(0.75,1), lty = c(1,2,3), lwd = 3,
                            ylab = "mean of CCR",xlab = "Group mean distance",trace.label = "Variables"))




# models  -----------------------------------------------------------------


mod_CCR0 <- gls(CCR ~ Variables, data = ssdf_no_na,
                correlation = corSymm(form = ~1|Nsim))

summary(mod_CCR0)

color_variables <- sapply(ssdf_no_na$Variables,function(i) {
  if(i == "True") "red"
  else if(i == "Selected") "green"
  else if(i == "All") "blue"
})


plot(mod_CCR0, col = color_variables)

plot(residuals(mod_CCR0), ylab = residuals, col = color_variables)

plot( 
  x = mod_CCR0,
  form = resid(., type = "p") ~ fitted(.) | Variables,
  abline = 0
  )

plot (
  x = mod_CCR0,
  form = Variables~ resid(.)
)

ACF(
  object = mod_CCR0,
  form = ~ 1 | Nsim
)

plot(ACF(
  object = mod_CCR0,
  form = ~ 1 | Nsim
))

qqnorm(
  y = mod_CCR0,
  abline = c(0,1)
)

gls_CCR0_varPower <- update(object = mod_CCR0,
                            weights = varPower() )

qqnorm(
  y = gls_CCR0_varPower,
  abline = c(0,1)
)


mod_CCR10 <- gls(CCR ~ Variables + Number_Classes  + 
                  Group_Mean_Distance+ Class_Proportion + 
                  Covariance_Structure + Number_Variables ,
               data = ssdf,
               correlation = corSymm(form = ~ 1|Nsim))


qqnorm(
  y = mod_CCR10,
  abline = c(0,1)
)

summary(mod_CCR10)
  
anova(mod_CCR)

colnames(ssdf)


mod_CCR50_no_interactions <- gls(CCR ~   Variables + Number_Classes + Number_Separating_Variables + Number_Variables+  
                    Class_Proportion  + Covariance_Structure + Group_Mean_Distance + Model_Size  ,
                 data = ssdf_no_na,
                 correlation = corSymm(form = ~ 1|Nsim))

anova(mod_CCR50_no_interactions)
summary(mod_CCR50_no_interactions)

anova(mod_CCR50_interactions,mod_CCR50_no_interactions)

qqnorm(
  y = mod_CCR50_no_interactions,
  abline = c(0,1)
)

summary(mod_CCR50_no_interactions)

intervals(mod_CCR50_no_interactions)

 
mod_CCR50_interactions <- gls(CCR ~   as.factor(Number_Classes)*Variables + Number_Separating_Variables*Variables + as.factor(Number_Variables)*Variables 
                 + Group_Mean_Distance * Variables+ Class_Proportion*Variables + 
                 Covariance_Structure*Variables ,
               data = ssdf_no_na,
               correlation = corSymm(form = ~ 1|Nsim))
library(car)


summary(mod_CCR50_interactions)
anova_CCR50 <- anova (mod_CCR50_interactions)
anova_CCR50

qqnorm(
  y = mod_CCR50_interactions,
  abline = c(0,1)
)


mod_Sensitivity_interactions <- gls(Recall_Cont ~   as.factor(Number_Classes)*Variables + Number_Separating_Variables*Variables + as.factor(Number_Variables)*Variables 
                              + Group_Mean_Distance * Variables+ Class_Proportion*Variables + 
                                Covariance_Structure*Variables ,
                              data = ssdf_no_na,
                              correlation = corSymm(form = ~ 1|Nsim))

summary(mod_Sensitivity_interactions)
anova(mod_Sensitivity_interactions)


mod5_Sensitivit_no_interactions <- gls(CCR ~  as.factor(Number_Classes)*Variables + Number_Separating_Variables*Variables + as.factor(Number_Variables)*Variables 
                                       The true variables is set as a baseline                    + Group_Mean_Distance  Class_Proportion*Variables + 
                                Covariance_Structure ,
                              data = ssdf5_no_na,
                              correlation = corSymm(form = ~ 1|Nsim))


library(car)



# Manova ------------------------------------------------------------------


yvars <- cbind(ssdf_no_na$CCR,ssdf_no_na$Recall_Cont)
treat_factor <- ssdf_no_na$Variables
rep_factor <- ssdf_no_na$Nsim

manova_mod <- manova (yvars ~ rep_factor + treat_factor + Number_Observations
                      , data= ssdf_no_na)

manova_mod

summary(manova_mod, test = "Wilks", intercept = TRUE)



