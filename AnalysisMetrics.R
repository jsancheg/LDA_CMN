MetricsPath <- "/home/jsancheg/Documents/LDA_CMN/"
library(dplyr)
library(lme4)
library(stringr)
library(tidyr)
library(ggplot2)
library(gridExtra)

dir(MetricsPath)
MetricsDf <- readRDS(paste0(MetricsPath,"Metrics.RDS"))

MetricsDf$Simulation <- 1:nrow(MetricsDf)
MetricsDf <- MetricsDf %>% relocate( Simulation,
                                   .before = Setting )

colnames(MetricsDf)
head(MetricsDf)

MetricsDf <- MetricsDf %>% mutate(Proportion = 
                                    ifelse(str_detect(MetricsDf$Setting, "5050"), "BAL","UNBAL") )

MetricsDf <- MetricsDf %>% mutate(Number_Separating_Variables = 
                                    str_split(MetricsDf$Setting,"_",simplify = TRUE)[,3])

MetricsDf <- MetricsDf %>% mutate(Covariance_Structure = 
                                    str_split(MetricsDf$Setting,"_",simplify = TRUE)[,6])

MetricsDf <- MetricsDf %>% mutate(Group_Mean_Distance = 
                                    str_split(MetricsDf$Setting,"_",simplify = TRUE)[,7])

head(MetricsDf)
colnames(MetricsDf)

auxDf1 <- MetricsDf %>% dplyr::select(Setting,Simulation,AccuracyTM,AccuracySM,AccuracySaturatedM,
                                      Accuracy_TM_contaminated,Accuracy_SM_contaminated,
                                      Accuracy_Saturated_Cont, 
                                      Accuracy_TM_no_contaminated,Accuracy_SM_no_contaminated,
                                      Accuracy_Saturated_NoCont,
                                      Precision_TM,Precision_SM,Precision_SaturatedM,
                                      Recall_TM,Recall_SM,Recall_SaturatedM,
                                      F1_TM,F1_SM,F1_SaturatedM,
                                      precision_saturated_V,precision_SM_V,precision_TM_V,
                                      recall_saturated_V,recall_SM_V,recall_TM_V,
                                      F1_Saturated_V,F1_SM_V,F1_TM_V)

colnames(auxDf1)
# 1: True
# 2: Selected
# 3: Complete

head(MetricsDf)



auxDf1 <- auxDf1 %>% rename("A1" = "AccuracyTM" ,
                            "A2"= "AccuracySM" ,
                            "A3" = "AccuracySaturatedM" ,
                            "N1" = "Accuracy_TM_no_contaminated",
                            "N2" = "Accuracy_SM_no_contaminated",
                            "N3" = "Accuracy_Saturated_Cont",
                            "C1" = "Accuracy_TM_contaminated"  ,
                            "C2" = "Accuracy_SM_contaminated"  ,
                            "C3" = "Accuracy_Saturated_NoCont" ,
                            "P1" = "Precision_TM"  ,
                            "P2" = "Precision_SM" ,
                            "P3" = "Precision_SaturatedM" ,
                            "R1" = "Recall_TM" ,
                            "R2" = "Recall_SM" ,
                            "R3" = "Recall_SaturatedM",
                            "F1" = "F1_TM",
                            "F2" = "F1_SM",
                            "F3" = "F1_SaturatedM",
                            "V1" = "precision_TM_V" ,
                            "V2" = "precision_SM_V" ,
                            "V3" = "precision_saturated_V",
                            "W1" = "recall_TM_V",
                            "W2" = "recall_SM_V",
                            "W3" = "recall_saturated_V",
                            "Z1" = "F1_TM_V",
                            "Z2" = "F1_SM_V",
                            "Z3" = "F1_Saturated_V"
)

auxDf1 <- auxDf1 %>% 
  pivot_longer(
    cols = A1:Z1,
    names_to = c(".value","Variables"),
    names_pattern = "(.)(.)"
  )


colnames(auxDf1)
head(auxDf1)

auxDf1 <- auxDf1 %>% rename("Accuracy_class" = "A" ,
                            "Accuracy_Cont" = "C"  ,
                            "Accuracy_No_Cont" = "N",
                            "Precision_Class" = "P"  ,
                            "Recall_Class" = "R" ,
                            "F1_Class" = "F",
                            "Precicison_Cont" = "V" ,
                            "Recall_Cont" = "W",
                            "F1_Cont" = "Z")

colnames(auxDf1)
head(auxDf1)
auxDf1 <- auxDf1 %>% mutate(Proportion = 
                              ifelse(str_detect(auxDf1$Setting, "5050"), "BAL","UNBAL") )

auxDf1 <- auxDf1 %>% mutate(Number_of_classes = 
                              str_split(auxDf1$Setting,"_",simplify = TRUE)[,3])

auxDf1 <- auxDf1 %>% mutate(Covariance_Structure = 
                              str_split(auxDf1$Setting,"_",simplify = TRUE)[,6])

auxDf1 <- auxDf1 %>% mutate(Group_Mean_Distance = 
                              str_split(auxDf1$Setting,"_",simplify = TRUE)[,7])



auxDf1 <- auxDf1 %>% relocate(c(Proportion, Number_of_classes,
                                Covariance_Structure,Group_Mean_Distance),
                              .after = Setting )

MetricsDf1 <- auxDf1 %>% mutate(Variables = recode(Variables,
                                                   '1' = "True",
                                                   '2' = "Selected",
                                                   '3' = "All"
))

#MetricsDf1 <- auxDf1 %>% mutate(Covariance_Structure = recode(Covariance_Structure,
#                                                   'SCBSV' = "SV",
#                                                   'SCBSNSV' = "SNSV",
#                                                   'SCBNSV' = "NSV",
#                                                   'IND' = 'IND'
#))

colnames(MetricsDf1)
head(MetricsDf1)


unique(MetricsDf1$Covariance_Structure)
colnames(MetricsDf)
MetricsDf %>% select(c(AccuracyTM,AccuracySM,AccuracySaturatedM)) %>%
  cor(use = "pairwise.complete.obs")

var.acc1 <- MetricsDf1 %>% group_by(Group_Mean_Distance) %>%
  summarise(mean.accuracy  = var(Accuracy_class))

var.acc2 <- MetricsDf1 %>% group_by(Number_of_classes) %>%
  summarise(mean.accuracy  = var(Accuracy_class))

var.acc3 <- MetricsDf1 %>% group_by(Covariance_Structure) %>%
  summarise(mean.accuracy = var(Accuracy_class))


mean.acc1 <- MetricsDf1 %>% group_by(Group_Mean_Distance,Simulation) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
  
ggplot(mean.acc1, aes(x = reorder(Simulation,mean.accuracy ), y = mean.accuracy, 
                      color = Group_Mean_Distance)) + geom_point()


  
g1<- ggplot(MetricsDf1, aes(x = Proportion, y = Accuracy_class, color = Variables)) +
  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Proportion")

g2 <- ggplot(MetricsDf1, aes(x = Number_of_classes, y = Accuracy_class, 
                       color = Variables)) + 
  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Number of classes")


g3 <- ggplot(MetricsDf1, aes(x = Covariance_Structure, y = Accuracy_class, 
                       color = Variables)) + 
  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Covariance structure")


g4 <- ggplot(MetricsDf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                       color = Variables)) + 
  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Group mean distance")

gr1 <- arrangeGrob(g1,g2, ncol = 1, nrow = 2)
plot(gr1)

gr2 <- arrangeGrob(g3, g4, ncol = 2, nrow = 2,
                   layout_matrix =  rbind(c(1,1),c(2,2) ) )
plot(gr2)


g5 <- MetricsDf1 %>% ggplot(mapping = aes(sample = Accuracy_class)) +
  geom_qq() +
  facet_wrap(facets = ~ Variables, scales = "free") +
  theme_bw()

g6 <- ggplot(MetricsDf1, aes(x = Accuracy_class, y = ..density.., fill = Variables )) +
  geom_density(alpha = 0.25) + 
  labs(tittle = "Distribution of Accuracy  for set of variables") + xlab("Accuracy") 

gr3 <- arrangeGrob(g5,g6,ncol=1,nrow = 2)
plot(gr3)

plot(gr3)
MetricsDf1 %>% ggplot(mapping = aes(sample = Accuracy_class)) +
  geom_qq() +
  facet_wrap(facets = ~ Group_Mean_Distance, scales = "free") +
  theme_bw()

MetricsDf1 %>% ggplot(mapping = aes(sample = Accuracy_class)) +
  geom_qq() +
  facet_wrap(facets = ~ Covariance_Structure, scales = "free") +
  theme_bw()


colnames(MetricsDf1)

MetricsDf1 %>% select(c(Accuracy_class,Variables)) %>% data.frame()  %>%
  pairwise.t.test(Accuracy_class ~ Variables, paired = TRUE,
                  p.adjust.method = "bonferroni")

library(nlme)
mod100 <- gls( Accuracy_class ~   Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance, data = MetricsDf1,
               correlation = corSymm(form = ~1|Simulation),
               weights = varIdent(form = ~1|Group_Mean_Distance))

summary(mod100)
anova(mod100)
plot(mod100)


mod110 <- gls( Accuracy_class ~   Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance + Variables*Proportion + 
                 Variables*Number_of_classes + Variables*Covariance_Structure +
                 Variables * Group_Mean_Distance, data = MetricsDf1,
               correlation = corSymm(form = ~1|Simulation))

summary(mod110)
anova(mod110)

mod120 <- gls(Accuracy_class ~  Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance + Variables*Proportion + 
                 Variables*Number_of_classes + Variables*Covariance_Structure +
                 Variables * Group_Mean_Distance, 
                 data = MetricsDf1,
                  correlation = corSymm(form = ~1|Simulation) )

summary(mod110)
anova(mod110)


mod1 <- lmer(Accuracy_class ~ Variables + Proportion + Number_of_classes + Covariance_Structure +
               Group_Mean_Distance + (1|Simulation), data = MetricsDf1 )

mod1 <- aov(Accuracy_class ~ Variables + Proportion + Number_of_classes + Covariance_Structure +
              Group_Mean_Distance + (1|Simulation), data = MetricsDf1)

anova(mod1)
summary(mod1)
plot(mod1)

mod2 <- lmer(log(Accuracy_class) ~ Variables + Proportion + Number_of_classes + Covariance_Structure +
               Group_Mean_Distance + Variables*Proportion + 
               Variables*Number_of_classes + Variables*Covariance_Structure+
               Variables*Group_Mean_Distance + (1|Variables), data = MetricsDf1 )

summary(mod2)
plot(mod2)
