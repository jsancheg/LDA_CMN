MetricsPath <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"

library(dplyr)
library(lme4)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(cowplot)
library(patchwork)
library(ggpubr)

dir(MetricsPath)
MetricsDf <- readRDS(paste0(MetricsPath,"Metrics4D.RDS"))

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



auxDf1 <- auxDf1 %>% dplyr::rename("A1" = "AccuracyTM" ,
                            "A2"= "AccuracySM" ,
                            "A3" = "AccuracySaturatedM" ,
                            "C1" = "Accuracy_TM_contaminated"  ,
                            "C2" = "Accuracy_SM_contaminated"  ,
                            "C3" = "Accuracy_Saturated_Cont",
                            "N1" = "Accuracy_TM_no_contaminated",
                            "N2" = "Accuracy_SM_no_contaminated",
                            "N3" = "Accuracy_Saturated_NoCont" ,
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

auxDf1 <- auxDf1 %>% dplyr::rename("Accuracy_class" = "A" ,
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

MetricsDf1$Variables <- as.factor(MetricsDf1$Variables)
MetricsDf1$Variables <- relevel(MetricsDf1$Variables,"True")
unique(MetricsDf1$Variables)


unique(MetricsDf1$Group_Mean_Distance)
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


mean.acc1 <- MetricsDf1 %>% dplyr::group_by(Group_Mean_Distance,Simulation) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
  
ggplot(mean.acc1, aes(x = reorder(Simulation,mean.accuracy ), y = mean.accuracy, 
                      color = Group_Mean_Distance)) + geom_point()


#g1 <- ggplot(MetricsDf1, aes(x = Proportion, y = Accuracy_class)) +
#  geom_boxplot_pattern(aes(pattern = Variables, pattern_fill = Variables),
#                       pattern_density = 0.02, outlier.shape = NA) + 
#  scale_pattern_manual(values = c("True" = "crosshatch", "All"="stripe",
#                                  "Selected" = "wave"))+
#  scale_pattern_fill_manual(values = c("True" = "red", "All" = "green",
#                                       "Selected" = "blue"))+
#  ylab("Accuracy(Classes)") + xlab("Proportion")



# Plots for conference ACSTA with Test Accuracy ---------------------------


g1 <- ggplot(MetricsDf1, aes(x = Proportion, y = Accuracy_class,color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test accuracy") + xlab("Proportion") + ylim(0.1,1) 



g2 <- ggplot(MetricsDf1, aes(x = Number_of_classes, y = Accuracy_class, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test accuracy") + xlab("Number of classes") + ylim(0.1,1)

g3 <- ggplot(MetricsDf1, aes(x = Covariance_Structure, y = Accuracy_class, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test accuracy") + xlab("Covariance strucutre") + ylim(0.1,1)


g4 <- ggplot(MetricsDf1, aes(x = Group_Mean_Distance, y = Accuracy_class, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  scale_x_discrete(labels = c("Medium distance","Very distance","Very Overlapping"))+
  ylab("Test accuracy") + xlab("Group mean distance") + ylim(0.1,1)

#g1<- ggplot(MetricsDf1, aes(x = Proportion, y = Accuracy_class, color = Variables)) +
#  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Proportion")

#g2 <- ggplot(MetricsDf1, aes(x = Number_of_classes, y = Accuracy_class, 
#                       color = Variables)) + 
#  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Number of classes")


#g3 <- ggplot(MetricsDf1, aes(x = Covariance_Structure, y = Accuracy_class, 
#                       color = Variables)) + 
#  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Covariance structure")


#g4 <- ggplot(MetricsDf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
##                       color = Variables)) + 
#  geom_boxplot() + ylab("Accuracy(Classes)") + xlab("Group mean distance")

# to combine plots we required "patchwork" library
combine <- g1 + g2 + g3 + g4 & theme(legend.position = "bottom")
combine + plot_layout(guides = "collect")



# Plots for chapter 3 Thesis where the CCR is for classification a --------

# Plots for conference ACSTA with Test Accuracy ---------------------------


g5 <- ggplot(MetricsDf1, aes(x = Proportion, y = Accuracy_class,color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Proportion") + ylim(0.1,1) 



g6 <- ggplot(MetricsDf1, aes(x = Number_of_classes, y = Accuracy_class, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Number of classes") + ylim(0.1,1)

g7 <- ggplot(MetricsDf1, aes(x = Covariance_Structure, y = Accuracy_class, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.1,1)


g8 <- ggplot(MetricsDf1, aes(x = Group_Mean_Distance, y = Accuracy_class, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  scale_x_discrete(labels = c("Medium distance","Very distance","Very Overlapping"))+
  ylab("Test CCR") + xlab("Group mean distance") + ylim(0.1,1)


# to combine plots we required "patchwork" library
combine <- g5 + g6 + g7 + g8 & theme(legend.position = "bottom")
combine + plot_layout(guides = "collect")




#MetricsDf1 %>% select(c(Accuracy_class,Variables)) %>% data.frame()  %>%
#  pairwise.t.test(Accuracy_class ~ Variables, paired = TRUE,
#                  p.adjust.method = "bonferroni")





library(nlme)

unique(MetricsDf1$Group_Mean_Distance)
VODf1 <- MetricsDf1 %>% dplyr::filter(Group_Mean_Distance == "VO")
class(VODf1$Variables)
unique(VODf1$Setting)
table(VODf1$Variables)

# Subset only very overlapping cases --------------------------------------


# "OutputS_2_3_4_BAL_SCBSV_VO"
VODf1 <- MetricsDf1 %>% dplyr::filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_3_4_BAL_SCBSV_VO")
class(VODf1$Variables)


mean2_3_4_BAL_SCBSV <- VODf1 %>% dplyr::group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_3_4_BAL_SCBSV


g2_3_4_BAL_SCBSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                          color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test Accuracy") + xlab("SCBSV") + ylim(0.4,1)


g2_3_4_BAL_SCBSV


mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
             correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")

VODf1 %>% ggplot(mapping = aes(sample = residuals(mod_VO_1))) +
  geom_qq() +
  facet_wrap(facets = ~ Variables, scales = "free") +
  theme_bw()


# "OutputS_2_3_4_BAL_SCBSNSV_VO" All and Selected similar (0.83) but different from True (0.74)

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_3_4_BAL_SCBSNSV_VO")

mean2_3_4_BAL_SCBSNSV <- VODf1 %>% dplyr::group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_3_4_BAL_SCBSNSV


g2_3_4_BAL_SCBSNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                         color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("SCBSNSV") + ylim(0.4,1)


g2_3_4_BAL_SCBSNSV

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_3_4_BAL_SCBNSV_VO"  Selected (0.95) while All and True (0.94)   

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_3_4_BAL_SCBNSV_VO")

mean2_3_4_BAL_SCBNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_3_4_BAL_SCBNSV


g2_3_4_BAL_SCBNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                          color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
   ylab("Test CCR") + xlab("SCBNSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



g2_3_4_BAL_SCBNSV

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")

# "OutputS_2_3_4_BAL_IND_VO"  Selected (0.75) slightly better than All (0.73) and True (0.74)  

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_3_4_BAL_IND_VO")

mean2_3_4_BAL_IND <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_3_4_BAL_IND


g2_3_4_BAL_IND <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                          color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("IND") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())


g2_3_4_BAL_IND

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")

# "OutputS_2_2_4_5050_SCBSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_5050_SCBSV_VO")

mean2_2_4_BAL_SCBSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_BAL_SCBSV


g2_2_4_BAL_SCBSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                      color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("SCBSV") + ylim(0.3,1)


g2_2_4_BAL_SCBSV

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_2_4_5050_SCBSNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_5050_SCBSNSV_VO")

mean2_2_4_BAL_SCBSNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_BAL_SCBSNSV


g2_2_4_BAL_SCBSNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                        color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("SCBSNSV") + ylim(0.3,1)+
  theme(axis.text.x = element_blank())


g2_2_4_BAL_SCBSNSV

mod_VO_1 <- gls(F1_Class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_2_4_5050_SCBNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_5050_SCBNSV_VO")

mean2_2_4_BAL_SCBNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_BAL_SCBNSV


g2_2_4_BAL_SCBNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                       color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("SCBNSV") + ylim(0.3,1)+
  theme(axis.text.x = element_blank())


g2_2_4_BAL_SCBNSV

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_2_4_5050_IND_VO"    All (0.80), Selected (0.82), True (0.81) are similar

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_5050_IND_VO")

mean2_2_4_BAL_IND <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_BAL_IND


g2_2_4_BAL_IND <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                    color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
   ylab("Test CCR") + xlab("IND") + ylim(0.3,1) +
  theme(axis.text.x = element_blank())


g2_2_4_BAL_IND

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")

mean_2_2_4_BAL <- rbind.data.frame(mean2_2_4_BAL_IND,
                                       mean2_2_4_BAL_SCBNSV,
                                       mean2_2_4_BAL_SCBSNSV,
                                       mean2_2_4_BAL_SCBSV) 
mean_2_2_4_BAL %>% group_by(Variables) %>%   
  summarise(mean.accuracy  = mean(mean.accuracy))




# Unbalanced data sets

# "OutputS_2_2_4_9010_SCBSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_SCBSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

mean2_2_4_UNBAL_SCBSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_UNBAL_SCBSV


g2_2_4_UNBAL_SCBSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                        color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("SCBSV") + ylim(0.3,1) 


g2_2_4_UNBAL_SCBSV

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_2_4_9010_SCBSNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_SCBSNSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

mean2_2_4_UNBAL_SCBSNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_UNBAL_SCBSNSV


g2_2_4_UNBAL_SCBSNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                          color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("SCBSNSV") + ylim(0.3,1) +
  theme(axis.text.x = element_blank())


g2_2_4_UNBAL_SCBSNSV

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_2_4_9010_SCBNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_SCBNSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

mean2_2_4_UNBAL_SCBNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_UNBAL_SCBNSV


g2_2_4_UNBAL_SCBNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                         color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("SCBNSV") + ylim(0.3,1)+
  theme(axis.text.x = element_blank())


g2_2_4_UNBAL_SCBNSV

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")

# "OutputS_2_2_4_9010_IND_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_IND_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

mean2_2_4_UNBAL_IND <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_4_UNBAL_IND


g2_2_4_UNBAL_IND <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                      color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("IND") + ylim(0.3,1)+
  theme(axis.text.x = element_blank())


g2_2_4_UNBAL_IND

mod_VO_1 <- gls(Accuracy_class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


mean_2_2_4_UNBAL <- rbind.data.frame(mean2_2_4_UNBAL_IND,
                                       mean2_2_4_UNBAL_SCBNSV,
                                       mean2_2_4_UNBAL_SCBSNSV,
                                       mean2_2_4_UNBAL_SCBSV) 
mean_2_2_4_UNBAL %>% group_by(Variables) %>%   
  summarise(mean.accuracy  = mean(mean.accuracy))


# F1 score
# "OutputS_2_2_4_9010_SCBSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_SCBSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

F1mean2_2_4_UNBAL_SCBSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.F1  = mean(F1_Class))
F1mean2_2_4_UNBAL_SCBSV


F1g2_2_4_UNBAL_SCBSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                       color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
   ylab("F1 test") + xlab("SCBSV") + ylim(0.4,1) 


F1g2_2_4_UNBAL_SCBSV

mod_VO_1 <- gls(F1_Class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_2_4_9010_SCBSNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_SCBSNSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

F1mean2_2_4_UNBAL_SCBSNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.F1  = mean(F1_Class))
F1mean2_2_4_UNBAL_SCBSNSV


F1g2_2_4_UNBAL_SCBSNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                         color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
   ylab("F1 test") + xlab("SCBSNSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())


F1g2_2_4_UNBAL_SCBSNSV

mod_VO_1 <- gls(F1_Class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")


# "OutputS_2_2_4_9010_SCBNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_SCBNSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

F1meanS_2_2_4_UNBAL_SCBNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.F1  = mean(F1_Class))
F1meanS_2_2_4_UNBAL_SCBNSV


F1g2_2_4_UNBAL_SCBNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                            color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
   ylab("F1 test") + xlab("SCBNSV") + ylim(0.4,1)+
  theme(axis.text.x = element_blank())


F1g2_2_4_UNBAL_SCBNSV

mod_VO_1 <- gls(F1_Class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")

# "OutputS_2_2_4_9010_IND_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_4_9010_IND_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

F1meanS_2_2_4_UNBAL_IND <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.F1  = mean(F1_Class))
F1meanS_2_2_4_UNBAL_IND


F1g2_2_4_UNBAL_IND <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                        color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("F1 test") + xlab("IND") + ylim(0.4,1)+
  theme(axis.text.x = element_blank())


F1g2_2_4_UNBAL_IND

mod_VO_1 <- gls(F1_Class ~ Variables, data = VODf1, 
                correlation = corSymm(form = ~1|Simulation))

summary(mod_VO_1)

color_variables <- sapply(VODf1$Variables,function(i) {
  if(i == "Selected") "green"
  else if(i == "All") "red"
  else if(i == "True") "blue"
})

plot(mod_VO_1, col = color_variables )

plot(residuals(mod_VO_1), ylab = "residuals")



# Panel of plots for VO very overlapping sub-setting -----------------------


g2_3_4_BAL_SCBNSV
g2_3_4_BAL_SCBSNSV
g2_3_4_BAL_SCBNSV
g2_3_4_BAL_IND
g2_2_4_UNBAL_SCBSV
g2_2_4_UNBAL_SCBSNSV
g2_2_4_UNBAL_SCBNSV
g2_2_4_UNBAL_IND
g2_2_4_BAL_SCBSV
g2_2_4_BAL_SCBSNSV
g2_2_4_BAL_SCBNSV
g2_2_4_BAL_IND


# 2 balanced groups 
gr10 <- ggarrange(  g2_2_4_BAL_IND,
                    g2_2_4_BAL_SCBNSV,
                    g2_2_4_BAL_SCBSNSV,
                    g2_2_4_BAL_SCBSV,
                    ncol=2,nrow = 2,
                    common.legend = TRUE,
                    legend = "bottom")


# 3 balanced groups
gr12 <- ggarrange(g2_3_4_BAL_IND,
                    g2_3_4_BAL_SCBNSV,
                    g2_3_4_BAL_SCBSNSV,
                    g2_3_4_BAL_SCBSV,
                    ncol = 2, nrow = 2,
                    common.legend = TRUE,
                    legend = "bottom")


# 2 unbalanced groups
gr11 <- ggarrange(g2_2_4_UNBAL_IND,
                    g2_2_4_UNBAL_SCBNSV,
                    g2_2_4_UNBAL_SCBSNSV,
                    g2_2_4_UNBAL_SCBSV,
                    ncol=2,nrow = 2,
                  common.legend = TRUE,
                  legend = "bottom")


# F1 score for 2 unbalanced groups
gr13 <- ggarrange(F1g2_2_4_UNBAL_IND,
                  F1g2_2_4_UNBAL_SCBNSV,
                  F1g2_2_4_UNBAL_SCBSNSV,
                  F1g2_2_4_UNBAL_SCBSV,
                  ncol=2,nrow = 2,
                  common.legend = TRUE,
                  legend = "bottom")

plot(gr10) # 2 Balanced groups in 4 Dim
plot(gr12) # 3 Balanced groups in 4 Dim


plot(gr11) # 2 Unbalanced groups in 4 Dim
plot(gr13) # F1 score 2 Unbalanced groups in 4 Dim


VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_3_4_BAL_SCBSV_VO")

unique(factor(VODf1$Setting))

summary(factor(VODf1$Setting))
nrow(VODf1)

meanAcc <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
meanAcc




mod10 <- gls(Accuracy_class ~ Variables, data = VODf1, 
             correlation = corSymm(form = ~1|Simulation))

summary(mod10)

MetricsDf1 %>% ggplot(mapping = aes(sample = Accuracy_class)) +
  geom_qq() +
  facet_wrap(facets = ~ Covariance_Structure, scales = "free") +
  theme_bw()

MetricsDf1.new <- groupedData(Accuracy_class ~ Variables |Group_Mean_Distance,
                              data = MetricsDf1, FUN = mean)


mod100 <- gls( Accuracy_class ~   Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance, data = MetricsDf1,
               correlation = corSymm(form = ~1|Simulation))

summary(mod100)
round(summary(mod100)[[4]],2)
anova(mod100)
plot(mod100)
nrow(MetricsDf1)



library(lattice)

bwplot(getGroups(MetricsDf1.new) ~ residuals(mod100))


mod109 <- gls(Accuracy_class ~ Variables, data = MetricsDf1,
              correlation = corSym(form = ~1|Simulation))

summary(mod109)
plot(residuals(mod109))

mod110 <- gls( Accuracy_class ~   Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance + Variables*Proportion + 
                 Variables*Number_of_classes + Variables*Covariance_Structure +
                 Variables * Group_Mean_Distance, data = MetricsDf1,
               correlation = corSymm(form = ~1|Simulation))

summary(mod110)
anova(mod110)
plot(residualas(mod110))

mod120 <- gls( Accuracy_class ~   Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance, data = MetricsDf1,
               correlation = corSymm(form = ~1|Simulation),
               weights = varIdent(form = ~1|Group_Mean_Distance))

mod130 <- gls(Accuracy_class ~  Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance + Variables*Proportion + 
                 Variables*Number_of_classes + Variables*Covariance_Structure +
                 Variables * Group_Mean_Distance, 
                 data = MetricsDf1,
                  correlation = corSymm(form = ~1|Simulation) )

summary(mod110)
anova(mod110)


mod200 <- gls( F1_Class ~   Variables + Proportion + Number_of_classes + Covariance_Structure +
                 Group_Mean_Distance, data = na.omit(MetricsDf1),
               correlation = corSymm(form = ~1|Simulation))

nrow(na.omit(MetricsDf1))

summary(mod200)
anova(mod200)
plot(mod200)



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

