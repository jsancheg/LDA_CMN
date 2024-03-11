

# NameMetricFile
# Metrics_SSFiles.RDS file with metrics of Semi-supervised learning fitted models (SSFiles)
# Metrics_SFiles.RDS file with metrics of Supervised learning fitted models (SFiles)




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
library(nlme)

source("ListScenarios.R")

# SSmetrics <- readRDS("Metrics_SSFiles.RDS")

#  Equivalent with SSMetrics but have corrected the column Nsim with the number of simulations
#  instead of replicates that were in the past 1-10
#  Also record the replicate for each file

SSmetrics <- readRDS("SSMetrics_Old_2024_03_10.RDS")


# Plot Model Size Selected Variables --------------------------------------

#SSmetrics$Model[SSmetrics$Number_Separating_Variables == 2 & is.na(SSmetrics$Model)] <- "X2-X4"
#SSmetrics$Model[SSmetrics$Number_Separating_Variables == 3 & is.na(SSmetrics$Model)] <- "X2-X4-X5"

SSmetrics$Model_Size[SSmetrics$Number_Separating_Variables == 2 & is.na(SSmetrics$Model)] <- 2
SSmetrics$Model_Size[SSmetrics$Number_Separating_Variables == 3 & is.na(SSmetrics$Model)] <- 3

SSmetrics$Model[SSmetrics$Number_Separating_Variables == 2 & SSmetrics$Model == ""] <- "X2-X4"
SSmetrics$Model[SSmetrics$Number_Separating_Variables == 3 & SSmetrics$Model ==""] <- "X2-X4-X5"
SSmetrics$Model1[SSmetrics$Number_Separating_Variables == 2 & SSmetrics$Model1 == ""] <- "X2-X4"
SSmetrics$Model1[SSmetrics$Number_Separating_Variables == 3 & SSmetrics$Model1 ==""] <- "X2-X4-X5"


SSmetrics$Model_Size[SSmetrics$Number_Separating_Variables == 2 & SSmetrics$Model ==""] <- 2
SSmetrics$Model_Size[SSmetrics$Number_Separating_Variables == 3 & SSmetrics$Model ==""] <- 3


nrow(SSmetrics)

data<- SSmetrics

# View(data %>% filter(Model == ""))

# View(data %>% filter(Model != ""))

data <- SSmetrics %>% filter(Model != "")

data <- data %>% mutate(IncludeX1 = as.numeric(str_detect(Model1,"X1")),
                          IncludeX3 = as.numeric(str_detect(Model1,"X3"))) %>%
  relocate(IncludeX1,IncludeX3, .after = IncludeX5)

data5 <- data %>% filter(Number_Variables == 5)
  
data100 <- data %>% filter(Number_Variables == 100)


boxplot(SSmetrics%>%filter(Variables == "Selected" & Model != "") %>% select(Model_Size),
        main = "Size of the selected model",
        col = "aliceblue")


ggplot(SSmetrics %>% filter(Model != ""), aes(x = Variables, y = Model_Size, fill = Variables)) +
  geom_boxplot()+
  labs(title = "Model Size of selected models", x = "Variables", y = "Model Size")



ggplot(data[data$Variables == "Selected", ], aes(x = "", y = Model_Size)) +
  geom_boxplot() +
  labs(title = "Model Size of selected models", x = "Set of variabkes", y = "Model size") +
  theme_minimal()


table(SSmetrics$Variables,SSmetrics$Model_Size)
SSmetrics %>% filter(Variables=="True") %>% select(Model1) %>% table 
SSmetrics %>% filter(Variables=="True" & Model == "") %>% select(File) %>% table

# 5 variables - 49 Scenarios
SSmetrics %>% filter(Variables=="True" & Model != "" & Number_Variables==5) %>% select(File) %>% table

SSmetrics %>% filter(Variables=="True" & Model != "" & Number_Variables==5) %>% select(File) %>% table %>% length

TrueModel5Good <- SSmetrics %>% filter(Variables=="True" & Model != "" & Number_Variables==5) %>% select(File) %>% table %>% rownames



#100 variables
SSmetrics %>% filter(Variables=="True" & Model != "" & Number_Variables==100) %>% select(File) %>% table

SSmetrics %>% filter(Variables=="True" & Model != "" & Number_Variables==100) %>% select(File) %>% table %>% length

TrueModel100WGood <- SSmetrics %>% filter(Variables=="True" & Model != "" & Number_Variables==100) %>% select(File) %>% table %>% rownames

# Missing files

setdiff(str_replace(Scenarios,"S_","SSV_"), unique(SSmetrics$File) )



# Inclusion ratio of variables  -------------------------------------------
colnames(data5)
variables_Inclusion5 <- data5 %>% filter(Variables == "Selected") %>%
  select(Number_Separating_Variables,IncludeX1,IncludeX2,IncludeX3,IncludeX4,IncludeX5)

variables_Inclusion5$Number_Separating_Variables <- as.numeric(variables_Inclusion5$Number_Separating_Variables)

aggregate(. ~ Number_Separating_Variables, variables_Inclusion5, sum)

      
colSums(variables_Inclusion5)

table(variables_Inclusion5$Number_Separating_Variables)

#InclusionVar5 <- variables_Inclusion5 %>% dplyr::group_by(Number_Separating_Variables) %>% 
#  mutate(total = rowSums(select(., starts_with("IncludeX") )))

InclusionVar5 <- variables_Inclusion5 %>% group_by(Number_Separating_Variables) %>% rowwise() %>% 
  mutate(total = sum(c_across(starts_with("IncludeX")),na.rm = TRUE))    



total2 <- nrow(InclusionVar5 %>% filter(Number_Separating_Variables == 2))
round(100*(apply(InclusionVar5 %>% filter(Number_Separating_Variables==2),2,sum)/total2),1)

total3 <- nrow(InclusionVar5 %>% filter(Number_Separating_Variables==3))
round(100*(apply(InclusionVar5 %>% filter(Number_Separating_Variables==3),2,sum)/total2),1)

ModelSize5.2 <- data5 %>% filter(Variables == "Selected", Number_Separating_Variables == 2) %>% 
  select(Model_Size) %>% table 

ModelSize5.3 <- data5 %>% filter(Variables == "Selected", Number_Separating_Variables == 3) %>% 
  select(Model_Size) %>% table 


ModelSize5.2

round(100*ModelSize5.2/sum(ModelSize5.2),1)
round(100*cumsum(ModelSize5.2)/sum(ModelSize5.2),1)

ModelSize5.3
round(100*ModelSize5.3/sum(ModelSize5.3),1)
round(100*cumsum(ModelSize5.3)/sum(ModelSize5.3),1)



variables_Inclusion100 <- data100 %>% filter(Variables == "Selected") %>%
  select(Number_Separating_Variables,IncludeX1,IncludeX2,IncludeX3,IncludeX4,IncludeX5)


#InclusionVar100 <- variables_Inclusion100 %>% group_by(Number_Separating_Variables) %>% 
#  mutate(total = rowSums(select(., starts_with("IncludeX") )))



variables_Inclusion100$Number_Separating_Variables <- as.numeric(variables_Inclusion100$Number_Separating_Variables)


total100.2 <- nrow(variables_Inclusion100 %>% filter(Number_Separating_Variables==2))

apply(variables_Inclusion100%>% filter(Number_Separating_Variables==2),2,sum)

round(100*(apply(variables_Inclusion100%>% filter(Number_Separating_Variables==2),2,sum)/total100.2),1)

total100.3 <- nrow(InclusionVar5 %>% filter(Number_Separating_Variables==3))
round(100*(apply(InclusionVar5 %>% filter(Number_Separating_Variables==3),2,sum)/total2),1)

ModelSize5 <- data5 %>% filter(Variables == "Selected") %>% select(Model_Size) %>% table 
round(100*ModelSize5/total2,1)


# Plots for chapter 4 Thesis where the CCR is for classification a --------


g5.1 <- ggplot(data5, aes(x =Class_Proportion, y = CCR,color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Proportion") + ylim(0.1,1) 



g5.2 <- ggplot(data5, aes(x = Number_Classes, y = CCR, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Number of classes") + ylim(0.1,1)

g5.3 <- ggplot(data5, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.1,1)


g5.4 <- ggplot(data5, aes(x = Group_Mean_Distance, y = CCR, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  scale_x_discrete(labels = c("Medium distance","Very distance","Very Overlapping"))+
  ylab("Test CCR") + xlab("Group mean distance") + ylim(0.1,1)

combine <- g5.1 + g5.2 + g5.3 + g5.4 & theme(legend.position = "bottom")
combine + plot_layout(guides = "collect")


g100.1 <- ggplot(data100, aes(x =Class_Proportion, y = CCR,color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Proportion") + ylim(0.1,1) 



g100.2 <- ggplot(data100, aes(x = Number_Classes, y = CCR, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Number of classes") + ylim(0.1,1)

g100.3 <- ggplot(data100, aes(x = Covariance_Structure, y = CCR, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Test CCR") + xlab("Covariance strucutre") + ylim(0.1,1)


g100.4 <- ggplot(data100, aes(x = Group_Mean_Distance, y = CCR, color = Variables)) +
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  scale_x_discrete(labels = c("Medium distance","Very distance","Very Overlapping"))+
  ylab("Test CCR") + xlab("Group mean distance") + ylim(0.1,1)

combine <- g100.1 + g100.2 + g100.3 + g100.4 & theme(legend.position = "bottom")
combine + plot_layout(guides = "collect")



# GLS models --------------------------------------------------------------

mod5.1 <- gls(CCR ~ Variables, data = data5,
              correlation  = corSymm(form = ~1|Nsim))

mod5.20 <- gls(CCR ~ Variables + Class_Proportion + Number_Classes +
                 Covariance_Structure + Group_Mean_Distance ,
               data = data5,
               correlation = corSymm(form = ~1|Nsim))
  