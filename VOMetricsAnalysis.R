pathOutput <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Output/"
simulation <- c( "OutputS_2_2_100_5050_SCBSV_VO", "OutputS_2_2_100_5050_SCBSNSV_VO",
                "OutputS_2_2_100_5050_SCBNSV_VO", "OutputS_2_2_100_5050_IND_VO",
                "OutputS_2_2_100_9010_SCBSV_VO", "OutputS_2_2_100_9010_SCBSNSV_VO",
                "OutputS_2_2_100_9010_SCBNSV_VO", "OutputS_2_2_100_9010_IND_VO")
                 
simulation <- c( "OutputS_2_2_100_5050_SCBSV_VO", "OutputS_2_2_100_5050_SCBSNSV_VO",
                 "OutputS_2_2_100_9010_SCBSV_VO", "OutputS_2_2_100_9010_SCBSNSV_VO",
                 "OutputS_2_2_100_9010_SCBNSV_VO", "OutputS_2_2_100_9010_IND_VO",
                 "OutoutS_2_2_100_5050_SCBSNSV_VD", "OutputS_2_2_100_9010_SCBNSV_VD",
                 "OutputS_2_2_100_9010_SCBSV_VD","OutputS_2_2_100_9010_IND_MD",
                 "OutputS_2_2_100_9010_SCBNSV_MD","OutputS_2_2_100_9010_IND_VD",
                 "OutputS_2_3_100_5050_IND_VO"
                 )


#simulation <- c( "OutputS_2_2_100_9010_SCBSV_VO", "OutputS_2_2_100_9010_SCBSNSV_VO",
#                 "OutputS_2_2_100_9010_SCBNSV_VO", "OutputS_2_2_100_9010_IND_VO")

library(tidyr)
library(plyr)
library(stringr)
library(dplyr)
library(plyr)
library(nlme)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(cowplot)
library(ggpubr)

find_unique_labels<-function(labels)
{  
  filas <- length(labels) 
  rawlabels<-unique(labels)
  rawlabels <- sort(rawlabels)
  nrawlabels <- length(rawlabels)
  transformlabels<-rawlabels
  registerchanges <- rep(0,nrawlabels)
  
  for(i_raw1 in 1:(nrawlabels-1) )
  {
    a <-  unlist(str_split(rawlabels[i_raw1],"-"))
    
    for(i_raw2 in 2:nrawlabels)
    {
      b <- unlist(str_split(rawlabels[i_raw2],"-")) 
      # compare two list of variables without taking in account order of variables
      if(registerchanges[i_raw2]==0)
        if(setequal(a,b)) 
        {
          transformlabels[i_raw1]<-rawlabels[i_raw1]
          transformlabels[i_raw2]<-rawlabels[i_raw1]
          registerchanges[i_raw2]<- 1
        } # end-if
    }# end-for i_raw2
  }# end-for i_raw1
  uniquelabels<- unique(transformlabels)
  nuniquelabels <- length(uniquelabels)
  newlabels <- labels
  registerchangedlabels <- rep(0,filas)
  for(i_raw1 in 1:(nuniquelabels) )
  {
    a <-  unlist(str_split(uniquelabels[i_raw1],"-"))
    
    for(i_raw2 in 1:filas)
    {
      b <- unlist(str_split(labels[i_raw2],"-")) 
      # compare two list of variables without taking in account order of variables
      if(registerchangedlabels[i_raw2]==0)
        if(setequal(a,b)) 
        {
          newlabels[i_raw1]<-uniquelabels[i_raw1]
          newlabels[i_raw2]<-uniquelabels[i_raw1]
          registerchangedlabels[i_raw2]<- 1
        } # end-if
    }# end-for i_raw2
  }# end-for i_raw1
  
  #cbind.data.frame(labels,newlabels)
  #uniquelabels<-unique(transformlabels)
  #uniquelabels
  return(cbind.data.frame(labels,newlabels))
}


combineSimulations <- function(aux_list)
{
  n <- length(aux_list)
  l1 <- list()
  for(i_sim in 1:n) 
  {
    aux <- aux_list[[i_sim]]$resumen
    colnames(aux)[1] <- "Model"
    
    l1[[i_sim]] <- aux
    
  }
  return(ldply(l1,data.frame))
}


CombineDataFrames <- function(pathDir)
{
  raw_files <- lapply(pathDir, function(directory) 
    {
      aux_list <- paste0(directory,"/",dir(directory))
      ind_rds <- str_detect(aux_list,".Rdata")
      aux_rds <- aux_list[ind_rds]
      return(aux_rds)
  }) 
  
  
  raw_filesDf <- ldply(raw_files,data.frame)
  colnames(raw_filesDf) <- "archivo"
  
  filesPath <- as.vector(raw_filesDf$archivo)
  
  aux_df <- lapply(filesPath,function(filename) {
        
      load(filename)
      cat("\nprocessing ",filename," with dimension ", dim(sim.A5[[1]]$resumen), "\n")
      class(combineSimulations(sim.A5))
      output <- combineSimulations(sim.A5)
      output$Setting <- filename
      colnames(output)
      head(output)
  #    output <- output$Simulation <- filename
      return(output)
  #    return(combineSimulations(sim.A5))
  })

  df <- ldply(aux_df,data.frame)
  df <- df %>% relocate(Setting, .before = Model)
  
#  colnames(df)
  return(df)
}



pathDir <- paste0(pathOutput,simulation)
simulation
df_resumen <- CombineDataFrames(pathDir)
head(df_resumen)
dim(df_resumen)
colnames(df_resumen)
unique(df_resumen$Setting)


df_resumen$Setting <- str_replace_all(df_resumen$Setting,pathOutput,"")
unique(df_resumen$Setting)


df_resumen$Setting <- str_extract_all(df_resumen$Setting,"OutputS_2_\\d+_100_\\d+0\\d+0_[A-Z]+_[A-Z]+",simplify = TRUE )
unique(df_resumen$Setting)


df_resumen <- mutate(df_resumen,Model1 = Model)
df_resumen <- df_resumen %>% relocate(Model1, .after = Model)
df_resumen$Simulation <- rownames(df_resumen)
df_resumen <- df_resumen %>% relocate(Simulation, .before = Setting)

colnames(df_resumen)
df_resumen <- df_resumen %>% relocate(Model, .after = Model1)
colnames(df_resumen)

labels<-df_resumen$Model1

# find_unique_labels(labels)
df_resumen$Model <- find_unique_labels(df_resumen$Model1)[,2]
df_resumen$IncludeX2 <- as.numeric(str_detect(df_resumen$Model,"X2"))
df_resumen$IncludeX4 <- as.numeric(str_detect(df_resumen$Model,"X4"))

df_resumen <- df_resumen %>% 
  relocate(IncludeX2,IncludeX4, .after = ModelSizeSM)
df_resumen <- df_resumen %>%
  relocate(Exclusion_correctness, .after = Inclusion_correctness)


#df_resumen$Setting <- simulation[i]

df_resumen <- df_resumen %>%
  relocate(Setting, .before = Model1)

colnames(df_resumen)

head(df_resumen)


MetricsDf <- df_resumen


MetricsDf <- MetricsDf %>% mutate(Proportion = 
                                    ifelse(str_detect(MetricsDf$Setting, "5050"), "BAL","UNBAL") )

MetricsDf <- MetricsDf %>% mutate(Number_of_classes = 
                                    str_split(MetricsDf$Setting,"_",simplify = TRUE)[,3])

MetricsDf <- MetricsDf %>% mutate(Covariance_Structure = 
                                    str_split(MetricsDf$Setting,"_",simplify = TRUE)[,6])

MetricsDf <- MetricsDf %>% mutate(Group_Mean_Distance = 
                                    str_split(MetricsDf$Setting,"_",simplify = TRUE)[,7])

MetricsDf %>% pivot_longer(cols=AccuracyTM:AccuracySaturatedM,
                           names_to = "Variables",
                           values_to = "Accuracy")



head(MetricsDf)
unique(MetricsDf$Number_of_classes)

# Accuracy_TM_cont, Accuracy_SM_cont, Accuracy_TM_nocont,Accuracy_ --------

auxDf1 <- MetricsDf %>% dplyr::select(Simulation,Setting,AccuracyTM,AccuracySM,AccuracySaturatedM,
                                      Precision_TM,Precision_SM,Precision_SaturatedM,
                                      Recall_TM,Recall_SM,Recall_SaturatedM,
                                      F1_TM,F1_SM,F1_SaturatedM,
                                      precision_TM_V,precision_SM_V,precision_saturated_V,
                                      recall_TM_V,recall_SM_V,recall_saturated_V,
                                      F1_TM_V,F1_SM_V,F1_Saturated_V)

colnames(auxDf1)
# 1: True
# 2: Selected
# 3: Complete

head(MetricsDf)

auxDf1 <- auxDf1 %>% rename("A1" = "AccuracyTM" ,
                            "A2"= "AccuracySM" ,
                            "A3" = "AccuracySaturatedM" ,
#                            "C1" = "Accuracy_TM_cont"  ,
#                            "C2" = "Accuracy_SM_cont"  ,
#                            "N1" = "Accuracy_TM_nocont",
#                            "N2" = "Accuracy_SM_nocont",
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
    cols = A1:Z3,
    names_to = c(".value","Variables"),
    names_pattern = "(.)(.)"
  )



auxDf1 <- auxDf1 %>% rename("Accuracy_class" = "A" ,
#                            "Accuracy_Cont" = "C"  ,
#                            "Accuracy_No_Cont" = "N",
                            "Precision_Class" = "P"  ,
                            "Recall_Class" = "R" ,
                            "F1_Class" = "F",
                            "Precicison_Cont" = "V" ,
                            "Recall_Cont" = "W",
                            "F1_Cont" = "Z")

colnames(auxDf1)

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

colnames(MetricsDf1)
head(MetricsDf1)

unique(MetricsDf1$Number_of_classes)
MetricsDf1$Variables <- as.factor(MetricsDf1$Variables)
MetricsDf1$Variables <- relevel(MetricsDf1$Variables,"True")
unique(MetricsDf1$Variables)
MetricsDf1$Proportion <- as.factor(MetricsDf1$Proportion)
MetricsDf1$Number_of_classes <- as.factor(MetricsDf1$Number_of_classes)
MetricsDf1$Covariance_Structure<- as.factor(MetricsDf1$Covariance_Structure)
MetricsDf1$Group_Mean_Distance <- as.factor(MetricsDf1$Group_Mean_Distance)

class(MetricsDf1$Covariance_Structure)
class(MetricsDf1$Number_of_classes)
class(MetricsDf1$Group_Mean_Distance)
summary(MetricsDf1$Number_of_classes)
summary(MetricsDf1$Covariance_Structure)
summary(MetricsDf1$Group_Mean_Distance)


mod100 <- gls( Accuracy_class ~   Variables + Proportion +  Covariance_Structure + 
                 Number_of_classes +
                 Group_Mean_Distance, data = MetricsDf1,
               correlation = corSymm(form = ~1|Simulation))

summary(mod100)
anova(mod100)
plot(mod100)
nrow(MetricsDf1)



unique(MetricsDf1$Group_Mean_Distance)
VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO")
unique(VODf1$Setting)
unique(VODf1$Variables)






# Subset only very overlapping cases --------------------------------------



# "OutputS_2_2_100_5050_SCBSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO" &
                               Setting == "OutputS_2_2_100_5050_SCBSV_VO")

nrow(VODf1)

mean2_2_100_BAL_SCBSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_BAL_SCBSV


g2_2_100_BAL_SCBSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                      color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("SCBSV") + ylim(0.4,1) +
 theme(axis.text.x = element_blank())


g2_2_100_BAL_SCBSV


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


# "OutputS_2_2_100_5050_SCBSNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_5050_SCBSNSV_VO")

mean2_2_100_BAL_SCBSNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_BAL_SCBSNSV


g2_2_100_BAL_SCBSNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                         color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("SCBSNSV") + ylim(0.4,1)+
  theme(axis.text.x = element_blank())


g2_2_100_BAL_SCBSNSV


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



# "OutputS_2_2_100_5050_SCBNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_5050_SCBNSV_VO")

mean2_2_100_BAL_SCBNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_BAL_SCBNSV


g2_2_100_BAL_SCBNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                         color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("SCBNSV") + ylim(0.4,1) + 
  theme(axis.text.x = element_blank())


g2_2_100_BAL_SCBNSV


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

# "OutputS_2_2_100_5050_IND_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_5050_IND_VO")
nrow(VODf1)

mean2_2_100_BAL_IND <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_BAL_IND


g2_2_100_BAL_IND <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                      color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("IND") + ylim(0.4,1) + 
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")


g2_2_100_BAL_IND


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


mean_2_2_100_BAL <- rbind.data.frame(mean2_2_100_BAL_IND,mean2_2_100_BAL_SCBNSV,mean2_2_100_BAL_SCBSNSV,mean2_2_100_BAL_SCBSV) 
mean_2_2_100_BAL %>% group_by(Variables) %>%   summarise(mean.accuracy  = mean(mean.accuracy))



  

# Unbalanced classes settings --------------------------------------

# Accuracy

# "OutputS_2_2_100_9010_SCBSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_SCBSV_VO")
head(VODf1)
VODf1 <- na.omit(VODf1)
nrow(VODf1)

mean2_2_100_UNBAL_SCBSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_UNBAL_SCBSV


g2_2_100_UNBAL_SCBSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                          color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("SCBSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



g2_2_100_UNBAL_SCBSV


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


# "OutputS_2_2_100_9010_SCBSNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_SCBSNSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)
head(VODf1)

mean2_2_100_UNBAL_SCBSNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_UNBAL_SCBSNSV


g2_2_100_UNBAL_SCBSNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                            color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("SCBSNSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



g2_2_100_UNBAL_SCBSNSV


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



# "OutputS_2_2_100_9010_SCBNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_SCBNSV_VO")

VODf1 <- na.omit(VODf1)
nrow(VODf1)

mean2_2_100_UNBAL_SCBNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_UNBAL_SCBNSV


g2_2_100_UNBAL_SCBNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                           color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("SCBNSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



g2_2_100_UNBAL_SCBNSV


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

# "OutputS_2_2_100_9010_IND_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_IND_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

nrow(VODf1)

mean2_2_100_UNBAL_IND <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(Accuracy_class))
mean2_2_100_UNBAL_IND


g2_2_100_UNBAL_IND <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = Accuracy_class, 
                                        color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("Accuracy test") + xlab("IND") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



g2_2_100_UNBAL_IND


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


mean_2_2_100_UNBAL <- rbind.data.frame(mean2_2_100_UNBAL_IND,
                                       mean2_2_100_UNBAL_SCBNSV,
                                       mean2_2_100_UNBAL_SCBSNSV,
                                       mean2_2_100_UNBAL_SCBSV) 
mean_2_2_100_UNBAL %>% group_by(Variables) %>%   
  summarise(mean.accuracy  = mean(mean.accuracy))


  # F1 score
# "OutputS_2_2_100_9010_SCBSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_SCBSV_VO")
head(VODf1)
VODf1 <- na.omit(VODf1)
nrow(VODf1)

F1mean2_2_100_UNBAL_SCBSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(F1_Class))
F1mean2_2_100_UNBAL_SCBSV


F1g2_2_100_UNBAL_SCBSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                        color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("F1 test") + xlab("SCBSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



F1g2_2_100_UNBAL_SCBSV


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





# "OutputS_2_2_100_9010_SCBSNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_SCBSNSV_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)
head(VODf1)

F1mean2_2_100_UNBAL_SCBSNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(F1_Class))
F1mean2_2_100_UNBAL_SCBSNSV


F1g2_2_100_UNBAL_SCBSNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                          color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("F1 test") + xlab("SCBSNSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



F1g2_2_100_UNBAL_SCBSNSV


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



# "OutputS_2_2_100_9010_SCBNSV_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_SCBNSV_VO")

VODf1 <- na.omit(VODf1)
nrow(VODf1)

F1mean2_2_100_UNBAL_SCBNSV <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(F1_Class))
F1mean2_2_100_UNBAL_SCBNSV


F1g2_2_100_UNBAL_SCBNSV <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                         color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("F1 test") + xlab("SCBNSV") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



F1g2_2_100_UNBAL_SCBNSV


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

# "OutputS_2_2_100_9010_IND_VO"

VODf1 <- MetricsDf1 %>% filter(Group_Mean_Distance == "VO",
                               Setting == "OutputS_2_2_100_9010_IND_VO")
VODf1 <- na.omit(VODf1)
nrow(VODf1)

nrow(VODf1)

F1mean2_2_100_UNBAL_IND <- VODf1 %>% group_by(Variables) %>%
  summarise(mean.accuracy  = mean(F1_Class))
F1mean2_2_100_UNBAL_IND


F1g2_2_100_UNBAL_IND <- ggplot(VODf1, aes(x = Group_Mean_Distance, y = F1_Class, 
                                      color = Variables)) + 
  geom_boxplot_pattern(pattern_color = "white",
                       pattern_fill = "black",
                       aes(pattern= Variables))+
  ylab("F1 test") + xlab("IND") + ylim(0.4,1) +
  theme(axis.text.x = element_blank())



F1g2_2_100_UNBAL_IND


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





# Accuracy for balanced groups settings
combine13 <- g2_2_100_BAL_IND + g2_2_100_BAL_SCBNSV + 
  g2_2_100_BAL_SCBSNSV + g2_2_100_BAL_SCBSV  & theme(legend.position = "bottom")


g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
}

mylegend <- g_legend(g2_2_100_BAL_IND)

# Accuracy for balanced groups settings
gr13 <- ggarrange(g2_2_100_BAL_IND,
                  g2_2_100_BAL_SCBNSV,
                  g2_2_100_BAL_SCBSNSV,
                  g2_2_100_BAL_SCBSV,
                  ncol = 2,nrow = 2,
                  common.legend = TRUE, legend = "bottom" )

# Accuracy for unbalanced groups settings
gr14 <- ggarrange(g2_2_100_UNBAL_SCBSV,
                    g2_2_100_UNBAL_SCBSNSV,
                    g2_2_100_UNBAL_SCBNSV,
                    g2_2_100_UNBAL_IND,
                    ncol = 2, nrow = 2,
                  common.legend = TRUE, legend = "bottom")


# F1 score for unbalanced groups settings
gr15 <- ggarrange(F1g2_2_100_UNBAL_SCBSV,
                  F1g2_2_100_UNBAL_SCBSNSV,
                  F1g2_2_100_UNBAL_SCBNSV,
                  F1g2_2_100_UNBAL_IND,
                  ncol = 2, nrow = 2,
                  common.legend = TRUE, legend = "bottom")


plot(gr13) # accuracy balanced
plot(gr14) # accuracy unbalanced

plot(gr15) # F1 score unbalanced





