library(dplyr)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(ggpattern)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(caret)


pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"

alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)
vpi <- as.vector(as.vector(table(wine$Type)/length(wine$Type)))



eta_values <- c(5,10,15)
alpha_values <-c(0.75,0.8,0.85)

etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))


name_file<-list()
ind_file <- list()
cont <- 1
for(i_a in 1:nrow(alphaM))
  for(i_eta in 1:nrow(etaM))
  {
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    name_file[[cont]] <- paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Wine.Rdata")
    cond <- which(str_detect(dir(pathWd),name_file[[cont]])==TRUE)
    if(!is_empty(cond))
      {
      ind_file[[cont]] <- cond
      cont <- cont + 1
    }    
  }

name_files <- unlist(name_file)
name_files
ind_files <- unlist(ind_file)
i_files <- length(name_files)
Metrics_Res <- list()
Metrics_Models <- list()


length(name_files)

aux <- list()
for (i_file in ind_files)
{  
    load(name_files[i_file])
    auxSim$Metrics_res
    auxSim$Metrics_models
    class(auxSim$Metrics_models)

    auxDf <- auxSim$Metrics_models
    auxDf$file <- name_files[i_file]
    auxDf <- auxDf %>% dplyr::select(file,everything())
    parameters <- str_split(str_replace_all(auxDf$file,"[(A-Z)|(a-z)|\\.]",""),"_",simplify = TRUE)[,1:6]
    auxDf$parameters <- parameters
    auxDf$Alpha <- ifelse(parameters[,1] == parameters[,2] & parameters[,2] == parameters[,3],"Equal","Inequal")
    auxDf$Eta <- ifelse(parameters[,4] == parameters[,5] & parameters[,5] == parameters[,6],"Equal","Inequal")
    
    aux[[i_file]] <- auxDf
    
  
}

auxDf <- ldply(aux)
auxDf$Alpha <- factor(auxDf$Alpha)
auxDf$Eta <- factor(auxDf$Eta)

colnames(auxDf)
nrow(auxDf)



dfAll <- auxDf %>% dplyr::select(-file)
nModels <- length(dfAll$Model)
ModelSize<-rep(0,nModels)
for(i_model in 1:nModels)
{
  ModelSize[i_model] <- length(unlist(str_split(dfAll$Model[i_model],"-")))
}

df_resumen <- mutate(dfAll,Model1 = Model)
df_resumen <- df_resumen %>% relocate(Model1, .after = Model)
colnames(df_resumen)
df_resumen <- df_resumen %>% relocate(Model, .after = Model1)
colnames(df_resumen)

#  df_resumen <- rename(df_resumen,Model1 = Model)


labels<-df_resumen$Model1
source("FunctionsConsolidate.R")

find_unique_labels(df_resumen$Model1)

df_resumen$Model <- find_unique_labels(df_resumen$Model1)$newlabels
head(df_resumen)

colnames(df_resumen)
nrow(df_resumen)

metrics_res <- df_resumen %>% group_by(Model) %>%
  summarise(CCR_SatMNc = mean(CR_SatMNc),
            CCR_SatMC = mean(CR_SatMC),
            CCR_SV = mean(CR_SV),
            Accuracy_Sat = mean(Accuracy_SatCont),
            Accuracy_SV = mean(Accuracy_SVCont),
            Sensitivity_SatM = mean(Sensitivity_SatM),
            Sensitivity_SelM = mean(Sensitivity_SelM),
            Specificity_SatM = mean(Sensitivity_SelM),
            Specificity_SelM = mean(Specificity_SelM))


tab1 <- table(df_resumen$Model)
my_tab1_sort <- tab1[order(tab1, decreasing = TRUE)]
head(my_tab1_sort,5)
sum(my_tab1_sort[1:40])/sum(my_tab1_sort)
sum(my_tab1_sort)

df_models <- data.frame(frequency = as.vector(my_tab1_sort),
                        model = rownames(my_tab1_sort))

# 8 first models represents the 80% 8f the 810 simulations
sum(tab1)


df_models_topn <- df_models %>% top_n(8,df_models$frequency)
nrow(df_models_topn)


ggp <- ggplot(df_models_topn, 
              aes(x = reorder(model,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Model") + xlab("Frequency") +
  geom_text(aes(x = model, y = frequency + 0.3, label = frequency),check_overlap = TRUE)
ggp

head(df_models)
library(plotly)
ggp %>% ggplotly



#metrics_res %>% filter(Model %in% c("BD-CL-FL-CW-RW","CL-RW",
#                                    "BD-CL-RW", "BD-CL-CW-RW",
#                                    "FL-CL-RW",
#                                    "CL-CW-FL-RW",
#                                    "BD-CL-FL-RW"))




freqModelSize <- table(ModelSize)
freqModelSize  
coul <- brewer.pal(9,"Set3")

par(mfrow = c(1,2))
barplot(prop.table(freqModelSize),col = coul,ylim = c(0,0.4))


freqVar <- table(unlist(str_split(dfAll$Model,"-")))
nb.cols <- 13
coul <- colorRampPalette(brewer.pal(8,"Set2"))(nb.cols) 


sort(round(prop.table(freqVar)*100,2),decreasing = TRUE)

barplot(prop.table(freqVar),col = coul, ylim = c(0,0.4))

par(mfrow = c(1,1))

colnames(dfAll)
#dfAll <- dfAll %>% filter(CR_SV >0 ) %>% mutate (DifCCR = CR_SV - CR_SatMC)

dfAll <- dfAll  %>% mutate (DifCCR = CR_SV - CR_SatMC)
dfAll <- dfAll %>% mutate (DifAccuracy = Accuracy_SVCont - Accuracy_SatCont  )
dfAll <- dfAll %>% mutate (DifSensitivity = Sensitivity_SelM - Sensitivity_SatM  )
dfAll <- dfAll %>% mutate (DifSpecificity = Specificity_SelM - Specificity_SatM  )

summary(dfAll$DifAccuracy)
summary(dfAll$DifCCR)

summary(dfAll$DifSensitivity)
summary(dfAll$DifSpecificity)

dfDifLong<- dfAll %>% dplyr::select(Alpha,Eta,DifCCR,DifAccuracy,
                                    DifSensitivity,DifSpecificity) %>% 
  pivot_longer(c(DifCCR,DifAccuracy,DifSensitivity,DifSpecificity),
               names_to = "Variables",
               values_to = "Dif")

head(dfDifLong)
dfDifLong <- dfDifLong %>% mutate(Variables = recode(Variables,
                                                     DifCCR = "CCR",
                                                     DifAccuracy = "Accuracy",
                                                     DifSensitivity = "Sensitivity",
                                                     DifSpecificity = "Specificity"))

g1<- ggplot(dfDifLong %>% filter(Variables %in% c("CCR","Accuracy")), 
            aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot()

g1


g3 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot()


g3


g4 <- ggplot(dfDifLong %>% filter(Variables %in% c("CCR","Accuracy")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot()

g4 

g6 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot()

g6

plot(g1)

plot(g2)

plot(g3)

plot(g4)