library(ggplot2)
library(RColorBrewer)
library(tictoc)
library(corrplot)
library(plotly)
library(psych)

pathWd <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
pathOutput <-"E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_Wdbc/"
source("http://www.sthda.com/upload/rquery_cormat.r")


setwd(pathWd)
dir(pathOutput)


dfAll <- Summarise_Files(pathOutput,nameDf,pattern,alphaM,etaM)

dfAll <- readRDS(paste0(pathOutput,"MetricsContDf.RDS"))

#dfAll$ContainColor <- as.numeric(str_detect(dfAll$Model,"Color"))

#table(dfAll$ContainColor)
colnames(dfAll)
nrow(dfAll)

# Prepare data to identify unique models, calculate and plot their frequency
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
nrow(df_resumen)



labels<-df_resumen$Model1
source("FunctionsConsolidate.R")

# find_unique_labels(df_resumen$Model1)

df_resumen$Model <- find_unique_labels(df_resumen$Model1)$newlabels
head(df_resumen)



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
sum(my_tab1_sort[1:7])/sum(my_tab1_sort)
sum(my_tab1_sort)

df_models <- data.frame(frequency = as.vector(my_tab1_sort),
                        model = rownames(my_tab1_sort))

# 8 first models represents the 80% 8f the 810 simulations

#  Calculate Dif between SV-All variables short and long format -----------

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



# Calculate frequency of model size ---------------------------------------

sum(tab1)


df_models_topn <- df_models %>% top_n(8,df_models$frequency)
nrow(df_models_topn)

freqModelSize <- table(ModelSize)
freqModelSize  
rownames(freqModelSize)
round(freqModelSize*100 /sum(freqModelSize)) 

tibble_freqModelSize <- tibble(Size = row.names(freqModelSize), Frequency = as.numeric(freqModelSize) )
tibble_freqModelSize
coul <- brewer.pal(9,"Set3")
par(mfrow = c(1,1))

df_Modsize <- data.frame(freqModelSize)
colnames(df_Modsize)[2] <- "frequency"
head(df_Modsize)

# Calculate frequency of variables including in the selected model --------



freqVar <- table(unlist(str_split(dfAll$Model,"-")))
nb.cols <- 13
coul <- colorRampPalette(brewer.pal(8,"Set2"))(nb.cols) 

sort(freqVar,decreasing = TRUE)
round(sort(100*freqVar,decreasing = TRUE)/nrow(dfAll) )



sort(round(prop.table(freqVar)*100,2),decreasing = TRUE)


df_freqSV <- data.frame(frequency = as.vector(freqVar),
                        Variables = rownames(freqVar))




colnames(dfAll)
#dfAll <- dfAll %>% filter(CR_SV >0 ) %>% mutate (DifCCR = CR_SV - CR_SatMC)


# Plot wine type of wine non-contaminated data set -----------------------
colnames(wdbc)
table(wdbc$Diagnosis)/nrow(wdbc)
unique(wine$Diagnosis)
XWdbc <- wdbc %>% select(-c(ID,Diagnosis))

colnames(XWdbc)

XWdbc_mean <- wdbc %>% dplyr::select(-c(ID)) %>% group_by(Diagnosis) %>% 
          summarise(across(everything(),mean)
          ,.groups = "drop"  )

XWdbc_mean

#meanClass_dist <- sqrt(sum( (XWine_mean[1,-1] - XWine_mean[2,-1])^2 ))
#meanClass_dist

mycols <- c("blue","green")
pairs(XWdbc %>% dplyr::select(Nconcave_extreme, Perimeter_extreme,
              Nconcave_mean, Radius_extreme, Perimeter_mean, Area_extreme) , oma = c(3,3,6,3),
      col = mycols[as.numeric(wdbc$Diagnosis)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(wdbc$Diagnosis),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)




# Calculate the  F statistics------------------------------------------------

y <- as.numeric(wdbc$Diagnosis)
dfRW <- getOW(XWdbc,as.numeric(y))
head(dfRW,6)

dfRWsort <- dfRW[order(-dfRW$Ftest),]


options(repr.plot.width=8, repr.plot.height=3)
gFtest <- ggplot(dfRWsort, aes(x = reorder(Var, +Ftest),y = Ftest) ) +
  geom_bar(stat = "identity",fill = "lightblue") +
  coord_flip() + 
  ylab("Variables") +  xlab("F score") +
  geom_text(aes(x=Var, y = Ftest + 0.14, label = round(Ftest,0) ) ,check_overlap = TRUE)
gFtest %>% ggplotly


# Plot correlations of Wdbc data set --------------------------------------

data(wdbc)
colnames(wdbc)
XWdbc <- wdbc %>% subset(select = -c(ID,Diagnosis))
corrplot::corrplot(cor(XWdbc),
                   method = "circle",
                   type = 'l',
                   diag = FALSE,
                   tl.col = "black",
                   bg = "white",
                   title = "",
                   col = rev(colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                                                "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                                                "#4393C3", "#2166AC", "#053061"))(200)))






# Plot of the frequency of the models
ggp <- ggplot(df_models_topn, 
              aes(x = reorder(model,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Model") + xlab("Frequency") +
  geom_text(aes(x = model, y = frequency + 0.3, label = frequency),check_overlap = TRUE)
ggp %>% ggplotly


# Plot for the frequency of model size 
bp <- barplot(as.numeric(freqModelSize),col = coul,ylim = c(0,760))
text(bp, as.numeric(freqModelSize), labels = round(freqModelSize,0))


# Plot for the frequency that a variable is selected in the model ---------

g_freqMS <- ggplot(df_Modsize, 
                   aes(x = reorder(ModelSize,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Frequency") + xlab("Model Size") +
  geom_text(aes(x = ModelSize, y = frequency + 0.3, 
                label = frequency),check_overlap = TRUE)
g_freqMS %>% ggplotly




# Plot for the frequency that a variable is selected in the model ---------

g_freqSV <- ggplot(df_freqSV, 
                   aes(x = reorder(Variables,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Frequency") + xlab("Variables") +
  geom_text(aes(x = Variables, y = frequency + 0.3, 
                label = frequency),check_overlap = TRUE)
g_freqSV %>% ggplotly




# plot differences between SV - All variables  ----------------------------
# plot the data


g1<- ggplot(dfDifLong %>% filter(Variables %in% c("CCR","Accuracy")), 
            aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)

g1


g2 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() + 
  geom_hline(yintercept = 0)


g2


g3 <- ggplot(dfDifLong %>% filter(Variables %in% c("CCR","Accuracy")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)


g3 

g4 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

g4

