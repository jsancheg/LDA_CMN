library(ggplot2)
library(RColorBrewer)
library(tictoc)
library(corrplot)
library(plotly)
library(psych)

sys_info <- Sys.info()

if (sys_info["nodename"] == "WildFree"){
  pathwd <- getwd()
  pathOutput <-paste0(pathwd,"/Proc_WineNew/")
  
}else {
  pathWd <- "E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
  pathOutput <-"E://University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_WineNew/"
  
}

library("ContaminatedMixt")
#library("HDclassif")
data("wine")

nrow(wine)


if(all (colnames(wine) == c("class",paste0("V",1:13) ) ) )
{
  colnames(wine) <- c("Class","Alcohol","Malic","Ash","Alcalinity","Magnesium","Phenols",
                      "Flavanoids","Nonflavanoids","Proanthocyanins","Color","Hue","Dilution",
                      "Proline")
}
colnames(wine)

Xwine <- wine %>% subset(select = -c(Class))
colnames(Xwine)
y <- as.numeric(wine$Class)
p <- ncol(Xwine)
G <- length(unique(y))

ng <- table(y)

lab <- 1:G
alpha_values <- c(0.75,0.8,0.85)
eta_values <- c(5,10,15)
ptrain <- c(0.7,0.7,0.7)


vpi <-  as.vector(as.vector(table(wine$Class)/length(wine$Class)))


alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))
etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
nameDf <- "WineContColor"

alphaM
etaM

#size_sets <- fun_CalNcont(G,ng,ptrain,alphaM[1,])



pattern <- "A[\\d]+_[\\d]+_[\\d]+_E[\\d]+_[\\d]+_[\\d]+_Wine"


# call function that run simulations only contaminating one variable in this case 
# the variable Colour



cont_vars <- "Color"




dfAll <- Summarise_Files(pathOutput,nameDf,pattern,alphaM,etaM)

dfAll <- readRDS(paste0(pathOutput,"MetricsContDf.RDS"))

dfAll$ContainColor <- as.numeric(str_detect(dfAll$Model,"Color"))


table(dfAll$ContainColor)
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

sort_labels <- function(model_string)
{
  sorted_model <- sort(unlist(str_split(model_string,"-")))
  return(paste(sorted_model,collapse="-"))
}


labels<-df_resumen$Model1

model_sorted <- sapply(df_resumen$Model1,sort_labels)

#find_unique_labels(df_resumen$Model1)

df_resumen$Model <- model_sorted
head(df_resumen)

df_resumen

# compare median of models that contain the variable color vs the ones that does not included it
colnames(df_resumen)
# mean

table(df_resumen$ContainColor)

missing_values <- colSums(is.na(df_resumen)) > 0

missing_values

mean_by_models_containing_color <- tibble(df_resumen) %>% dplyr::select(ContainColor, CR_SatMC,CR_SV,
                                                                Sensitivity_SatM,Sensitivity_SelM,Specificity_SatM,Specificity_SelM)    


aux_matrix <- as.matrix(mean_by_models_containing_color)
x
table(aux_matrix[,1])

apply(aux_matrix[aux_matrix[,1] == 0,-1],2,mean)
apply(aux_matrix[aux_matrix[,1] == 1,-1],2,mean)

apply(aux_matrix[aux_matrix[,1] == 0,-1],2,median)
apply(aux_matrix[aux_matrix[,1] == 1,-1],2,median)


sapply(0:1,function(i) {
  apply(mean_by_models_containing_color[mean_by_models_containing_color$ContainColor == i,-1],2,mean)
})

mean_by_models_containing_color <- tibble(df_resumen) %>% dplyr::select(ContainColor, CR_SatMC,CR_SV,
                                Sensitivity_SatM,Sensitivity_SelM,Specificity_SatM,Specificity_SelM) %>%
                                dplyr::group_by(ContainColor) %>% summary(across(where(is.numeric), mean, na.rm = TRUE))

mean_by_models_containing_color


colnames(mean_by_models_containing_color)
mean_by_models_containing_color %>% dplyr::group_by(ContainColor) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))

#cross_group_mean_distance <- svo_df %>% dplyr::select(Group_Mean_Distance,Variables,CCR,Recall_Cont,Precision_Cont) %>%
#  group_by(Group_Mean_Distance,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))


mean_by_models_containing_color

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


colnames(dfAll)
df_aux_0 <- dfAll %>% dplyr::select(Model,Alpha,Eta,CR_SatMC,CR_SV,Sensitivity_SatM,Sensitivity_SelM,Specificity_SatM,Specificity_SelM) %>% 
  dplyr::rename ("CCR_All" = "CR_SatMC",
                 "CCR_SM" = "CR_SV",
                 "Sensitivity_All" = "Sensitivity_SatM",
                 "Sensitivity_SM" = "Sensitivity_SelM",
                 "Specificity_All" = "Specificity_SatM",
                 "Specificity_SM" = "Specificity_SelM")

colnames(df_aux_0)
df_aux_1 <- df_aux_0 %>%  dplyr::rename("A1" = "CCR_All",
                                        "A2" = "CCR_SM",
                                        "B1" = "Sensitivity_All",
                                        "B2" = "Sensitivity_SM",
                                        "C1" = "Specificity_All",
                                        "C2" = "Specificity_SM")
colnames(df_aux_1)

df_long_2 <- df_aux_1 %>%
  tidyr::pivot_longer(
    cols = A1:C2,
    names_to = c(".value","Variables"),
    names_pattern = "(.)(.)"
  )

colnames(df_long_2)

df_long_3 <- df_long_2 %>% dplyr::rename("CCR" = "A",
                                         "Sensitivity" ="B",
                                         "Specificity" = "C")

colnames(df_long_3)

unique(df_long_3$Variables)
head(dfAll,2)


head(df_long_3)
unique(df_long_3$Variables )

df_long_3 <- df_long_3 %>% mutate(Variables = fct_recode(Variables,
                                                         "All" = "1",
                                                         "Selected" = "2"))

colnames(df_long_3)
df_long_4 <- df_long_3 %>% mutate(Model,Alpha,Eta,CCR,Sensitivity,Specificity) %>%
  pivot_longer(c(CCR,Sensitivity,Specificity),
               names_to = "Metrics",
               values_to = "Value")

  head(df_long_4,10)


dfDifLong<- dfAll %>% dplyr::select(Alpha,Eta,ContainColor,DifCCR,DifAccuracy,
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



dfDifLong_1<- dfAll %>% dplyr::select(Alpha,Eta,DifCCR,
                                      DifSensitivity,DifSpecificity) %>% 
  pivot_longer(c(DifCCR,DifSensitivity,DifSpecificity),
               names_to = "Metrics",
               values_to = "Dif")

head(dfDifLong)
head(dfDifLong_1)

dfDifLong_1 <- dfDifLong_1 %>% mutate(Metrics = recode(Metrics,
                                                       DifCCR = "CCR",
                                                       DifSensitivity = "Sensitivity" ,
                                                       DifSpecificity = "Specificity"))


head(dfDifLong_1)

# Calculate frequency of model size ---------------------------------------

sum(tab1)


df_models_topn <- df_models %>% top_n(8,df_models$frequency)
nrow(df_models_topn)

freqModelSize <- table(ModelSize)
freqModelSize  
round(freqModelSize*100 /sum(freqModelSize)) 

tibble_freqModelSize <- tibble(Size = row.names(freqModelSize), Frequency = as.numeric(freqModelSize) )
tibble_freqModelSize
coul <- brewer.pal(9,"Set3")
par(mfrow = c(1,1))



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
if(any(str_detect(colnames(wine),"Type")) )
{
  colnames(wine)
  unique(wine$Type)
  XWine <- wine %>% select(-c(Type))
  
  colnames(XWine)
  
  XWine_mean <- wine %>% group_by(Type) %>%
    summarise(Alcohol_mean = mean(Alcohol),
              Malic_mean = mean(Malic),
              Ash_mean = mean(Ash),
              Alcalinity_mean = mean(Alcalinity),
              Magnesium_mean = mean(Magnesium),
              Phenols_mean = mean(Phenols),
              Flavanovids_mean = mean(Flavanoids),
              Nonflavanoid_mean = mean(Nonflavanoid),
              Proanthocyanins_mean = mean(Proanthocyanins),
              Color_mean = mean(Color),
              Hue_mean = mean(Hue),
              Dilution_mean = mean(Dilution),
              Proline_mean = mean(Proline)
    )
  
  }else 
    {
          colnames(wine)
          unique(wine$Class)
          XWine <- wine %>% dplyr::select(-c(Class))
  
          colnames(XWine)
  
            XWine_mean <- wine %>% dplyr::group_by(Class) %>%
              summarise(Alcohol_mean = mean(Alcohol),
              Malic_mean = mean(Malic),
              Ash_mean = mean(Ash),
              Alcalinity_mean = mean(Alcalinity),
              Magnesium_mean = mean(Magnesium),
              Phenols_mean = mean(Phenols),
              Flavanovids_mean = mean(Flavanoids),
              Nonflavanoid_mean = mean(Nonflavanoids),
              Proanthocyanins_mean = mean(Proanthocyanins),
              Color_mean = mean(Color),
              Hue_mean = mean(Hue),
              Dilution_mean = mean(Dilution),
              Proline_mean = mean(Proline)
              )
}

XWine_mean

#meanClass_dist <- sqrt(sum( (XWine_mean[1,-1] - XWine_mean[2,-1])^2 ))
#meanClass_dist

mycols <- c("blue","green","red")
pairs(XWine %>% dplyr::select(Color,Hue,Dilution,Alcohol,Malic,Flavanoids) , 
      oma = c(3,3,6,3),
      col = mycols[as.numeric(wine$Type)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(wine$Type),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)



# Calculate the  F statistics------------------------------------------------

y <- as.numeric(wine$Type)
dfRW <- getOW(XWine,as.numeric(y))
head(dfRW,6)

dfRWsort <- dfRW[order(-dfRW$Ftest),]


options(repr.plot.width=8, repr.plot.height=3)
gFtest <- ggplot(dfRWsort, aes(x = reorder(Var, +Ftest),y = Ftest) ) +
  geom_bar(stat = "identity",fill = "lightblue") +
  coord_flip() + 
  ylab("Variables") +  xlab("F score") +
  geom_text(aes(x=Var, y = Ftest + 0.14, label = round(Ftest,0) ) ,check_overlap = TRUE)
gFtest %>% ggplotly



# Plot correlations of Wine data set --------------------------------------

data(wine)

colnames(wine)
XWine <- wine %>% subset(select = -c(Type))
rquery.cormat(XWine)
corrplot::corrplot(cor(XWine),
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

g_freqSV <- ggplot(df_freqSV, 
                   aes(x = reorder(Variables,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Frequency") + xlab("Variables") +
  geom_text(aes(x = Variables, y = frequency + 0.3, 
                label = frequency),check_overlap = TRUE)
g_freqSV %>% ggplotly


colnames(dfAll)
# Plot 1: Boxplot of CR_SatMC and CR_SV
plot1<- ggplot(df_long_4 %>% filter(Metrics %in% c("CCR","Sensitivity","Specificity")), 
               aes(x = Metrics, y = Value, color = Variables)) + 
  ylab("Ratio")+ xlab("Metric")+ylim(0.5,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

plot1





# plot differences between SV - All variables  ----------------------------


# plot differences between SV - All variables  ----------------------------
g10<- ggplot(dfDifLong_1 %>% filter(Metrics %in% c("CCR","Sensitivity","Specificity")), 
             aes(x = Metrics, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.5,0.5) + geom_boxplot() +
  geom_hline(yintercept = 0)

g10


g20 <- ggplot(dfDifLong_1 %>% filter(Metrics %in% c("CCR","Sensitivity")), 
              aes(x = Metrics, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot() +
  geom_hline(yintercept = 0)

g20

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


# plot differences between SV- All variables with COLOR in the mod --------


g5<- ggplot(dfDifLong %>% filter(ContainColor == 1)%>% filter(Variables %in% c("CCR","Accuracy")), 
            aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)

g5


g6 <- ggplot(dfDifLong %>% filter(ContainColor == 1) %>%filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() + 
  geom_hline(yintercept = 0)


g6


g7 <- ggplot(dfDifLong %>% filter(ContainColor == 1) %>% filter(Variables %in% c("CCR","Accuracy")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)


g7 

g8 <- ggplot(dfDifLong %>% filter(ContainColor == 1) %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

g8


# plot differences between SV- All variables without COLOR in the mod --------


g9<- ggplot(dfDifLong %>% filter(ContainColor == 0)%>% filter(Variables %in% c("CCR","Accuracy")), 
            aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)

g9


g10 <- ggplot(dfDifLong %>% filter(ContainColor == 0) %>%filter(Variables %in% c("Sensitivity","Specificity")), 
              aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() + 
  geom_hline(yintercept = 0)


g10


g11 <- ggplot(dfDifLong %>% filter(ContainColor == 0) %>% filter(Variables %in% c("CCR","Accuracy")), 
              aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)


g11 

g12 <- ggplot(dfDifLong %>% filter(ContainColor == 0) %>% filter(Variables %in% c("Sensitivity","Specificity")), 
              aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

g12



