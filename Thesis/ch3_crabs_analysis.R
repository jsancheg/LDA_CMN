source("VSCMN.R")
library(ggplot2)
library(RColorBrewer)
library(tictoc)
library(corrplot)
library(plotly)

sort_labels <- function(model_string)
{
  sorted_model <- sort(unlist(str_split(model_string,"-")))
  return(paste(sorted_model,collapse="-"))
}



dfAll <- readRDS("Crabs_Metrics_25_03_2024.RDS")
head(dfAll)

dfAll$ContainCL <- as.numeric(str_detect(dfAll$Model,"CL"))
dfAll$ContainRW <- as.numeric(str_detect(dfAll$Model,"RW"))
dfAll$ContainCW <- as.numeric(str_detect(dfAll$Model,"CW"))
dfAll$ContainFL <- as.numeric(str_detect(dfAll$Model,"FL"))
dfAll$ContainBD <- as.numeric(str_detect(dfAll$Model,"BD"))



table(dfAll$ContainCL)
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

df_resumen <- df_resumen %>% mutate(Model1 = sapply(Model,sort_labels)) %>% 
  relocate(Model1, .after = Model)

df_resumen <- df_resumen %>% dplyr::select( -Model )

df_resumen <- df_resumen %>% dplyr::rename(Model = Model1)



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
sum(my_tab1_sort[1:10])/sum(my_tab1_sort)
sum(my_tab1_sort)

df_models <- data.frame(frequency = as.vector(my_tab1_sort),
                        model = rownames(my_tab1_sort))

# 8 first models represents the 80% 8f the 810 simulations


aux <- str_split(df$Model,"-",simplify = TRUE)
model_size <- apply(aux,1, function(row) sum(row!="") )
dfAll$Model_Size <- model_size

non_empty_count <- apply(matrix_data, 1, function(row) sum(row != ""))


class(aux)


dfAll$Model[1:2]


sapply(dfAll$Model, function(m) length(unlist(split(m,"-") ) ) )

dfAll <- dfAll  %>% mutate (DifCCR = CR_SV - CR_SatMC)
dfAll <- dfAll %>% mutate (DifAccuracy = Accuracy_SVCont - Accuracy_SatCont  )
dfAll <- dfAll %>% mutate (DifSensitivity = Sensitivity_SelM - Sensitivity_SatM  )
dfAll <- dfAll %>% mutate (DifSpecificity = Specificity_SelM - Specificity_SatM  )

summary(dfAll$DifAccuracy)
summary(dfAll$DifCCR)

summary(dfAll$DifSensitivity)
summary(dfAll$DifSpecificity)

str_split(dfAll$source,"_",simplify = TRUE)[,2]

str_split(dfAll$source,"_",simplify = TRUE)[,4]

dfAll$Alpha1 <-str_replace_all(str_split(dfAll$source,"_",simplify = TRUE)[,1],"A","")
dfAll$Alpha2 <- str_split(dfAll$source,"_",simplify = TRUE)[,2]
dfAll$Eta1 <- str_replace_all(str_split(dfAll$source,"_",simplify = TRUE)[,3],"E","")
dfAll$Eta2 <- str_split(dfAll$source,"_",simplify = TRUE)[,4]



dfAll$Alpha <- ifelse(dfAll$Alpha1 == dfAll$Alpha2,"Equal","Inequal")

dfAll$Eta <- ifelse(dfAll$Eta1 == dfAll$Eta2,"Equal","Inequal")

head(dfAll)
dfAll[dfAll$source]


# Calculate frequency of variables including in the selected model --------

dfAll %>% dplyr::select(ContainCL,ContainRW,ContainCW, ContainFL, ContainBD) %>% apply(2,mean) %>% round(2)

# Calculate the model size



dfAll %>% colnames
df_resumen %>% colnames



dfAll$Model <- as.factor(dfAll$Model)

dfAll %>% dplyr::select(ContainCL,ContainRW,ContainCW, ContainFL, ContainBD) %>% apply(2,mean) %>%  round(2)


# Calculate cross table of the metircs by Model and Model Size 
head(dfAll$Model)
dfAll %>% dplyr::select(Model,CR_SV,Sensitivity_SelM,CR_SatMC,Accuracy_SatCont,Sensitivity_SatM) %>% 
  dplyr::group_by(Model) %>%  dplyr::summarise(across(where(is.numeric), median, na.rm = TRUE) )

dfAll %>% dplyr::select(Model,CR_SV,Sensitivity_SelM,CR_SatMC,Accuracy_SatCont,Sensitivity_SatM) %>% 
  dplyr::group_by(Model) %>%  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE) )


dfAll %>% dplyr::select(Model_Size,CR_SV,Sensitivity_SelM,CR_SatMC,Accuracy_SatCont,Sensitivity_SatM) %>% 
  dplyr::group_by(Model_Size) %>%  dplyr::summarise(across(where(is.numeric), median, na.rm = TRUE) )

dfAll %>% dplyr::select(Model_Size,CR_SV,Sensitivity_SelM,CR_SatMC,Accuracy_SatCont,Sensitivity_SatM) %>% 
  dplyr::group_by(Model_Size) %>%  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE) )


dfAll %>% dplyr::select(CR_SatMC,CR_SV,Sensitivity_SatM,Sensitivity_SelM) %>% apply(2,mean)

dfAll %>% dplyr::select(CR_SatMC,CR_SV,Sensitivity_SatM,Sensitivity_SelM) %>% apply(2,median)


#  Calculate Dif between SV-All variables short and long format -----------
colnames(dfAll)
df_aux_0 <- dfAll %>% dplyr::select(Model,Alpha,Eta,CR_SatMC,CR_SV,Sensitivity_SatM,Sensitivity_SelM) %>% 
                      dplyr::rename ("CCR_All" = "CR_SatMC",
                                    "CCR_SM" = "CR_SV",
                                    "Sensitivity_All" = "Sensitivity_SatM",
                                    "Sensitivity_SM" = "Sensitivity_SelM")

colnames(df_aux_0)
df_aux_1 <- df_aux_0 %>%  dplyr::rename("A1" = "CCR_All",
                                    "A2" = "CCR_SM",
                                    "B1" = "Sensitivity_All",
                                    "B2" = "Sensitivity_SM")
colnames(df_aux_1)

df_long_2 <- df_aux_1 %>%
  tidyr::pivot_longer(
    cols = A1:B2,
    names_to = c(".value","Variables"),
    names_pattern = "(.)(.)"
  )

colnames(df_long_2)

df_long_3 <- df_long_2 %>% dplyr::rename("CCR" = "A",
                                         "Sensitivity" ="B")

colnames(df_long_3)

unique(df_long_3$Variables)
head(dfAll,2)


head(df_long_3)
unique(df_long_3$Variables )

df_long_3 <- df_long_3 %>% mutate(Variables = fct_recode(Variables,
                                                         "All" = "1",
                                                         "Selected" = "2"))


df_long_4 <- df_long_3 %>% mutate(Model,Alpha,Eta,CR_SatMC,CR_SV,Sensitivity_SatM,Sensitivity_SelM,
                                  CCR,Sensitivity) %>%
  pivot_longer(c(CCR,Sensitivity),
               names_to = "Metrics",
               values_to = "Value")

dfDifLong<- dfAll %>% dplyr::select(Alpha,Eta,ContainCL,DifCCR,DifAccuracy,
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
                                    DifSensitivity) %>% 
  pivot_longer(c(DifCCR,DifSensitivity),
               names_to = "Metrics",
               values_to = "Dif")

head(dfDifLong)
dfDifLong_1 <- dfDifLong_1 %>% mutate(Metrics = recode(Metrics,
                                                     DifCCR = "CCR",
                                                     DifSensitivity = "Sensitivity") )







dfDifLong_1 <- dfDifLong %>% select(Alpha,Eta,DifAccuracy,DifSensitivity)

# Calculate frequency of model size ---------------------------------------

sum(tab1)


df_models_topn <- df_models %>% top_n(8,df_models$frequency)
nrow(df_models_topn)

freqModelSize <- table(ModelSize)
freqModelSize  
tibble_freqModelSize <- tibble(Size = row.names(freqModelSize), Frequency = as.numeric(freqModelSize) )
tibble_freqModelSize
coul <- brewer.pal(9,"Set3")
par(mfrow = c(1,1))




mean(dfAll$Model_Size)  

#cross_group_mean_distance <- svo_df %>% dplyr::select(Group_Mean_Distance,Variables,CCR,Recall_Cont,Precision_Cont) %>%
#  group_by(Group_Mean_Distance,Variables) %>% summarise(across(where(is.numeric), median, na.rm = TRUE))


  
freqVar <- table(unlist(str_split(dfAll$Model,"-")))
nb.cols <- 13
coul <- colorRampPalette(brewer.pal(8,"Set2"))(nb.cols) 

sort(freqVar,decreasing = TRUE)
round(sort(100*freqVar,decreasing = TRUE)/sum(freqVar))


sort(round(prop.table(freqVar)*100,2),decreasing = TRUE)


df_freqSV <- data.frame(frequency = as.vector(freqVar),
                        Variables = rownames(freqVar))


str_detect(df_resumen$Model,"RW")

colnames(dfAll)
#dfAll <- dfAll %>% filter(CR_SV >0 ) %>% mutate (DifCCR = CR_SV - CR_SatMC)






# Plot correlation matrix of original data Crabs --------------------------
data(crabs)
colnames(crabs)
XCrabs <- crabs %>% subset(select = -c(index,sp,sex))
rquery.cormat(XCrabs)


# Plot of the frequency of the models
ggp <- ggplot(df_models_topn, 
              aes(x = reorder(model,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Model") + xlab("Frequency") +
  geom_text(aes(x = model, y = frequency + 0.3, label = frequency),check_overlap = TRUE)
ggp %>% ggplotly


# Plot for the frequency of model size 
bp <- barplot(as.numeric(freqModelSize),col = coul,ylim = c(0,260))
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


# Plot crab female and male groups for blue species -----------------------
library("MASS")
view_data()
colnames(crabs)
unique(crabs$sp)
XBlueCrabs <- crabs %>% filter(sp == "B")

XBlueCrabs_mean <- XBlueCrabs %>% group_by(sex) %>%
  summarise(FL_mean = mean(FL),
            RW_mean = mean(RW),
            CL_mean = mean(CL),
            CW_mean = mean(CW),
            BD_mean = mean(BD) )

XBlueCrabs_mean
meanClass_dist <- sqrt(sum( (XBlueCrabs_mean[1,-1] - XBlueCrabs_mean[2,-1])^2 ))
meanClass_dist

mycols <- c("blue","green")
pairs(XBlueCrabs %>% dplyr::select(FL,RW,CL,CW,BD), oma = c(3,3,6,3),
      col = mycols[as.numeric(XBlueCrabs$sex)],
      gap = 0, lower.panel = NULL)
legend("top", col = mycols, legend = levels(XBlueCrabs$sex),pch = 20,
       xpd = NA, ncol = 3, bty = "n", inset = 0.01, pt.cex = 1.5)

colnames(XBlueCrabs)
head(XBlueCrabs,30)
tail(XBlueCrabs)


plot(XBlueCrabs[,c("CL","RW")], col = ifelse(XBlueCrabs$sex == "M", "blue", "green"), 
     pch = 15+GenDataD.1$ltrain,
     xlab = "X1", ylab = "X2")
legend("bottomleft", legend = c("Non Contaminated","Contaminated"), 
       col = c("blue","red"),
       pch = c(16,16))
#text(3.614436,-1.094842,"56",-0.5)



# Metrics -----------------------------------------------------------------



# Calculate the  F statistics------------------------------------------------

y <- as.numeric(XBlueCrabs$sex)
dfRW <- getOW(XBlueCrabs%>% dplyr::select(FL,RW,CL,CW,BD),as.numeric(y))
head(dfRW,6)

dfRWsort <- dfRW[order(-dfRW$Ftest),]


options(repr.plot.width=8, repr.plot.height=3)
gFtest <- ggplot(dfRWsort, aes(x = reorder(Var, +Ftest),y = Ftest) ) +
  geom_bar(stat = "identity",fill = "lightblue") +
  coord_flip() + 
  ylab("Variables") +  xlab("F score") +
  geom_text(aes(x=Var, y = Ftest + 0.14, label = round(Ftest,0) ) ,check_overlap = TRUE)
gFtest %>% ggplotly


colnames(dfAll)
# Plot 1: Boxplot of CR_SatMC and CR_SV
plot1<- ggplot(df_long_4 %>% filter(Metrics %in% c("CCR","Sensitivity")), 
            aes(x = Metrics, y = Value, color = Variables)) + 
  ylab("Ratio")+ xlab("Metric")+ylim(0.5,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

plot1



plot1

# plot differences between SV - All variables  ----------------------------
g10<- ggplot(dfDifLong_1 %>% filter(Metrics %in% c("CCR","Sensitivity")), 
            aes(x = Metrics, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot() +
  geom_hline(yintercept = 0)

g10


g20 <- ggplot(dfDifLong_1 %>% filter(Metrics %in% c("CCR","Sensitivity")), 
             aes(x = Metrics, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot() +
  geom_hline(yintercept = 0)


g20 

unique(dfDifLong_1)

g4 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

g4



# plot differences between SV- All variables with CL in the mod --------


g1<- ggplot(dfDifLong %>% filter(ContainCL == 1)%>% filter(Variables %in% c("CCR","Accuracy")), 
            aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)

g1


g2 <- ggplot(dfDifLong %>% filter(ContainCL == 1) %>%filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() + 
  geom_hline(yintercept = 0)


g2


g3 <- ggplot(dfDifLong %>% filter(ContainCL == 1) %>% filter(Variables %in% c("CCR","Accuracy")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)


g3 

g4 <- ggplot(dfDifLong %>% filter(ContainCL == 1) %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

g4



# plot diffrences between SV- All variables withouth CL in the mod --------


g1<- ggplot(dfDifLong %>% filter(ContainCL == 0)%>% filter(Variables %in% c("CCR","Accuracy")), 
            aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)

g1


g2 <- ggplot(dfDifLong %>% filter(ContainCL == 0) %>%filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() + 
  geom_hline(yintercept = 0)


g2


g3 <- ggplot(dfDifLong %>% filter(ContainCL == 0) %>% filter(Variables %in% c("CCR","Accuracy")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)


g3 

g4 <- ggplot(dfDifLong %>% filter(ContainCL == 0) %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

g4



