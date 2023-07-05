library(dplyr)
library(tidyr)
library(tidyverse)
library(gclus)
library(RColorBrewer)
library(tictoc)


pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")

pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"

data("wine")
colnames(wine)
table(wine$Type)

Xwine <- wine %>% subset(select = -c(Type))
colnames(Xwine)
y <- as.numeric(wine$Type)
G <- length(unique(y))
ind_1 <- y == 1
ind_2 <- y == 2
ind_3 <- y == 3

meanWine_1 <- Xwine[ind_1,] %>% apply(2,mean)
meanWine_2 <- Xwine[ind_2,] %>% apply(2,mean)
meanWine_3 <- Xwine[ind_3,] %>% apply(2,mean)

# p number of variables
p <- length(meanWine_1)

GWine_1 <- Xwine[ind_1,] %>% var
GWine_2 <- Xwine[ind_2,] %>% var
GWine_3 <- Xwine[ind_3,] %>% var

summary(factor(wine$Class))/nrow(wine)

mug <- matrix (0.0, nrow = p, ncol = 3)

mug[,1] <- meanWine_1
mug[,2] <- meanWine_2
mug[,3] <- meanWine_3

sg <- array(0.0, dim = c(p,p,G))

sg[,,1] <- GWine_1

sg[,,2] <- GWine_2

sg[,,3] <- GWine_3

lab <- y
ns<-1

dfRW <- getOW(Xwine,as.numeric(y))
head(dfRW,6)

dfRWsort <- dfRW[order(-dfRW$Ftest),]


options(repr.plot.width=8, repr.plot.height=3)
ggplot(dfRWsort, aes(x = reorder(Var, +Ftest),y = Ftest) ) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_continuous(name="Variables") +
  scale_x_discrete(name="F score") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))


colnames(Xwine)
table(wine$Type)/length(wine$Type)
as.vector(as.vector(table(wine$Type)/length(wine$Type)))

alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)
vpi <- c(0.34,0.33,0.33)
vpi <- as.vector(as.vector(table(wine$Type)/length(wine$Type)))



eta_values <- c(5,10,15)
alpha_values <-c(0.75,0.8,0.85)

etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))



paste(eta, collapse = "_")

file_names <- list()
cont <- 1
for(i_a in 1:nrow(alphaM))
  for(i_eta in 1:nrow(etaM))
  {
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
   file_names[[cont]]<- paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Wine.Rdata")
#    file_names[[cont]]<- paste0("A",name_alpha,"_E",name_eta,"_Wine.Rdata")
    
    cont <- cont + 1
  }

str_detect(unlist(file_names), "A0.8_0.75_0.75_E10_15_10_Wine.Rdata")


tic("simulation")
for(i_a in 6:nrow(alphaM))
  for(i_eta in 2:nrow(etaM))
  {
    auxSim <- contDf (Xwine,y,lab,vpi,alphaM[i_a,],etaM[i_eta,],ptrain,ns = 10)
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    name_file <- paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Wine.Rdata")
    save(auxSim,file = name_file)
    cat("\n -- saving ", name_file,"---\n")
  }
toc()



sA80E_EQ_BAL <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

sA80E_EQ_BAL$Test[[1]]

dfAEQ_10_10_10 <- sA80E_EQ_BAL$Metrics_models
dfAEQ_10_10_10$alpha <- "Equal"
dfAEQ_10_10_10$eta <- "Equal"
saveRDS(dfAEQ_10_10_10,"AEQ_E10_10_10Wine.RDS")


alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,5)
ptrain <- c(0.7,0.7,0.7)


sA80E_10_10_5 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAE_E10_10_5 <- sA80E_10_10_5$Metrics_models
dfAE_E10_10_5$alpha <- "Equal"
dfAE_E10_10_5$eta <- "Inequal"
saveRDS(dfAE_E10_10_5,"AEQ_E10_10_5Wine.RDS")


alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,15)
ptrain <- c(0.7,0.7,0.7)


sA80E_10_10_5 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAE_E10_10_5 <- sA80E_10_10_5$Metrics_models
dfAE_E10_10_5$alpha <- "Equal"
dfAE_E10_10_5$eta <- "Inequal"
saveRDS(dfAEQ_E15_10_5,"AEQ_E10_10_5Wine.RDS")


alpha <- c(0.8,0.8,0.8)
eta <- c(10,5,10)
ptrain <- c(0.7,0.7,0.7)


sA80E_10_5_10 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAE_E10_5_10 <- sA80E_10_5_10$Metrics_models
dfAE_E10_5_10$alpha <- "Equal"
dfAE_E10_5_10$eta <- "Inequal"
saveRDS(dfAEQ_E15_5_10,"AEQ_E10_5_10Wine.RDS")



alpha <- c(0.8,0.8,0.8)
eta <- c(15,15,15)
ptrain <- c(0.7,0.7,0.7)


sA80E_15_15_15 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAE_E15_15_15 <- sA80E_15_15_15$Metrics_models
dfAE_E15_15_15$alpha <- "Equal"
dfAE_E15_15_15$eta <- "Inequal"
saveRDS(dfAEQ_E15_15_15,"AEQ_E15_15_15Wine.RDS")


alpha <- c(0.8,0.8,0.8)
eta <- c(15,10,5)
ptrain <- c(0.7,0.7,0.7)


sA80E_15_10_5 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAEQ_E15_10_5 <- sA80E_15_10_5$Metrics_models
dfAEQ_E15_10_5$alpha <- "Equal"
dfAEQ_E15_10_5$eta <- "Inequal"
saveRDS(dfAEQ_E15_10_5,"AEQ_E15_10_5Wine.RDS")


alpha <- c(0.8,0.8,0.8)
eta <- c(15,10,10)
ptrain <- c(0.7,0.7,0.7)


sA80E_15_10_10 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAEQ_E15_10_10 <- sA80E_15_10_10$Metrics_models
dfAEQ_E15_10_10$alpha <- "Equal"
dfAEQ_E15_10_10$eta <- "Inequal"
saveRDS(dfAEQ_E15_10_10,"AEQ_E15_10_10Wine.RDS")



alpha <- c(0.8,0.8,0.8)
eta <- c(15,5,10)
ptrain <- c(0.7,0.7,0.7)


sA80E_15_5_10 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAE_E15_5_10 <- sA80E_15_5_10$Metrics_models
dfAE_E15_5_10$alpha <- "Equal"
dfAE_E15_5_10$eta <- "Inequal"
saveRDS(dfAEQ_E15_5_10,"AEQ_E15_5_10Wine.RDS")


alpha <- c(0.8,0.8,0.8)
eta <- c(15,5,5)
ptrain <- c(0.7,0.7,0.7)


sA80E_15_5_5 <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAEQ_E15_5_5 <- sA80E_15_5_5$Metrics_models
dfAEQ_E15_5_5$alpha <- "Equal"
dfAEQ_E15_5_5$eta <- "Inequal"
saveRDS(dfAEQ_E15_5_5,"AEQ_E15_5_5Wine.RDS")




alpha <- c(0.8,0.8,0.8)
eta <- c(5,10,15)
ptrain <- c(0.7,0.7,0.7)


sA80E_5_10_15_BAL <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAEQ_E5_10_15 <- sA80E_5_10_15_BAL$Metrics_models
dfAEQ_E5_10_15$alpha <- "Equal"
dfAEQ_E5_10_15$eta <- "Inequal"
saveRDS(dfAEQ_E5_10_15,"AEQ_ENEQBALWine.RDS")



alpha <- c(0.9,0.85,0.8)
eta <- c(10,10,10)
ptrain <- c(0.7,0.7,0.7)

# run this line
sANEQ_BNEQ_BAL <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)

dfANEQ_BNEQ_BAL <- sANEQ_BNEQ_BAL$Metrics_models
dfANEQ_BNEQ_BAL$alpha <-"Inequal"
dfANEQ_BNEQ_BAL$eta <- "Equal"
saveRDS(dfANEQ_BNEQ_BAL,"ANEQ_EEQBALWine.RDS")



alpha <- c(0.8,0.8,0.8)
eta <- c(10,10,10)
ptrain <- c(0.8,0.8,0.8)


sA80E_NEQ_UNB <- contDf (Xwine,y,lab,vpi,alpha,eta,ptrain,ns = 10)
dfAEQ_EEQ_UNBAL <- sA80E_NEQ_UNB$Metrics_models
dfAEQ_EEQ_UNBAL$alpha <-"Equal"
dfAEQ_EEQ_UNBAL$eta <- "Equal"
saveRDS(dfAEQ_EEQ_UNBAL,"AEQ_EEQUNBALWine.RDS")



# Read RDS ----------------------------------------------------------------
dfAEQ_EEQBALWine <- readRDS(paste0(pathWd,"AEQ_EEQBALWine.RDS") )
dfAEQ_ENEQBALWine <- readRDS(paste0(pathWd,"AEQ_ENEQBALWine.RDS") )
dfANEQ_EEQBALWine <-readRDS(paste0(pathWd,"ANEQ_EEQBALWine.RDS"))
dfAEQ_EEQUNBALWine <-readRDS(paste0(pathWd,"AEQ_EEQUNBALWine.RDS"))


dfAll <- rbind.data.frame(dfAEQ_EEQBALWine,
                          dfAEQ_ENEQBALWine,
                          dfANEQ_EEQBALWine,
                          dfAEQ_EEQUNBALWine)

freqVar <- table(unlist(str_split(dfAll$Model,"-")))
coul <- brewer.pal(5,"Set2")
class(as.vector(freqVar))
dfVar <- data.frame(Variables = rownames(freqVar),
                    Frequency = as.vector(freqVar) )

head(dfVar)

options(repr.plot.width=8, repr.plot.height=3)
ggplot(dfVar, aes(x = reorder(Variables, +Frequency),y = Frequency) ) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_y_continuous(name="Number of times selected") +
  scale_x_discrete(name="Variables") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))




colnames(dfAll)
dfAll <- dfAll %>% mutate (DifCCR = CR_SV - CR_SatMC)
dfAll <- dfAll %>% mutate (DifAccuracy = Accuracy_SVCont- Accuracy_SatCont  )
nrow(dfAll)

dfAll <- na.omit(dfAll)
nrow(dfAll)


dfDifLong<- dfAll %>% dplyr::select(alpha,eta,proportion,DifCCR,DifAccuracy) %>% 
  pivot_longer(c(DifCCR,DifAccuracy),
               names_to = "Variables",
               values_to = "Dif")

head(dfDifLong)


dfDifLong <- dfDifLong %>% mutate(Variables = recode(Variables,
                                                     DifCCR = "CCR",
                                                     DifAccuracy = "Accuracy"))
dfAll %>% filter(alpha == "Equal")


ggplot(dfDifLong, aes(x = Variables, y = Dif, color = eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot()

ggplot(dfDifLong, aes(x = Variables, y = Dif, color = alpha)) +
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot()

ggplot(dfDifLong, aes(x = Variables, y = Dif, color = proportion)) +
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.2) + geom_boxplot()


auxTest <- sA80E_EQ_BAL$Test[[1]]
auxTrain <- sA80E_EQ_BAL$Train[[1]]
auxModel <- sA80E_EQ_BAL$Metrics_models[1,]

table(auxTest$class)
table(auxTest$Cont)

auxTest %>% filter(Cont == 1) %>% dplyr::select(class) %>% table()

colnames(auxTest)
colnames(auxTrain)

DfTrainX <- auxTrain %>% dplyr::select(-c(index,class))
DfTestX <- auxTest %>% dplyr::select(-c(index,class))
DfTrainl <- auxTrain$class
DfTestl <- auxTest$class

Selected_var <- unlist(str_split(auxModel$Model,"-"))
indcols <- str_detect(colnames(DfTrainX),Selected_var)

DfSVTrainX <- auxTrain %>% dplyr::select(Selected_var)
  
DfSVTestX <- auxTest %>% dplyr::select(Selected_var)

# model including all variables
saturated_mod  <- ModelAccuracy2(DfTrainX,
                                 DfTestX,
                                 as.numeric(DfTrainl),
                                 as.numeric(DfTestl),"EII",
                                 alpharef = 0.98, 
                                 tol = 0.01)
sat_mod_predv <- 1-sapply(1:51,function(j) { saturated_mod$predv[j,DfTestl[j]] })

table(auxTest$Cont, sat_mod_predv)
sum(sat_mod_predv==auxTest$Cont)/length(auxTest$Cont)

cbind(auxTest$Cont, sat_mod_predv)

# model including selected variables

SM <- ModelAccuracy2(DfSVTrainX,
                     DfSVTestX,
                     as.numeric(DfTrainl),
                     as.numeric(DfTestl),"EII",
                     alpharef = 0.98, 
                     tol = 0.01)

sv_predv <- 1-sapply(1:51,function(j) { SM$predv[j,DfTestl[j]] })

table(auxTest$Cont, sv_predv)
sum(sv_predv==auxTest$Cont)/length(auxTest$Cont)

cbind(auxTest$Cont, sv_predv)

SM$lpredv[[20]]
SM$predv
apply(SM$predv,1,max)
predv <-1-apply(SM$predv,1,max)

table(predv)
auxTest$Cont

sum(predv == auxTest$Cont)/length(auxTest$Cont)

table(auxTest$Cont)



modSV <-fHLvarSearch2(DfTrainX
                      ,DfTestX,RW,
                      as.numeric(DfTrainl),
                      as.numeric(DfTestl),"E",
                      alpharef =0.99,tol=0.01,epsilon = 0)

modSV$Selectedmodel
