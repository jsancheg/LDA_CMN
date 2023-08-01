# This file contaminate only the variable color in the contaminated 
# data set


library(dplyr)
library(tidyr)
library(tidyverse)
library(gclus)
library(RColorBrewer)
library(tictoc)
library(corrplot)
library(plotly)



pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")
source("http://www.sthda.com/upload/rquery_cormat.r")

pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
pathProWine <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_WineContColor/"

data("wine")
colnames(wine)
table(wine$Type)

Xwine <- wine %>% subset(select = -c(Type))
rquery.cormat(Xwine)

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
vpi <- as.vector(as.vector(table(wine$Type)/length(wine$Type)))



eta_values <- c(5,10,15)
alpha_values <-c(0.75,0.8,0.85)

X <- wine %>% subset(select = -c(Type))
colnames(Xwine)
y <- as.numeric(wine$Type)
G <- length(unique(y))
ns <- 1

SVmodel <- list() 
Train_subset <- list()
Test_subset <- list()

AccuracyClassSV <- rep(0,ns)
AccuracyContSV <-rep(0,ns)
AccuracyClassSatM_C <- rep(0,ns)
AccuracyContSatM_C <- rep(0,ns)
AccuracyClassSatM_Nc <- rep(0,ns)
sensitivity_SatM <- rep(0,ns)
specificity_SatM <- rep(0,ns)
sensitivity_SelM <- rep(0,ns)
specificity_SelM <- rep(0,ns)
G <- length(unique(y))
ncont <- rep (0,G)
nocont <- rep(0,G)
nocont_train <- rep(0,G)
nocont_test <- rep(0,G)
ncont_train <- rep(0,G)
ncont_test <- rep(0,G)
ntrain <- rep(0,G)
ntest <- rep(0,G)
ng <- rep(0,G)
indsamples <- funcSample(X,y,vpi)
p <- ncol(X)
mug <- matrix(0.0,nrow = p, ncol = G)
sg <- array(0.0, dim = c(p,p,G))

for (g in 1:G) 
{
  mug[,g] <- X[y==g,] %>% apply(2,mean)
  sg[,,g] <- X[y == g,] %>% var
  ng[g] <- length(indsamples[[g]])
}




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

unlist(file_names)



#BlueCrabsCont$sex <- ifelse(BlueCrabsCont$sex == "F",1,2)
for (g in 1:G)
{
  nocont_train[g] = round(ng[g] * ptrain[g],0) 
  nocont_test[g] = ng[g] - nocont_train[g]
  
  # number of contaminated samples in the train set
  ncont_train[g] = round(nocont_train[g]/alpha[g],0) - nocont_train[g]
  
  # number of contaminated samples in the test set
  ncont_test[g] = round(nocont_test[g]/alpha[g],0) - nocont_test[g]
  
  nocont[g] = nocont_train[g] + nocont_test[g]
  
  # number of contaminated observations to be simulated
  ncont[g] = ncont_train[g] + ncont_test[g]
  
  # train size
  ntrain[g] = nocont_train[g] + ncont_train[g]
  
  # test set for each sex
  ntest[g]= nocont_test[g] + ncont_test[g]
  
}

ncontg = rep(0,G)
contSamples <- vector("list",length = G)
# check parameter ncont
if (class(ncont) == "numeric")
{
  if(G == 1 & length(ncont) == 1) {
    ncontg[1] <- ncont 
  }else if (G>1 & length(ncont) == 1) {
    ncontg <- rep(ncont,G)
  } else if(G>1 & length(ncont) == G){
    ncontg <- ncont
  } else if (G > 1 & length(ncont )!= G & length(ncont) > 1){
    stop("Error vector with the number of contaminated samples has different dimension to the number of groups")
  }
}

nrow(alphaM)
nrow(etaM)

# Unequal
rand_alpha <- sample(1:nrow(alphaM),4,replace = FALSE)
# 25, 15, 20, 23
rand_eta <- sample(1:nrow(etaM),4,replace = FALSE)
ns <- 10
# 6, 25, 11, 5
  
rand_alpha <- c(1,14,27)
rand_eta <- c(1,14,27)
etaM
tic("simulation")

for(i_a in rand_alpha)
  for(i_eta in rand_eta)
  {
    
    #    auxSim <- contDf (Xwine,y,lab,vpi,alphaM[i_a,],etaM[i_eta,],ptrain,ns = 10)
    indnc <- vector("list",G)
    indc <- vector("list",G)
    Xgnc_train <- vector("list",G)
    Xgnc_test <- vector("list",G)
    Xgc_train <- vector("list",G)
    Xgc_test <- vector("list",G)
    iterations <- 10
    i<-1    
    
    contSamples <- vector("list",length = G)
    
    for (i in 1:ns)
    {
      
        if(G >1)
          {
            for( g in 1:G)
            {
                  SimSamples =  rMVNorm(ncontg[g],mug[,g],sg[,,g])
                  aux =  rnorm(ncontg[g],mug[10,g],etaM[i_eta]*sg[10,10,g] )
                  SimSamples[,10] <- aux
                  contSamples[[g]] = SimSamples
        
            }
      
          }else if(G == 1) 
          {
            SimSamples =  rMVNorm(ncontg[g],mug[,g],sg[,,g])
            aux =  rnorm(ncontg[g],mug[10,g],etaM[i_eta]*sg[10,10] )
            SimSamples[,10] <- aux
            contSamples[[g]] = SimSamples
      
          }
    
            
      
            XC <- ldply(contSamples)
            vlab <- rep(1:G,c(ncontg))
            if(length(1:G) == length(ncontg))
            {
                XC <- XC %>% mutate(class = vlab)%>% dplyr::select(class, everything())
      #  XC$class <- rep(lab,ncontg)
            } else stop("Error length of vector of contaminated samples differ with the number of groups")
    
            cat("\n simulation = ", i, "\n")
            GenContSamples <- SimCont(mug,sg,1:G,ncont,eta)
            GenContSamples$index <- (nrow(X)+1):(nrow(X) +nrow(GenContSamples) )
            GenContSamples <- GenContSamples %>% dplyr::select(index, everything())
            colnames(GenContSamples)
            ncolumns <- ncol(GenContSamples)
            colnames(GenContSamples)[3:ncolumns] <-  colnames(X)
            GenContSamples$Cont <- 1
            colnames(GenContSamples)
            head(GenContSamples)
            
            # Generate a set with the composition required for each group  
            auxindnc <- funcSample(X,y,vpi)
            winedf <- data.frame(X)
            winedf$Cont <- 0
            winedf$class <- y
            winedf$index <- 1:nrow(X)
            winedf <- winedf %>% dplyr::select(index,class, everything())
            colnames(winedf)
            
            # Generate contaminated observation with the sane group composition used 
            # in non contaminated set
            auxindc <- funcSample(GenContSamples[,-1], GenContSamples$class, vpi)
            #    indsampleTrain_nc <- vector("list",G)
            GenContSamples$class
            
            for (g in 1:G)
            {
              # non contaminated set
              auxDfg <- winedf %>% filter(class == g)
              subsetg <- auxDfg[auxindnc[[g]],]
              Xgnc_train[[g]] <- subsetg %>% slice_sample(n=nocont_train[g], replace = FALSE)
              
              indsampleTrain_nc <- Xgnc_train[[g]]$index
              indsampleTest_nc <- setdiff(subsetg$index,indsampleTrain_nc)
              
              Xgnc_test[[g]] <-   subsetg %>% filter(index %in% indsampleTest_nc)
              
              
              # contaminated
              auxDfcont <- GenContSamples %>% filter (class == g)
              subsetgcont <- auxDfcont[auxindc[[g]],]
              
              Xgc_train[[g]] <- subsetgcont %>% slice_sample(n = ncont_train[g], replace = FALSE)
              
              indsampleTrain_c <- Xgc_train[[g]]$index
              indsampleTest_c <- setdiff(subsetgcont$index,indsampleTrain_c)
              
              Xgc_test[[g]] <- subsetgcont %>% filter(index %in% indsampleTest_c)
              
            }
            nrow(winedf)
            
            colnames(GenContSamples)
            table(GenContSamples$Cont)
            table(winedf$Cont)
            
            table(GenContSamples$class)
            table(winedf$class)
            
            # getting rid off index column    
            WineCont <- rbind.data.frame(winedf %>% dplyr::select(-index),
                                         GenContSamples %>% dplyr::select(-index))
            colnames(WineCont)
            nrow(WineCont)
            
            auxTrain_nc <- ldply(Xgnc_train)
            auxTest_nc <- ldply(Xgnc_test)
            indnc_train <- auxTrain_nc$index
            
            table(auxTrain_nc$class)
            table(auxTest_nc$class)
            
            auxTrain_c <- ldply(Xgc_train)
            auxTest_c <- ldply(Xgc_test)
            indc_train <- auxTrain_c$index
            
            auxTrain_c$class
            
            #  DfTrain <- rbind.data.frame(winedf[indnc_train,]%>% dplyr::select(-index),
            #                                 GenContSamples[unlist(indc_train),] %>% dplyr::select(-index))
            
            
            # apply(Xgnc_train[[3]],2,function(x) any(is.na(x)))
            # apply(Xgc_train[[3]],2,function(x) any(is.na(x)))
            
            # apply(ldply(Xgnc_train),2,function(x) any(is.na(x)))
            
            # apply(ldply(Xgc_train),2,function(x) any(is.na(x)))
            Xgc_train[[2]]$class  
            
            dfTrain <- rbind.data.frame(ldply(Xgnc_train),ldply(Xgc_train))
            
            apply(dfTrain,2,function(x) any(is.na(x)))
            dfTrain$class
            dfTrain$Cont
            
            colnames(dfTrain)
            DfTrain <- dfTrain[sample(1:nrow(dfTrain)),]
            apply(dfTrain,2,function(x) any(is.na(x)))
            
            table(DfTrain$Cont)
            table(DfTrain$class)
            DfTrain$class
            
            apply(DfTrain,2,function(x) any(is.na(x)))
            
            dfTest <- rbind.data.frame(ldply(Xgnc_test),ldply(Xgc_test))
            
            # DfTest <- rbind.data.frame(winedf[-indnc_train,-2], 
            #                              GenContSamples[-indc_train,-2])
            
            colnames(dfTest)
            
            DfTest <- dfTest[sample(1:nrow(dfTest)),]
            table(DfTest$Cont)
            table(DfTest$class)    
            
            colnames(DfTrain)
            DfTrainX <- DfTrain %>% dplyr::select(-c(class,index,Cont))
            DfTrainl <- DfTrain$class
            DfTestX <- DfTest %>% dplyr::select(-c(class,index,Cont))
            DfTestl <- DfTest$class
            Train_subset[[i]] <-DfTrain
            Test_subset[[i]] <- DfTest
            #  SexTrain <- ifelse(BlueCrabsTrain$sex == 1, "F","M")
            #  SexTrain <- factor(SexTrain)
            ContTrain <- ifelse(DfTrain$Cont == 0, "NC","C")
            ContTrain <- factor(ContTrain)
            
            #SexTest <- ifelse(BlueCrabsTest$sex == 1, "F", "M")
            ContTest <- ifelse(DfTest$Cont == 0, "NC","C")
            
            
            colnames(DfTrain)
            colnames(DfTrainX)
            
            dfRW <- getOW(DfTrainX,DfTrainl)
            RW <- dfRW$Var
            variables_saturated_model <- RW
            
            # model including all variables
            saturated_mod  <- ModelAccuracy2(DfTrainX,
                                             DfTestX,
                                             as.numeric(DfTrainl),
                                             as.numeric(DfTestl),"EII",
                                             alpharef = 0.98, 
                                             tol = 0.01, iterations = iterations)
            
            saturated_mod
            AccuracyClassSatM_Nc[i] <- saturated_mod$accTestNc
            AccuracyClassSatM_C[i] <- saturated_mod$accTestC
            
            saturated_mod$accTestNc
            
            #    auxTestCont <- rep(0,length(DfTestl))
            # 1 : means contaminated sample
            # 0 : means non contaminated sample
            
            # need to substract saturated_mod$predv  prediction of v from 1 to change
            # 0 : means no contaminated sample
            # 1 : means contaminated sample
            pred_cont_Sat_M <- 1-sapply(1:length(DfTestl),function(j) { saturated_mod$predv[j,DfTestl[j]] })
            
            #    for (j in 1:length(auxTestCont))
            #    {
            #      auxTestCont[j] <- 1-saturated_mod$predv[j,DfTestl[j]]
            #    }
            
            AccuracyContSatM_C[i] <-sum(pred_cont_Sat_M == DfTest$Cont)/length(DfTest$Cont)
            
            # model including selected variables
            modSV <-fHLvarSearch2(DfTrainX
                                  ,DfTestX,RW,
                                  as.numeric(DfTrainl),
                                  as.numeric(DfTestl),"E",
                                  alpharef =0.99,tol=0.01,epsilon = 0,
                                  iterations = iterations)
            
            SVmodel[[i]] <- modSV$Selectedmodel
            AccuracyClassSV[i] <-  modSV$Accuracy
            
            modSV$posCM
            #    TestContSV <- rep(0,length(DfTestl))
            
            pred_cont_Sel_M <- 1-sapply(1:length(DfTestl),function(j) { modSV$models[[modSV$posCM]]$predv[j,DfTestl[j]] })
            
            #    for (j in 1:length(TestContSV))
            #    {
            #       TestContSV[j] <- 1- modSV$models[[modSV$posCM]]$predv[j,DfTestl[j]]
            #    }
            AccuracyContSV[i] <- sum(pred_cont_Sel_M == DfTest$Cont)/ length(DfTest$Cont)
            
            sat_mod_cfm <- matrix(0,ncol = 2, nrow = 2)
            sel_mod_cfm <- matrix(0,ncol = 2, nrow = 2)
            
            if (all(pred_cont_Sat_M == 1))
            {
              specificity_SatM[i] <- 0
              Sensitivity_SatM[i] <- sum(DfTest$Cont==1)/sum(pred_cont_Sat_M==1)
              
            }else if(all(pred_cont_Sat_M == 0))
            {
              specificity_SatM[i] <- sum(DfTest$Cont==0)/sum(pred_cont_Sat_M==0)
              sensitivity_SatM[i] <- 0
              
            }else {
              sat_mod_cfm <- table(DfTest$Cont,pred_cont_Sat_M)
              sensitivity_SatM[i] <- sensitivity(sat_mod_cfm)
              specificity_SatM[i] <- specificity(sat_mod_cfm)
              
            }
            
            if (all(pred_cont_Sel_M == 1))
            {
              specificity_SelM[i] <- 0
              Sensitivity_SelM[i] <- sum(DfTest$Cont==1)/sum(pred_cont_Sel_M==1)
              
            }else if(all(pred_cont_Sel_M == 0))
            {
              
              specificity_SelM[i] <- sum(DfTest$Cont==0)/sum(pred_cont_Sel_M==0)
              sensitivity_SelM[i] <- 0
              
            }else {
              sel_mod_cfm <- table(DfTest$Cont,pred_cont_Sel_M)
              
              
              sensitivity_SelM[i] <- sensitivity(sel_mod_cfm)
              specificity_SelM[i] <- specificity(sel_mod_cfm)
              
            }
            
    }
          SVmodel1 <- lapply(SVmodel, function(i) paste(unlist(i),collapse = "-"))
          SVmodel1 <- ldply(SVmodel1)
          colnames(SVmodel1) <- "Model"
          SVmodel1
    
          aux_df <- data.frame(CR_SatMNc = AccuracyClassSatM_Nc,
                         CR_SatMC = AccuracyClassSatM_C,
                         Accuracy_SatCont = AccuracyContSatM_C,
                         CR_SV = AccuracyClassSV,
                         Accuracy_SVCont = AccuracyContSV,
                         Sensitivity_SatM = sensitivity_SatM,
                         Specificity_SatM = specificity_SatM,
                         Sensitivity_SelM = sensitivity_SelM,
                         Specificity_SelM = specificity_SelM)
    
           metrics_res <- aux_df %>% 
           summarise(Accuracy_SatMNc = mean(CR_SatMNc),
                Accuracy_SatMC = mean(CR_SatMC),
                Accuracy_SatCont = mean(Accuracy_SatCont),
                Accuracy_SV = mean(CR_SV),
                Accuracy_SVCont = mean(Accuracy_SVCont),
                Sensitivity_SatM = mean(Sensitivity_SatM),
                Specificity_SatM = mean(Specificity_SatM),
                Sensitivity_SelM = mean(Sensitivity_SelM),
                Specificity_SelM = mean(Specificity_SelM))
            
            df_resumen <- cbind.data.frame(SVmodel1,aux_df)
    
            output <-list ( Metrics_res = metrics_res, 
                    Metrics_models = df_resumen,
                    Train = Train_subset,
                    Test = Test_subset)
    
            name_eta <- paste(etaM[i_eta,],collapse="_")
            name_alpha <- paste(alphaM[i_a,],collapse = "_")
            name_file <- paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_ColorWine.Rdata")
            save(output,file = name_file)
            cat("\n -- saving ", name_file,"---\n")
            
    
            
  }
toc()


dir(pathProWine)


name_files <- dir(pathProWine)[!is.na(str_match(dir(pathProWine),"A[\\d]+_[\\d]+_[\\d]+_E[\\d]+_[\\d]+_[\\d]+_ColorWine"))]


name_files
Metrics_Res <- list()
Metrics_Models <- list()


length(name_files)

aux <- list()
cont <- 1
for (i_file in name_files)
{  
  load(paste0(pathProWine,i_file))
  output$Metrics_res
  output$Metrics_models

  auxDf <- output$Metrics_models
  auxDf$file <- i_file
  auxDf <- auxDf %>% dplyr::select(file,everything())
  parameters <- str_split(str_replace_all(auxDf$file,"[(A-Z)|(a-z)|\\.]",""),"_",simplify = TRUE)[,1:6]
  auxDf$parameters <- parameters
  auxDf$Alpha <- ifelse(parameters[,1] == parameters[,2] & parameters[,2] == parameters[,3],"Equal","Inequal")
  auxDf$Eta <- ifelse(parameters[,4] == parameters[,5] & parameters[,5] == parameters[,6],"Equal","Inequal")
  
  aux[[cont]] <- auxDf
  cont <- cont + 1
  
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
ggp %>% ggplotly



#metrics_res %>% filter(Model %in% c("BD-CL-FL-CW-RW","CL-RW",
#                                    "BD-CL-RW", "BD-CL-CW-RW",
#                                    "FL-CL-RW",
#                                    "CL-CW-FL-RW",
#                                    "BD-CL-FL-RW"))




freqModelSize <- table(ModelSize)
freqModelSize  
coul <- brewer.pal(9,"Set3")

par(mfrow = c(1,1))
barplot(prop.table(freqModelSize),col = coul,ylim = c(0,0.4))


freqVar <- table(unlist(str_split(dfAll$Model,"-")))
nb.cols <- 13
coul <- colorRampPalette(brewer.pal(8,"Set2"))(nb.cols) 


sort(round(prop.table(freqVar)*100,2),decreasing = TRUE)

barplot(prop.table(freqVar),col = coul, ylim = c(0,0.4))

par(mfrow = c(1,1))


df_freqSV <- data.frame(frequency = as.vector(freqVar),
                        Variables = rownames(freqVar))


ggp <- ggplot(df_freqSV, 
              aes(x = reorder(Variables,+frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip()+
  ylab("Variables") + xlab("Frequency") +
  geom_text(aes(x = Variables, y = frequency + 0.3, 
                label = frequency),check_overlap = TRUE)
ggp

head(df_models)
ggp %>% ggplotly


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
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)

g1


g3 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Alpha)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() + 
  geom_hline(yintercept = 0)


g3


g4 <- ggplot(dfDifLong %>% filter(Variables %in% c("CCR","Accuracy")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-0.2,0.3) + geom_boxplot() +
  geom_hline(yintercept = 0)
  

g4 

g6 <- ggplot(dfDifLong %>% filter(Variables %in% c("Sensitivity","Specificity")), 
             aes(x = Variables, y = Dif, color = Eta)) + 
  ylab("Dif. Sel. var - All var.")+ xlab("Metric")+ylim(-1,1) + geom_boxplot() +
  geom_hline(yintercept = 0)

g6

plot(g1)

plot(g2)

plot(g3)

plot(g4)



colnames(dfAll)
colnames(dfDifLong)
head(unique(dfAll$Model))

dfAll_Color <- dfAll
dfAll_Color$ContainColor <- as.numeric(str_detect(dfAll_Color$Model,"Color"))
colnames(dfAll_Color)

dfDifLong_Color<- dfAll_Color %>% dplyr::select(Alpha,Eta,ContainColor,DifCCR,DifAccuracy,
                                    DifSensitivity,DifSpecificity) %>% 
  pivot_longer(c(DifCCR,DifAccuracy,DifSensitivity,DifSpecificity),
               names_to = "Variables",
               values_to = "Dif")

colnames(dfDifLong_Color)
dfAll_Color$ContainColor <- factor(dfAll_Color$ContainColor)
summary(dfAll_Color$ContainColor)

dfAll_Color %>% group_by(ContainColor) %>% summarise(mean = mean(CR_SV))

dfAll_Color %>% group_by(ContainColor) %>% summarise(mean = mean(CR_SatMC))

dfAll_Color %>% group_by(ContainColor) %>% summarise(mean = mean(CR_SV - CR_SatMC))


