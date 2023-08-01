library(dplyr)
library(tidyr)
library(tidyverse)
library(gclus)
library(RColorBrewer)

pathFile <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Raw Data/"
pathWd <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/LDA_CMN/"
setwd(pathWd)
source("VSCMN.R")
pathOutput <- "E:/University of Glasgow/Literature review/R Code/Food Analysis/LDA_CMN/Proc_wdbc/"

data("wdbc")
colnames(wdbc)
Xwdbc <- wdbc %>% subset(select = -c(ID, Diagnosis))
colnames(Xwdbc)
y <- as.numeric(wdbc$Diagnosis)
G <- length(unique(y))
ind_1 <- y == 1  # Benign B
ind_2 <- y == 2  # Malign M


meanWdbc_1 <- Xwdbc[ind_1,] %>% apply(2,mean)
meanWdbc_2 <- Xwdbc[ind_2,] %>% apply(2,mean)

# p number of variables
p <- length(meanWdbc_1)

GWdbc_1 <- Xwdbc[ind_1,] %>% var
GWdbc_2 <- Xwdbc[ind_2,] %>% var

summary(factor(wdbc$Diagnosis))/nrow(wdbc)

mug <- matrix (0.0, nrow = p, ncol = G)

mug[,1] <- meanWdbc_1
mug[,2] <- meanWdbc_2

sg <- array(0.0, dim = c(p,p,G))

sg[,,1] <- GWdbc_1

sg[,,2] <- GWdbc_2


lab <- y
ns<-1

dfRW <- getOW(Xwdbc,as.numeric(y))
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


colnames(Xwdbc)


alpha_values <- c(0.75,0.8,0.85)
eta_values <- c(5,10,15)
ptrain <- c(0.7,0.7)
vpi <- as.vector(as.vector(table(wdbc$Diagnosis)/length(wdbc$Diagnosis)))


etaM <- as.matrix(expand.grid(eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values))
i_a <- 1
i_b <- 1
list_processed_files <- dir(pathOutput)
length(list_processed_files)

tic("simulation")
for(i_a in 1:nrow(alphaM))
  for(i_eta in 1:nrow(etaM))
  {
    
    name_alpha <- paste(alphaM[i_a,],collapse = "_")
    name_eta <- paste(etaM[i_eta,],collapse="_")
    name_file <- paste0(pathOutput,"A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Wdbc.Rdata")
    flag_existing_file <- str_detect(list_processed_files,paste0("A",str_replace_all(name_alpha,"\\.",""),"_E",name_eta,"_Wdbc.Rdata"))
    cat("\n File ",name_file, " processed status ", any(flag_existing_file == TRUE), "\n" )
    if (all(flag_existing_file == FALSE))
    {
      auxSim <- contDf (Xwdbc,y,lab,vpi,alphaM[i_a,],etaM[i_eta,],ptrain,ns = 10, iterations = 10)
      save(auxSim,file = name_file)
      cat("\n -- saving ", name_file,"---\n")
    }
        
  }
toc()


