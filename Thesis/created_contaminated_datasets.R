

source("create_paths.R")
setwd(path_wd)


source("Semisupervised.R")  
source("GSFile.R")
pathwd <- getwd()

nruns=5

#gen_cont_files <- function( nruns = 1)


gen_data <-vector("list", nruns)


rm(wine)
library(gclus)
data(wine)
colnames(wine)

data()

if(all(str_detect("Class", colnames(wine))) == FALSE )
  



# Wine
colnames(wine)
Xwine <- wine %>% subset(select = -c(Type))
colnames(Xwine)
y <- as.numeric(wine$Type)
p <- ncol(Xwine)
G <- length(unique(y))

lab <- 1:G
alpha_values <- c(0.70,0.8)
eta_values <- c(5,30)
vpi <-  as.vector(as.vector(table(wine$Type)/length(wine$Type)))
ptrain <- c(0.5,0.5,0.5)

alphaM <-as.matrix(expand.grid(alpha_values,alpha_values,alpha_values))
etaM <- as.matrix(expand.grid(eta_values,eta_values,eta_values))
nameDf <- "Wine"

pattern <- "A[\\d]+_[\\d]+_[\\d]+_E[\\d]+_[\\d]+_[\\d]+_Wine"

unique(wine$Type)
X <- wine %>% select(-Type)
colnames(X)


p = ncol(X)
name_file_time <- "wine_cont_execution_times"

time_df <- data.frame(File = character(), Time_Execution = numeric())

time_sim <- numeric()
 
aux_sigma <- array(0.0, dim = c(p , p, G) )
for(i_g in 1:G) aux_sigma[,,G] <- var(X[y == lab[i_g],]) 


# wdbc --------------------------------------------------------------------


data(wdbc)
Xwdbc <- wdbc %>% subset(select = -c(ID, Diagnosis))
X <-Xwdbc
colnames(X)
y <- as.numeric(wdbc$Diagnosis)
p <- ncol(Xwdbc)
G <- length(unique(y))


lab <- 1:G
alpha_values <- c(0.70,0.80)
eta_values <- c(5,15)
ptrain <- c(0.5,0.5)
vpi <- as.vector(as.vector(table(wdbc$Diagnosis)/length(wdbc$Diagnosis)))

etaM <- as.matrix(expand.grid(eta_values,eta_values))
alphaM <-as.matrix(expand.grid(alpha_values,alpha_values))
nameDf <- "Wdbc"

pattern <- "A[\\d]+_[\\d]+_E[\\d]+_[\\d]+_Wdbc"








