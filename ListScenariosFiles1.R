<<<<<<< HEAD

# Windows path
system_info <- Sys.info()
#OS_name <- system_info("")
pc_name <- system_info['modename'] 

if(pc_name == "LAPTOP-ADR3M911")
{
  pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
  pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
  pathSFiles <- "E:/University of Glasgow/Thesis/SFiles/"
  
}else if(pc_name == "WildFree")
{
  pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
  pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
  pathSFiles <- "/home/jsancheg/Documents/SFiles/"
  
} else
{
  pathScenarios <- "M:/Scenarios/"
  pathSSFiles <- "M:/SSFiles/"
  pathSFiles <- "M:/SFiles/"
}

=======
# Get system information
system_info <- Sys.info()
system_info

operative_system <- system_info["sysname"]
nodename <- system_info["nodename"]

if(operative_system == "Linux" & nodename == "WildFree") {
  pathScenarios <- "/home/jsancheg/Documents/Scenarios/"
  pathSSFiles <- "/home/jsancheg/Documents/SSFiles/"
  pathSSFiles <- "/home/jsancheg/Documents/SFiles/"
}else{
  pathScenarios <-"/home/jsancheg/git_environment/LDA_CMN/"
  pathScenarios <- "E:/University of Glasgow/Thesis/Scenarios/"
  pathSSFiles <- "E:/University of Glasgow/Thesis/SSFiles/"
}
>>>>>>> d56d2f130255c2b7e2879f93b8b6c7222476c15c

F1 <- c("VD","MD","VO") # F1 : Mean distance
F2 <- c(2,3)            # F2 : Number of classes
F3 <- c("BAL","INB")    # F3 : class proportion matrix with the number of columns equals to F2
F4 <- c(3000,4000)      # F4 : Number of observations
F5 <- c(5,100)          # F5 : Number of variables
F6 <- c(0.75,0.85)      # F6 : Percentage of samples used as training
F7 <- c("SCBSV","SCBNSV","SCBSNSV","IND") # F7 : Correlation structure
F8 <- c(0.8,0.9)        # F8 : Percentage of non contaminated samples (alpha)
F9 <- c(20,30)          # F9 : Variance inflation factor
F10 <- c(2,3)           # F10: number of separating variables

# posibilities
# possibilities for 2 classes
Sets2 <- as.matrix(expand.grid(2,F10,F5,F4,F3,F1,F7,F6,F8,F8,0,F9,F9,0))
# possibilities for 2 classes
Sets3 <- as.matrix(expand.grid(3,F10,F5,F4,F3,F1,F7,F6,F8,F8,F8,F9,F9,F9))
n2 <- nrow(Sets2)
n3 <- nrow(Sets3)
Sets <- rbind(Sets2,Sets3)
n <- nrow(Sets)

Scenarios <- sapply(1:n,function(i) {
  # prefix file: "S_(F2)Sets[i,1]_(F10)S_Sets[i,2]_(F5)S_Sets[i,3]_(F4)S_Sets[i,4]_
  #               (F3)S_Sets[i,8]_(F1)Sets[i,5]_(F7)Sets[i,7]_(F6)Sets[i,6]_(F8.1)Sets[i,9]_
  #               (F8.2)Sets[i,10]_(F8.3)Sets[i,11]_(F9.1)Sets[i,12]_(F9.2)Sets[i,13]_
  #               (F9.3)Sets[i,14]"
  #             "(1)S_(2)NumberofClases_(3)NumberofSeparatingVariables_(4)NumberofVariables_
  #               (5)NumberofObservations_(6)ClassPorportion_(7)MeanDistance_
  #               (8)CorrelationStructure_(9)PercentageTraining_(10)Alpha1_
  #               (11)Alpha2_(12)Alpha3_(13)Eta1_(14)Eta2_(15)Eta3
  # 1) S from Scenario
  # 2) Sets[i,1](F2): Number of classes
  # 3) Sets[i,2](F10): Number of separating variables
  # 4) Sets[i,3](F5): Number of variables
  # 5) Sets[i,4](F4): Number of observations
  # 6) Sets[i,8](F3): Class proportion
  # 7) Sets[i,5](F1): Mean distance
  # 8) Sets[i,7](F7): Correlation structure
  # 9) Sets[i,6](F6): Pecerntage of samples used in training
  # 10) Sets[i,9](F8.1): alpha for the 1st group
  # 11) Sets[i,10](F8.2): alpha for the 2nd group
  # 12) Sets[i,11](F8.3): alpha for the 3rd group
  # 13) Sets[i,12](F9.1): Eta for the 1st group
  # 14) Sets[i,13](F9.2): Eta for the 2nd group
  # 15) Sets[i,14](F9.3): Eta for the 3rd group
  
  
  
  
  if( as.numeric(Sets[i,1]) == 2) # Number of classes
  {
    paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
           as.numeric(Sets[i,3]),"_",
           as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
           Sets[i,5],"_",Sets[i,7],"_",
           Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
           as.numeric(Sets[i,10])*100,"_E",
           as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),"_10.RDS")
    
  }else if (as.numeric(Sets[i,1]) == 3)
  {
    paste0("S_",as.numeric(Sets[i,1]),"_",as.numeric(Sets[i,2]),"_",
           as.numeric(Sets[i,3]),"_",
           as.numeric(Sets[i,4]),"_",as.numeric(Sets[i,8])*100,"_",
           Sets[i,5],"_",Sets[i,7],"_",
           Sets[i,6],"_A",as.numeric(Sets[i,9])*100, 
           as.numeric(Sets[i,10])*100,as.numeric(Sets[i,11])*100,
           "_E",as.numeric(Sets[i,12]), as.numeric(Sets[i,13]),
           as.numeric(Sets[i,14]),"_10.RDS")
  }
  
})


ind2.5vars  <- sapply(Scenarios , function(x)
{
  aux <- str_split_1(x,"_")
  ind2.5 <- (as.numeric(aux[2]) == 2) & (as.numeric(aux[4]) == 5)
  return(ind2.5)
})

ind2.100vars <- sapply(Scenarios , function(x)
{
  aux <- str_split_1(x,"_")
  ind2.5 <- (as.numeric(aux[2]) == 2) & (as.numeric(aux[4]) == 100)
  return(ind2.5)
})


Scenarios2.5 <- Scenarios[ind2.5vars]
Scenarios2.100 <- Scenarios[ind2.100vars]

n2.5 <- length(Scenarios2.5)
n2.100 <- length(Scenarios2.100)

n2.5
n2.100


Scenarios
length(Scenarios)




ind5vars <- sapply(Scenarios , function(x)
{
  aux <- str_split_1(x,"_")
  ind5 <- as.numeric(aux[4]) == 5
  return(ind5)
})

ind100vars <- !ind5vars


Scenarios5 <- Scenarios[ind5vars]
Scenarios100 <- Scenarios[ind100vars]


n5 <- length(Scenarios5)
n100 <- length(Scenarios100)

n5.1 <- floor(n5*1/10)
n5.2 <- floor(n5*2/10)
n5.3 <- floor(n5*3/10)
n5.4 <- floor(n5*4/10)
n5.5 <- floor(n5*5/10)
n5.6 <- floor(n5*6/10)
n5.7 <- floor(n5*7/10)
n5.8 <- floor(n5*8/10)
n5.9 <- floor(n5*9/10)

n100.1 <- floor(n100*1/10)
n100.2 <- floor(n100*2/10)
n100.3 <- floor(n100*3/10)
n100.4 <- floor(n100*4/10)
n100.5 <- floor(n100*5/10)
n100.6 <- floor(n100*6/10)
n100.7 <- floor(n100*7/10)
n100.8 <- floor(n100*8/10)
n100.9 <- floor(n100*9/10)



